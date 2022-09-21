const std = @import("std");
const Allocator = std.mem.Allocator;
const big_int = std.math.big.int;

const util = @import("../util.zig");
const Value = @This();

pub const Error = error{
    InvalidLiteral,
    InvalidOperation,
} || Allocator.Error;

pub const Register = enum {
    a,
    af,
    @"af'",
    b,
    bc,
    c,
    d,
    de,
    e,
    f,
    h,
    hl,
    i,
    ix,
    ixh,
    ixl,
    iy,
    iyh,
    iyl,
    l,
    m,
    nc,
    nz,
    p,
    pe,
    po,
    r,
    sp,
    z,
};

const Storage = packed union {
    comptime {
        std.debug.assert(@sizeOf(Storage) == @sizeOf(usize));
    }

    const Small = packed struct(usize) {
        const Integer = std.meta.Int(.signed, @bitSizeOf(usize) - 1 - 2);

        tag: enum(u1) { big, small } = .small,
        base: enum(u2) { none, register, ix, iy },
        integer: Integer,
    };
    const Big = struct {
        comptime {
            std.debug.assert(@alignOf(Big) >= 2);
        }

        base: ?Register,
        offset: big_int.Managed,
    };

    small: Small,
    big: *Big,
};
const Contents = union(enum) {
    absolute: Storage.Small.Integer,
    register: Register,
    ix: Storage.Small.Integer,
    iy: Storage.Small.Integer,
    big: *Storage.Big,
};

storage: Storage,

fn toContents(self: Value) Contents {
    return switch (self.storage.small.tag) {
        .small => switch (self.storage.small.base) {
            .none => .{ .absolute = self.storage.small.integer },
            .register => .{ .register = @intToEnum(Register, self.storage.small.integer) },
            .ix => .{ .ix = self.storage.small.integer },
            .iy => .{ .iy = self.storage.small.integer },
        },
        .big => .{ .big = self.storage.big },
    };
}
fn fromContents(contents: Contents) Value {
    return .{ .storage = switch (contents) {
        .absolute => |absolute| .{ .small = .{ .base = .none, .integer = absolute } },
        .register => |register| .{ .small = .{
            .base = .register,
            .integer = @enumToInt(register),
        } },
        .ix, .iy => |offset| storage: {
            std.debug.assert(offset != 0);
            break :storage .{ .small = .{ .tag = .small, .base = switch (contents) {
                else => unreachable,
                .ix => .ix,
                .iy => .iy,
            }, .integer = offset } };
        },
        .big => |big| .{ .big = big },
    } };
}

pub fn init(allocator: Allocator, base: ?Register, offset: anytype) Allocator.Error!Value {
    const Small = Storage.Small.Integer;

    const offset_type = switch (@typeInfo(@TypeOf(offset))) {
        .Pointer => |pointer| pointer.child,
        else => @TypeOf(offset),
    };
    const offset_info = @typeInfo(offset_type);
    const small_offset = switch (offset_info) {
        .Int, .ComptimeInt => util.cast(Small, offset),
        .Struct => offset.to(Small) catch null,
        else => unreachable,
    };

    if (base) |register| switch (register) {
        else => if (small_offset) |small| if (small == 0) return fromContents(.{ .register = register }),
        .ix, .iy => if (small_offset) |small| return fromContents(switch (register) {
            .ix => .{ .ix = small },
            .iy => .{ .iy = small },
            else => unreachable,
        }),
    } else if (small_offset) |small| return fromContents(.{ .absolute = small });

    const big = try allocator.create(Storage.Big);
    errdefer allocator.destroy(big);

    big.base = base;
    big.offset = try switch (offset_info) {
        .Int, .ComptimeInt => big_int.Managed.initCapacity(allocator, big_int.calcLimbLen(offset)),
        .Struct => if (@hasDecl(offset_type, "toManaged"))
            offset.toManaged(allocator)
        else
            offset.cloneWithDifferentAllocator(allocator),
        else => unreachable,
    };
    errdefer big.offset.deinit();
    switch (offset_info) {
        .Int, .ComptimeInt => try big.offset.set(offset),
        else => {},
    }

    return fromContents(.{ .big = big });
}
pub fn parseLiteral(allocator: Allocator, literal: []const u8) Error!Value {
    const len = literal.len;
    if (len == 0) return Error.InvalidLiteral;
    var base: u8 = undefined;
    var number: []const u8 = undefined;
    switch (literal[0]) {
        '"', '\'' => std.debug.todo("string literal"),
        '$' => {
            base = 16;
            number = literal[1..];
        },
        '%' => {
            base = 2;
            number = literal[1..];
        },
        '@' => {
            base = 8;
            number = literal[1..];
        },
        else => switch (literal[len - 1]) {
            'B', 'b' => {
                base = 2;
                number = literal[0 .. len - 1];
            },
            'H', 'h' => {
                base = 16;
                number = literal[0 .. len - 1];
            },
            'O', 'o' => {
                base = 8;
                number = literal[0 .. len - 1];
            },
            else => {
                base = 10;
                number = literal;
            },
        },
    }

    var value = try big_int.Managed.initCapacity(
        allocator,
        big_int.calcSetStringLimbCount(base, number.len),
    );
    defer value.deinit();
    value.setString(base, number) catch |err| return switch (err) {
        error.InvalidBase => unreachable,
        error.InvalidCharacter => Error.InvalidLiteral,
        else => @errSetCast(Error, err),
    };

    return init(allocator, null, value);
}
pub fn deinit(self: Value) void {
    switch (self.toContents()) {
        else => {},
        .big => |big| {
            const allocator = big.offset.allocator;
            big.offset.deinit();
            allocator.destroy(big);
        },
    }
}

pub fn getBase(self: Value) ?Register {
    return switch (self.toContents()) {
        .absolute => null,
        .register => |register| register,
        .ix => .ix,
        .iy => .iy,
        .big => |big| big.base,
    };
}
pub fn getOffset(self: Value) union(enum) { small: Storage.Small.Integer, big: big_int.Const } {
    return switch (self.toContents()) {
        .absolute => |absolute| .{ .small = absolute },
        .register => .{ .small = 0 },
        .ix, .iy => |offset| .{ .small = offset },
        .big => |big| .{ .big = big.offset.toConst() },
    };
}
pub fn zeroOffset(self: Value) bool {
    return switch (self.getOffset()) {
        .small => |small| small == 0,
        .big => |big| big.eqZero(),
    };
}

pub fn isImmediate(self: Value) bool {
    return self.getBase() == null;
}
pub fn isRegister(self: Value) bool {
    return self.getBase() != null and self.zeroOffset();
}
pub fn isRegisterOffset(self: Value) bool {
    return self.getBase() != null;
}

pub fn add(lhs: Value, rhs: Value, allocator: Allocator) Error!Value {
    if (lhs.getBase() != null and rhs.getBase() != null) return Error.InvalidOperation;
    return init(allocator, null, 0);
}
pub fn subtract(lhs: Value, rhs: Value, allocator: Allocator) Error!Value {
    if (lhs.getBase() != rhs.getBase()) return Error.InvalidOperation;
    return init(allocator, null, 0);
}
pub fn multiply(lhs: Value, rhs: Value, allocator: Allocator) Error!Value {
    if (lhs.getBase() != null or rhs.getBase() != null) return Error.InvalidOperation;
    return init(allocator, null, 0);
}
pub fn divide(lhs: Value, rhs: Value, allocator: Allocator) Error!Value {
    if (lhs.getBase() != null or rhs.getBase() != null) return Error.InvalidOperation;
    return init(allocator, null, switch (lhs.getOffset()) {
        .small => |lhs_small| switch (rhs.getOffset()) {
            .small => |rhs_small| {
                _ = .{ lhs_small, rhs_small };
            },
            .big => |rhs_big| {
                _ = .{ lhs_small, rhs_big };
            },
        },
        .big => |lhs_big| switch (rhs.getOffset()) {
            .small => |rhs_small| {
                _ = .{ lhs_big, rhs_small };
            },
            .big => |rhs_big| {
                _ = .{ lhs_big, rhs_big };
            },
        },
    });
}

pub fn format(
    self: Value,
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    const offset = self.getOffset();
    const offset_sign = switch (offset) {
        .small => |small| std.math.order(small, 0),
        .big => |big| if (big.eqZero()) .eq else if (big.positive) .gt else std.math.Order.lt,
    };
    if (self.getBase()) |register| {
        try writer.writeAll(@tagName(register));
        try writer.writeByte(switch (offset_sign) {
            .lt => '-',
            .eq => return,
            .gt => '+',
        });
    } else if (offset_sign == .lt) try writer.writeByte('-');
    try switch (offset) {
        .small => |small| writer.print("{}", .{std.math.absCast(small)}),
        .big => |big| writer.print("{}", .{big.abs()}),
    };
}

fn testGetFormat(string: []const u8, base: ?Register, offset: anytype) !void {
    const value = try Value.init(std.testing.allocator, base, offset);
    defer value.deinit();

    try std.testing.expectEqual(base, value.getBase());
    try std.testing.expect(switch (value.getOffset()) {
        .small => |small| offset == small,
        .big => |big| ok: {
            var managedOffset = try big_int.Managed.initSet(std.testing.allocator, offset);
            defer managedOffset.deinit();
            break :ok managedOffset.toConst().eq(big);
        },
    });
    try std.testing.expectFmt(string, "{}", .{value});
}

test "get/format" {
    try testGetFormat("0", null, 0);
    try testGetFormat("-1", null, -1);
    try testGetFormat("1", null, 1);
    try testGetFormat("af'", .@"af'", 0);
    try testGetFormat("b", .b, 0);
    try testGetFormat("bc", .bc, 0);
    try testGetFormat("ix+127", .ix, 127);
    try testGetFormat("iy-128", .iy, -128);
    try testGetFormat("9" ** 20, null, util.pow(10, 20) - 1);
    try testGetFormat("-" ++ "9" ** 20, null, -util.pow(10, 20) + 1);
    try testGetFormat("ix+" ++ "9" ** 20, .ix, util.pow(10, 20) - 1);
    try testGetFormat("iy-" ++ "9" ** 20, .iy, -util.pow(10, 20) + 1);
    try testGetFormat("de+1", .de, 1);
    try testGetFormat("hl-1", .hl, -1);
}
