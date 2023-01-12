const std = @import("std");
const Allocator = std.mem.Allocator;
const Limb = std.math.big.Limb;
const big_int = std.math.big.int;

const util = @import("../util.zig");
const Value = @This();

pub const Error = error{
    InvalidLiteral,
    InvalidOperation,
} || Allocator.Error;

pub const Reg = enum {
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
    mb,
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
        const WideInteger = std.meta.Int(.signed, @bitSizeOf(Integer) + 1);
        comptime {
            std.debug.assert(big_int.calcLimbLen(std.math.minInt(Integer)) <= 1);
        }

        tag: enum(u1) { big, small } = .small,
        base: enum(u2) { none, reg, ix, iy },
        integer: Integer,
    };
    const Big = struct {
        comptime {
            std.debug.assert(@alignOf(Big) >= 2);
        }

        base: ?Reg,
        offset: big_int.Managed,
    };

    small: Small,
    big: *Big,
};
const Contents = union(enum) {
    absolute: Storage.Small.Integer,
    reg: Reg,
    ix: Storage.Small.Integer,
    iy: Storage.Small.Integer,
    big: *Storage.Big,
};

storage: Storage,

fn toContents(self: Value) Contents {
    return switch (self.storage.small.tag) {
        .small => switch (self.storage.small.base) {
            .none => .{ .absolute = self.storage.small.integer },
            .reg => .{ .reg = @intToEnum(Reg, self.storage.small.integer) },
            .ix => .{ .ix = self.storage.small.integer },
            .iy => .{ .iy = self.storage.small.integer },
        },
        .big => .{ .big = self.storage.big },
    };
}
fn fromContents(contents: Contents) Value {
    return .{ .storage = switch (contents) {
        .absolute => |absolute| .{ .small = .{ .base = .none, .integer = absolute } },
        .reg => |reg| .{ .small = .{
            .base = .reg,
            .integer = @enumToInt(reg),
        } },
        .ix, .iy => |offset| result: {
            std.debug.assert(offset != 0);
            break :result .{ .small = .{ .tag = .small, .base = switch (contents) {
                else => unreachable,
                .ix => .ix,
                .iy => .iy,
            }, .integer = offset } };
        },
        .big => |big| .{ .big = big },
    } };
}

pub fn init(allocator: Allocator, base: ?Reg, offset: anytype) Allocator.Error!Value {
    const Small = Storage.Small.Integer;

    const offset_type = switch (@typeInfo(@TypeOf(offset))) {
        .Pointer => |pointer| pointer.child,
        else => @TypeOf(offset),
    };
    const offset_info = @typeInfo(offset_type);
    const small_offset = switch (offset_info) {
        else => unreachable,
        .Int, .ComptimeInt => std.math.cast(Small, offset),
        .Struct => offset.to(Small) catch null,
    };

    if (base) |reg| switch (reg) {
        else => if (small_offset) |small| if (small == 0) return fromContents(.{ .reg = reg }),
        .ix, .iy => if (small_offset) |small|
            return fromContents(if (small == 0) .{ .reg = reg } else switch (reg) {
                else => unreachable,
                .ix => .{ .ix = small },
                .iy => .{ .iy = small },
            }),
    } else if (small_offset) |small| return fromContents(.{ .absolute = small });

    const big = try allocator.create(Storage.Big);
    errdefer allocator.destroy(big);

    big.base = base;
    big.offset = try switch (offset_info) {
        else => unreachable,
        .Int, .ComptimeInt => big_int.Managed.initCapacity(allocator, big_int.calcLimbLen(offset)),
        .Struct => if (@hasDecl(offset_type, "toManaged"))
            offset.toManaged(allocator)
        else
            offset.cloneWithDifferentAllocator(allocator),
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
        '"', '\'' => util.todo("string literal"),
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

    if (number.len == 0) return Error.InvalidLiteral;

    var value = try big_int.Managed.initCapacity(
        allocator,
        big_int.calcSetStringLimbCount(base, number.len),
    );
    defer value.deinit();
    value.setString(base, number) catch |err| return switch (err) {
        error.InvalidBase => unreachable,
        error.InvalidCharacter => Error.InvalidLiteral,
        else => |e| e,
    };

    return init(allocator, null, value);
}
pub fn deinit(self: *Value) void {
    switch (self.toContents()) {
        else => {},
        .big => |big| {
            const allocator = big.offset.allocator;
            big.offset.deinit();
            allocator.destroy(big);
        },
    }
    self.* = undefined;
}

pub fn getBase(self: Value) ?Reg {
    return switch (self.toContents()) {
        .absolute => null,
        .reg => |reg| reg,
        .ix => .ix,
        .iy => .iy,
        .big => |big| big.base,
    };
}
pub fn getOffset(self: Value) union(enum) { small: Storage.Small.Integer, big: big_int.Const } {
    return switch (self.toContents()) {
        .absolute => |absolute| .{ .small = absolute },
        .reg => .{ .small = 0 },
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
pub fn orderOffset(self: Value, scalar: anytype) std.math.Order {
    return switch (self.getOffset()) {
        .small => |small| std.math.order(small, scalar),
        .big => |big| big.orderAgainstScalar(scalar),
    };
}

pub fn add(lhs: Value, rhs: Value, allocator: Allocator) Error!Value {
    if (lhs.getBase() != null and rhs.getBase() != null) return Error.InvalidOperation;
    const base = lhs.getBase() orelse rhs.getBase();
    switch (lhs.getOffset()) {
        .small => |lhs_small| switch (rhs.getOffset()) {
            .small => |rhs_small| return init(
                allocator,
                base,
                @as(Storage.Small.WideInteger, lhs_small) + rhs_small,
            ),
            .big => |rhs_big| {
                var result = try big_int.Managed.initCapacity(allocator, rhs_big.limbs.len + 1);
                defer result.deinit();
                var mutable = result.toMutable();
                mutable.addScalar(rhs_big, lhs_small);
                return init(allocator, base, result.toConst());
            },
        },
        .big => |lhs_big| switch (rhs.getOffset()) {
            .small => |rhs_small| {
                var result = try big_int.Managed.initCapacity(allocator, lhs_big.limbs.len + 1);
                defer result.deinit();
                var mutable = result.toMutable();
                mutable.addScalar(lhs_big, rhs_small);
                return init(allocator, base, result.toConst());
            },
            .big => |rhs_big| {
                var result = try big_int.Managed.initCapacity(allocator, lhs_big.limbs.len + 1);
                defer result.deinit();
                var mutable = result.toMutable();
                mutable.add(lhs_big, rhs_big);
                return init(allocator, base, result.toConst());
            },
        },
    }
}
pub fn subtract(lhs: Value, rhs: Value, allocator: Allocator) Error!Value {
    const different_bases = lhs.getBase() != rhs.getBase();
    if (different_bases and rhs.getBase() != null) return Error.InvalidOperation;
    const base = if (different_bases) lhs.getBase() else null;
    switch (lhs.getOffset()) {
        .small => |lhs_small| switch (rhs.getOffset()) {
            .small => |rhs_small| return init(
                allocator,
                base,
                @as(Storage.Small.WideInteger, lhs_small) - rhs_small,
            ),
            .big => |rhs_big| {
                var result = try big_int.Managed.initCapacity(allocator, rhs_big.limbs.len + 1);
                defer result.deinit();
                var mutable = result.toMutable();
                mutable.addScalar(rhs_big.negate(), lhs_small);
                return init(allocator, base, result.toConst());
            },
        },
        .big => |lhs_big| switch (rhs.getOffset()) {
            .small => |rhs_small| {
                var result = try big_int.Managed.initCapacity(allocator, lhs_big.limbs.len + 1);
                defer result.deinit();
                var mutable = result.toMutable();
                mutable.addScalar(lhs_big, -rhs_small);
                return init(allocator, base, result.toConst());
            },
            .big => |rhs_big| {
                var result = try big_int.Managed.initCapacity(allocator, lhs_big.limbs.len + 1);
                defer result.deinit();
                var mutable = result.toMutable();
                mutable.add(lhs_big, rhs_big.negate());
                return init(allocator, base, result.toConst());
            },
        },
    }
}
pub fn multiply(lhs: Value, rhs: Value, allocator: Allocator) Error!Value {
    if (lhs.getBase() != null or rhs.getBase() != null) return Error.InvalidOperation;

    _ = allocator;
    util.todo("unimplemented");
}
pub fn divide(lhs: Value, rhs: Value, allocator: Allocator) Error!Value {
    if (lhs.getBase() != null or rhs.getBase() != null) return Error.InvalidOperation;
    return init(allocator, null, switch (lhs.getOffset()) {
        .small => |lhs_small| switch (rhs.getOffset()) {
            .small => |rhs_small| {
                _ = .{ lhs_small, rhs_small };
                util.todo("small / small");
            },
            .big => |rhs_big| {
                _ = .{ lhs_small, rhs_big };
                util.todo("small / big");
            },
        },
        .big => |lhs_big| switch (rhs.getOffset()) {
            .small => |rhs_small| {
                _ = .{ lhs_big, rhs_small };
                util.todo("big / small");
            },
            .big => |rhs_big| {
                _ = .{ lhs_big, rhs_big };
                util.todo("big / big");
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
    if (self.getBase()) |reg| {
        try writer.writeAll(@tagName(reg));
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

fn testGetFormat(string: []const u8, base: ?Reg, offset: anytype) !void {
    var value = try Value.init(std.testing.allocator, base, offset);
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
