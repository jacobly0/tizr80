const std = @import("std");

const Assembler = @This();
const Tokenizer = @import("as/tokenizer.zig");
const Value = @import("as/value.zig");

const Error = error{
    ExpectedComma,
    ExpectedEndOfLine,
    IllegalInstruction,
    IllegalSuffix,
    ImmediateOutOfRange,
    UnclosedParentheses,
    UnexpectedToken,
} || Tokenizer.Error || Value.Error || std.mem.Allocator.Error;

const Fixup = struct {
    location: u24,
    operation: enum { add, subtract },
    symbol: []const u8,
};

const Expression = struct {
    indirect: bool = false,
    value: Value,

    pub fn isImmediate(self: Expression) bool {
        return !self.indirect and self.value.isImmediate();
    }
    pub fn isIndirectImmediate(self: Expression) bool {
        return self.indirect and self.value.isIndirectImmediate();
    }
    pub fn isRegister(self: Expression) bool {
        return !self.indirect and self.value.isRegister();
    }
    pub fn isIndirectRegister(self: Expression) bool {
        return self.indirect and self.value.isRegister();
    }
    pub fn isRegisterOffset(self: Expression) bool {
        return !self.indirect and self.value.isRegisterOffset();
    }
    pub fn isIndirectRegisterOffset(self: Expression) bool {
        return !self.indirect and self.value.isRegisterOffset();
    }

    pub fn format(
        self: Expression,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (self.indirect) try writer.writeByte('(');
        try self.value.format(fmt, options, writer);
        if (self.indirect) try writer.writeByte(')');
    }
};

allocator: std.mem.Allocator,
tokenizer: Tokenizer,
fixups: std.ArrayListUnmanaged(Fixup) = .{},
output: std.ArrayListUnmanaged(u8) = .{},
origin: u24 = 0,
adl: bool = false,

fn init(allocator: std.mem.Allocator, source: [:0]const u8) std.mem.Allocator.Error!Assembler {
    return .{ .allocator = allocator, .tokenizer = Tokenizer.init(source) };
}
fn deinit(self: *Assembler) void {
    self.output.deinit(self.allocator);
    self.* = undefined;
}

fn emit(self: *Assembler, data: []const u8) std.mem.Allocator.Error!void {
    try self.output.appendSlice(self.allocator, data);
}
fn emitInt(self: *Assembler, value: anytype) std.mem.Allocator.Error!void {
    var data: [@sizeOf(@TypeOf(value))]u8 = undefined;
    std.mem.writeIntSliceLittle(@TypeOf(value), &data, value);
    try self.emit(&data);
}
fn emitImmediate(
    self: *Assembler,
    comptime UnsignedInt: type,
    comptime SignedInt: type,
    value: Value,
) Error!void {
    std.debug.assert(value.isImmediate());
    switch (value.getOffset()) {
        .small => |small| if (small >= 0)
            try self.emitInt(std.math.cast(UnsignedInt, small) orelse return Error.ImmediateOutOfRange)
        else
            try self.emitInt(std.math.cast(SignedInt, small) orelse return Error.ImmediateOutOfRange),
        .big => |big| if (big.positive)
            try self.emitInt(big.to(UnsignedInt) catch return Error.ImmediateOutOfRange)
        else
            try self.emitInt(big.to(SignedInt) catch return Error.ImmediateOutOfRange),
    }
}
fn emitWordImmediate(self: *Assembler, suffix: ?Tokenizer.Keyword, value: Value) Error!void {
    try switch (if (suffix) |token| switch (token) {
        .sis, .lis => false,
        .sil, .lil => true,
        else => unreachable,
    } else self.adl) {
        false => self.emitImmediate(u16, i16, value),
        true => self.emitImmediate(u24, i24, value),
    };
}

pub fn assemble(allocator: std.mem.Allocator, source: [:0]const u8) Error![]u8 {
    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    defer arena_allocator.deinit();

    var self = try Assembler.init(arena_allocator.allocator(), source);
    defer self.deinit();

    defer std.debug.print("{}\n", .{std.fmt.fmtSliceHexUpper(self.output.items)});
    try self.parseFile();

    return allocator.dupe(u8, self.output.items);
}

fn parseFile(self: *Assembler) Error!void {
    while (try self.tokenizer.peek() != .eof) try self.parseLine();
}

fn parseSuffix(self: *Assembler) Error!?Tokenizer.Keyword {
    if (try self.tokenizer.peek() != .dot) return null;
    _ = try self.tokenizer.next();
    const suffix = switch (try self.tokenizer.next()) {
        .keyword => |keyword| keyword,
        else => return Error.IllegalSuffix,
    };
    try self.emit(&[_]u8{switch (suffix) {
        .sis => 0o100,
        .lis => 0o111,
        .sil => 0o122,
        .lil => 0o133,
        else => return Error.IllegalSuffix,
    }});
    return suffix;
}
fn parseLine(self: *Assembler) Error!void {
    switch (try self.tokenizer.next()) {
        .comment => {},
        .keyword => |keyword| switch (keyword) {
            .ld => {
                const suffix = try self.parseSuffix();

                const lhs = try self.parseExpression();
                defer lhs.value.deinit();

                if (try self.tokenizer.next() != .comma) return Error.ExpectedComma;

                const rhs = try self.parseExpression();
                defer rhs.value.deinit();

                std.debug.print("ld\t{}, {}\n", .{ lhs, rhs });
                if (lhs.isRegister() and rhs.isImmediate()) {
                    switch (lhs.value.getBase().?) {
                        .bc => try self.emit(&[_]u8{0o001}),
                        .de => try self.emit(&[_]u8{0o021}),
                        .hl => try self.emit(&[_]u8{0o041}),
                        .sp => try self.emit(&[_]u8{0o061}),
                        else => return Error.IllegalInstruction,
                    }
                    try self.emitWordImmediate(suffix, rhs.value);
                } else return Error.IllegalInstruction;
            },
            .nop => {
                _ = try self.parseSuffix();
                try self.emit(&[_]u8{0o000});
            },
            else => return Error.UnexpectedToken,
        },
        else => return Error.UnexpectedToken,
    }
    while (true) switch (try self.tokenizer.next()) {
        .eof, .eol => break,
        .comment => continue,
        else => return Error.ExpectedEndOfLine,
    };
}

fn expression(self: *Assembler, base: ?Value.Register, offset: anytype) Error!Expression {
    return .{ .value = try Value.init(self.allocator, base, offset) };
}
fn parseAtom(self: *Assembler) Error!Expression {
    return switch (try self.tokenizer.next()) {
        .literal => |literal| .{ .value = try Value.parseLiteral(self.allocator, literal) },
        .keyword => |keyword| switch (keyword) {
            .a => try self.expression(.a, 0),
            .af => try self.expression(.af, 0),
            .@"af'" => try self.expression(.@"af'", 0),
            .b => try self.expression(.b, 0),
            .bc => try self.expression(.bc, 0),
            .c => try self.expression(.c, 0),
            .d => try self.expression(.d, 0),
            .de => try self.expression(.de, 0),
            .e => try self.expression(.e, 0),
            .h => try self.expression(.h, 0),
            .hl => try self.expression(.hl, 0),
            .l => try self.expression(.l, 0),
            .i => try self.expression(.i, 0),
            .ix => try self.expression(.ix, 0),
            .ixh => try self.expression(.ixh, 0),
            .ixl => try self.expression(.ixl, 0),
            .iy => try self.expression(.iy, 0),
            .iyh => try self.expression(.iyh, 0),
            .iyl => try self.expression(.iyl, 0),
            .m => try self.expression(.m, 0),
            .nc => try self.expression(.nc, 0),
            .nz => try self.expression(.nz, 0),
            .p => try self.expression(.p, 0),
            .pe => try self.expression(.pe, 0),
            .po => try self.expression(.po, 0),
            .sp => try self.expression(.sp, 0),
            .z => try self.expression(.z, 0),
            else => return Error.UnexpectedToken,
        },
        .lparen => value: {
            const sub_expression = try self.parseExpression();
            errdefer sub_expression.value.deinit();

            if (try self.tokenizer.next() != .rparen) return Error.UnclosedParentheses;

            break :value .{ .indirect = true, .value = sub_expression.value };
        },
        else => return Error.UnexpectedToken,
    };
}
fn parseMultiplicativeExpression(self: *Assembler) Error!Expression {
    var accumulator = try self.parseAtom();
    errdefer accumulator.value.deinit();

    while (true) {
        const operation = switch (try self.tokenizer.peek()) {
            .times, .divide => try self.tokenizer.next(),
            else => return accumulator,
        };

        const lhs = accumulator.value;
        const rhs = (try self.parseAtom()).value;
        defer rhs.deinit();

        accumulator = .{ .value = try switch (operation) {
            .times => lhs.multiply(rhs, self.allocator),
            .divide => try lhs.divide(rhs, self.allocator),
            else => unreachable,
        } };
        lhs.deinit();
    }
}
fn parseAdditiveExpression(self: *Assembler) Error!Expression {
    var accumulator = try self.parseMultiplicativeExpression();
    defer accumulator.value.deinit();

    while (true) {
        const operation = switch (try self.tokenizer.peek()) {
            .plus, .minus => try self.tokenizer.next(),
            else => return accumulator,
        };

        const lhs = accumulator.value;
        const rhs = (try self.parseMultiplicativeExpression()).value;
        defer rhs.deinit();

        accumulator = .{ .value = try switch (operation) {
            .plus => lhs.add(rhs, self.allocator),
            .minus => lhs.subtract(rhs, self.allocator),
            else => unreachable,
        } };
        lhs.deinit();
    }
}
fn parseExpression(self: *Assembler) Error!Expression {
    return try self.parseAdditiveExpression();
}

test "as" {
    try std.testing.expectEqualSlices(
        u8,
        @embedFile("as/ez80insts.bin"),
        try assemble(std.testing.allocator, @embedFile("as/ez80insts.src")),
    );
}

test {
    std.testing.refAllDecls(Assembler);
}
