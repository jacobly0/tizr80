const std = @import("std");

const Assembler = @This();
const SmallArrayList = @import("small_array_list.zig").SmallArrayList;
const Tokenizer = @import("as/tokenizer.zig");
const Value = @import("as/value.zig");

const Error = error{
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

    pub fn deinit(self: *Expression) void {
        self.value.deinit();
        self.* = undefined;
    }

    pub fn isImmediate(self: Expression) bool {
        return !self.indirect and self.value.isImmediate();
    }
    pub fn isIndirectImmediate(self: Expression) bool {
        return self.indirect and self.value.isImmediate();
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
    pub fn getRegister(self: Expression) Value.Register {
        return self.value.getBase().?;
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

const Mode = packed struct(u2) {
    const Instruction = enum(u1) { s, l };
    const Immediate = enum(u1) { is, il };

    inst: Instruction,
    imm: Immediate,

    pub fn opcode(self: Mode) u8 {
        return switch (self.imm) {
            .is => switch (self.inst) {
                .s => 0o100,
                .l => 0o111,
            },
            .il => switch (self.inst) {
                .s => 0o122,
                .l => 0o133,
            },
        };
    }
};

allocator: std.mem.Allocator,
tokenizer: Tokenizer,
fixups: std.ArrayListUnmanaged(Fixup) = .{},
output: std.ArrayListUnmanaged(u8) = .{},
origin: u24 = 0,
adl: Mode = .{ .inst = .l, .imm = .il },

pub fn assemble(allocator: std.mem.Allocator, source: [:0]const u8) Error![]u8 {
    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    defer arena_allocator.deinit();

    var self = try Assembler.init(arena_allocator.allocator(), source);
    defer self.deinit();

    defer if (false) std.debug.print("{}\n", .{std.fmt.fmtSliceHexUpper(self.output.items)});
    defer if (true) std.debug.print("{s}correct prefix\n", .{
        if (std.mem.startsWith(u8, @embedFile("as/ez80insts.bin"), self.output.items)) "" else "in",
    });
    {
        errdefer if (true) self.tokenizer.debugPrintLocation();
        try self.parseFile();
    }

    return allocator.dupe(u8, self.output.items);
}

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
    var data: [try std.math.divCeil(usize, @bitSizeOf(@TypeOf(value)), 8)]u8 = undefined;
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
fn emitByteImmediate(self: *Assembler, expression: Expression) Error!void {
    try self.emitImmediate(u8, i8, expression.value);
}
fn emitShortImmediate(self: *Assembler, expression: Expression) Error!void {
    try self.emitImmediate(u16, i16, expression.value);
}
fn emitLongImmediate(self: *Assembler, expression: Expression) Error!void {
    try self.emitImmediate(u24, i24, expression.value);
}
fn emitRelativeImmediate(self: *Assembler, expression: Expression) Error!void {
    var pc = try Value.init(self.allocator, null, self.origin + self.output.items.len + 1);
    defer pc.deinit();
    var offset = try expression.value.subtract(pc, self.allocator);
    defer offset.deinit();
    if (false) std.debug.print("\nexpression = {}, pc = {}, offset = {}\n", .{ expression, pc, offset });
    try self.emitImmediate(i8, i8, offset);
}
fn emitWordImmediate(self: *Assembler, suffix: ?Mode, expression: Expression) Error!void {
    try switch ((suffix orelse self.adl).imm) {
        .is => self.emitShortImmediate(expression),
        .il => self.emitLongImmediate(expression),
    };
}

fn parseFile(self: *Assembler) Error!void {
    while (try self.tokenizer.peek() != .eof) try self.parseLine();
}

fn parseSuffix(self: *Assembler) Error!?Mode {
    if (try self.tokenizer.peek() != .dot) return null;
    _ = try self.tokenizer.next();
    return switch (try self.tokenizer.next()) {
        .keyword => |keyword| switch (keyword) {
            .s => .{ .inst = .s, .imm = self.adl.imm },
            .l => .{ .inst = .l, .imm = self.adl.imm },
            .is => .{ .inst = self.adl.inst, .imm = .is },
            .il => .{ .inst = self.adl.inst, .imm = .il },
            .sis => .{ .inst = .s, .imm = .is },
            .lis => .{ .inst = .l, .imm = .is },
            .sil => .{ .inst = .s, .imm = .il },
            .lil => .{ .inst = .l, .imm = .il },
            else => return Error.IllegalSuffix,
        },
        else => return Error.IllegalSuffix,
    };
}
fn parseInstruction(self: *Assembler) Error!void {
    const mnemonic = (try self.tokenizer.next()).keyword;
    const suffix = try self.parseSuffix();

    var operand_list: SmallArrayList(Expression, 2) = .{};
    defer operand_list.deinit(self.allocator);
    defer for (operand_list.items()) |*operand| operand.deinit();
    if (switch (try self.tokenizer.peek()) {
        else => true,
        .eof, .eol, .comment => false,
    }) while (true) {
        try operand_list.append(self.allocator, try self.parseExpression());
        if (try self.tokenizer.peek() != .comma) break;
        _ = try self.tokenizer.next();
    };
    const operands = operand_list.items();

    if (suffix) |mode| try self.emit(&[_]u8{mode.opcode()});
    errdefer if (true) {
        std.debug.print("\n{s}\t", .{@tagName(mnemonic)});
        var first = true;
        for (operands) |operand| {
            if (first)
                first = false
            else
                std.debug.print(", ", .{});
            std.debug.print("{}", .{operand});
        }
    };
    switch (mnemonic) {
        .adc => if (operands.len == 2 and operands[0].isRegister())
            switch (operands[0].getRegister()) {
                .hl => if (operands[1].isRegister()) try self.emit(&[_]u8{
                    0o355, switch (operands[1].getRegister()) {
                        .bc => 0o112,
                        .de => 0o132,
                        .hl => 0o152,
                        .sp => 0o172,
                        else => return Error.IllegalInstruction,
                    },
                }) else return Error.IllegalInstruction,
                .a => if (operands[1].isRegister()) try self.emit(&[_]u8{
                    switch (operands[1].getRegister()) {
                        .b => 0o210,
                        .c => 0o211,
                        .d => 0o212,
                        .e => 0o213,
                        .h => 0o214,
                        .l => 0o215,
                        .a => 0o217,
                        else => return Error.IllegalInstruction,
                    },
                }) else if (operands[1].isIndirectRegister() and operands[1].getRegister() == .hl)
                    try self.emit(&[_]u8{0o216})
                else if (operands[1].isImmediate()) {
                    try self.emit(&[_]u8{0o316});
                    try self.emitByteImmediate(operands[1]);
                } else return Error.IllegalInstruction,
                else => return Error.IllegalInstruction,
            }
        else
            return Error.IllegalInstruction,
        .add => if (operands.len == 2 and operands[0].isRegister())
            switch (operands[0].getRegister()) {
                .hl => if (operands[1].isRegister()) try self.emit(&[_]u8{
                    switch (operands[1].getRegister()) {
                        .bc => 0o011,
                        .de => 0o031,
                        .hl => 0o051,
                        .sp => 0o071,
                        else => return Error.IllegalInstruction,
                    },
                }) else return Error.IllegalInstruction,
                .a => if (operands[1].isRegister()) try self.emit(&[_]u8{
                    switch (operands[1].getRegister()) {
                        .b => 0o200,
                        .c => 0o201,
                        .d => 0o202,
                        .e => 0o203,
                        .h => 0o204,
                        .l => 0o205,
                        .a => 0o207,
                        else => return Error.IllegalInstruction,
                    },
                }) else if (operands[1].isIndirectRegister() and operands[1].getRegister() == .hl)
                    try self.emit(&[_]u8{0o206})
                else if (operands[1].isImmediate()) {
                    try self.emit(&[_]u8{0o306});
                    try self.emitByteImmediate(operands[1]);
                } else return Error.IllegalInstruction,
                else => return Error.IllegalInstruction,
            }
        else
            return Error.IllegalInstruction,
        .@"and" => if (operands.len == 2 and operands[0].isRegister())
            switch (operands[0].getRegister()) {
                .a => if (operands[1].isRegister()) try self.emit(&[_]u8{
                    switch (operands[1].getRegister()) {
                        .b => 0o240,
                        .c => 0o241,
                        .d => 0o242,
                        .e => 0o243,
                        .h => 0o244,
                        .l => 0o245,
                        .a => 0o247,
                        else => return Error.IllegalInstruction,
                    },
                }) else if (operands[1].isIndirectRegister() and operands[1].getRegister() == .hl)
                    try self.emit(&[_]u8{0o246})
                else
                    return Error.IllegalInstruction,
                else => return Error.IllegalInstruction,
            }
        else
            return Error.IllegalInstruction,
        .bit, .res, .set => if (operands.len == 2 and operands[0].isImmediate())
            try self.emit(&[_]u8{
                0o313, @as(u8, switch (mnemonic) {
                    .bit => 0o100,
                    .res => 0o200,
                    .set => 0o300,
                    else => unreachable,
                }) | @as(u8, switch (operands[0].value.getOffset()) {
                    .small => |small| std.math.cast(u3, small),
                    .big => |big| big.to(u3) catch null,
                } orelse return Error.IllegalInstruction) << 3 | @as(u8, if (operands[1].isRegister())
                    switch (operands[1].getRegister()) {
                        .b => 0o000,
                        .c => 0o001,
                        .d => 0o002,
                        .e => 0o003,
                        .h => 0o004,
                        .l => 0o005,
                        .a => 0o007,
                        else => return Error.IllegalInstruction,
                    }
                else if (operands[1].isIndirectRegister() and operands[1].getRegister() == .hl)
                    0o006
                else
                    return Error.IllegalInstruction),
            })
        else
            return Error.IllegalInstruction,
        .call => if (operands.len != 0 and operands[operands.len - 1].isImmediate()) {
            try self.emit(&[_]u8{if (operands.len == 1)
                0o315
            else if (operands.len == 2 and operands[0].isRegister())
                switch (operands[0].getRegister()) {
                    .nz => 0o304,
                    .z => 0o314,
                    .nc => 0o324,
                    .c => 0o334,
                    .po => 0o344,
                    .pe => 0o354,
                    .p => 0o364,
                    .m => 0o374,
                    else => return Error.IllegalInstruction,
                }
            else
                return Error.IllegalInstruction});
            try self.emitWordImmediate(suffix, operands[operands.len - 1]);
        } else return Error.IllegalInstruction,
        .ccf, .cpl, .daa, .exx, .halt, .nop, .rla, .rlca, .rra, .rrca, .scf => if (operands.len == 0)
            try self.emit(&[_]u8{switch (mnemonic) {
                .nop => 0o000,
                .rlca => 0o007,
                .rrca => 0o017,
                .rla => 0o027,
                .rra => 0o037,
                .daa => 0o047,
                .cpl => 0o057,
                .scf => 0o067,
                .ccf => 0o077,
                .halt => 0o166,
                .exx => 0o331,
                else => unreachable,
            }})
        else
            return Error.IllegalInstruction,
        .cp => if (operands.len == 2 and operands[0].isRegister())
            switch (operands[0].getRegister()) {
                .a => if (operands[1].isRegister()) try self.emit(&[_]u8{
                    switch (operands[1].getRegister()) {
                        .b => 0o270,
                        .c => 0o271,
                        .d => 0o272,
                        .e => 0o273,
                        .h => 0o274,
                        .l => 0o275,
                        .a => 0o277,
                        else => return Error.IllegalInstruction,
                    },
                }) else if (operands[1].isIndirectRegister() and operands[1].getRegister() == .hl)
                    try self.emit(&[_]u8{0o276})
                else
                    return Error.IllegalInstruction,
                else => return Error.IllegalInstruction,
            }
        else
            return Error.IllegalInstruction,
        .dec => if (operands.len == 1) try self.emit(&[_]u8{
            if (operands[0].isRegister())
                switch (operands[0].getRegister()) {
                    .b => 0o005,
                    .c => 0o015,
                    .d => 0o025,
                    .e => 0o035,
                    .h => 0o045,
                    .l => 0o055,
                    .a => 0o075,
                    .bc => 0o013,
                    .de => 0o033,
                    .hl => 0o053,
                    .sp => 0o073,
                    else => return Error.IllegalInstruction,
                }
            else if (operands[0].isIndirectRegister() and operands[0].getRegister() == .hl)
                0o065
            else
                return Error.IllegalInstruction,
        }) else return Error.IllegalInstruction,
        .djnz => if (operands.len == 1 and operands[0].isImmediate()) {
            try self.emit(&[_]u8{0o020});
            try self.emitRelativeImmediate(operands[0]);
        } else return Error.IllegalInstruction,
        .ex => if (operands.len == 2)
            if (operands[0].isRegister() and operands[1].isRegister())
                if (operands[0].getRegister() == .af and operands[1].getRegister() == .@"af'" or
                    operands[1].getRegister() == .af and operands[0].getRegister() == .@"af'")
                    try self.emit(&[_]u8{0o010})
                else
                    return Error.IllegalInstruction
            else
                return Error.IllegalInstruction,
        .in => if (operands.len == 2 and operands[0].isRegister() and
            operands[0].getRegister() == .a and operands[1].isIndirectImmediate())
        {
            try self.emit(&[_]u8{0o333});
            try self.emitByteImmediate(operands[1]);
        } else return Error.IllegalInstruction,
        .inc => if (operands.len == 1) try self.emit(&[_]u8{
            if (operands[0].isRegister())
                switch (operands[0].getRegister()) {
                    .b => 0o004,
                    .c => 0o014,
                    .d => 0o024,
                    .e => 0o034,
                    .h => 0o044,
                    .l => 0o054,
                    .a => 0o074,
                    .bc => 0o003,
                    .de => 0o023,
                    .hl => 0o043,
                    .sp => 0o063,
                    else => return Error.IllegalInstruction,
                }
            else if (operands[0].isIndirectRegister() and operands[0].getRegister() == .hl)
                0o064
            else
                return Error.IllegalInstruction,
        }) else return Error.IllegalInstruction,
        .jp => if (operands.len != 0 and operands[operands.len - 1].isImmediate()) {
            try self.emit(&[_]u8{if (operands.len == 1)
                0o303
            else if (operands.len == 2 and operands[0].isRegister())
                switch (operands[0].getRegister()) {
                    .nz => 0o302,
                    .z => 0o312,
                    .nc => 0o322,
                    .c => 0o332,
                    .po => 0o342,
                    .pe => 0o352,
                    .p => 0o362,
                    .m => 0o372,
                    else => return Error.IllegalInstruction,
                }
            else
                return Error.IllegalInstruction});
            try self.emitWordImmediate(suffix, operands[operands.len - 1]);
        } else return Error.IllegalInstruction,
        .jr => if (operands.len != 0 and operands[operands.len - 1].isImmediate()) {
            try self.emit(&[_]u8{if (operands.len == 1)
                0o030
            else if (operands.len == 2 and operands[0].isRegister())
                switch (operands[0].getRegister()) {
                    .nz => 0o040,
                    .z => 0o050,
                    .nc => 0o060,
                    .c => 0o070,
                    else => return Error.IllegalInstruction,
                }
            else
                return Error.IllegalInstruction});
            try self.emitRelativeImmediate(operands[operands.len - 1]);
        } else return Error.IllegalInstruction,
        .ld => if (operands.len == 2) {
            if (operands[0].isRegister() and operands[1].isRegister())
                switch (operands[0].getRegister()) {
                    .b => switch (operands[1].getRegister()) {
                        .c => try self.emit(&[_]u8{0o101}),
                        .d => try self.emit(&[_]u8{0o102}),
                        .e => try self.emit(&[_]u8{0o103}),
                        .h => try self.emit(&[_]u8{0o104}),
                        .l => try self.emit(&[_]u8{0o105}),
                        .a => try self.emit(&[_]u8{0o107}),
                        else => return Error.IllegalInstruction,
                    },
                    .c => switch (operands[1].getRegister()) {
                        .b => try self.emit(&[_]u8{0o110}),
                        .d => try self.emit(&[_]u8{0o112}),
                        .e => try self.emit(&[_]u8{0o113}),
                        .h => try self.emit(&[_]u8{0o114}),
                        .l => try self.emit(&[_]u8{0o115}),
                        .a => try self.emit(&[_]u8{0o117}),
                        else => return Error.IllegalInstruction,
                    },
                    .d => switch (operands[1].getRegister()) {
                        .b => try self.emit(&[_]u8{0o120}),
                        .c => try self.emit(&[_]u8{0o121}),
                        .e => try self.emit(&[_]u8{0o123}),
                        .h => try self.emit(&[_]u8{0o124}),
                        .l => try self.emit(&[_]u8{0o125}),
                        .a => try self.emit(&[_]u8{0o127}),
                        else => return Error.IllegalInstruction,
                    },
                    .e => switch (operands[1].getRegister()) {
                        .b => try self.emit(&[_]u8{0o130}),
                        .c => try self.emit(&[_]u8{0o131}),
                        .d => try self.emit(&[_]u8{0o132}),
                        .h => try self.emit(&[_]u8{0o134}),
                        .l => try self.emit(&[_]u8{0o135}),
                        .a => try self.emit(&[_]u8{0o137}),
                        else => return Error.IllegalInstruction,
                    },
                    .h => switch (operands[1].getRegister()) {
                        .b => try self.emit(&[_]u8{0o140}),
                        .c => try self.emit(&[_]u8{0o141}),
                        .d => try self.emit(&[_]u8{0o142}),
                        .e => try self.emit(&[_]u8{0o143}),
                        .h => try self.emit(&[_]u8{0o144}),
                        .l => try self.emit(&[_]u8{0o145}),
                        .a => try self.emit(&[_]u8{0o147}),
                        else => return Error.IllegalInstruction,
                    },
                    .l => switch (operands[1].getRegister()) {
                        .b => try self.emit(&[_]u8{0o150}),
                        .c => try self.emit(&[_]u8{0o151}),
                        .d => try self.emit(&[_]u8{0o152}),
                        .e => try self.emit(&[_]u8{0o153}),
                        .h => try self.emit(&[_]u8{0o154}),
                        .l => try self.emit(&[_]u8{0o155}),
                        .a => try self.emit(&[_]u8{0o157}),
                        else => return Error.IllegalInstruction,
                    },
                    .a => switch (operands[1].getRegister()) {
                        .b => try self.emit(&[_]u8{0o170}),
                        .c => try self.emit(&[_]u8{0o171}),
                        .d => try self.emit(&[_]u8{0o172}),
                        .e => try self.emit(&[_]u8{0o173}),
                        .h => try self.emit(&[_]u8{0o174}),
                        .l => try self.emit(&[_]u8{0o175}),
                        .a => try self.emit(&[_]u8{0o177}),
                        else => return Error.IllegalInstruction,
                    },
                    else => return Error.IllegalInstruction,
                }
            else if (operands[0].isRegister() and operands[1].isImmediate())
                switch (operands[0].getRegister()) {
                    .b, .c, .d, .e, .h, .l, .a => |register| {
                        try self.emit(&[_]u8{switch (register) {
                            .b => 0o006,
                            .c => 0o016,
                            .d => 0o026,
                            .e => 0o036,
                            .h => 0o046,
                            .l => 0o056,
                            .a => 0o076,
                            else => unreachable,
                        }});
                        try self.emitByteImmediate(operands[1]);
                    },
                    .bc, .de, .hl, .sp => |register| {
                        try self.emit(&[_]u8{switch (register) {
                            .bc => 0o001,
                            .de => 0o021,
                            .hl => 0o041,
                            .sp => 0o061,
                            else => unreachable,
                        }});
                        try self.emitWordImmediate(suffix, operands[1]);
                    },
                    else => return Error.IllegalInstruction,
                }
            else if (operands[0].isIndirectRegister() and operands[0].getRegister() == .hl and
                operands[1].isImmediate())
            {
                try self.emit(&[_]u8{0o066});
                try self.emitByteImmediate(operands[1]);
            } else if (operands[0].isIndirectRegister() and operands[1].isRegister())
                switch (operands[0].getRegister()) {
                    .bc => switch (operands[1].getRegister()) {
                        .a => try self.emit(&[_]u8{0o002}),
                        else => return Error.IllegalInstruction,
                    },
                    .de => switch (operands[1].getRegister()) {
                        .a => try self.emit(&[_]u8{0o022}),
                        else => return Error.IllegalInstruction,
                    },
                    .hl => switch (operands[1].getRegister()) {
                        .b => try self.emit(&[_]u8{0o160}),
                        .c => try self.emit(&[_]u8{0o161}),
                        .d => try self.emit(&[_]u8{0o162}),
                        .e => try self.emit(&[_]u8{0o163}),
                        .h => try self.emit(&[_]u8{0o164}),
                        .l => try self.emit(&[_]u8{0o165}),
                        .a => try self.emit(&[_]u8{0o167}),
                        else => return Error.IllegalInstruction,
                    },
                    else => return Error.IllegalInstruction,
                }
            else if (operands[0].isRegister() and operands[1].isIndirectRegister())
                switch (operands[1].getRegister()) {
                    .bc => switch (operands[0].getRegister()) {
                        .a => try self.emit(&[_]u8{0o012}),
                        else => return Error.IllegalInstruction,
                    },
                    .de => switch (operands[0].getRegister()) {
                        .a => try self.emit(&[_]u8{0o032}),
                        else => return Error.IllegalInstruction,
                    },
                    .hl => switch (operands[0].getRegister()) {
                        .b => try self.emit(&[_]u8{0o106}),
                        .c => try self.emit(&[_]u8{0o116}),
                        .d => try self.emit(&[_]u8{0o126}),
                        .e => try self.emit(&[_]u8{0o136}),
                        .h => try self.emit(&[_]u8{0o146}),
                        .l => try self.emit(&[_]u8{0o156}),
                        .a => try self.emit(&[_]u8{0o176}),
                        else => return Error.IllegalInstruction,
                    },
                    else => return Error.IllegalInstruction,
                }
            else if (operands[0].isIndirectImmediate() and operands[1].isRegister())
                switch (operands[1].getRegister()) {
                    .hl => {
                        try self.emit(&[_]u8{0o042});
                        try self.emitWordImmediate(suffix, operands[0]);
                    },
                    .a => {
                        try self.emit(&[_]u8{0o062});
                        try self.emitWordImmediate(suffix, operands[0]);
                    },
                    else => return Error.IllegalInstruction,
                }
            else if (operands[0].isRegister() and operands[1].isIndirectImmediate())
                switch (operands[0].getRegister()) {
                    .hl => {
                        try self.emit(&[_]u8{0o052});
                        try self.emitWordImmediate(suffix, operands[1]);
                    },
                    .a => {
                        try self.emit(&[_]u8{0o072});
                        try self.emitWordImmediate(suffix, operands[1]);
                    },
                    else => return Error.IllegalInstruction,
                }
            else
                return Error.IllegalInstruction;
        },
        .@"or" => if (operands.len == 2 and operands[0].isRegister())
            switch (operands[0].getRegister()) {
                .a => if (operands[1].isRegister()) try self.emit(&[_]u8{
                    switch (operands[1].getRegister()) {
                        .b => 0o260,
                        .c => 0o261,
                        .d => 0o262,
                        .e => 0o263,
                        .h => 0o264,
                        .l => 0o265,
                        .a => 0o267,
                        else => return Error.IllegalInstruction,
                    },
                }) else if (operands[1].isIndirectRegister() and operands[1].getRegister() == .hl)
                    try self.emit(&[_]u8{0o266})
                else
                    return Error.IllegalInstruction,
                else => return Error.IllegalInstruction,
            }
        else
            return Error.IllegalInstruction,
        .out => if (operands.len == 2 and operands[0].isIndirectImmediate() and
            operands[1].isRegister() and operands[1].getRegister() == .a)
        {
            try self.emit(&[_]u8{0o323});
            try self.emitByteImmediate(operands[0]);
        } else return Error.IllegalInstruction,
        .pop => if (operands.len == 1 and operands[0].isRegister())
            try self.emit(&[_]u8{
                switch (operands[0].getRegister()) {
                    .bc => 0o301,
                    .de => 0o321,
                    .hl => 0o341,
                    .sp => 0o361,
                    else => return Error.IllegalInstruction,
                },
            })
        else
            return Error.IllegalInstruction,
        .push => if (operands.len == 1 and operands[0].isRegister())
            try self.emit(&[_]u8{
                switch (operands[0].getRegister()) {
                    .bc => 0o305,
                    .de => 0o325,
                    .hl => 0o345,
                    .sp => 0o365,
                    else => return Error.IllegalInstruction,
                },
            })
        else
            return Error.IllegalInstruction,
        .ret => try self.emit(
            &[_]u8{if (operands.len == 0)
                0o311
            else if (operands.len == 1 and operands[0].isRegister())
                switch (operands[0].getRegister()) {
                    .nz => 0o300,
                    .z => 0o310,
                    .nc => 0o320,
                    .c => 0o330,
                    .po => 0o340,
                    .pe => 0o350,
                    .p => 0o360,
                    .m => 0o370,
                    else => return Error.IllegalInstruction,
                }
            else
                return Error.IllegalInstruction},
        ),
        .rl, .rlc, .rr, .rrc, .sla, .sra, .srl => if (operands.len == 1)
            try self.emit(&[_]u8{
                0o313, @as(u8, switch (mnemonic) {
                    .rlc => 0o000,
                    .rrc => 0o010,
                    .rl => 0o020,
                    .rr => 0o030,
                    .sla => 0o040,
                    .sra => 0o050,
                    .srl => 0o070,
                    else => unreachable,
                }) | @as(u8, if (operands[0].isRegister())
                    switch (operands[0].getRegister()) {
                        .b => 0o000,
                        .c => 0o001,
                        .d => 0o002,
                        .e => 0o003,
                        .h => 0o004,
                        .l => 0o005,
                        .a => 0o007,
                        else => return Error.IllegalInstruction,
                    }
                else if (operands[0].isIndirectRegister() and operands[0].getRegister() == .hl)
                    0o006
                else
                    return Error.IllegalInstruction),
            })
        else
            return Error.IllegalInstruction,
        .rst => if (operands.len == 1 and operands[0].isImmediate())
            try self.emit(&[_]u8{switch (switch (operands[0].value.getOffset()) {
                .small => |small| std.math.cast(u8, small),
                .big => |big| big.to(u8) catch null,
            } orelse return Error.IllegalInstruction) {
                0o000 => 0o307,
                0o010 => 0o317,
                0o020 => 0o327,
                0o030 => 0o337,
                0o040 => 0o347,
                0o050 => 0o357,
                0o060 => 0o367,
                0o070 => 0o377,
                else => return Error.IllegalInstruction,
            }})
        else
            return Error.IllegalInstruction,
        .sbc => if (operands.len == 2 and operands[0].isRegister())
            switch (operands[0].getRegister()) {
                .hl => if (operands[1].isRegister()) try self.emit(&[_]u8{
                    0o355, switch (operands[1].getRegister()) {
                        .bc => 0o102,
                        .de => 0o122,
                        .hl => 0o142,
                        .sp => 0o162,
                        else => return Error.IllegalInstruction,
                    },
                }) else return Error.IllegalInstruction,
                .a => if (operands[1].isRegister()) try self.emit(&[_]u8{
                    switch (operands[1].getRegister()) {
                        .b => 0o230,
                        .c => 0o231,
                        .d => 0o232,
                        .e => 0o233,
                        .h => 0o234,
                        .l => 0o235,
                        .a => 0o237,
                        else => return Error.IllegalInstruction,
                    },
                }) else if (operands[1].isIndirectRegister() and operands[1].getRegister() == .hl)
                    try self.emit(&[_]u8{0o236})
                else
                    return Error.IllegalInstruction,
                else => return Error.IllegalInstruction,
            }
        else
            return Error.IllegalInstruction,
        .sub => if (operands.len == 2 and operands[0].isRegister())
            switch (operands[0].getRegister()) {
                .a => if (operands[1].isRegister()) try self.emit(&[_]u8{
                    switch (operands[1].getRegister()) {
                        .b => 0o220,
                        .c => 0o221,
                        .d => 0o222,
                        .e => 0o223,
                        .h => 0o224,
                        .l => 0o225,
                        .a => 0o227,
                        else => return Error.IllegalInstruction,
                    },
                }) else if (operands[1].isIndirectRegister() and operands[1].getRegister() == .hl)
                    try self.emit(&[_]u8{0o226})
                else if (operands[1].isImmediate()) {
                    try self.emit(&[_]u8{0o326});
                    try self.emitByteImmediate(operands[1]);
                } else return Error.IllegalInstruction,
                else => return Error.IllegalInstruction,
            }
        else
            return Error.IllegalInstruction,
        else => unreachable,
        .xor => if (operands.len == 2 and operands[0].isRegister())
            switch (operands[0].getRegister()) {
                .a => if (operands[1].isRegister()) try self.emit(&[_]u8{
                    switch (operands[1].getRegister()) {
                        .b => 0o250,
                        .c => 0o251,
                        .d => 0o252,
                        .e => 0o253,
                        .h => 0o254,
                        .l => 0o255,
                        .a => 0o257,
                        else => return Error.IllegalInstruction,
                    },
                }) else if (operands[1].isIndirectRegister() and operands[1].getRegister() == .hl)
                    try self.emit(&[_]u8{0o256})
                else
                    return Error.IllegalInstruction,
                else => return Error.IllegalInstruction,
            }
        else
            return Error.IllegalInstruction,
    }
}
fn parseData(self: *Assembler) Error!void {
    const mnemonic = (try self.tokenizer.next()).keyword;

    if (switch (try self.tokenizer.peek()) {
        else => true,
        .eof, .eol, .comment => false,
    }) while (true) {
        var value = try self.parseExpression();
        defer value.deinit();

        try switch (mnemonic) {
            .db => self.emitByteImmediate(value),
            .dw => self.emitShortImmediate(value),
            .dl => self.emitLongImmediate(value),
            else => unreachable,
        };

        if (try self.tokenizer.peek() != .comma) break;
        _ = try self.tokenizer.next();
    };
}
fn parseStatement(self: *Assembler) Error!void {
    switch (try self.tokenizer.peek()) {
        .eof, .eol, .comment => {},
        .keyword => |keyword| switch (keyword) {
            .adc,
            .add,
            .@"and",
            .bit,
            .call,
            .ccf,
            .cp,
            .cpl,
            .daa,
            .dec,
            .djnz,
            .ex,
            .exx,
            .halt,
            .in,
            .inc,
            .jp,
            .jr,
            .ld,
            .nop,
            .@"or",
            .out,
            .pop,
            .push,
            .res,
            .ret,
            .rl,
            .rla,
            .rlc,
            .rlca,
            .rr,
            .rra,
            .rrc,
            .rrca,
            .rst,
            .sbc,
            .scf,
            .set,
            .sla,
            .sra,
            .srl,
            .sub,
            .xor,
            => try self.parseInstruction(),
            .db, .dw, .dl => try self.parseData(),
            else => return Error.IllegalInstruction,
        },
        else => return Error.IllegalInstruction,
    }
}
fn parseLine(self: *Assembler) Error!void {
    try self.parseStatement();
    while (true) switch (try self.tokenizer.next()) {
        .eof, .eol => break,
        .comment => continue,
        else => return Error.ExpectedEndOfLine,
    };
}

fn initExpression(self: *Assembler, base: ?Value.Register, offset: anytype) Error!Expression {
    return .{ .value = try Value.init(self.allocator, base, offset) };
}
fn parseAtom(self: *Assembler) Error!Expression {
    return switch (try self.tokenizer.next()) {
        .literal => |literal| .{ .value = try Value.parseLiteral(self.allocator, literal) },
        .keyword => |keyword| switch (keyword) {
            .a => try self.initExpression(.a, 0),
            .af => try self.initExpression(.af, 0),
            .@"af'" => try self.initExpression(.@"af'", 0),
            .b => try self.initExpression(.b, 0),
            .bc => try self.initExpression(.bc, 0),
            .c => try self.initExpression(.c, 0),
            .d => try self.initExpression(.d, 0),
            .de => try self.initExpression(.de, 0),
            .e => try self.initExpression(.e, 0),
            .h => try self.initExpression(.h, 0),
            .hl => try self.initExpression(.hl, 0),
            .l => try self.initExpression(.l, 0),
            .i => try self.initExpression(.i, 0),
            .ix => try self.initExpression(.ix, 0),
            .ixh => try self.initExpression(.ixh, 0),
            .ixl => try self.initExpression(.ixl, 0),
            .iy => try self.initExpression(.iy, 0),
            .iyh => try self.initExpression(.iyh, 0),
            .iyl => try self.initExpression(.iyl, 0),
            .m => try self.initExpression(.m, 0),
            .nc => try self.initExpression(.nc, 0),
            .nz => try self.initExpression(.nz, 0),
            .p => try self.initExpression(.p, 0),
            .pe => try self.initExpression(.pe, 0),
            .po => try self.initExpression(.po, 0),
            .sp => try self.initExpression(.sp, 0),
            .z => try self.initExpression(.z, 0),
            else => return Error.UnexpectedToken,
        },
        .dollar => try self.initExpression(null, self.origin + self.output.items.len),
        .lparen => value: {
            var sub_expression = try self.parseExpression();
            errdefer sub_expression.deinit();

            if (try self.tokenizer.next() != .rparen) return Error.UnclosedParentheses;

            break :value .{ .indirect = true, .value = sub_expression.value };
        },
        else => return Error.UnexpectedToken,
    };
}
fn parseMultiplicativeExpression(self: *Assembler) Error!Expression {
    var accumulator = try self.parseAtom();
    errdefer accumulator.deinit();

    while (true) {
        const operation = switch (try self.tokenizer.peek()) {
            .times, .divide => try self.tokenizer.next(),
            else => return accumulator,
        };

        var lhs = accumulator;
        var rhs = try self.parseAtom();
        defer rhs.deinit();

        accumulator = .{ .value = try switch (operation) {
            .times => lhs.value.multiply(rhs.value, self.allocator),
            .divide => try lhs.value.divide(rhs.value, self.allocator),
            else => unreachable,
        } };
        if (false) std.debug.print("\n{} {} {} = {}\n", .{ lhs, operation, rhs, accumulator });
        lhs.deinit();
    }
}
fn parseAdditiveExpression(self: *Assembler) Error!Expression {
    var accumulator = try self.parseMultiplicativeExpression();
    defer accumulator.deinit();

    while (true) {
        const operation = switch (try self.tokenizer.peek()) {
            .plus, .minus => try self.tokenizer.next(),
            else => return accumulator,
        };

        var lhs = accumulator;
        var rhs = try self.parseMultiplicativeExpression();
        defer rhs.deinit();

        accumulator = .{ .value = try switch (operation) {
            .plus => lhs.value.add(rhs.value, self.allocator),
            .minus => lhs.value.subtract(rhs.value, self.allocator),
            else => unreachable,
        } };
        if (false) std.debug.print("\n{} {} {} = {}\n", .{ lhs, operation, rhs, accumulator });
        lhs.deinit();
    }
}
fn parseExpression(self: *Assembler) Error!Expression {
    return try self.parseAdditiveExpression();
}

test "as" {
    const expected = @embedFile("as/ez80insts.bin");
    const actual = try assemble(std.testing.allocator, @embedFile("as/ez80insts.src"));
    try std.testing.expectEqualSlices(u8, expected, actual);
}

test {
    std.testing.refAllDecls(Assembler);
}
