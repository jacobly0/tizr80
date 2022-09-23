const std = @import("std");

const Assembler = @This();
const SmallArrayList = @import("small_array_list.zig").SmallArrayList;
const Tokenizer = @import("as/tokenizer.zig");
const util = @import("util.zig");
const Value = @import("as/value.zig");

const Error = error{
    ExpectedEndOfLine,
    IllegalCondition,
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

const Expr = struct {
    indirect: bool = false,
    value: Value,

    pub fn deinit(self: *Expr) void {
        self.value.deinit();
        self.* = undefined;
    }

    pub fn isImmediate(self: Expr) bool {
        return !self.indirect and self.value.isImmediate();
    }
    pub fn isIndirectImmediate(self: Expr) bool {
        return self.indirect and self.value.isImmediate();
    }
    pub fn isRegister(self: Expr) bool {
        return !self.indirect and self.value.isRegister();
    }
    pub fn isIndirectRegister(self: Expr) bool {
        return self.indirect and self.value.isRegister();
    }
    pub fn isRegisterOffset(self: Expr) bool {
        return !self.indirect and self.value.isRegisterOffset();
    }
    pub fn isIndirectRegisterOffset(self: Expr) bool {
        return !self.indirect and self.value.isRegisterOffset();
    }
    pub fn getRegister(self: Expr) Value.Register {
        return self.value.getBase().?;
    }

    pub fn format(
        self: Expr,
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

const Emit = union(enum) {
    prefix: Expr,
    opcode: u8,
    byte: Expr,
    offset: Expr,
    relative: Expr,
    word: Expr,
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

fn init(allocator: std.mem.Allocator, source: [:0]const u8) !Assembler {
    return .{ .allocator = allocator, .tokenizer = Tokenizer.init(source) };
}
fn deinit(self: *Assembler) void {
    self.output.deinit(self.allocator);
    self.* = undefined;
}

fn emitBytes(self: *Assembler, bytes: []const u8) !void {
    try self.output.appendSlice(self.allocator, bytes);
}
fn encodePrefix(expr: Expr) !?u8 {
    return switch (expr.value.getBase() orelse return Error.IllegalInstruction) {
        else => null,
        .ix, .ixh, .ixl => 0o335,
        .iy, .iyh, .iyl => 0o375,
    };
}
fn exactByteSizeOf(comptime T: type) usize {
    return @divExact(@bitSizeOf(T), 8);
}
fn encodeInt(value: anytype) [exactByteSizeOf(@TypeOf(value))]u8 {
    var bytes: [exactByteSizeOf(@TypeOf(value))]u8 = undefined;
    std.mem.writeIntSliceLittle(@TypeOf(value), &bytes, value);
    return bytes;
}
fn encodeImmediate(
    comptime PositiveInt: type,
    comptime NegativeInt: type,
    value: Value,
) error{ImmediateOutOfRange}!PositiveInt {
    return switch (value.getOffset()) {
        .small => |small| if (small >= 0)
            std.math.cast(PositiveInt, small) orelse return Error.ImmediateOutOfRange
        else
            @bitCast(
                PositiveInt,
                std.math.cast(NegativeInt, small) orelse return Error.ImmediateOutOfRange,
            ),
        .big => |big| if (big.positive)
            big.to(PositiveInt) catch return Error.ImmediateOutOfRange
        else
            @bitCast(PositiveInt, big.to(NegativeInt) catch return Error.ImmediateOutOfRange),
    };
}
fn emit(self: *Assembler, suffix: ?Mode, items: []const Emit) !void {
    if (suffix) |mode| try self.emitBytes(&[_]u8{mode.opcode()});
    for (items) |item| try self.emitBytes(&switch (item) {
        .prefix => |expr| [_]u8{switch (expr.value.getBase() orelse return Error.IllegalInstruction) {
            else => continue,
            .ix, .ixh, .ixl => 0o335,
            .iy, .iyh, .iyl => 0o375,
        }},
        .opcode => |opcode| [_]u8{opcode},
        .byte => |expr| if (expr.value.getBase() == null)
            encodeInt(try encodeImmediate(u8, i8, expr.value))
        else
            return Error.IllegalInstruction,
        .offset => |expr| switch (expr.value.getBase() orelse return Error.IllegalInstruction) {
            else => continue,
            .ix, .iy => encodeInt(try encodeImmediate(i8, i8, expr.value)),
        },
        .relative => |expr| if (expr.value.getBase() == null) bytes: {
            var pc = try Value.init(self.allocator, null, self.origin + self.output.items.len + 1);
            defer pc.deinit();

            var offset = try expr.value.subtract(pc, self.allocator);
            defer offset.deinit();

            if (false) std.debug.print("\nexpr = {}, pc = {}, offset = {}\n", .{ expr, pc, offset });
            break :bytes encodeInt(try encodeImmediate(i8, i8, offset));
        } else return Error.IllegalInstruction,
        .word => |expr| if (expr.value.getBase() == null)
            switch ((suffix orelse self.adl).imm) {
                .is => encodeInt(try encodeImmediate(u16, i16, expr.value)),
                .il => encodeInt(try encodeImmediate(u24, i24, expr.value)),
            }
        else
            return Error.IllegalInstruction,
    });
}

// TODO: remove
fn emitInt(self: *Assembler, value: anytype) !void {
    var data: [exactByteSizeOf(@TypeOf(value))]u8 = undefined;
    std.mem.writeIntSliceLittle(@TypeOf(value), &data, value);
    try self.emitBytes(&data);
}
fn emitImmediate(
    self: *Assembler,
    comptime PositiveInt: type,
    comptime NegativeInt: type,
    value: Value,
) !void {
    std.debug.assert(value.isImmediate());
    switch (value.getOffset()) {
        .small => |small| if (small >= 0)
            try self.emitInt(std.math.cast(PositiveInt, small) orelse return Error.ImmediateOutOfRange)
        else
            try self.emitInt(std.math.cast(NegativeInt, small) orelse return Error.ImmediateOutOfRange),
        .big => |big| if (big.positive)
            try self.emitInt(big.to(PositiveInt) catch return Error.ImmediateOutOfRange)
        else
            try self.emitInt(big.to(NegativeInt) catch return Error.ImmediateOutOfRange),
    }
}
fn emitByteImmediate(self: *Assembler, expr: Expr) !void {
    try self.emitImmediate(u8, i8, expr.value);
}
fn emitOffsetImmediate(self: *Assembler, expr: Expr) !void {
    try self.emitImmediate(i8, i8, expr.value);
}
fn emitShortImmediate(self: *Assembler, expr: Expr) !void {
    try self.emitImmediate(u16, i16, expr.value);
}
fn emitLongImmediate(self: *Assembler, expr: Expr) !void {
    try self.emitImmediate(u24, i24, expr.value);
}
fn emitRelativeImmediate(self: *Assembler, expr: Expr) !void {
    var pc = try Value.init(self.allocator, null, self.origin + self.output.items.len + 1);
    defer pc.deinit();
    var offset = try expr.value.subtract(pc, self.allocator);
    defer offset.deinit();
    if (false) std.debug.print("\nexpr = {}, pc = {}, offset = {}\n", .{ expr, pc, offset });
    try self.emitImmediate(i8, i8, offset);
}
fn emitWordImmediate(self: *Assembler, suffix: ?Mode, expr: Expr) !void {
    try switch ((suffix orelse self.adl).imm) {
        .is => self.emitShortImmediate(expr),
        .il => self.emitLongImmediate(expr),
    };
}

fn encodeSimpleCondition(expr: Expr) error{IllegalCondition}!u2 {
    return if (!expr.indirect and expr.value.zeroOffset())
        switch (expr.value.getBase() orelse return Error.IllegalCondition) {
            .nz => 0,
            .z => 1,
            .nc => 2,
            .c => 3,
            else => Error.IllegalCondition,
        }
    else
        Error.IllegalCondition;
}
fn encodeCondition(expr: Expr) error{IllegalCondition}!u3 {
    return if (!expr.indirect and expr.value.zeroOffset())
        switch (expr.value.getBase() orelse return Error.IllegalCondition) {
            .nz => 0,
            .z => 1,
            .nc => 2,
            .c => 3,
            .po => 4,
            .pe => 5,
            .p => 6,
            .m => 7,
            else => Error.IllegalCondition,
        }
    else
        Error.IllegalCondition;
}
fn encodeArithmetic(mnemonic: Tokenizer.Keyword) u3 {
    return switch (mnemonic) {
        .add => 0,
        .adc => 1,
        .sub => 2,
        .sbc => 3,
        .@"and" => 4,
        .xor => 5,
        .@"or" => 6,
        .cp => 7,
        else => unreachable,
    };
}
fn encodeByteRegister(
    expr: Expr,
    comptime half_index_allowed: enum { no_half_index, allow_half_index },
    comptime index_allowed: enum { no_index, allow_index },
) error{IllegalInstruction}!u3 {
    return if (!expr.indirect)
        if (expr.value.zeroOffset())
            switch (expr.value.getBase() orelse return Error.IllegalInstruction) {
                .b => 0,
                .c => 1,
                .d => 2,
                .e => 3,
                .h => 4,
                .ixh, .iyh => switch (half_index_allowed) {
                    .no_half_index => Error.IllegalInstruction,
                    .allow_half_index => 4,
                },
                .l => 5,
                .ixl, .iyl => switch (half_index_allowed) {
                    .no_half_index => Error.IllegalInstruction,
                    .allow_half_index => 5,
                },
                .a => 7,
                else => Error.IllegalInstruction,
            }
        else
            Error.IllegalInstruction
    else switch (expr.value.getBase() orelse return Error.IllegalInstruction) {
        .hl => if (expr.value.zeroOffset()) 6 else Error.IllegalInstruction,
        .ix, .iy => switch (index_allowed) {
            .no_index => Error.IllegalInstruction,
            .allow_index => 6,
        },
        else => Error.IllegalInstruction,
    };
}
fn encodeWordRegister(
    expr: Expr,
    matching: ?Expr,
    comptime alternate_register: enum { sp, af },
) error{IllegalInstruction}!u2 {
    return if (!expr.indirect and expr.value.zeroOffset())
        switch (expr.value.getBase() orelse return Error.IllegalInstruction) {
            .bc => 0,
            .de => 1,
            .hl, .ix, .iy => |register| if (matching == null or
                register == matching.?.value.getBase().?) 2 else Error.IllegalInstruction,
            .sp => switch (alternate_register) {
                .sp => 3,
                .af => Error.IllegalInstruction,
            },
            .af => switch (alternate_register) {
                .af => 3,
                .sp => Error.IllegalInstruction,
            },
            else => Error.IllegalInstruction,
        }
    else
        Error.IllegalInstruction;
}
fn encodeBitIndex(expr: Expr) error{ IllegalInstruction, ImmediateOutOfRange }!u3 {
    return if (!expr.indirect and expr.value.getBase() == null)
        encodeImmediate(u3, u3, expr.value)
    else
        Error.IllegalInstruction;
}
fn encodeRstTarget(expr: Expr) error{IllegalInstruction}!u3 {
    if (expr.indirect or expr.value.getBase() != null) return Error.IllegalInstruction;
    const target = encodeImmediate(u6, u6, expr.value) catch return Error.IllegalInstruction;
    return if (util.bit.extract(target, u3, 0) == 0)
        util.bit.extract(target, u3, 3)
    else
        Error.IllegalInstruction;
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

    var operand_list: SmallArrayList(Expr, 2) = .{};
    defer operand_list.deinit(self.allocator);
    defer for (operand_list.items()) |*operand| operand.deinit();
    if (switch (try self.tokenizer.peek()) {
        else => true,
        .eof, .eol, .comment => false,
    }) while (true) {
        try operand_list.append(self.allocator, try self.parseExpr());
        if (try self.tokenizer.peek() != .comma) break;
        _ = try self.tokenizer.next();
    };
    const operands = operand_list.items();

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
    try self.emit(suffix, switch (mnemonic) {
        .adc,
        .add,
        .@"and",
        .cp,
        .@"or",
        .sbc,
        .sub,
        .xor,
        => if (operands.len == 2 and operands[0].isRegister()) switch (operands[0].getRegister()) {
            .a => if (operands[1].value.getBase() != null)
                &[_]Emit{
                    .{ .prefix = operands[1] },
                    .{ .opcode = util.bit.concat(.{
                        @as(u2, 2),
                        encodeArithmetic(mnemonic),
                        try encodeByteRegister(operands[1], .allow_half_index, .allow_index),
                    }) },
                    .{ .offset = operands[1] },
                }
            else
                &[_]Emit{
                    .{ .opcode = util.bit.concat(.{
                        @as(u2, 3),
                        encodeArithmetic(mnemonic),
                        @as(u3, 6),
                    }) },
                    .{ .byte = operands[1] },
                },
            else => switch (mnemonic) {
                .add => switch (operands[0].getRegister()) {
                    .hl, .ix, .iy => &[_]Emit{
                        .{ .prefix = operands[0] },
                        .{ .opcode = util.bit.concat(.{
                            @as(u2, 0),
                            try encodeWordRegister(operands[1], operands[0], .sp),
                            @as(u4, 0o11),
                        }) },
                    },
                    else => return Error.IllegalInstruction,
                },
                .sbc, .adc => switch (operands[0].getRegister()) {
                    .hl => &[_]Emit{
                        .{ .opcode = 0o355 },
                        .{ .opcode = util.bit.concat(.{
                            @as(u2, 1),
                            try encodeWordRegister(operands[1], operands[0], .sp),
                            @as(u1, switch (mnemonic) {
                                .sbc => 0,
                                .adc => 1,
                                else => unreachable,
                            }),
                            @as(u3, 2),
                        }) },
                    },
                    else => return Error.IllegalInstruction,
                },
                else => return Error.IllegalInstruction,
            },
        } else return Error.IllegalInstruction,
        .bit, .res, .set => if (operands.len == 2)
            &[_]Emit{
                .{ .prefix = operands[1] },
                .{ .opcode = 0o313 },
                .{ .offset = operands[1] },
                .{ .opcode = util.bit.concat(.{
                    @as(u2, switch (mnemonic) {
                        .bit => 1,
                        .res => 2,
                        .set => 3,
                        else => unreachable,
                    }),
                    try encodeBitIndex(operands[0]),
                    try encodeByteRegister(operands[1], .no_half_index, .allow_index),
                }) },
            }
        else
            return Error.IllegalInstruction,
        .call => &[_]Emit{
            .{ .opcode = switch (operands.len) {
                1 => 0o315,
                2 => util.bit.concat(.{
                    @as(u2, 3),
                    try encodeCondition(operands[0]),
                    @as(u3, 4),
                }),
                else => return Error.IllegalInstruction,
            } },
            .{ .word = operands[operands.len - 1] },
        },
        .ccf,
        .cpl,
        .daa,
        .di,
        .ei,
        .exx,
        .halt,
        .nop,
        .rla,
        .rlca,
        .rra,
        .rrca,
        .scf,
        => if (operands.len == 0)
            &[_]Emit{.{ .opcode = switch (mnemonic) {
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
                .di => 0o363,
                .ei => 0o373,
                else => unreachable,
            } }}
        else
            return Error.IllegalInstruction,
        .dec, .inc => if (operands.len == 1)
            if (encodeWordRegister(operands[0], null, .sp) catch null) |word_register|
                &[_]Emit{
                    .{ .prefix = operands[0] },
                    .{ .opcode = util.bit.concat(.{
                        @as(u2, 0),
                        word_register,
                        @as(u1, switch (mnemonic) {
                            .inc => 0,
                            .dec => 1,
                            else => unreachable,
                        }),
                        @as(u3, 3),
                    }) },
                }
            else
                &[_]Emit{
                    .{ .prefix = operands[0] },
                    .{ .opcode = util.bit.concat(.{
                        @as(u2, 0),
                        try encodeByteRegister(operands[0], .allow_half_index, .allow_index),
                        @as(u2, 2),
                        @as(u1, switch (mnemonic) {
                            .inc => 0,
                            .dec => 1,
                            else => unreachable,
                        }),
                    }) },
                    .{ .offset = operands[0] },
                }
        else
            return Error.IllegalInstruction,
        .djnz => if (operands.len == 1)
            &[_]Emit{
                .{ .opcode = 0o020 },
                .{ .relative = operands[0] },
            }
        else
            return Error.IllegalInstruction,
        .ex => if (operands.len == 2)
            if (operands[0].isRegister() and operands[0].getRegister() == .af and
                operands[1].isRegister() and operands[1].getRegister() == .@"af'")
                &[_]Emit{.{ .opcode = 0o010 }}
            else if (operands[0].isIndirectRegister() and operands[0].getRegister() == .sp and
                operands[1].isRegister() and operands[1].getRegister() == .hl)
                &[_]Emit{ .{ .prefix = operands[1] }, .{ .opcode = 0o343 } }
            else if (operands[0].isRegister() and operands[0].getRegister() == .de and
                operands[1].isRegister() and operands[1].getRegister() == .hl)
                &[_]Emit{.{ .opcode = 0o353 }}
            else
                return Error.IllegalInstruction
        else
            return Error.IllegalInstruction,
        .in => if (operands.len == 2 and operands[0].isRegister() and
            operands[0].getRegister() == .a and operands[1].isIndirectImmediate())
            &[_]Emit{
                .{ .opcode = 0o333 },
                .{ .byte = operands[1] },
            }
        else
            return Error.IllegalInstruction,
        .jp => if (operands.len == 1 and operands[0].isIndirectRegister())
            switch (operands[0].getRegister()) {
                .hl, .ix, .iy => &[_]Emit{
                    .{ .prefix = operands[0] },
                    .{ .opcode = 0o351 },
                },
                else => return Error.IllegalInstruction,
            }
        else
            &[_]Emit{
                .{ .opcode = switch (operands.len) {
                    1 => 0o303,
                    2 => util.bit.concat(.{
                        @as(u2, 3),
                        try encodeCondition(operands[0]),
                        @as(u3, 2),
                    }),
                    else => return Error.IllegalInstruction,
                } },
                .{ .word = operands[operands.len - 1] },
            },
        .jr => &[_]Emit{
            .{ .opcode = switch (operands.len) {
                1 => 0o030,
                2 => util.bit.concat(.{
                    @as(u3, 1),
                    try encodeSimpleCondition(operands[0]),
                    @as(u3, 0),
                }),
                else => return Error.IllegalInstruction,
            } },
            .{ .relative = operands[operands.len - 1] },
        },
        .ld => if (operands.len == 2)
            if (operands[0].isRegister() and operands[1].isRegister())
                switch (operands[0].getRegister()) {
                    .sp => switch (operands[1].getRegister()) {
                        .hl, .ix, .iy => &[_]Emit{
                            .{ .prefix = operands[1] },
                            .{ .opcode = 0o371 },
                        },
                        else => null,
                    },
                    else => null,
                } orelse &[_]Emit{}
            else
                &[_]Emit{}
        else
            return Error.IllegalInstruction,
        .out => if (operands.len == 2 and operands[0].isIndirectImmediate() and
            operands[1].isRegister() and operands[1].getRegister() == .a)
            &[_]Emit{
                .{ .opcode = 0o323 },
                .{ .byte = operands[0] },
            }
        else
            return Error.IllegalInstruction,
        .pop, .push => if (operands.len == 1)
            &[_]Emit{
                .{ .prefix = operands[0] },
                .{ .opcode = util.bit.concat(.{
                    @as(u2, 3),
                    try encodeWordRegister(operands[0], null, .af),
                    @as(u1, 0),
                    @as(u1, switch (mnemonic) {
                        .pop => 0,
                        .push => 1,
                        else => unreachable,
                    }),
                    @as(u2, 1),
                }) },
            }
        else
            return Error.IllegalInstruction,
        .ret => &[_]Emit{.{ .opcode = switch (operands.len) {
            0 => 0o311,
            1 => util.bit.concat(.{
                @as(u2, 3),
                try encodeCondition(operands[0]),
                @as(u3, 0),
            }),
            else => return Error.IllegalInstruction,
        } }},
        .rl, .rlc, .rr, .rrc, .sla, .sra, .srl => if (operands.len == 1)
            &[_]Emit{
                .{ .prefix = operands[0] },
                .{ .opcode = 0o313 },
                .{ .offset = operands[0] },
                .{ .opcode = util.bit.concat(.{
                    @as(u2, 0),
                    @as(u3, switch (mnemonic) {
                        .rlc => 0,
                        .rrc => 1,
                        .rl => 2,
                        .rr => 3,
                        .sla => 4,
                        .sra => 5,
                        .srl => 7,
                        else => unreachable,
                    }),
                    try encodeByteRegister(operands[0], .no_half_index, .allow_index),
                }) },
            }
        else
            return Error.IllegalInstruction,
        .rst => if (operands.len == 1)
            &[_]Emit{.{ .opcode = util.bit.concat(.{
                @as(u2, 3),
                try encodeRstTarget(operands[0]),
                @as(u3, 7),
            }) }}
        else
            return Error.IllegalInstruction,
        else => &[_]Emit{}, // TODO: unreachable
    });
    switch (mnemonic) {
        .adc,
        .add,
        .@"and",
        .bit,
        .res,
        .set,
        .call,
        .ccf,
        .cpl,
        .daa,
        .di,
        .ei,
        .exx,
        .halt,
        .nop,
        .rla,
        .rlca,
        .rra,
        .rrca,
        .scf,
        .cp,
        .dec,
        .djnz,
        .ex,
        .in,
        .inc,
        .jp,
        .jr,
        => {},
        .ld => if (operands.len == 2)
            if (operands[0].isRegister() and operands[1].isRegister())
                switch (operands[0].getRegister()) {
                    .b => switch (operands[1].getRegister()) {
                        .c => try self.emitBytes(&[_]u8{0o101}),
                        .d => try self.emitBytes(&[_]u8{0o102}),
                        .e => try self.emitBytes(&[_]u8{0o103}),
                        .h => try self.emitBytes(&[_]u8{0o104}),
                        .l => try self.emitBytes(&[_]u8{0o105}),
                        .a => try self.emitBytes(&[_]u8{0o107}),
                        else => return Error.IllegalInstruction,
                    },
                    .c => switch (operands[1].getRegister()) {
                        .b => try self.emitBytes(&[_]u8{0o110}),
                        .d => try self.emitBytes(&[_]u8{0o112}),
                        .e => try self.emitBytes(&[_]u8{0o113}),
                        .h => try self.emitBytes(&[_]u8{0o114}),
                        .l => try self.emitBytes(&[_]u8{0o115}),
                        .a => try self.emitBytes(&[_]u8{0o117}),
                        else => return Error.IllegalInstruction,
                    },
                    .d => switch (operands[1].getRegister()) {
                        .b => try self.emitBytes(&[_]u8{0o120}),
                        .c => try self.emitBytes(&[_]u8{0o121}),
                        .e => try self.emitBytes(&[_]u8{0o123}),
                        .h => try self.emitBytes(&[_]u8{0o124}),
                        .l => try self.emitBytes(&[_]u8{0o125}),
                        .a => try self.emitBytes(&[_]u8{0o127}),
                        else => return Error.IllegalInstruction,
                    },
                    .e => switch (operands[1].getRegister()) {
                        .b => try self.emitBytes(&[_]u8{0o130}),
                        .c => try self.emitBytes(&[_]u8{0o131}),
                        .d => try self.emitBytes(&[_]u8{0o132}),
                        .h => try self.emitBytes(&[_]u8{0o134}),
                        .l => try self.emitBytes(&[_]u8{0o135}),
                        .a => try self.emitBytes(&[_]u8{0o137}),
                        else => return Error.IllegalInstruction,
                    },
                    .h => switch (operands[1].getRegister()) {
                        .b => try self.emitBytes(&[_]u8{0o140}),
                        .c => try self.emitBytes(&[_]u8{0o141}),
                        .d => try self.emitBytes(&[_]u8{0o142}),
                        .e => try self.emitBytes(&[_]u8{0o143}),
                        .h => try self.emitBytes(&[_]u8{0o144}),
                        .l => try self.emitBytes(&[_]u8{0o145}),
                        .a => try self.emitBytes(&[_]u8{0o147}),
                        else => return Error.IllegalInstruction,
                    },
                    .l => switch (operands[1].getRegister()) {
                        .b => try self.emitBytes(&[_]u8{0o150}),
                        .c => try self.emitBytes(&[_]u8{0o151}),
                        .d => try self.emitBytes(&[_]u8{0o152}),
                        .e => try self.emitBytes(&[_]u8{0o153}),
                        .h => try self.emitBytes(&[_]u8{0o154}),
                        .l => try self.emitBytes(&[_]u8{0o155}),
                        .a => try self.emitBytes(&[_]u8{0o157}),
                        else => return Error.IllegalInstruction,
                    },
                    .a => switch (operands[1].getRegister()) {
                        .b => try self.emitBytes(&[_]u8{0o170}),
                        .c => try self.emitBytes(&[_]u8{0o171}),
                        .d => try self.emitBytes(&[_]u8{0o172}),
                        .e => try self.emitBytes(&[_]u8{0o173}),
                        .h => try self.emitBytes(&[_]u8{0o174}),
                        .l => try self.emitBytes(&[_]u8{0o175}),
                        .a => try self.emitBytes(&[_]u8{0o177}),
                        else => return Error.IllegalInstruction,
                    },
                    .sp => {},
                    else => return Error.IllegalInstruction,
                }
            else if (operands[0].isRegister() and operands[1].isImmediate())
                switch (operands[0].getRegister()) {
                    .b, .c, .d, .e, .h, .l, .a => |register| {
                        try self.emitBytes(&[_]u8{switch (register) {
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
                        try self.emitBytes(&[_]u8{switch (register) {
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
                try self.emitBytes(&[_]u8{0o066});
                try self.emitByteImmediate(operands[1]);
            } else if (operands[0].isIndirectRegister() and operands[1].isRegister())
                switch (operands[0].getRegister()) {
                    .bc => switch (operands[1].getRegister()) {
                        .a => try self.emitBytes(&[_]u8{0o002}),
                        else => return Error.IllegalInstruction,
                    },
                    .de => switch (operands[1].getRegister()) {
                        .a => try self.emitBytes(&[_]u8{0o022}),
                        else => return Error.IllegalInstruction,
                    },
                    .hl => switch (operands[1].getRegister()) {
                        .b => try self.emitBytes(&[_]u8{0o160}),
                        .c => try self.emitBytes(&[_]u8{0o161}),
                        .d => try self.emitBytes(&[_]u8{0o162}),
                        .e => try self.emitBytes(&[_]u8{0o163}),
                        .h => try self.emitBytes(&[_]u8{0o164}),
                        .l => try self.emitBytes(&[_]u8{0o165}),
                        .a => try self.emitBytes(&[_]u8{0o167}),
                        else => return Error.IllegalInstruction,
                    },
                    else => return Error.IllegalInstruction,
                }
            else if (operands[0].isRegister() and operands[1].isIndirectRegister())
                switch (operands[1].getRegister()) {
                    .bc => switch (operands[0].getRegister()) {
                        .a => try self.emitBytes(&[_]u8{0o012}),
                        else => return Error.IllegalInstruction,
                    },
                    .de => switch (operands[0].getRegister()) {
                        .a => try self.emitBytes(&[_]u8{0o032}),
                        else => return Error.IllegalInstruction,
                    },
                    .hl => switch (operands[0].getRegister()) {
                        .b => try self.emitBytes(&[_]u8{0o106}),
                        .c => try self.emitBytes(&[_]u8{0o116}),
                        .d => try self.emitBytes(&[_]u8{0o126}),
                        .e => try self.emitBytes(&[_]u8{0o136}),
                        .h => try self.emitBytes(&[_]u8{0o146}),
                        .l => try self.emitBytes(&[_]u8{0o156}),
                        .a => try self.emitBytes(&[_]u8{0o176}),
                        else => return Error.IllegalInstruction,
                    },
                    else => return Error.IllegalInstruction,
                }
            else if (operands[0].isRegister() and operands[1].isIndirectRegisterOffset()) {
                try self.emitBytes(&[_]u8{
                    switch (operands[1].getRegister()) {
                        .ix => 0o335,
                        .iy => 0o375,
                        else => return Error.IllegalInstruction,
                    },
                    switch (operands[0].getRegister()) {
                        .bc => 0o007,
                        .de => 0o027,
                        .hl => 0o047,
                        .ix => switch (operands[1].getRegister()) {
                            .ix => 0o067,
                            .iy => 0o061,
                            else => unreachable,
                        },
                        .iy => switch (operands[1].getRegister()) {
                            .ix => 0o061,
                            .iy => 0o067,
                            else => unreachable,
                        },
                        else => return Error.IllegalInstruction,
                    },
                });
                try self.emitOffsetImmediate(operands[1]);
            } else if (operands[0].isIndirectImmediate() and operands[1].isRegister())
                switch (operands[1].getRegister()) {
                    .hl => {
                        try self.emitBytes(&[_]u8{0o042});
                        try self.emitWordImmediate(suffix, operands[0]);
                    },
                    .a => {
                        try self.emitBytes(&[_]u8{0o062});
                        try self.emitWordImmediate(suffix, operands[0]);
                    },
                    else => return Error.IllegalInstruction,
                }
            else if (operands[0].isRegister() and operands[1].isIndirectImmediate())
                switch (operands[0].getRegister()) {
                    .hl => {
                        try self.emitBytes(&[_]u8{0o052});
                        try self.emitWordImmediate(suffix, operands[1]);
                    },
                    .a => {
                        try self.emitBytes(&[_]u8{0o072});
                        try self.emitWordImmediate(suffix, operands[1]);
                    },
                    else => return Error.IllegalInstruction,
                }
            else
                return Error.IllegalInstruction
        else
            return Error.IllegalInstruction,
        .@"or",
        .out,
        .pop,
        .push,
        .ret,
        .rl,
        .rlc,
        .rr,
        .rrc,
        .sla,
        .sra,
        .srl,
        .rst,
        .sbc,
        .sub,
        .xor,
        => {},
        else => unreachable,
    }
}
fn parseData(self: *Assembler) Error!void {
    const mnemonic = (try self.tokenizer.next()).keyword;

    if (switch (try self.tokenizer.peek()) {
        else => true,
        .eof, .eol, .comment => false,
    }) while (true) {
        var value = try self.parseExpr();
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
            .di,
            .djnz,
            .ei,
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

fn initExpr(self: *Assembler, base: ?Value.Register, offset: anytype) !Expr {
    return .{ .value = try Value.init(self.allocator, base, offset) };
}
fn parseAtom(self: *Assembler) Error!Expr {
    return switch (try self.tokenizer.next()) {
        .literal => |literal| .{ .value = try Value.parseLiteral(self.allocator, literal) },
        .keyword => |keyword| switch (keyword) {
            .a => try self.initExpr(.a, 0),
            .af => try self.initExpr(.af, 0),
            .@"af'" => try self.initExpr(.@"af'", 0),
            .b => try self.initExpr(.b, 0),
            .bc => try self.initExpr(.bc, 0),
            .c => try self.initExpr(.c, 0),
            .d => try self.initExpr(.d, 0),
            .de => try self.initExpr(.de, 0),
            .e => try self.initExpr(.e, 0),
            .h => try self.initExpr(.h, 0),
            .hl => try self.initExpr(.hl, 0),
            .l => try self.initExpr(.l, 0),
            .i => try self.initExpr(.i, 0),
            .ix => try self.initExpr(.ix, 0),
            .ixh => try self.initExpr(.ixh, 0),
            .ixl => try self.initExpr(.ixl, 0),
            .iy => try self.initExpr(.iy, 0),
            .iyh => try self.initExpr(.iyh, 0),
            .iyl => try self.initExpr(.iyl, 0),
            .m => try self.initExpr(.m, 0),
            .nc => try self.initExpr(.nc, 0),
            .nz => try self.initExpr(.nz, 0),
            .p => try self.initExpr(.p, 0),
            .pe => try self.initExpr(.pe, 0),
            .po => try self.initExpr(.po, 0),
            .sp => try self.initExpr(.sp, 0),
            .z => try self.initExpr(.z, 0),
            else => return Error.UnexpectedToken,
        },
        .dollar => try self.initExpr(null, self.origin + self.output.items.len),
        .lparen => value: {
            var sub_expr = try self.parseExpr();
            errdefer sub_expr.deinit();

            if (try self.tokenizer.next() != .rparen) return Error.UnclosedParentheses;

            break :value .{ .indirect = true, .value = sub_expr.value };
        },
        else => return Error.UnexpectedToken,
    };
}
fn parseMultiplicativeExpr(self: *Assembler) Error!Expr {
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
fn parseAdditiveExpr(self: *Assembler) Error!Expr {
    var accumulator = try self.parseMultiplicativeExpr();
    defer accumulator.deinit();

    while (true) {
        const operation = switch (try self.tokenizer.peek()) {
            .plus, .minus => try self.tokenizer.next(),
            else => return accumulator,
        };

        var lhs = accumulator;
        var rhs = try self.parseMultiplicativeExpr();
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
fn parseExpr(self: *Assembler) Error!Expr {
    return try self.parseAdditiveExpr();
}

test "as" {
    const expected = @embedFile("as/ez80insts.bin");
    const actual = try assemble(std.testing.allocator, @embedFile("as/ez80insts.src"));
    defer std.testing.allocator.free(actual);
    if (false) try std.testing.expectEqualSlices(u8, expected, actual);
}

test {
    std.testing.refAllDecls(Assembler);
}
