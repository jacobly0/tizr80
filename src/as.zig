const std = @import("std");

const Assembler = @This();
const Tokenizer = @import("as/tokenizer.zig");
const util = @import("util.zig");
const Value = @import("as/value.zig");

const Error = error{
    ExpectedEndOfLine,
    IllegalCondition,
    IllegalInstruction,
    IllegalSuffix,
    ImmOutOfRange,
    UnclosedParentheses,
    UnexpectedToken,
} || Tokenizer.Error || Value.Error || std.mem.Allocator.Error;

const Fixup = struct {
    location: u24,
    operation: enum { add, subtract },
    symbol: []const u8,
};

const Expr = struct {
    ind: bool = false,
    value: Value,

    pub fn deinit(self: *Expr) void {
        self.value.deinit();
        self.* = undefined;
    }

    pub fn format(
        self: Expr,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (self.ind) try writer.writeByte('(');
        try self.value.format(fmt, options, writer);
        if (self.ind) try writer.writeByte(')');
    }
};

const Mode = packed struct(u2) {
    const Inst = enum(u1) { s, l };
    const Imm = enum(u1) { is, il };

    inst: Inst,
    imm: Imm,

    pub fn pre(self: Mode) u8 {
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

const Pattern = union(enum) {
    reg: Value.Reg,
    ind_reg: Value.Reg,
    reg_off: Value.Reg,
    ind_reg_off: Value.Reg,
    imm,
    ind_imm,
    specific_imm: u8,

    pub fn match(self: Pattern, expr: Expr) bool {
        return switch (self) {
            .reg, .reg_off, .imm, .specific_imm => !expr.ind,
            .ind_reg, .ind_reg_off, .ind_imm => expr.ind,
        } and switch (self) {
            .reg, .ind_reg, .reg_off, .ind_reg_off => |reg| expr.value.getBase() == reg,
            .imm, .ind_imm => expr.value.getBase() == null,
            .specific_imm => |imm| expr.value.getBase() == null and expr.value.orderOffset(imm) == .eq,
        } and switch (self) {
            .reg, .ind_reg => expr.value.zeroOffset(),
            else => true,
        };
    }
};

const Encoder = union(enum) {
    bit_pre,
    ext_pre,
    idx_pre,
    ext_idx_pre,
    opc: u8,
    byte_imm,
    off_imm,
    rel_imm,
    word_imm,
};

const Rule = struct {
    patterns: []const []const Pattern,
    encoders: []const Encoder,
};

allocator: std.mem.Allocator,
tokenizer: Tokenizer,
fixups: std.SegmentedList(Fixup, 0) = .{},
output: std.ArrayListUnmanaged(u8) = .{},
origin: u24 = 0,
adl: Mode = .{ .inst = .l, .imm = .il },

const ez80 = std.enums.directEnumArrayDefault(Tokenizer.Keyword, []const Rule, &.{}, 0, .{
    .adc = &.{
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .b } }}, .encoders = &.{.{ .opc = 0o210 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .c } }}, .encoders = &.{.{ .opc = 0o211 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .d } }}, .encoders = &.{.{ .opc = 0o212 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .e } }}, .encoders = &.{.{ .opc = 0o213 }} },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .reg = .h } },
            &.{ .{ .reg = .a }, .{ .reg = .ixh } },
            &.{ .{ .reg = .a }, .{ .reg = .iyh } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o214 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .reg = .l } },
            &.{ .{ .reg = .a }, .{ .reg = .ixl } },
            &.{ .{ .reg = .a }, .{ .reg = .iyl } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o215 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .ind_reg = .hl } },
            &.{ .{ .reg = .a }, .{ .ind_reg_off = .ix } },
            &.{ .{ .reg = .a }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o216 }, .off_imm } },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .a } }}, .encoders = &.{.{ .opc = 0o217 }} },

        .{ .patterns = &.{&.{ .{ .reg = .a }, .imm }}, .encoders = &.{ .{ .opc = 0o316 }, .byte_imm } },

        .{
            .patterns = &.{&.{ .{ .reg = .hl }, .{ .reg = .bc } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o112 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .hl }, .{ .reg = .de } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o132 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .hl }, .{ .reg = .hl } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o152 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .hl }, .{ .reg = .sp } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o172 } },
        },
    },
    .add = &.{
        .{ .patterns = &.{
            &.{ .{ .reg = .hl }, .{ .reg = .bc } },
            &.{ .{ .reg = .ix }, .{ .reg = .bc } },
            &.{ .{ .reg = .iy }, .{ .reg = .bc } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o011 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .hl }, .{ .reg = .de } },
            &.{ .{ .reg = .ix }, .{ .reg = .de } },
            &.{ .{ .reg = .iy }, .{ .reg = .de } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o031 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .hl }, .{ .reg = .hl } },
            &.{ .{ .reg = .ix }, .{ .reg = .ix } },
            &.{ .{ .reg = .iy }, .{ .reg = .iy } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o051 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .hl }, .{ .reg = .sp } },
            &.{ .{ .reg = .ix }, .{ .reg = .sp } },
            &.{ .{ .reg = .iy }, .{ .reg = .sp } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o071 } } },

        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .b } }}, .encoders = &.{.{ .opc = 0o200 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .c } }}, .encoders = &.{.{ .opc = 0o201 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .d } }}, .encoders = &.{.{ .opc = 0o202 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .e } }}, .encoders = &.{.{ .opc = 0o203 }} },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .reg = .h } },
            &.{ .{ .reg = .a }, .{ .reg = .ixh } },
            &.{ .{ .reg = .a }, .{ .reg = .iyh } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o204 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .reg = .l } },
            &.{ .{ .reg = .a }, .{ .reg = .ixl } },
            &.{ .{ .reg = .a }, .{ .reg = .iyl } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o205 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .ind_reg = .hl } },
            &.{ .{ .reg = .a }, .{ .ind_reg_off = .ix } },
            &.{ .{ .reg = .a }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o206 }, .off_imm } },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .a } }}, .encoders = &.{.{ .opc = 0o207 }} },

        .{ .patterns = &.{&.{ .{ .reg = .a }, .imm }}, .encoders = &.{ .{ .opc = 0o306 }, .byte_imm } },
    },
    .@"and" = &.{
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .b } }}, .encoders = &.{.{ .opc = 0o240 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .c } }}, .encoders = &.{.{ .opc = 0o241 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .d } }}, .encoders = &.{.{ .opc = 0o242 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .e } }}, .encoders = &.{.{ .opc = 0o243 }} },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .reg = .h } },
            &.{ .{ .reg = .a }, .{ .reg = .ixh } },
            &.{ .{ .reg = .a }, .{ .reg = .iyh } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o244 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .reg = .l } },
            &.{ .{ .reg = .a }, .{ .reg = .ixl } },
            &.{ .{ .reg = .a }, .{ .reg = .iyl } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o245 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .ind_reg = .hl } },
            &.{ .{ .reg = .a }, .{ .ind_reg_off = .ix } },
            &.{ .{ .reg = .a }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o246 }, .off_imm } },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .a } }}, .encoders = &.{.{ .opc = 0o247 }} },

        .{ .patterns = &.{&.{ .{ .reg = .a }, .imm }}, .encoders = &.{ .{ .opc = 0o346 }, .byte_imm } },
    },
    .bit = &.{
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o100 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o101 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o102 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o103 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o104 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o105 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 0 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 0 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 0 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o106 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o107 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o110 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o111 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o112 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o113 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o114 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o115 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 1 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 1 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 1 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o116 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o117 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o120 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o121 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o122 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o123 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o124 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o125 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 2 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 2 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 2 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o126 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o127 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o130 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o131 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o132 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o133 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o134 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o135 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 3 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 3 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 3 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o136 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o137 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o140 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o141 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o142 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o143 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o144 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o145 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 4 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 4 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 4 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o146 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o147 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o150 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o151 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o152 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o153 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o154 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o155 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 5 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 5 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 5 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o156 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o157 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o160 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o161 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o162 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o163 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o164 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o165 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 6 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 6 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 6 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o166 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o167 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o170 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o171 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o172 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o173 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o174 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o175 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 7 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 7 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 7 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o176 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o177 } },
        },
    },
    .call = &.{
        .{
            .patterns = &.{&.{ .{ .reg = .nz }, .imm }},
            .encoders = &.{ .{ .opc = 0o304 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .z }, .imm }},
            .encoders = &.{ .{ .opc = 0o314 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .nc }, .imm }},
            .encoders = &.{ .{ .opc = 0o324 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .c }, .imm }},
            .encoders = &.{ .{ .opc = 0o334 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .po }, .imm }},
            .encoders = &.{ .{ .opc = 0o344 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .pe }, .imm }},
            .encoders = &.{ .{ .opc = 0o354 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .p }, .imm }},
            .encoders = &.{ .{ .opc = 0o364 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .m }, .imm }},
            .encoders = &.{ .{ .opc = 0o374 }, .word_imm },
        },

        .{ .patterns = &.{&.{.imm}}, .encoders = &.{ .{ .opc = 0o315 }, .word_imm } },
    },
    .ccf = &.{.{ .patterns = &.{&.{}}, .encoders = &.{.{ .opc = 0o077 }} }},
    .cp = &.{
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .b } }}, .encoders = &.{.{ .opc = 0o270 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .c } }}, .encoders = &.{.{ .opc = 0o271 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .d } }}, .encoders = &.{.{ .opc = 0o272 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .e } }}, .encoders = &.{.{ .opc = 0o273 }} },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .reg = .h } },
            &.{ .{ .reg = .a }, .{ .reg = .ixh } },
            &.{ .{ .reg = .a }, .{ .reg = .iyh } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o274 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .reg = .l } },
            &.{ .{ .reg = .a }, .{ .reg = .ixl } },
            &.{ .{ .reg = .a }, .{ .reg = .iyl } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o275 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .ind_reg = .hl } },
            &.{ .{ .reg = .a }, .{ .ind_reg_off = .ix } },
            &.{ .{ .reg = .a }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o276 }, .off_imm } },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .a } }}, .encoders = &.{.{ .opc = 0o277 }} },

        .{ .patterns = &.{&.{ .{ .reg = .a }, .imm }}, .encoders = &.{ .{ .opc = 0o376 }, .byte_imm } },
    },
    .cpd = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o251 } } }},
    .cpdr = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o271 } } }},
    .cpi = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o241 } } }},
    .cpir = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o261 } } }},
    .cpl = &.{.{ .patterns = &.{&.{}}, .encoders = &.{.{ .opc = 0o057 }} }},
    .daa = &.{.{ .patterns = &.{&.{}}, .encoders = &.{.{ .opc = 0o047 }} }},
    .dec = &.{
        .{ .patterns = &.{&.{.{ .reg = .b }}}, .encoders = &.{.{ .opc = 0o005 }} },
        .{ .patterns = &.{&.{.{ .reg = .bc }}}, .encoders = &.{.{ .opc = 0o013 }} },
        .{ .patterns = &.{&.{.{ .reg = .c }}}, .encoders = &.{.{ .opc = 0o015 }} },

        .{ .patterns = &.{&.{.{ .reg = .d }}}, .encoders = &.{.{ .opc = 0o025 }} },
        .{ .patterns = &.{&.{.{ .reg = .de }}}, .encoders = &.{.{ .opc = 0o033 }} },
        .{ .patterns = &.{&.{.{ .reg = .e }}}, .encoders = &.{.{ .opc = 0o035 }} },

        .{ .patterns = &.{
            &.{.{ .reg = .h }},
            &.{.{ .reg = .ixh }},
            &.{.{ .reg = .iyh }},
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o045 } } },
        .{ .patterns = &.{
            &.{.{ .reg = .hl }},
            &.{.{ .reg = .ix }},
            &.{.{ .reg = .iy }},
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o053 } } },
        .{ .patterns = &.{
            &.{.{ .reg = .l }},
            &.{.{ .reg = .ixl }},
            &.{.{ .reg = .iyl }},
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o055 } } },

        .{ .patterns = &.{
            &.{.{ .ind_reg = .hl }},
            &.{.{ .ind_reg_off = .ix }},
            &.{.{ .ind_reg_off = .iy }},
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o065 }, .off_imm } },
        .{ .patterns = &.{&.{.{ .reg = .sp }}}, .encoders = &.{.{ .opc = 0o073 }} },
        .{ .patterns = &.{&.{.{ .reg = .a }}}, .encoders = &.{.{ .opc = 0o075 }} },
    },
    .di = &.{.{ .patterns = &.{&.{}}, .encoders = &.{.{ .opc = 0o363 }} }},
    .djnz = &.{.{ .patterns = &.{&.{.imm}}, .encoders = &.{ .{ .opc = 0o020 }, .rel_imm } }},
    .ei = &.{.{ .patterns = &.{&.{}}, .encoders = &.{.{ .opc = 0o373 }} }},
    .ex = &.{
        .{
            .patterns = &.{&.{ .{ .reg = .af }, .{ .reg = .@"af'" } }},
            .encoders = &.{.{ .opc = 0o010 }},
        },
        .{
            .patterns = &.{&.{ .{ .reg = .de }, .{ .reg = .hl } }},
            .encoders = &.{.{ .opc = 0o353 }},
        },
        .{ .patterns = &.{
            &.{ .{ .ind_reg = .sp }, .{ .reg = .hl } },
            &.{ .{ .ind_reg = .sp }, .{ .reg = .ix } },
            &.{ .{ .ind_reg = .sp }, .{ .reg = .iy } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o343 } } },
    },
    .exx = &.{.{ .patterns = &.{&.{}}, .encoders = &.{.{ .opc = 0o331 }} }},
    .halt = &.{.{ .patterns = &.{&.{}}, .encoders = &.{.{ .opc = 0o166 }} }},
    .im = &.{
        .{ .patterns = &.{&.{.{ .specific_imm = 0 }}}, .encoders = &.{ .ext_pre, .{ .opc = 0o106 } } },
        .{ .patterns = &.{&.{.{ .specific_imm = 1 }}}, .encoders = &.{ .ext_pre, .{ .opc = 0o126 } } },
        .{ .patterns = &.{&.{.{ .specific_imm = 2 }}}, .encoders = &.{ .ext_pre, .{ .opc = 0o136 } } },
    },
    .in = &.{
        .{
            .patterns = &.{&.{ .{ .reg = .a }, .ind_imm }},
            .encoders = &.{ .{ .opc = 0o333 }, .byte_imm },
        },

        .{
            .patterns = &.{&.{ .{ .reg = .b }, .{ .ind_reg = .bc } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o100 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .c }, .{ .ind_reg = .bc } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o110 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .d }, .{ .ind_reg = .bc } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o120 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .e }, .{ .ind_reg = .bc } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o130 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .h }, .{ .ind_reg = .bc } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o140 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .l }, .{ .ind_reg = .bc } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o150 } },
        },
        .{ .patterns = &.{
            &.{.{ .ind_reg = .bc }},
            &.{ .{ .reg = .f }, .{ .ind_reg = .bc } },
        }, .encoders = &.{ .ext_pre, .{ .opc = 0o160 } } },
        .{
            .patterns = &.{&.{ .{ .reg = .a }, .{ .ind_reg = .bc } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o170 } },
        },
    },
    .in0 = &.{
        .{
            .patterns = &.{&.{ .{ .reg = .b }, .ind_imm }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o000 }, .byte_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .c }, .ind_imm }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o010 }, .byte_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .d }, .ind_imm }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o020 }, .byte_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .e }, .ind_imm }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o030 }, .byte_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .h }, .ind_imm }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o040 }, .byte_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .l }, .ind_imm }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o050 }, .byte_imm },
        },
        .{ .patterns = &.{
            &.{.ind_imm},
            &.{ .{ .reg = .f }, .ind_imm },
        }, .encoders = &.{ .ext_pre, .{ .opc = 0o060 }, .byte_imm } },
        .{
            .patterns = &.{&.{ .{ .reg = .a }, .ind_imm }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o070 }, .byte_imm },
        },
    },
    .inc = &.{
        .{ .patterns = &.{&.{.{ .reg = .bc }}}, .encoders = &.{.{ .opc = 0o003 }} },
        .{ .patterns = &.{&.{.{ .reg = .b }}}, .encoders = &.{.{ .opc = 0o004 }} },
        .{ .patterns = &.{&.{.{ .reg = .c }}}, .encoders = &.{.{ .opc = 0o014 }} },

        .{ .patterns = &.{&.{.{ .reg = .de }}}, .encoders = &.{.{ .opc = 0o023 }} },
        .{ .patterns = &.{&.{.{ .reg = .d }}}, .encoders = &.{.{ .opc = 0o024 }} },
        .{ .patterns = &.{&.{.{ .reg = .e }}}, .encoders = &.{.{ .opc = 0o034 }} },

        .{ .patterns = &.{
            &.{.{ .reg = .hl }},
            &.{.{ .reg = .ix }},
            &.{.{ .reg = .iy }},
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o043 } } },
        .{ .patterns = &.{
            &.{.{ .reg = .h }},
            &.{.{ .reg = .ixh }},
            &.{.{ .reg = .iyh }},
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o044 } } },
        .{ .patterns = &.{
            &.{.{ .reg = .l }},
            &.{.{ .reg = .ixl }},
            &.{.{ .reg = .iyl }},
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o054 } } },

        .{ .patterns = &.{&.{.{ .reg = .sp }}}, .encoders = &.{.{ .opc = 0o063 }} },
        .{ .patterns = &.{
            &.{.{ .ind_reg = .hl }},
            &.{.{ .ind_reg_off = .ix }},
            &.{.{ .ind_reg_off = .iy }},
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o064 }, .off_imm } },
        .{ .patterns = &.{&.{.{ .reg = .a }}}, .encoders = &.{.{ .opc = 0o074 }} },
    },
    .ind = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o252 } } }},
    .ind2 = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o214 } } }},
    .ind2r = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o234 } } }},
    .indm = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o212 } } }},
    .indmr = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o232 } } }},
    .indr = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o272 } } }},
    .indrx = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o312 } } }},
    .ini = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o242 } } }},
    .ini2 = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o204 } } }},
    .ini2r = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o224 } } }},
    .inim = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o202 } } }},
    .inimr = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o222 } } }},
    .inir = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o262 } } }},
    .inirx = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o302 } } }},
    .jp = &.{
        .{
            .patterns = &.{&.{ .{ .reg = .nz }, .imm }},
            .encoders = &.{ .{ .opc = 0o302 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .z }, .imm }},
            .encoders = &.{ .{ .opc = 0o312 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .nc }, .imm }},
            .encoders = &.{ .{ .opc = 0o322 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .c }, .imm }},
            .encoders = &.{ .{ .opc = 0o332 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .po }, .imm }},
            .encoders = &.{ .{ .opc = 0o342 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .pe }, .imm }},
            .encoders = &.{ .{ .opc = 0o352 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .p }, .imm }},
            .encoders = &.{ .{ .opc = 0o362 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .m }, .imm }},
            .encoders = &.{ .{ .opc = 0o372 }, .word_imm },
        },

        .{ .patterns = &.{&.{.imm}}, .encoders = &.{ .{ .opc = 0o303 }, .word_imm } },

        .{ .patterns = &.{
            &.{.{ .ind_reg = .hl }},
            &.{.{ .ind_reg = .ix }},
            &.{.{ .ind_reg = .iy }},
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o351 } } },
    },
    .jr = &.{
        .{ .patterns = &.{&.{.imm}}, .encoders = &.{ .{ .opc = 0o030 }, .rel_imm } },

        .{ .patterns = &.{&.{ .{ .reg = .nz }, .imm }}, .encoders = &.{ .{ .opc = 0o040 }, .rel_imm } },
        .{ .patterns = &.{&.{ .{ .reg = .z }, .imm }}, .encoders = &.{ .{ .opc = 0o050 }, .rel_imm } },
        .{ .patterns = &.{&.{ .{ .reg = .nc }, .imm }}, .encoders = &.{ .{ .opc = 0o060 }, .rel_imm } },
        .{ .patterns = &.{&.{ .{ .reg = .c }, .imm }}, .encoders = &.{ .{ .opc = 0o070 }, .rel_imm } },
    },
    .ld = &.{
        .{
            .patterns = &.{&.{ .{ .reg = .bc }, .imm }},
            .encoders = &.{ .{ .opc = 0o001 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .ind_reg = .bc }, .{ .reg = .a } }},
            .encoders = &.{.{ .opc = 0o002 }},
        },
        .{
            .patterns = &.{&.{ .{ .reg = .b }, .imm }},
            .encoders = &.{ .{ .opc = 0o006 }, .byte_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .a }, .{ .ind_reg = .bc } }},
            .encoders = &.{.{ .opc = 0o012 }},
        },
        .{
            .patterns = &.{&.{ .{ .reg = .c }, .imm }},
            .encoders = &.{ .{ .opc = 0o016 }, .byte_imm },
        },

        .{
            .patterns = &.{&.{ .{ .reg = .de }, .imm }},
            .encoders = &.{ .{ .opc = 0o021 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .ind_reg = .de }, .{ .reg = .a } }},
            .encoders = &.{.{ .opc = 0o022 }},
        },
        .{
            .patterns = &.{&.{ .{ .reg = .d }, .imm }},
            .encoders = &.{ .{ .opc = 0o026 }, .byte_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .a }, .{ .ind_reg = .de } }},
            .encoders = &.{.{ .opc = 0o032 }},
        },
        .{
            .patterns = &.{&.{ .{ .reg = .e }, .imm }},
            .encoders = &.{ .{ .opc = 0o036 }, .byte_imm },
        },

        .{ .patterns = &.{
            &.{ .{ .reg = .hl }, .imm },
            &.{ .{ .reg = .ix }, .imm },
            &.{ .{ .reg = .iy }, .imm },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o041 }, .word_imm } },
        .{ .patterns = &.{
            &.{ .ind_imm, .{ .reg = .hl } },
            &.{ .ind_imm, .{ .reg = .ix } },
            &.{ .ind_imm, .{ .reg = .iy } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o042 }, .word_imm } },

        .{ .patterns = &.{
            &.{ .{ .reg = .h }, .imm },
            &.{ .{ .reg = .ixh }, .imm },
            &.{ .{ .reg = .iyh }, .imm },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o046 }, .byte_imm } },
        .{ .patterns = &.{
            &.{ .{ .reg = .hl }, .ind_imm },
            &.{ .{ .reg = .ix }, .ind_imm },
            &.{ .{ .reg = .iy }, .ind_imm },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o052 }, .word_imm } },
        .{ .patterns = &.{
            &.{ .{ .reg = .l }, .imm },
            &.{ .{ .reg = .ixl }, .imm },
            &.{ .{ .reg = .iyl }, .imm },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o056 }, .byte_imm } },

        .{
            .patterns = &.{&.{ .{ .reg = .sp }, .imm }},
            .encoders = &.{ .{ .opc = 0o061 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .ind_imm, .{ .reg = .a } }},
            .encoders = &.{ .{ .opc = 0o062 }, .word_imm },
        },
        .{ .patterns = &.{
            &.{ .{ .ind_reg = .hl }, .imm },
            &.{ .{ .ind_reg_off = .ix }, .imm },
            &.{ .{ .ind_reg_off = .iy }, .imm },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o066 }, .off_imm, .byte_imm } },
        .{
            .patterns = &.{&.{ .{ .reg = .a }, .ind_imm }},
            .encoders = &.{ .{ .opc = 0o072 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .a }, .imm }},
            .encoders = &.{ .{ .opc = 0o076 }, .byte_imm },
        },

        .{
            .patterns = &.{&.{ .{ .reg = .b }, .{ .reg = .c } }},
            .encoders = &.{.{ .opc = 0o101 }},
        },
        .{
            .patterns = &.{&.{ .{ .reg = .b }, .{ .reg = .d } }},
            .encoders = &.{.{ .opc = 0o102 }},
        },
        .{
            .patterns = &.{&.{ .{ .reg = .b }, .{ .reg = .e } }},
            .encoders = &.{.{ .opc = 0o103 }},
        },
        .{ .patterns = &.{
            &.{ .{ .reg = .b }, .{ .reg = .h } },
            &.{ .{ .reg = .b }, .{ .reg = .ixh } },
            &.{ .{ .reg = .b }, .{ .reg = .iyh } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o104 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .b }, .{ .reg = .l } },
            &.{ .{ .reg = .b }, .{ .reg = .ixl } },
            &.{ .{ .reg = .b }, .{ .reg = .iyl } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o105 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .b }, .{ .ind_reg = .hl } },
            &.{ .{ .reg = .b }, .{ .ind_reg_off = .ix } },
            &.{ .{ .reg = .b }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o106 }, .off_imm } },
        .{ .patterns = &.{&.{ .{ .reg = .b }, .{ .reg = .a } }}, .encoders = &.{.{ .opc = 0o107 }} },

        .{ .patterns = &.{&.{ .{ .reg = .c }, .{ .reg = .b } }}, .encoders = &.{.{ .opc = 0o110 }} },
        .{ .patterns = &.{&.{ .{ .reg = .c }, .{ .reg = .d } }}, .encoders = &.{.{ .opc = 0o112 }} },
        .{ .patterns = &.{&.{ .{ .reg = .c }, .{ .reg = .e } }}, .encoders = &.{.{ .opc = 0o113 }} },
        .{ .patterns = &.{
            &.{ .{ .reg = .c }, .{ .reg = .h } },
            &.{ .{ .reg = .c }, .{ .reg = .ixh } },
            &.{ .{ .reg = .c }, .{ .reg = .iyh } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o114 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .c }, .{ .reg = .l } },
            &.{ .{ .reg = .c }, .{ .reg = .ixl } },
            &.{ .{ .reg = .c }, .{ .reg = .iyl } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o115 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .c }, .{ .ind_reg = .hl } },
            &.{ .{ .reg = .c }, .{ .ind_reg_off = .ix } },
            &.{ .{ .reg = .c }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o116 }, .off_imm } },
        .{ .patterns = &.{&.{ .{ .reg = .c }, .{ .reg = .a } }}, .encoders = &.{.{ .opc = 0o117 }} },

        .{ .patterns = &.{&.{ .{ .reg = .d }, .{ .reg = .b } }}, .encoders = &.{.{ .opc = 0o120 }} },
        .{ .patterns = &.{&.{ .{ .reg = .d }, .{ .reg = .c } }}, .encoders = &.{.{ .opc = 0o121 }} },
        .{ .patterns = &.{&.{ .{ .reg = .d }, .{ .reg = .e } }}, .encoders = &.{.{ .opc = 0o123 }} },
        .{ .patterns = &.{
            &.{ .{ .reg = .d }, .{ .reg = .h } },
            &.{ .{ .reg = .d }, .{ .reg = .ixh } },
            &.{ .{ .reg = .d }, .{ .reg = .iyh } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o124 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .d }, .{ .reg = .l } },
            &.{ .{ .reg = .d }, .{ .reg = .ixl } },
            &.{ .{ .reg = .d }, .{ .reg = .iyl } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o125 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .d }, .{ .ind_reg = .hl } },
            &.{ .{ .reg = .d }, .{ .ind_reg_off = .ix } },
            &.{ .{ .reg = .d }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o126 }, .off_imm } },
        .{ .patterns = &.{&.{ .{ .reg = .d }, .{ .reg = .a } }}, .encoders = &.{.{ .opc = 0o127 }} },

        .{ .patterns = &.{&.{ .{ .reg = .e }, .{ .reg = .b } }}, .encoders = &.{.{ .opc = 0o130 }} },
        .{ .patterns = &.{&.{ .{ .reg = .e }, .{ .reg = .c } }}, .encoders = &.{.{ .opc = 0o131 }} },
        .{ .patterns = &.{&.{ .{ .reg = .e }, .{ .reg = .d } }}, .encoders = &.{.{ .opc = 0o132 }} },
        .{ .patterns = &.{
            &.{ .{ .reg = .e }, .{ .reg = .h } },
            &.{ .{ .reg = .e }, .{ .reg = .ixh } },
            &.{ .{ .reg = .e }, .{ .reg = .iyh } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o134 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .e }, .{ .reg = .l } },
            &.{ .{ .reg = .e }, .{ .reg = .ixl } },
            &.{ .{ .reg = .e }, .{ .reg = .iyl } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o135 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .e }, .{ .ind_reg = .hl } },
            &.{ .{ .reg = .e }, .{ .ind_reg_off = .ix } },
            &.{ .{ .reg = .e }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o136 }, .off_imm } },
        .{ .patterns = &.{&.{ .{ .reg = .e }, .{ .reg = .a } }}, .encoders = &.{.{ .opc = 0o137 }} },

        .{ .patterns = &.{
            &.{ .{ .reg = .h }, .{ .reg = .b } },
            &.{ .{ .reg = .ixh }, .{ .reg = .b } },
            &.{ .{ .reg = .iyh }, .{ .reg = .b } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o140 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .h }, .{ .reg = .c } },
            &.{ .{ .reg = .ixh }, .{ .reg = .c } },
            &.{ .{ .reg = .iyh }, .{ .reg = .c } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o141 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .h }, .{ .reg = .d } },
            &.{ .{ .reg = .ixh }, .{ .reg = .d } },
            &.{ .{ .reg = .iyh }, .{ .reg = .d } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o142 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .h }, .{ .reg = .e } },
            &.{ .{ .reg = .ixh }, .{ .reg = .e } },
            &.{ .{ .reg = .iyh }, .{ .reg = .e } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o143 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .h }, .{ .reg = .h } },
            &.{ .{ .reg = .ixh }, .{ .reg = .ixh } },
            &.{ .{ .reg = .iyh }, .{ .reg = .iyh } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o144 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .h }, .{ .reg = .l } },
            &.{ .{ .reg = .ixh }, .{ .reg = .ixl } },
            &.{ .{ .reg = .iyh }, .{ .reg = .iyl } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o145 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .h }, .{ .ind_reg = .hl } },
            &.{ .{ .reg = .h }, .{ .ind_reg_off = .ix } },
            &.{ .{ .reg = .h }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o146 }, .off_imm } },
        .{ .patterns = &.{
            &.{ .{ .reg = .h }, .{ .reg = .a } },
            &.{ .{ .reg = .ixh }, .{ .reg = .a } },
            &.{ .{ .reg = .iyh }, .{ .reg = .a } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o147 } } },

        .{ .patterns = &.{
            &.{ .{ .reg = .l }, .{ .reg = .b } },
            &.{ .{ .reg = .ixl }, .{ .reg = .b } },
            &.{ .{ .reg = .iyl }, .{ .reg = .b } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o150 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .l }, .{ .reg = .c } },
            &.{ .{ .reg = .ixl }, .{ .reg = .c } },
            &.{ .{ .reg = .iyl }, .{ .reg = .c } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o151 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .l }, .{ .reg = .d } },
            &.{ .{ .reg = .ixl }, .{ .reg = .d } },
            &.{ .{ .reg = .iyl }, .{ .reg = .d } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o152 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .l }, .{ .reg = .e } },
            &.{ .{ .reg = .ixl }, .{ .reg = .e } },
            &.{ .{ .reg = .iyl }, .{ .reg = .e } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o153 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .l }, .{ .reg = .h } },
            &.{ .{ .reg = .ixl }, .{ .reg = .ixh } },
            &.{ .{ .reg = .iyl }, .{ .reg = .iyh } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o154 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .l }, .{ .reg = .l } },
            &.{ .{ .reg = .ixl }, .{ .reg = .ixl } },
            &.{ .{ .reg = .iyl }, .{ .reg = .iyl } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o155 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .l }, .{ .ind_reg = .hl } },
            &.{ .{ .reg = .l }, .{ .ind_reg_off = .ix } },
            &.{ .{ .reg = .l }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o156 }, .off_imm } },
        .{ .patterns = &.{
            &.{ .{ .reg = .l }, .{ .reg = .a } },
            &.{ .{ .reg = .ixl }, .{ .reg = .a } },
            &.{ .{ .reg = .iyl }, .{ .reg = .a } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o157 } } },

        .{ .patterns = &.{
            &.{ .{ .ind_reg = .hl }, .{ .reg = .b } },
            &.{ .{ .ind_reg_off = .ix }, .{ .reg = .b } },
            &.{ .{ .ind_reg_off = .iy }, .{ .reg = .b } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o160 }, .off_imm } },
        .{ .patterns = &.{
            &.{ .{ .ind_reg = .hl }, .{ .reg = .c } },
            &.{ .{ .ind_reg_off = .ix }, .{ .reg = .c } },
            &.{ .{ .ind_reg_off = .iy }, .{ .reg = .c } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o161 }, .off_imm } },
        .{ .patterns = &.{
            &.{ .{ .ind_reg = .hl }, .{ .reg = .d } },
            &.{ .{ .ind_reg_off = .ix }, .{ .reg = .d } },
            &.{ .{ .ind_reg_off = .iy }, .{ .reg = .d } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o162 }, .off_imm } },
        .{ .patterns = &.{
            &.{ .{ .ind_reg = .hl }, .{ .reg = .e } },
            &.{ .{ .ind_reg_off = .ix }, .{ .reg = .e } },
            &.{ .{ .ind_reg_off = .iy }, .{ .reg = .e } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o163 }, .off_imm } },
        .{ .patterns = &.{
            &.{ .{ .ind_reg = .hl }, .{ .reg = .h } },
            &.{ .{ .ind_reg_off = .ix }, .{ .reg = .h } },
            &.{ .{ .ind_reg_off = .iy }, .{ .reg = .h } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o164 }, .off_imm } },
        .{ .patterns = &.{
            &.{ .{ .ind_reg = .hl }, .{ .reg = .l } },
            &.{ .{ .ind_reg_off = .ix }, .{ .reg = .l } },
            &.{ .{ .ind_reg_off = .iy }, .{ .reg = .l } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o165 }, .off_imm } },
        .{ .patterns = &.{
            &.{ .{ .ind_reg = .hl }, .{ .reg = .a } },
            &.{ .{ .ind_reg_off = .ix }, .{ .reg = .a } },
            &.{ .{ .ind_reg_off = .iy }, .{ .reg = .a } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o167 }, .off_imm } },

        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .b } }}, .encoders = &.{.{ .opc = 0o170 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .c } }}, .encoders = &.{.{ .opc = 0o171 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .d } }}, .encoders = &.{.{ .opc = 0o172 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .e } }}, .encoders = &.{.{ .opc = 0o173 }} },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .reg = .h } },
            &.{ .{ .reg = .a }, .{ .reg = .ixh } },
            &.{ .{ .reg = .a }, .{ .reg = .iyh } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o174 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .reg = .l } },
            &.{ .{ .reg = .a }, .{ .reg = .ixl } },
            &.{ .{ .reg = .a }, .{ .reg = .iyl } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o175 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .ind_reg = .hl } },
            &.{ .{ .reg = .a }, .{ .ind_reg_off = .ix } },
            &.{ .{ .reg = .a }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o176 }, .off_imm } },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .a } }}, .encoders = &.{.{ .opc = 0o177 }} },

        .{ .patterns = &.{
            &.{ .{ .reg = .bc }, .{ .ind_reg = .hl } },
            &.{ .{ .reg = .bc }, .{ .ind_reg_off = .ix } },
            &.{ .{ .reg = .bc }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .ext_idx_pre, .{ .opc = 0o007 }, .off_imm } },
        .{ .patterns = &.{
            &.{ .{ .ind_reg = .hl }, .{ .reg = .bc } },
            &.{ .{ .ind_reg_off = .ix }, .{ .reg = .bc } },
            &.{ .{ .ind_reg_off = .iy }, .{ .reg = .bc } },
        }, .encoders = &.{ .ext_idx_pre, .{ .opc = 0o017 }, .off_imm } },
        .{ .patterns = &.{
            &.{ .{ .reg = .de }, .{ .ind_reg = .hl } },
            &.{ .{ .reg = .de }, .{ .ind_reg_off = .ix } },
            &.{ .{ .reg = .de }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .ext_idx_pre, .{ .opc = 0o027 }, .off_imm } },
        .{ .patterns = &.{
            &.{ .{ .ind_reg = .hl }, .{ .reg = .de } },
            &.{ .{ .ind_reg_off = .ix }, .{ .reg = .de } },
            &.{ .{ .ind_reg_off = .iy }, .{ .reg = .de } },
        }, .encoders = &.{ .ext_idx_pre, .{ .opc = 0o037 }, .off_imm } },
        .{ .patterns = &.{
            &.{ .{ .reg = .hl }, .{ .ind_reg = .hl } },
            &.{ .{ .reg = .hl }, .{ .ind_reg_off = .ix } },
            &.{ .{ .reg = .hl }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .ext_idx_pre, .{ .opc = 0o047 }, .off_imm } },
        .{ .patterns = &.{
            &.{ .{ .ind_reg = .hl }, .{ .reg = .hl } },
            &.{ .{ .ind_reg_off = .ix }, .{ .reg = .hl } },
            &.{ .{ .ind_reg_off = .iy }, .{ .reg = .hl } },
        }, .encoders = &.{ .ext_idx_pre, .{ .opc = 0o057 }, .off_imm } },
        .{ .patterns = &.{
            &.{ .{ .reg = .iy }, .{ .ind_reg = .hl } },
            &.{ .{ .reg = .iy }, .{ .ind_reg_off = .ix } },
            &.{ .{ .reg = .ix }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .ext_idx_pre, .{ .opc = 0o061 }, .off_imm } },
        .{ .patterns = &.{
            &.{ .{ .reg = .ix }, .{ .ind_reg = .hl } },
            &.{ .{ .reg = .ix }, .{ .ind_reg_off = .ix } },
            &.{ .{ .reg = .iy }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .ext_idx_pre, .{ .opc = 0o067 }, .off_imm } },
        .{ .patterns = &.{
            &.{ .{ .ind_reg = .hl }, .{ .reg = .iy } },
            &.{ .{ .ind_reg_off = .ix }, .{ .reg = .iy } },
            &.{ .{ .ind_reg_off = .iy }, .{ .reg = .ix } },
        }, .encoders = &.{ .ext_idx_pre, .{ .opc = 0o076 }, .off_imm } },
        .{ .patterns = &.{
            &.{ .{ .ind_reg = .hl }, .{ .reg = .ix } },
            &.{ .{ .ind_reg_off = .ix }, .{ .reg = .ix } },
            &.{ .{ .ind_reg_off = .iy }, .{ .reg = .iy } },
        }, .encoders = &.{ .ext_idx_pre, .{ .opc = 0o077 }, .off_imm } },

        .{ .patterns = &.{
            &.{ .{ .reg = .sp }, .{ .reg = .hl } },
            &.{ .{ .reg = .sp }, .{ .reg = .ix } },
            &.{ .{ .reg = .sp }, .{ .reg = .iy } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o371 } } },

        .{
            .patterns = &.{&.{ .ind_imm, .{ .reg = .bc } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o103 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .bc }, .ind_imm }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o113 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .ind_imm, .{ .reg = .de } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o123 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .de }, .ind_imm }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o133 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .ind_imm, .{ .reg = .hl } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o143 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .hl }, .ind_imm }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o153 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .ind_imm, .{ .reg = .sp } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o163 }, .word_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .sp }, .ind_imm }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o173 }, .word_imm },
        },

        .{
            .patterns = &.{&.{ .{ .reg = .i }, .{ .reg = .a } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o107 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .r }, .{ .reg = .a } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o117 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .i } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o127 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .r } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o137 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .mb }, .{ .reg = .a } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o155 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .mb } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o156 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .i }, .{ .reg = .hl } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o307 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .hl }, .{ .reg = .i } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o327 } },
        },
    },
    .ldd = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o250 } } }},
    .lddr = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o270 } } }},
    .ldi = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o240 } } }},
    .ldir = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o260 } } }},
    .lea = &.{
        .{
            .patterns = &.{&.{ .{ .reg = .bc }, .{ .reg_off = .ix } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o002 }, .off_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .bc }, .{ .reg_off = .iy } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o003 }, .off_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .de }, .{ .reg_off = .ix } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o022 }, .off_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .de }, .{ .reg_off = .iy } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o023 }, .off_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .hl }, .{ .reg_off = .ix } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o042 }, .off_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .hl }, .{ .reg_off = .iy } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o043 }, .off_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .ix }, .{ .reg_off = .ix } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o062 }, .off_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .iy }, .{ .reg_off = .iy } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o063 }, .off_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .ix }, .{ .reg_off = .iy } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o124 }, .off_imm },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .iy }, .{ .reg_off = .ix } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o125 }, .off_imm },
        },
    },
    .mlt = &.{
        .{ .patterns = &.{&.{.{ .reg = .bc }}}, .encoders = &.{ .ext_pre, .{ .opc = 0o114 } } },
        .{ .patterns = &.{&.{.{ .reg = .de }}}, .encoders = &.{ .ext_pre, .{ .opc = 0o134 } } },
        .{ .patterns = &.{&.{.{ .reg = .hl }}}, .encoders = &.{ .ext_pre, .{ .opc = 0o154 } } },
        .{ .patterns = &.{&.{.{ .reg = .sp }}}, .encoders = &.{ .ext_pre, .{ .opc = 0o174 } } },
    },
    .neg = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o104 } } }},
    .nop = &.{.{ .patterns = &.{&.{}}, .encoders = &.{.{ .opc = 0o000 }} }},
    .@"or" = &.{
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .b } }}, .encoders = &.{.{ .opc = 0o260 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .c } }}, .encoders = &.{.{ .opc = 0o261 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .d } }}, .encoders = &.{.{ .opc = 0o262 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .e } }}, .encoders = &.{.{ .opc = 0o263 }} },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .reg = .h } },
            &.{ .{ .reg = .a }, .{ .reg = .ixh } },
            &.{ .{ .reg = .a }, .{ .reg = .iyh } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o264 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .reg = .l } },
            &.{ .{ .reg = .a }, .{ .reg = .ixl } },
            &.{ .{ .reg = .a }, .{ .reg = .iyl } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o265 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .ind_reg = .hl } },
            &.{ .{ .reg = .a }, .{ .ind_reg_off = .ix } },
            &.{ .{ .reg = .a }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o266 }, .off_imm } },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .a } }}, .encoders = &.{.{ .opc = 0o267 }} },

        .{ .patterns = &.{&.{ .{ .reg = .a }, .imm }}, .encoders = &.{ .{ .opc = 0o366 }, .byte_imm } },
    },
    .otd2r = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o274 } } }},
    .otdm = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o213 } } }},
    .otdmr = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o233 } } }},
    .otdr = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o273 } } }},
    .otdrx = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o313 } } }},
    .oti2r = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o264 } } }},
    .otim = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o203 } } }},
    .otimr = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o223 } } }},
    .otir = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o263 } } }},
    .otirx = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o303 } } }},
    .out = &.{
        .{
            .patterns = &.{&.{ .ind_imm, .{ .reg = .a } }},
            .encoders = &.{ .{ .opc = 0o323 }, .byte_imm },
        },

        .{
            .patterns = &.{&.{ .{ .ind_reg = .bc }, .{ .reg = .b } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o101 } },
        },
        .{
            .patterns = &.{&.{ .{ .ind_reg = .bc }, .{ .reg = .c } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o111 } },
        },
        .{
            .patterns = &.{&.{ .{ .ind_reg = .bc }, .{ .reg = .d } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o121 } },
        },
        .{
            .patterns = &.{&.{ .{ .ind_reg = .bc }, .{ .reg = .e } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o131 } },
        },
        .{
            .patterns = &.{&.{ .{ .ind_reg = .bc }, .{ .reg = .h } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o141 } },
        },
        .{
            .patterns = &.{&.{ .{ .ind_reg = .bc }, .{ .reg = .l } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o151 } },
        },
        .{
            .patterns = &.{&.{ .{ .ind_reg = .bc }, .{ .reg = .a } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o171 } },
        },
    },
    .out0 = &.{
        .{
            .patterns = &.{&.{ .ind_imm, .{ .reg = .b } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o001 }, .byte_imm },
        },
        .{
            .patterns = &.{&.{ .ind_imm, .{ .reg = .c } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o011 }, .byte_imm },
        },
        .{
            .patterns = &.{&.{ .ind_imm, .{ .reg = .d } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o021 }, .byte_imm },
        },
        .{
            .patterns = &.{&.{ .ind_imm, .{ .reg = .e } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o031 }, .byte_imm },
        },
        .{
            .patterns = &.{&.{ .ind_imm, .{ .reg = .h } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o041 }, .byte_imm },
        },
        .{
            .patterns = &.{&.{ .ind_imm, .{ .reg = .l } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o051 }, .byte_imm },
        },
        .{
            .patterns = &.{&.{ .ind_imm, .{ .reg = .a } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o071 }, .byte_imm },
        },
    },
    .outd = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o253 } } }},
    .outd2 = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o254 } } }},
    .outi = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o243 } } }},
    .outi2 = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o244 } } }},
    .pea = &.{
        .{
            .patterns = &.{&.{.{ .reg_off = .ix }}},
            .encoders = &.{ .ext_pre, .{ .opc = 0o145 }, .off_imm },
        },
        .{
            .patterns = &.{&.{.{ .reg_off = .iy }}},
            .encoders = &.{ .ext_pre, .{ .opc = 0o146 }, .off_imm },
        },
    },
    .pop = &.{
        .{ .patterns = &.{&.{.{ .reg = .bc }}}, .encoders = &.{.{ .opc = 0o301 }} },
        .{ .patterns = &.{&.{.{ .reg = .de }}}, .encoders = &.{.{ .opc = 0o321 }} },
        .{ .patterns = &.{
            &.{.{ .reg = .hl }},
            &.{.{ .reg = .ix }},
            &.{.{ .reg = .iy }},
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o341 } } },
        .{ .patterns = &.{&.{.{ .reg = .af }}}, .encoders = &.{.{ .opc = 0o361 }} },
    },
    .push = &.{
        .{ .patterns = &.{&.{.{ .reg = .bc }}}, .encoders = &.{.{ .opc = 0o305 }} },
        .{ .patterns = &.{&.{.{ .reg = .de }}}, .encoders = &.{.{ .opc = 0o325 }} },
        .{ .patterns = &.{
            &.{.{ .reg = .hl }},
            &.{.{ .reg = .ix }},
            &.{.{ .reg = .iy }},
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o345 } } },
        .{ .patterns = &.{&.{.{ .reg = .af }}}, .encoders = &.{.{ .opc = 0o365 }} },
    },
    .res = &.{
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o200 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o201 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o202 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o203 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o204 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o205 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 0 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 0 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 0 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o206 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o207 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o210 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o211 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o212 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o213 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o214 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o215 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 1 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 1 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 1 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o216 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o217 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o220 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o221 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o222 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o223 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o224 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o225 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 2 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 2 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 2 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o226 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o227 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o230 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o231 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o232 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o233 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o234 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o235 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 3 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 3 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 3 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o236 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o237 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o240 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o241 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o242 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o243 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o244 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o245 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 4 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 4 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 4 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o246 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o247 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o250 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o251 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o252 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o253 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o254 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o255 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 5 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 5 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 5 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o256 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o257 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o260 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o261 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o262 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o263 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o264 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o265 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 6 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 6 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 6 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o266 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o267 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o270 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o271 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o272 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o273 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o274 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o275 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 7 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 7 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 7 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o276 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o277 } },
        },
    },
    .ret = &.{
        .{ .patterns = &.{&.{.{ .reg = .nz }}}, .encoders = &.{.{ .opc = 0o300 }} },
        .{ .patterns = &.{&.{.{ .reg = .z }}}, .encoders = &.{.{ .opc = 0o310 }} },
        .{ .patterns = &.{&.{.{ .reg = .nc }}}, .encoders = &.{.{ .opc = 0o320 }} },
        .{ .patterns = &.{&.{.{ .reg = .c }}}, .encoders = &.{.{ .opc = 0o330 }} },
        .{ .patterns = &.{&.{.{ .reg = .po }}}, .encoders = &.{.{ .opc = 0o340 }} },
        .{ .patterns = &.{&.{.{ .reg = .pe }}}, .encoders = &.{.{ .opc = 0o350 }} },
        .{ .patterns = &.{&.{.{ .reg = .p }}}, .encoders = &.{.{ .opc = 0o360 }} },
        .{ .patterns = &.{&.{.{ .reg = .m }}}, .encoders = &.{.{ .opc = 0o370 }} },

        .{ .patterns = &.{&.{}}, .encoders = &.{.{ .opc = 0o311 }} },
    },
    .reti = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o115 } } }},
    .retn = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o105 } } }},
    .rl = &.{
        .{ .patterns = &.{&.{.{ .reg = .b }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o020 } } },
        .{ .patterns = &.{&.{.{ .reg = .c }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o021 } } },
        .{ .patterns = &.{&.{.{ .reg = .d }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o022 } } },
        .{ .patterns = &.{&.{.{ .reg = .e }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o023 } } },
        .{ .patterns = &.{&.{.{ .reg = .h }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o024 } } },
        .{ .patterns = &.{&.{.{ .reg = .l }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o025 } } },
        .{ .patterns = &.{
            &.{.{ .ind_reg = .hl }},
            &.{.{ .ind_reg_off = .ix }},
            &.{.{ .ind_reg_off = .iy }},
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o026 } } },
        .{ .patterns = &.{&.{.{ .reg = .a }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o027 } } },
    },
    .rla = &.{.{ .patterns = &.{&.{}}, .encoders = &.{.{ .opc = 0o027 }} }},
    .rlc = &.{
        .{ .patterns = &.{&.{.{ .reg = .b }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o000 } } },
        .{ .patterns = &.{&.{.{ .reg = .c }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o001 } } },
        .{ .patterns = &.{&.{.{ .reg = .d }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o002 } } },
        .{ .patterns = &.{&.{.{ .reg = .e }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o003 } } },
        .{ .patterns = &.{&.{.{ .reg = .h }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o004 } } },
        .{ .patterns = &.{&.{.{ .reg = .l }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o005 } } },
        .{ .patterns = &.{
            &.{.{ .ind_reg = .hl }},
            &.{.{ .ind_reg_off = .ix }},
            &.{.{ .ind_reg_off = .iy }},
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o006 } } },
        .{ .patterns = &.{&.{.{ .reg = .a }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o007 } } },
    },
    .rlca = &.{.{ .patterns = &.{&.{}}, .encoders = &.{.{ .opc = 0o007 }} }},
    .rld = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o157 } } }},
    .rr = &.{
        .{ .patterns = &.{&.{.{ .reg = .b }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o030 } } },
        .{ .patterns = &.{&.{.{ .reg = .c }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o031 } } },
        .{ .patterns = &.{&.{.{ .reg = .d }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o032 } } },
        .{ .patterns = &.{&.{.{ .reg = .e }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o033 } } },
        .{ .patterns = &.{&.{.{ .reg = .h }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o034 } } },
        .{ .patterns = &.{&.{.{ .reg = .l }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o035 } } },
        .{ .patterns = &.{
            &.{.{ .ind_reg = .hl }},
            &.{.{ .ind_reg_off = .ix }},
            &.{.{ .ind_reg_off = .iy }},
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o036 } } },
        .{ .patterns = &.{&.{.{ .reg = .a }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o037 } } },
    },
    .rra = &.{.{ .patterns = &.{&.{}}, .encoders = &.{.{ .opc = 0o037 }} }},
    .rrc = &.{
        .{ .patterns = &.{&.{.{ .reg = .b }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o010 } } },
        .{ .patterns = &.{&.{.{ .reg = .c }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o011 } } },
        .{ .patterns = &.{&.{.{ .reg = .d }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o012 } } },
        .{ .patterns = &.{&.{.{ .reg = .e }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o013 } } },
        .{ .patterns = &.{&.{.{ .reg = .h }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o014 } } },
        .{ .patterns = &.{&.{.{ .reg = .l }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o015 } } },
        .{ .patterns = &.{
            &.{.{ .ind_reg = .hl }},
            &.{.{ .ind_reg_off = .ix }},
            &.{.{ .ind_reg_off = .iy }},
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o016 } } },
        .{ .patterns = &.{&.{.{ .reg = .a }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o017 } } },
    },
    .rrca = &.{.{ .patterns = &.{&.{}}, .encoders = &.{.{ .opc = 0o017 }} }},
    .rrd = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o147 } } }},
    .rsmix = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o176 } } }},
    .rst = &.{
        .{ .patterns = &.{&.{.{ .specific_imm = 0o000 }}}, .encoders = &.{.{ .opc = 0o307 }} },
        .{ .patterns = &.{&.{.{ .specific_imm = 0o010 }}}, .encoders = &.{.{ .opc = 0o317 }} },
        .{ .patterns = &.{&.{.{ .specific_imm = 0o020 }}}, .encoders = &.{.{ .opc = 0o327 }} },
        .{ .patterns = &.{&.{.{ .specific_imm = 0o030 }}}, .encoders = &.{.{ .opc = 0o337 }} },
        .{ .patterns = &.{&.{.{ .specific_imm = 0o040 }}}, .encoders = &.{.{ .opc = 0o347 }} },
        .{ .patterns = &.{&.{.{ .specific_imm = 0o050 }}}, .encoders = &.{.{ .opc = 0o357 }} },
        .{ .patterns = &.{&.{.{ .specific_imm = 0o060 }}}, .encoders = &.{.{ .opc = 0o367 }} },
        .{ .patterns = &.{&.{.{ .specific_imm = 0o070 }}}, .encoders = &.{.{ .opc = 0o377 }} },
    },
    .sbc = &.{
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .b } }}, .encoders = &.{.{ .opc = 0o230 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .c } }}, .encoders = &.{.{ .opc = 0o231 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .d } }}, .encoders = &.{.{ .opc = 0o232 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .e } }}, .encoders = &.{.{ .opc = 0o233 }} },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .reg = .h } },
            &.{ .{ .reg = .a }, .{ .reg = .ixh } },
            &.{ .{ .reg = .a }, .{ .reg = .iyh } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o234 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .reg = .l } },
            &.{ .{ .reg = .a }, .{ .reg = .ixl } },
            &.{ .{ .reg = .a }, .{ .reg = .iyl } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o235 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .ind_reg = .hl } },
            &.{ .{ .reg = .a }, .{ .ind_reg_off = .ix } },
            &.{ .{ .reg = .a }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o236 }, .off_imm } },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .a } }}, .encoders = &.{.{ .opc = 0o237 }} },

        .{ .patterns = &.{&.{ .{ .reg = .a }, .imm }}, .encoders = &.{ .{ .opc = 0o336 }, .byte_imm } },

        .{
            .patterns = &.{&.{ .{ .reg = .hl }, .{ .reg = .bc } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o102 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .hl }, .{ .reg = .de } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o122 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .hl }, .{ .reg = .hl } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o142 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .hl }, .{ .reg = .sp } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o162 } },
        },
    },
    .scf = &.{.{ .patterns = &.{&.{}}, .encoders = &.{.{ .opc = 0o067 }} }},
    .set = &.{
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o300 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o301 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o302 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o303 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o304 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o305 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 0 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 0 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 0 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o306 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 0 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o307 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o310 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o311 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o312 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o313 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o314 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o315 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 1 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 1 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 1 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o316 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 1 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o317 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o320 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o321 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o322 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o323 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o324 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o325 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 2 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 2 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 2 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o326 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 2 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o327 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o330 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o331 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o332 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o333 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o334 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o335 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 3 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 3 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 3 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o336 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 3 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o337 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o340 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o341 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o342 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o343 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o344 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o345 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 4 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 4 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 4 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o346 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 4 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o347 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o350 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o351 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o352 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o353 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o354 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o355 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 5 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 5 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 5 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o356 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 5 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o357 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o360 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o361 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o362 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o363 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o364 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o365 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 6 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 6 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 6 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o366 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 6 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o367 } },
        },

        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .b } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o370 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .c } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o371 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .d } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o372 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .e } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o373 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .h } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o374 } },
        },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .l } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o375 } },
        },
        .{ .patterns = &.{
            &.{ .{ .specific_imm = 7 }, .{ .ind_reg = .hl } },
            &.{ .{ .specific_imm = 7 }, .{ .ind_reg_off = .ix } },
            &.{ .{ .specific_imm = 7 }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o376 } } },
        .{
            .patterns = &.{&.{ .{ .specific_imm = 7 }, .{ .reg = .a } }},
            .encoders = &.{ .bit_pre, .{ .opc = 0o377 } },
        },
    },
    .sla = &.{
        .{ .patterns = &.{&.{.{ .reg = .b }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o040 } } },
        .{ .patterns = &.{&.{.{ .reg = .c }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o041 } } },
        .{ .patterns = &.{&.{.{ .reg = .d }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o042 } } },
        .{ .patterns = &.{&.{.{ .reg = .e }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o043 } } },
        .{ .patterns = &.{&.{.{ .reg = .h }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o044 } } },
        .{ .patterns = &.{&.{.{ .reg = .l }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o045 } } },
        .{ .patterns = &.{
            &.{.{ .ind_reg = .hl }},
            &.{.{ .ind_reg_off = .ix }},
            &.{.{ .ind_reg_off = .iy }},
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o046 } } },
        .{ .patterns = &.{&.{.{ .reg = .a }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o047 } } },
    },
    .slp = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o166 } } }},
    .sra = &.{
        .{ .patterns = &.{&.{.{ .reg = .b }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o050 } } },
        .{ .patterns = &.{&.{.{ .reg = .c }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o051 } } },
        .{ .patterns = &.{&.{.{ .reg = .d }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o052 } } },
        .{ .patterns = &.{&.{.{ .reg = .e }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o053 } } },
        .{ .patterns = &.{&.{.{ .reg = .h }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o054 } } },
        .{ .patterns = &.{&.{.{ .reg = .l }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o055 } } },
        .{ .patterns = &.{
            &.{.{ .ind_reg = .hl }},
            &.{.{ .ind_reg_off = .ix }},
            &.{.{ .ind_reg_off = .iy }},
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o056 } } },
        .{ .patterns = &.{&.{.{ .reg = .a }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o057 } } },
    },
    .srl = &.{
        .{ .patterns = &.{&.{.{ .reg = .b }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o070 } } },
        .{ .patterns = &.{&.{.{ .reg = .c }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o071 } } },
        .{ .patterns = &.{&.{.{ .reg = .d }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o072 } } },
        .{ .patterns = &.{&.{.{ .reg = .e }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o073 } } },
        .{ .patterns = &.{&.{.{ .reg = .h }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o074 } } },
        .{ .patterns = &.{&.{.{ .reg = .l }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o075 } } },
        .{ .patterns = &.{
            &.{.{ .ind_reg = .hl }},
            &.{.{ .ind_reg_off = .ix }},
            &.{.{ .ind_reg_off = .iy }},
        }, .encoders = &.{ .idx_pre, .bit_pre, .off_imm, .{ .opc = 0o076 } } },
        .{ .patterns = &.{&.{.{ .reg = .a }}}, .encoders = &.{ .bit_pre, .{ .opc = 0o077 } } },
    },
    .stmix = &.{.{ .patterns = &.{&.{}}, .encoders = &.{ .ext_pre, .{ .opc = 0o175 } } }},
    .sub = &.{
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .b } }}, .encoders = &.{.{ .opc = 0o220 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .c } }}, .encoders = &.{.{ .opc = 0o221 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .d } }}, .encoders = &.{.{ .opc = 0o222 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .e } }}, .encoders = &.{.{ .opc = 0o223 }} },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .reg = .h } },
            &.{ .{ .reg = .a }, .{ .reg = .ixh } },
            &.{ .{ .reg = .a }, .{ .reg = .iyh } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o224 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .reg = .l } },
            &.{ .{ .reg = .a }, .{ .reg = .ixl } },
            &.{ .{ .reg = .a }, .{ .reg = .iyl } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o225 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .ind_reg = .hl } },
            &.{ .{ .reg = .a }, .{ .ind_reg_off = .ix } },
            &.{ .{ .reg = .a }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o226 }, .off_imm } },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .a } }}, .encoders = &.{.{ .opc = 0o227 }} },

        .{ .patterns = &.{&.{ .{ .reg = .a }, .imm }}, .encoders = &.{ .{ .opc = 0o326 }, .byte_imm } },
    },
    .tst = &.{
        .{
            .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .b } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o004 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .c } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o014 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .d } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o024 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .e } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o034 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .h } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o044 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .l } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o054 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .a }, .{ .ind_reg = .hl } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o064 } },
        },
        .{
            .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .a } }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o074 } },
        },

        .{
            .patterns = &.{&.{ .{ .reg = .a }, .imm }},
            .encoders = &.{ .ext_pre, .{ .opc = 0o144 }, .byte_imm },
        },
    },
    .tstio = &.{.{ .patterns = &.{&.{.imm}}, .encoders = &.{ .ext_pre, .{ .opc = 0o164 }, .byte_imm } }},
    .xor = &.{
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .b } }}, .encoders = &.{.{ .opc = 0o250 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .c } }}, .encoders = &.{.{ .opc = 0o251 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .d } }}, .encoders = &.{.{ .opc = 0o252 }} },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .e } }}, .encoders = &.{.{ .opc = 0o253 }} },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .reg = .h } },
            &.{ .{ .reg = .a }, .{ .reg = .ixh } },
            &.{ .{ .reg = .a }, .{ .reg = .iyh } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o254 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .reg = .l } },
            &.{ .{ .reg = .a }, .{ .reg = .ixl } },
            &.{ .{ .reg = .a }, .{ .reg = .iyl } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o255 } } },
        .{ .patterns = &.{
            &.{ .{ .reg = .a }, .{ .ind_reg = .hl } },
            &.{ .{ .reg = .a }, .{ .ind_reg_off = .ix } },
            &.{ .{ .reg = .a }, .{ .ind_reg_off = .iy } },
        }, .encoders = &.{ .idx_pre, .{ .opc = 0o256 }, .off_imm } },
        .{ .patterns = &.{&.{ .{ .reg = .a }, .{ .reg = .a } }}, .encoders = &.{.{ .opc = 0o257 }} },

        .{ .patterns = &.{&.{ .{ .reg = .a }, .imm }}, .encoders = &.{ .{ .opc = 0o356 }, .byte_imm } },
    },
});

pub fn assemble(allocator: std.mem.Allocator, source: [:0]const u8) Error![]u8 {
    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    defer arena_allocator.deinit();

    var self = try Assembler.init(arena_allocator.allocator(), source);
    defer self.deinit();

    {
        errdefer self.tokenizer.debugPrintLocation();
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

fn emitByte(self: *Assembler, byte: u8) !void {
    try self.output.append(self.allocator, byte);
}
fn exactByteSizeOf(comptime T: type) usize {
    return @divExact(@bitSizeOf(T), 8);
}
fn encodeInt(value: anytype) [exactByteSizeOf(@TypeOf(value))]u8 {
    var bytes: [exactByteSizeOf(@TypeOf(value))]u8 = undefined;
    std.mem.writeIntSliceLittle(@TypeOf(value), &bytes, value);
    return bytes;
}
fn encodeImm(
    comptime PositiveInt: type,
    comptime NegativeInt: type,
    value: Value,
) error{ImmOutOfRange}!PositiveInt {
    return switch (value.getOffset()) {
        .small => |small| if (small >= 0)
            std.math.cast(PositiveInt, small) orelse return Error.ImmOutOfRange
        else
            @bitCast(
                PositiveInt,
                std.math.cast(NegativeInt, small) orelse return Error.ImmOutOfRange,
            ),
        .big => |big| if (big.positive)
            big.to(PositiveInt) catch return Error.ImmOutOfRange
        else
            @bitCast(PositiveInt, big.to(NegativeInt) catch return Error.ImmOutOfRange),
    };
}

fn emitInt(self: *Assembler, value: anytype) !void {
    var bytes: [exactByteSizeOf(@TypeOf(value))]u8 = undefined;
    std.mem.writeIntSliceLittle(@TypeOf(value), &bytes, value);
    try self.output.appendSlice(self.allocator, &bytes);
}
fn emitImm(
    self: *Assembler,
    comptime PositiveInt: type,
    comptime NegativeInt: type,
    value: Value,
) !void {
    switch (value.getOffset()) {
        .small => |small| if (small >= 0)
            try self.emitInt(std.math.cast(PositiveInt, small) orelse return Error.ImmOutOfRange)
        else
            try self.emitInt(std.math.cast(NegativeInt, small) orelse return Error.ImmOutOfRange),
        .big => |big| if (big.positive)
            try self.emitInt(big.to(PositiveInt) catch return Error.ImmOutOfRange)
        else
            try self.emitInt(big.to(NegativeInt) catch return Error.ImmOutOfRange),
    }
}

fn encodeSimpleCondition(expr: Expr) error{IllegalCondition}!u2 {
    return if (!expr.ind and expr.value.zeroOffset())
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
    return if (!expr.ind and expr.value.zeroOffset())
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
fn encodeByteReg(
    expr: Expr,
    comptime half_index_allowed: enum { no_half_index, allow_half_index },
    comptime index_allowed: enum { no_index, allow_index },
) error{IllegalInstruction}!u3 {
    return if (!expr.ind)
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
fn encodeWordReg(
    expr: Expr,
    matching: ?Expr,
    comptime alternate_reg: enum { sp, af },
) error{IllegalInstruction}!u2 {
    return if (!expr.ind and expr.value.zeroOffset())
        switch (expr.value.getBase() orelse return Error.IllegalInstruction) {
            .bc => 0,
            .de => 1,
            .hl, .ix, .iy => |reg| if (matching == null or
                reg == matching.?.value.getBase().?) 2 else Error.IllegalInstruction,
            .sp => switch (alternate_reg) {
                .sp => 3,
                .af => Error.IllegalInstruction,
            },
            .af => switch (alternate_reg) {
                .af => 3,
                .sp => Error.IllegalInstruction,
            },
            else => Error.IllegalInstruction,
        }
    else
        Error.IllegalInstruction;
}
fn encodeBitIndex(expr: Expr) error{ IllegalInstruction, ImmOutOfRange }!u3 {
    return if (!expr.ind and expr.value.getBase() == null)
        encodeImm(u3, u3, expr.value)
    else
        Error.IllegalInstruction;
}
fn encodeRstTarget(expr: Expr) error{IllegalInstruction}!u3 {
    if (expr.ind or expr.value.getBase() != null) return Error.IllegalInstruction;
    const target = encodeImm(u6, u6, expr.value) catch return Error.IllegalInstruction;
    return if (util.bit.extract(target, u3, 0) == 0)
        util.bit.extract(target, u3, 3)
    else
        Error.IllegalInstruction;
}

fn parseFile(self: *Assembler) Error!void {
    while (try self.tokenizer.peek() != .eof) try self.parseLine();
}

fn parseLine(self: *Assembler) Error!void {
    try self.parseStatement();
    while (true) switch (try self.tokenizer.next()) {
        .eof, .eol => break,
        .comment => continue,
        else => return Error.ExpectedEndOfLine,
    };
}

fn parseData(self: *Assembler) Error!void {
    const mnemonic = (try self.tokenizer.next()).keyword;

    if (switch (try self.tokenizer.peek()) {
        else => true,
        .eof, .eol, .comment => false,
    }) while (true) {
        var expr = try self.parseExpr();
        defer expr.deinit();

        try switch (mnemonic) {
            .db => self.emitImm(u8, i8, expr.value),
            .dw => self.emitImm(u16, i16, expr.value),
            .dl => self.emitImm(u24, i24, expr.value),
            else => unreachable,
        };

        if (try self.tokenizer.peek() != .comma) break;
        _ = try self.tokenizer.next();
    };
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

    var operand_list: std.BoundedArray(Expr, 2) = .{};
    defer for (operand_list.slice()) |*operand| operand.deinit();
    if (switch (try self.tokenizer.peek()) {
        else => true,
        .eof, .eol, .comment => false,
    }) while (true) {
        operand_list.append(try self.parseExpr()) catch |err| switch (err) {
            error.Overflow => return Error.IllegalInstruction,
        };
        if (try self.tokenizer.peek() != .comma) break;
        _ = try self.tokenizer.next();
    };
    const operands = operand_list.constSlice();

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
    for (ez80[@enumToInt(mnemonic)]) |rule| {
        const pattern_index = for (rule.patterns) |pattern, pattern_index|
            if (pattern.len == operands.len and for (pattern) |operand_pattern, operand|
                if (operand_pattern.match(operands[operand])) continue else break false
            else
                true) break pattern_index else continue
        else
            continue;
        if (suffix) |mode| try self.emitByte(mode.pre());
        for (rule.encoders) |encoder| switch (encoder) {
            .bit_pre, .ext_pre, .idx_pre, .ext_idx_pre, .opc => if (@as(?u8, switch (encoder) {
                .bit_pre => 0o313,
                .ext_pre => 0o355,
                .idx_pre => switch (pattern_index) {
                    0 => null,
                    1 => 0o335,
                    2 => 0o375,
                    else => unreachable,
                },
                .ext_idx_pre => switch (pattern_index) {
                    0 => 0o355,
                    1 => 0o335,
                    2 => 0o375,
                    else => unreachable,
                },
                .opc => |opc| opc,
                else => unreachable,
            })) |opc| try self.emitByte(opc),
            .off_imm => for (rule.patterns[pattern_index]) |pattern, operand| switch (pattern) {
                .reg_off, .ind_reg_off => try self.emitImm(i8, i8, operands[operand].value),
                else => continue,
            },
            .byte_imm,
            .rel_imm,
            .word_imm,
            => for (rule.patterns[pattern_index]) |pattern, operand| switch (pattern) {
                .imm, .ind_imm => switch (encoder) {
                    .byte_imm => try self.emitImm(u8, i8, operands[operand].value),
                    .rel_imm => {
                        var pc = try Value.init(
                            self.allocator,
                            null,
                            self.origin + self.output.items.len + 1,
                        );
                        defer pc.deinit();
                        var offset = try operands[operand].value.subtract(pc, self.allocator);
                        defer offset.deinit();
                        try self.emitImm(i8, i8, offset);
                    },
                    .word_imm => try switch ((suffix orelse self.adl).imm) {
                        .is => self.emitImm(u16, i16, operands[operand].value),
                        .il => self.emitImm(u24, i24, operands[operand].value),
                    },
                    else => unreachable,
                },
                else => continue,
            },
        };
        break;
    } else return Error.IllegalInstruction;
}

fn parseStatement(self: *Assembler) Error!void {
    switch (try self.tokenizer.peek()) {
        .eof, .eol, .comment => {},
        .keyword => |keyword| switch (keyword) {
            .db, .dw, .dl => try self.parseData(),
            else => try self.parseInstruction(),
        },
        else => return Error.IllegalInstruction,
    }
}

fn parseExpr(self: *Assembler) Error!Expr {
    return try self.parseAdditiveExpr();
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

fn initExpr(self: *Assembler, base: ?Value.Reg, offset: anytype) !Expr {
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
            .mb => try self.initExpr(.mb, 0),
            .nc => try self.initExpr(.nc, 0),
            .nz => try self.initExpr(.nz, 0),
            .p => try self.initExpr(.p, 0),
            .pe => try self.initExpr(.pe, 0),
            .po => try self.initExpr(.po, 0),
            .r => try self.initExpr(.r, 0),
            .sp => try self.initExpr(.sp, 0),
            .z => try self.initExpr(.z, 0),
            else => return Error.UnexpectedToken,
        },
        .dollar => try self.initExpr(null, self.origin + self.output.items.len),
        .lparen => value: {
            var sub_expr = try self.parseExpr();
            errdefer sub_expr.deinit();

            if (try self.tokenizer.next() != .rparen) return Error.UnclosedParentheses;

            break :value .{ .ind = true, .value = sub_expr.value };
        },
        else => return Error.UnexpectedToken,
    };
}

test "as" {
    var timer = try std.time.Timer.start();

    const expected = @embedFile("as/ez80insts.bin");
    const actual = try assemble(std.testing.allocator, @embedFile("as/ez80insts.src"));
    defer std.testing.allocator.free(actual);
    try std.testing.expectEqualSlices(u8, expected, actual);

    if (false) std.debug.print("\n{} ns\n", .{timer.read()});
}

test {
    std.testing.refAllDecls(Assembler);
}
