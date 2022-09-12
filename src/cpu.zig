const std = @import("std");

const CEmuCore = @import("cemucore.zig");
const Cpu = @This();
const decode = @import("cpu/decode.zig");
const Dummy = @import("cpu/dummy.zig");
const Interpreter = @import("cpu/interp.zig");
const util = @import("util.zig");

pub const ExecuteMode = enum { flush, step, run };
pub const Backend = struct {
    flush: bool = true,
    execute: *const fn (*Backend, *CEmuCore, ExecuteMode) void,
    destroy: *const fn (*Backend, std.mem.Allocator) void,
};

pub const RegisterId = enum {
    // 1-bit state
    adl,
    ief,

    // 1-bit flags
    cf,
    nf,
    pv,
    xf,
    hc,
    yf,
    zf,
    sf,

    // 8-bit registers
    f,
    a,
    c,
    b,
    bcu,
    e,
    d,
    deu,
    l,
    h,
    hlu,
    ixl,
    ixh,
    ixu,
    iyl,
    iyh,
    iyu,
    r,
    mb,

    // 16-bit registers
    af,
    bc,
    de,
    hl,
    ix,
    iy,
    sps,
    i,

    // 24-bit registers
    ubc,
    ude,
    uhl,
    uix,
    uiy,
    spl,
    pc,
};

pub const Adl = enum(u1) { z80, ez80 };

const Mode = packed struct(u2) {
    adl: Adl,
    madl: Adl,
};

pub const Flags = packed struct(u8) {
    cf: bool,
    nf: bool,
    pv: bool,
    xf: bool,
    hc: bool,
    yf: bool,
    zf: bool,
    sf: bool,
};

const u8u8 = packed struct(u16) {
    low: u8,
    high: u8,
};

const u8u8u8 = packed struct(u24) {
    low: u8,
    high: u8,
    upper: u8,
};

pub const u8u16 = packed struct(u24) {
    short: u16,
    upper: u8,
};

cf: bool = false,
nf: bool = false,
pv: bool = false,
xf: bool = false,
hc: bool = false,
yf: bool = false,
zf: bool = false,
sf: bool = false,

a: u8 = 0,
bc: u24 = 0,
de: u24 = 0,
hl: u24 = 0,

@"af'": u16 = 0,
@"bc'": u24 = 0,
@"de'": u24 = 0,
@"hl'": u24 = 0,

ix: u24 = 0,
iy: u24 = 0,

sps: u16 = 0,
spl: u24 = 0,
pc: u24 = 0,
raw_pc: u24 = 0,
r: u8 = 0,
mbi: u24 = 0,

mode: Mode = .{ .adl = .z80, .madl = .z80 },

ief1: bool = false,
ief2: bool = false,

cycles: u64 = 0,

backend: *Backend,

pub fn RegisterType(comptime id: RegisterId) type {
    return switch (id) {
        .adl, .ief, .cf, .nf, .pv, .xf, .hc, .yf, .zf, .sf => u1,
        .f, .a, .c, .b, .bcu, .e, .d, .deu, .l, .h, .hlu, .ixl, .ixh, .ixu, .iyl, .iyh, .iyu, .r, .mb => u8,
        .af, .bc, .de, .hl, .ix, .iy, .sps, .i => u16,
        .ubc, .ude, .uhl, .uix, .uiy, .spl, .pc => u24,
    };
}

pub fn get(self: *const Cpu, comptime id: RegisterId) RegisterType(id) {
    return switch (id) {
        // 1-bit state
        .adl => @enumToInt(self.mode.adl),
        .ief => @boolToInt(self.ief1),

        // 1-bit flags
        .cf => @boolToInt(self.cf),
        .nf => @boolToInt(self.nf),
        .pv => @boolToInt(self.pv),
        .xf => @boolToInt(self.xf),
        .hc => @boolToInt(self.hc),
        .yf => @boolToInt(self.yf),
        .zf => @boolToInt(self.zf),
        .sf => @boolToInt(self.sf),

        // 8-bit registers
        .f => util.toBacking(Flags{
            .cf = self.get(.cf) != 0,
            .nf = self.get(.nf) != 0,
            .pv = self.get(.pv) != 0,
            .xf = self.get(.xf) != 0,
            .hc = self.get(.hc) != 0,
            .yf = self.get(.yf) != 0,
            .zf = self.get(.zf) != 0,
            .sf = self.get(.sf) != 0,
        }),
        .a => self.a,
        .c => util.fromBacking(u8u8u8, self.get(.ubc)).low,
        .b => util.fromBacking(u8u8u8, self.get(.ubc)).high,
        .bcu => util.fromBacking(u8u8u8, self.get(.ubc)).upper,
        .e => util.fromBacking(u8u8u8, self.get(.ude)).low,
        .d => util.fromBacking(u8u8u8, self.get(.ude)).high,
        .deu => util.fromBacking(u8u8u8, self.get(.ude)).upper,
        .l => util.fromBacking(u8u8u8, self.get(.uhl)).low,
        .h => util.fromBacking(u8u8u8, self.get(.uhl)).high,
        .hlu => util.fromBacking(u8u8u8, self.get(.uhl)).upper,
        .ixl => util.fromBacking(u8u8u8, self.get(.uix)).low,
        .ixh => util.fromBacking(u8u8u8, self.get(.uix)).high,
        .ixu => util.fromBacking(u8u8u8, self.get(.uix)).upper,
        .iyl => util.fromBacking(u8u8u8, self.get(.uiy)).low,
        .iyh => util.fromBacking(u8u8u8, self.get(.uiy)).high,
        .iyu => util.fromBacking(u8u8u8, self.get(.uiy)).upper,
        .r => std.math.rotr(u8, self.r, 1),
        .mb => util.fromBacking(u8u8u8, self.mbi).upper,

        // 16-bit registers
        .af => util.toBacking(u8u8{ .low = self.get(.f), .high = self.get(.a) }),
        .bc => util.fromBacking(u8u16, self.get(.ubc)).short,
        .de => util.fromBacking(u8u16, self.get(.ude)).short,
        .hl => util.fromBacking(u8u16, self.get(.uhl)).short,
        .ix => util.fromBacking(u8u16, self.get(.uix)).short,
        .iy => util.fromBacking(u8u16, self.get(.uiy)).short,
        .sps => self.sps,
        .i => util.fromBacking(u8u16, self.mbi).short,

        // 24-bit registers
        .ubc => self.bc,
        .ude => self.de,
        .uhl => self.hl,
        .uix => self.ix,
        .uiy => self.iy,
        .spl => self.spl,
        .pc => self.pc,
    };
}

pub fn getShadow(self: *const Cpu, comptime id: RegisterId) RegisterType(id) {
    return switch (id) {
        // 1-bit state
        .adl => @enumToInt(self.mode.madl),
        .ief => @boolToInt(self.ief2),

        // 1-bit flags
        .cf => @boolToInt(util.fromBacking(Flags, self.getShadow(.f)).cf),
        .nf => @boolToInt(util.fromBacking(Flags, self.getShadow(.f)).nf),
        .pv => @boolToInt(util.fromBacking(Flags, self.getShadow(.f)).pv),
        .xf => @boolToInt(util.fromBacking(Flags, self.getShadow(.f)).xf),
        .hc => @boolToInt(util.fromBacking(Flags, self.getShadow(.f)).hc),
        .yf => @boolToInt(util.fromBacking(Flags, self.getShadow(.f)).yf),
        .zf => @boolToInt(util.fromBacking(Flags, self.getShadow(.f)).zf),
        .sf => @boolToInt(util.fromBacking(Flags, self.getShadow(.f)).sf),

        // 8-bit registers
        .f => util.fromBacking(u8u8, self.getShadow(.af)).low,
        .a => util.fromBacking(u8u8, self.getShadow(.af)).high,
        .c => util.fromBacking(u8u8u8, self.getShadow(.ubc)).low,
        .b => util.fromBacking(u8u8u8, self.getShadow(.ubc)).high,
        .bcu => util.fromBacking(u8u8u8, self.getShadow(.ubc)).upper,
        .e => util.fromBacking(u8u8u8, self.getShadow(.ude)).low,
        .d => util.fromBacking(u8u8u8, self.getShadow(.ude)).high,
        .deu => util.fromBacking(u8u8u8, self.getShadow(.ude)).upper,
        .l => util.fromBacking(u8u8u8, self.getShadow(.uhl)).low,
        .h => util.fromBacking(u8u8u8, self.getShadow(.uhl)).high,
        .hlu => util.fromBacking(u8u8u8, self.getShadow(.uhl)).upper,
        .ixl, .ixh, .ixu, .iyu, .iyl, .iyh, .r, .mb => unreachable,

        // 16-bit registers
        .af => self.@"af'",
        .bc => util.fromBacking(u8u16, self.getShadow(.ubc)).short,
        .de => util.fromBacking(u8u16, self.getShadow(.ude)).short,
        .hl => util.fromBacking(u8u16, self.getShadow(.uhl)).short,
        .ix, .iy, .sps, .i => unreachable,

        // 24-bit registers
        .ubc => self.@"bc'",
        .ude => self.@"de'",
        .uhl => self.@"hl'",
        .uix, .uiy, .spl => unreachable,
        .pc => self.raw_pc,
    };
}

pub fn set(self: *Cpu, comptime id: RegisterId, value: RegisterType(id)) void {
    switch (id) {
        // 1-bit state
        .adl => self.mode.adl = @intToEnum(Adl, value),
        .ief => {
            self.ief1 = value != 0;
            self.setShadow(.ief, value);
        },

        // 1-bit flags
        .cf => self.cf = value != 0,
        .nf => self.nf = value != 0,
        .pv => self.pv = value != 0,
        .xf => self.xf = value != 0,
        .hc => self.hc = value != 0,
        .yf => self.yf = value != 0,
        .zf => self.zf = value != 0,
        .sf => self.sf = value != 0,

        // 8-bit registers
        .f => {
            const flags = util.fromBacking(Flags, value);
            self.cf = flags.cf;
            self.nf = flags.nf;
            self.pv = flags.pv;
            self.xf = flags.xf;
            self.hc = flags.hc;
            self.yf = flags.yf;
            self.zf = flags.zf;
            self.sf = flags.sf;
        },
        .a => self.a = value,
        .c => @ptrCast(*u8u8u8, &self.bc).low = value,
        .b => @ptrCast(*u8u8u8, &self.bc).high = value,
        .bcu => @ptrCast(*u8u8u8, &self.bc).upper = value,
        .e => @ptrCast(*u8u8u8, &self.de).low = value,
        .d => @ptrCast(*u8u8u8, &self.de).high = value,
        .deu => @ptrCast(*u8u8u8, &self.de).upper = value,
        .l => @ptrCast(*u8u8u8, &self.hl).low = value,
        .h => @ptrCast(*u8u8u8, &self.hl).high = value,
        .hlu => @ptrCast(*u8u8u8, &self.hl).upper = value,
        .ixl => @ptrCast(*u8u8u8, &self.ix).low = value,
        .ixh => @ptrCast(*u8u8u8, &self.ix).high = value,
        .ixu => @ptrCast(*u8u8u8, &self.ix).upper = value,
        .iyl => @ptrCast(*u8u8u8, &self.iy).low = value,
        .iyh => @ptrCast(*u8u8u8, &self.iy).high = value,
        .iyu => @ptrCast(*u8u8u8, &self.iy).upper = value,
        .r => self.r = std.math.rotl(u8, value, 1),
        .mb => @ptrCast(*u8u16, &self.mbi).upper = value,

        // 16-bit registers
        .af => {
            self.set(.f, util.fromBacking(u8u8, value).low);
            self.set(.a, util.fromBacking(u8u8, value).high);
        },
        .bc => @ptrCast(*u8u16, &self.bc).short = value,
        .de => @ptrCast(*u8u16, &self.de).short = value,
        .hl => @ptrCast(*u8u16, &self.hl).short = value,
        .ix => @ptrCast(*u8u16, &self.ix).short = value,
        .iy => @ptrCast(*u8u16, &self.iy).short = value,
        .sps => self.sps = value,
        .i => @ptrCast(*u8u16, &self.mbi).short = value,

        // 24-bit registers
        .ubc => self.bc = value,
        .ude => self.de = value,
        .uhl => self.hl = value,
        .uix => self.ix = value,
        .uiy => self.iy = value,
        .spl => self.spl = value,
        .pc => self.pc = value,
    }
}

pub fn setShadow(self: *Cpu, comptime id: RegisterId, value: RegisterType(id)) void {
    switch (id) {
        // 1-bit state
        .adl => self.mode.madl = @intToEnum(Adl, value),
        .ief => self.ief2 = value != 0,

        // 1-bit flags
        .cf => @ptrCast(*Flags, &@ptrCast(*u8u8, &self.@"af'").low).cf = value != 0,
        .nf => @ptrCast(*Flags, &@ptrCast(*u8u8, &self.@"af'").low).nf = value != 0,
        .pv => @ptrCast(*Flags, &@ptrCast(*u8u8, &self.@"af'").low).pv = value != 0,
        .xf => @ptrCast(*Flags, &@ptrCast(*u8u8, &self.@"af'").low).xf = value != 0,
        .hc => @ptrCast(*Flags, &@ptrCast(*u8u8, &self.@"af'").low).hc = value != 0,
        .yf => @ptrCast(*Flags, &@ptrCast(*u8u8, &self.@"af'").low).yf = value != 0,
        .zf => @ptrCast(*Flags, &@ptrCast(*u8u8, &self.@"af'").low).zf = value != 0,
        .sf => @ptrCast(*Flags, &@ptrCast(*u8u8, &self.@"af'").low).sf = value != 0,

        // 8-bit registers
        .f => @ptrCast(*u8u8, &self.@"af'").low = value,
        .a => @ptrCast(*u8u8, &self.@"af'").high = value,
        .c => @ptrCast(*u8u8u8, &self.@"bc'").low = value,
        .b => @ptrCast(*u8u8u8, &self.@"bc'").high = value,
        .bcu => @ptrCast(*u8u8u8, &self.@"bc'").upper = value,
        .e => @ptrCast(*u8u8u8, &self.@"de'").low = value,
        .d => @ptrCast(*u8u8u8, &self.@"de'").high = value,
        .deu => @ptrCast(*u8u8u8, &self.@"de'").upper = value,
        .l => @ptrCast(*u8u8u8, &self.@"hl'").low = value,
        .h => @ptrCast(*u8u8u8, &self.@"hl'").high = value,
        .hlu => @ptrCast(*u8u8u8, &self.@"hl'").upper = value,
        .ixl, .ixh, .ixu, .iyu, .iyl, .iyh, .r, .mb => unreachable,

        // 16-bit registers
        .af => self.@"af'" = value,
        .bc => @ptrCast(*u8u16, &self.@"bc'").short = value,
        .de => @ptrCast(*u8u16, &self.@"de'").short = value,
        .hl => @ptrCast(*u8u16, &self.@"hl'").short = value,
        .ix, .iy, .sps, .i => unreachable,

        // 24-bit registers
        .ubc => self.@"bc'" = value,
        .ude => self.@"de'" = value,
        .uhl => self.@"hl'" = value,
        .uix, .uiy, .spl => unreachable,
        .pc => self.raw_pc = value,
    }
}

test "registers" {
    var cpu: Cpu = undefined;
    cpu.backend = try Dummy.create(std.testing.allocator);
    defer cpu.deinit(std.testing.allocator);

    cpu.set(.af, 0x0123);
    cpu.set(.ubc, 0x456789);
    cpu.set(.ude, 0xABCDEF);
    cpu.set(.uhl, 0x02468A);
    cpu.setShadow(.af, 0xCE13);
    cpu.setShadow(.ubc, 0x579ACE);
    cpu.setShadow(.ude, 0x0369CF);
    cpu.setShadow(.uhl, 0x147AD2);
    cpu.set(.uix, 0x58BE04);
    cpu.set(.uiy, 0x8C159D);
    cpu.set(.sps, 0x26AE);
    cpu.set(.spl, 0x37BF05);
    cpu.set(.pc, 0xAF16B2);
    cpu.set(.i, 0x7C38);
    cpu.set(.r, 0xD4);
    cpu.set(.mb, 0x9E);

    try std.testing.expectEqual(@as(u1, 1), cpu.get(.cf));
    try std.testing.expectEqual(@as(u1, 1), cpu.get(.nf));
    try std.testing.expectEqual(@as(u1, 0), cpu.get(.pv));
    try std.testing.expectEqual(@as(u1, 0), cpu.get(.xf));
    try std.testing.expectEqual(@as(u1, 0), cpu.get(.hc));
    try std.testing.expectEqual(@as(u1, 1), cpu.get(.yf));
    try std.testing.expectEqual(@as(u1, 0), cpu.get(.zf));
    try std.testing.expectEqual(@as(u1, 0), cpu.get(.sf));

    try std.testing.expectEqual(@as(u1, 1), cpu.getShadow(.cf));
    try std.testing.expectEqual(@as(u1, 1), cpu.getShadow(.nf));
    try std.testing.expectEqual(@as(u1, 0), cpu.getShadow(.pv));
    try std.testing.expectEqual(@as(u1, 0), cpu.getShadow(.xf));
    try std.testing.expectEqual(@as(u1, 1), cpu.getShadow(.hc));
    try std.testing.expectEqual(@as(u1, 0), cpu.getShadow(.yf));
    try std.testing.expectEqual(@as(u1, 0), cpu.getShadow(.zf));
    try std.testing.expectEqual(@as(u1, 0), cpu.getShadow(.sf));

    try std.testing.expectEqual(@as(u8, 0x23), cpu.get(.f));
    try std.testing.expectEqual(@as(u8, 0x01), cpu.get(.a));
    try std.testing.expectEqual(@as(u8, 0x89), cpu.get(.c));
    try std.testing.expectEqual(@as(u8, 0x67), cpu.get(.b));
    try std.testing.expectEqual(@as(u8, 0x45), cpu.get(.bcu));
    try std.testing.expectEqual(@as(u8, 0xEF), cpu.get(.e));
    try std.testing.expectEqual(@as(u8, 0xCD), cpu.get(.d));
    try std.testing.expectEqual(@as(u8, 0xAB), cpu.get(.deu));
    try std.testing.expectEqual(@as(u8, 0x8A), cpu.get(.l));
    try std.testing.expectEqual(@as(u8, 0x46), cpu.get(.h));
    try std.testing.expectEqual(@as(u8, 0x02), cpu.get(.hlu));
    try std.testing.expectEqual(@as(u8, 0x04), cpu.get(.ixl));
    try std.testing.expectEqual(@as(u8, 0xBE), cpu.get(.ixh));
    try std.testing.expectEqual(@as(u8, 0x58), cpu.get(.ixu));
    try std.testing.expectEqual(@as(u8, 0x9D), cpu.get(.iyl));
    try std.testing.expectEqual(@as(u8, 0x15), cpu.get(.iyh));
    try std.testing.expectEqual(@as(u8, 0x8C), cpu.get(.iyu));
    try std.testing.expectEqual(@as(u8, 0xD4), cpu.get(.r));
    try std.testing.expectEqual(@as(u8, 0x9E), cpu.get(.mb));

    try std.testing.expectEqual(@as(u8, 0x13), cpu.getShadow(.f));
    try std.testing.expectEqual(@as(u8, 0xCE), cpu.getShadow(.a));
    try std.testing.expectEqual(@as(u8, 0xCE), cpu.getShadow(.c));
    try std.testing.expectEqual(@as(u8, 0x9A), cpu.getShadow(.b));
    try std.testing.expectEqual(@as(u8, 0x57), cpu.getShadow(.bcu));
    try std.testing.expectEqual(@as(u8, 0xCF), cpu.getShadow(.e));
    try std.testing.expectEqual(@as(u8, 0x69), cpu.getShadow(.d));
    try std.testing.expectEqual(@as(u8, 0x03), cpu.getShadow(.deu));
    try std.testing.expectEqual(@as(u8, 0xD2), cpu.getShadow(.l));
    try std.testing.expectEqual(@as(u8, 0x7A), cpu.getShadow(.h));
    try std.testing.expectEqual(@as(u8, 0x14), cpu.getShadow(.hlu));

    try std.testing.expectEqual(@as(u16, 0x0123), cpu.get(.af));
    try std.testing.expectEqual(@as(u16, 0x6789), cpu.get(.bc));
    try std.testing.expectEqual(@as(u16, 0xCDEF), cpu.get(.de));
    try std.testing.expectEqual(@as(u16, 0x468A), cpu.get(.hl));
    try std.testing.expectEqual(@as(u16, 0xBE04), cpu.get(.ix));
    try std.testing.expectEqual(@as(u16, 0x159D), cpu.get(.iy));
    try std.testing.expectEqual(@as(u16, 0x26AE), cpu.get(.sps));
    try std.testing.expectEqual(@as(u16, 0x7C38), cpu.get(.i));

    try std.testing.expectEqual(@as(u16, 0xCE13), cpu.getShadow(.af));
    try std.testing.expectEqual(@as(u16, 0x9ACE), cpu.getShadow(.bc));
    try std.testing.expectEqual(@as(u16, 0x69CF), cpu.getShadow(.de));
    try std.testing.expectEqual(@as(u16, 0x7AD2), cpu.getShadow(.hl));

    try std.testing.expectEqual(@as(u24, 0x456789), cpu.get(.ubc));
    try std.testing.expectEqual(@as(u24, 0xABCDEF), cpu.get(.ude));
    try std.testing.expectEqual(@as(u24, 0x02468A), cpu.get(.uhl));
    try std.testing.expectEqual(@as(u24, 0x58BE04), cpu.get(.uix));
    try std.testing.expectEqual(@as(u24, 0x8C159D), cpu.get(.uiy));
    try std.testing.expectEqual(@as(u24, 0x37BF05), cpu.get(.spl));
    try std.testing.expectEqual(@as(u24, 0xAF16B2), cpu.get(.pc));

    try std.testing.expectEqual(@as(u24, 0x579ACE), cpu.getShadow(.ubc));
    try std.testing.expectEqual(@as(u24, 0x0369CF), cpu.getShadow(.ude));
    try std.testing.expectEqual(@as(u24, 0x147AD2), cpu.getShadow(.uhl));
}

pub fn init(self: *Cpu, allocator: std.mem.Allocator) !void {
    self.* = Cpu{
        .backend = try Interpreter.create(allocator),
    };
}
pub fn deinit(self: *Cpu, allocator: std.mem.Allocator) void {
    self.backend.destroy(self.backend, allocator);
}

pub fn needFlush(self: *Cpu) void {
    self.backend.flush = true;
}

pub fn flush(self: *Cpu) void {
    self.execute(.flush);
}

pub fn step(self: *Cpu) void {
    self.execute(.step);
}

pub fn run(self: *Cpu) void {
    self.execute(.run);
}

fn execute(self: *Cpu, mode: ExecuteMode) void {
    const core = @fieldParentPtr(CEmuCore, "cpu", self);
    self.backend.execute(self.backend, core, mode);
}
