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
    execute: *const fn (*Backend, *Cpu, ExecuteMode) void,
    destroy: *const fn (*Backend, std.mem.Allocator) void,
};

pub const RegisterId = enum {
    // state
    adl,
    ief,
    im,

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
    i,
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
    ui,

    // 24-bit registers
    ubc,
    ude,
    uhl,
    uix,
    uiy,
    spl,
    mbi,
    pc,
};

pub const Adl = enum(u1) { z80, ez80 };

pub const Mode = packed struct(u2) {
    adl: Adl,
    madl: Adl,
};

pub const Flag = bool;

pub const Flags = packed struct(u8) {
    cf: Flag,
    nf: Flag,
    pv: Flag,
    xf: Flag,
    hc: Flag,
    yf: Flag,
    zf: Flag,
    sf: Flag,
};

pub const Word = packed union {
    byte: packed struct(u16) { low: u8, high: u8 },
    word: u16,
};

pub const Long = packed union {
    byte: packed struct(u24) { low: u8, high: u8, upper: u8 },
    word: Word,
    ext: packed struct(u24) { word: u16, upper: u8 },
    long: u24,
};

cf: Flag = false,
nf: Flag = false,
pv: Flag = false,
xf: Flag = false,
hc: Flag = false,
yf: Flag = false,
zf: Flag = false,
sf: Flag = false,

a: u8 = 0,
bc: Long = .{ .long = 0 },
de: Long = .{ .long = 0 },
hl: Long = .{ .long = 0 },

@"af'": packed union {
    flags: Flags,
    word: Word,
} = .{ .word = .{ .word = 0 } },
@"bc'": Long = .{ .long = 0 },
@"de'": Long = .{ .long = 0 },
@"hl'": Long = .{ .long = 0 },

ix: Long = .{ .long = 0 },
iy: Long = .{ .long = 0 },

sps: Word = .{ .word = 0 },
spl: Long = .{ .long = 0 },
epc: Long = .{ .long = 0 },
pc: Long = .{ .long = 0 },
r: u8 = 0,
mbi: Long = .{ .long = 0 },

mode: Mode = .{ .adl = .z80, .madl = .z80 },

ief1: Flag = false,
ief2: Flag = false,

im: u2 = 1,

cycles: u64 = 0,

backend: *Backend,

pub fn getF(self: *const Cpu) u8 {
    return util.toBacking(Flags{
        .cf = self.cf,
        .nf = self.nf,
        .pv = self.pv,
        .xf = self.xf,
        .hc = self.hc,
        .yf = self.yf,
        .zf = self.zf,
        .sf = self.sf,
    });
}
pub fn setF(self: *Cpu, value: u8) void {
    const flags = util.fromBacking(Flags, value);
    self.cf = flags.cf;
    self.nf = flags.nf;
    self.pv = flags.pv;
    self.xf = flags.xf;
    self.hc = flags.hc;
    self.yf = flags.yf;
    self.zf = flags.zf;
    self.sf = flags.sf;
}

pub fn getAF(self: *const Cpu) u16 {
    return (Word{ .byte = .{ .low = self.getF(), .high = self.a } }).word;
}
pub fn setAF(self: *const Cpu, value: u16) void {
    const word = Word{ .word = value };
    self.setF(word.byte.low);
    self.a = word.byte.high;
}

pub fn getR(self: *const Cpu) u8 {
    return std.math.rotr(u8, self.r, 1);
}
pub fn setR(self: *Cpu, value: u8) void {
    self.r = std.math.rotl(u8, value, 1);
}
pub fn addR(self: *Cpu, comptime offset: comptime_int) void {
    self.r = @bitCast(u8, @bitCast(i8, self.r) +% (offset << 1));
}

pub fn get(self: *const Cpu, id: RegisterId) u24 {
    return switch (id) {
        // state
        .adl => @enumToInt(self.mode.adl),
        .ief => @boolToInt(self.ief1),
        .im => self.im,

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
        .f => self.getF(),
        .a => self.a,
        .c => self.bc.byte.low,
        .b => self.bc.byte.high,
        .bcu => self.bc.byte.upper,
        .e => self.de.byte.low,
        .d => self.de.byte.high,
        .deu => self.de.byte.upper,
        .l => self.hl.byte.low,
        .h => self.hl.byte.high,
        .hlu => self.hl.byte.upper,
        .ixl => self.ix.byte.low,
        .ixh => self.ix.byte.high,
        .ixu => self.ix.byte.upper,
        .iyl => self.iy.byte.low,
        .iyh => self.iy.byte.high,
        .iyu => self.iy.byte.upper,
        .i => self.mbi.byte.low,
        .r => self.getR(),
        .mb => self.mbi.ext.upper,

        // 16-bit registers
        .af => (Word{ .byte = .{ .low = self.getF(), .high = self.a } }).word,
        .bc => self.bc.word.word,
        .de => self.de.word.word,
        .hl => self.hl.word.word,
        .ix => self.ix.word.word,
        .iy => self.iy.word.word,
        .sps => self.sps.word,
        .ui => self.mbi.ext.word,

        // 24-bit registers
        .ubc => self.bc.long,
        .ude => self.de.long,
        .uhl => self.hl.long,
        .uix => self.ix.long,
        .uiy => self.iy.long,
        .spl => self.spl.long,
        .mbi => self.mbi.long,
        .pc => self.epc.long,
    };
}

pub fn getShadow(self: *const Cpu, id: RegisterId) u24 {
    return switch (id) {
        // state
        .adl => @enumToInt(self.mode.madl),
        .ief => @boolToInt(self.ief2),
        .im => unreachable,

        // 1-bit flags
        .cf => @boolToInt(self.@"af'".flags.cf),
        .nf => @boolToInt(self.@"af'".flags.nf),
        .pv => @boolToInt(self.@"af'".flags.pv),
        .xf => @boolToInt(self.@"af'".flags.xf),
        .hc => @boolToInt(self.@"af'".flags.hc),
        .yf => @boolToInt(self.@"af'".flags.yf),
        .zf => @boolToInt(self.@"af'".flags.zf),
        .sf => @boolToInt(self.@"af'".flags.sf),

        // 8-bit registers
        .f => self.@"af'".word.byte.low,
        .a => self.@"af'".word.byte.high,
        .c => self.@"bc'".byte.low,
        .b => self.@"bc'".byte.high,
        .bcu => self.@"bc'".byte.upper,
        .e => self.@"de'".byte.low,
        .d => self.@"de'".byte.high,
        .deu => self.@"de'".byte.upper,
        .l => self.@"hl'".byte.low,
        .h => self.@"hl'".byte.high,
        .hlu => self.@"hl'".byte.upper,
        .ixl, .ixh, .ixu, .iyu, .iyl, .iyh, .i, .r, .mb => unreachable,

        // 16-bit registers
        .af => self.@"af'".word.word,
        .bc => self.@"bc'".word.word,
        .de => self.@"de'".word.word,
        .hl => self.@"hl'".word.word,
        .ix, .iy, .sps, .ui => unreachable,

        // 24-bit registers
        .ubc => self.@"bc'".long,
        .ude => self.@"de'".long,
        .uhl => self.@"hl'".long,
        .uix, .uiy, .spl, .mbi => unreachable,
        .pc => self.pc.long,
    };
}

pub fn set(self: *Cpu, id: RegisterId, value: u24) void {
    switch (id) {
        // state
        .adl => self.mode.adl = @intToEnum(Adl, value),
        .ief => {
            self.ief1 = @intCast(u1, value) != 0;
            self.setShadow(.ief, value);
        },
        .im => {
            std.debug.assert(value < 2);
            self.im = @intCast(u2, value);
        },

        // 1-bit flags
        .cf => self.cf = @intCast(u1, value) != 0,
        .nf => self.nf = @intCast(u1, value) != 0,
        .pv => self.pv = @intCast(u1, value) != 0,
        .xf => self.xf = @intCast(u1, value) != 0,
        .hc => self.hc = @intCast(u1, value) != 0,
        .yf => self.yf = @intCast(u1, value) != 0,
        .zf => self.zf = @intCast(u1, value) != 0,
        .sf => self.sf = @intCast(u1, value) != 0,

        // 8-bit registers
        .f => self.setF(@intCast(u8, value)),
        .a => self.a = @intCast(u8, value),
        .c => self.bc.byte.low = @intCast(u8, value),
        .b => self.bc.byte.high = @intCast(u8, value),
        .bcu => self.bc.byte.upper = @intCast(u8, value),
        .e => self.de.byte.low = @intCast(u8, value),
        .d => self.de.byte.high = @intCast(u8, value),
        .deu => self.de.byte.upper = @intCast(u8, value),
        .l => self.hl.byte.low = @intCast(u8, value),
        .h => self.hl.byte.high = @intCast(u8, value),
        .hlu => self.hl.byte.upper = @intCast(u8, value),

        .ixl => self.ix.byte.low = @intCast(u8, value),
        .ixh => self.ix.byte.high = @intCast(u8, value),
        .ixu => self.ix.byte.upper = @intCast(u8, value),
        .iyl => self.iy.byte.low = @intCast(u8, value),
        .iyh => self.iy.byte.high = @intCast(u8, value),
        .iyu => self.iy.byte.upper = @intCast(u8, value),
        .i => self.mbi.byte.low = @intCast(u8, value),
        .r => self.setR(@intCast(u8, value)),
        .mb => self.mbi.ext.upper = @intCast(u8, value),

        // 16-bit registers
        .af => {
            const word = Word{ .word = @intCast(u16, value) };
            self.setF(word.byte.low);
            self.a = word.byte.high;
        },
        .bc => self.bc.word.word = @intCast(u16, value),
        .de => self.de.word.word = @intCast(u16, value),
        .hl => self.hl.word.word = @intCast(u16, value),
        .ix => self.ix.word.word = @intCast(u16, value),
        .iy => self.iy.word.word = @intCast(u16, value),
        .sps => self.sps.word = @intCast(u16, value),
        .ui => self.mbi.ext.word = @intCast(u16, value),

        // 24-bit registers
        .ubc => self.bc.long = value,
        .ude => self.de.long = value,
        .uhl => self.hl.long = value,
        .uix => self.ix.long = value,
        .uiy => self.iy.long = value,
        .spl => self.spl.long = value,
        .mbi => self.mbi.long = value,
        .pc => {
            self.epc.long = value;
            self.backend.flush = true;
        },
    }
}

pub fn setShadow(self: *Cpu, id: RegisterId, value: u24) void {
    switch (id) {
        // state
        .adl => self.mode.madl = @intToEnum(Adl, value),
        .ief => self.ief2 = @intCast(u1, value) != 0,
        .im => unreachable,

        // 1-bit flags
        .cf => self.@"af'".flags.cf = @intCast(u1, value) != 0,
        .nf => self.@"af'".flags.nf = @intCast(u1, value) != 0,
        .pv => self.@"af'".flags.pv = @intCast(u1, value) != 0,
        .xf => self.@"af'".flags.xf = @intCast(u1, value) != 0,
        .hc => self.@"af'".flags.hc = @intCast(u1, value) != 0,
        .yf => self.@"af'".flags.yf = @intCast(u1, value) != 0,
        .zf => self.@"af'".flags.zf = @intCast(u1, value) != 0,
        .sf => self.@"af'".flags.sf = @intCast(u1, value) != 0,

        // 8-bit registers
        .f => self.@"af'".word.byte.low = @intCast(u8, value),
        .a => self.@"af'".word.byte.high = @intCast(u8, value),
        .c => self.@"bc'".byte.low = @intCast(u8, value),
        .b => self.@"bc'".byte.high = @intCast(u8, value),
        .bcu => self.@"bc'".byte.upper = @intCast(u8, value),
        .e => self.@"de'".byte.low = @intCast(u8, value),
        .d => self.@"de'".byte.high = @intCast(u8, value),
        .deu => self.@"de'".byte.upper = @intCast(u8, value),
        .l => self.@"de'".byte.low = @intCast(u8, value),
        .h => self.@"de'".byte.high = @intCast(u8, value),
        .hlu => self.@"hl'".byte.upper = @intCast(u8, value),
        .ixl, .ixh, .ixu, .iyu, .iyl, .iyh, .i, .r, .mb => unreachable,

        // 16-bit registers
        .af => self.@"af'".word.word = @intCast(u16, value),
        .bc => self.@"bc'".word.word = @intCast(u16, value),
        .de => self.@"de'".word.word = @intCast(u16, value),
        .hl => self.@"hl'".word.word = @intCast(u16, value),
        .ix, .iy, .sps, .ui => unreachable,

        // 24-bit registers
        .ubc => self.@"bc'".long = value,
        .ude => self.@"de'".long = value,
        .uhl => self.@"hl'".long = value,
        .uix, .uiy, .spl, .mbi => unreachable,
        .pc => self.pc.long = value,
    }
}

pub fn init(self: *Cpu, allocator: std.mem.Allocator) !void {
    self.* = Cpu{
        .backend = try Interpreter.create(allocator),
    };
}
pub fn deinit(self: *Cpu, allocator: std.mem.Allocator) void {
    self.backend.destroy(self.backend, allocator);
}

fn core(self: *Cpu) *CEmuCore {
    return @fieldParentPtr(CEmuCore, "cpu", self);
}

pub fn read(self: *Cpu, address: u24) u8 {
    return self.core().mem.read(address, &self.cycles);
}
pub fn write(self: *Cpu, address: u24, value: u8) void {
    self.core().mem.write(address, value, &self.cycles);
}

pub fn in(self: *Cpu, address: u16) u8 {
    return self.core().ports.read(address, &self.cycles);
}
pub fn out(self: *Cpu, address: u16, value: u8) void {
    self.core().ports.write(address, value, &self.cycles);
}

fn execute(self: *Cpu, mode: ExecuteMode) void {
    self.backend.execute(self.backend, self, mode);
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
    cpu.set(.ui, 0x7C38);
    cpu.set(.r, 0xD4);
    cpu.set(.mb, 0x9E);

    try std.testing.expectEqual(@as(u1, 1), @intCast(u1, cpu.get(.cf)));
    try std.testing.expectEqual(@as(u1, 1), @intCast(u1, cpu.get(.nf)));
    try std.testing.expectEqual(@as(u1, 0), @intCast(u1, cpu.get(.pv)));
    try std.testing.expectEqual(@as(u1, 0), @intCast(u1, cpu.get(.xf)));
    try std.testing.expectEqual(@as(u1, 0), @intCast(u1, cpu.get(.hc)));
    try std.testing.expectEqual(@as(u1, 1), @intCast(u1, cpu.get(.yf)));
    try std.testing.expectEqual(@as(u1, 0), @intCast(u1, cpu.get(.zf)));
    try std.testing.expectEqual(@as(u1, 0), @intCast(u1, cpu.get(.sf)));

    try std.testing.expectEqual(@as(u1, 1), @intCast(u1, cpu.getShadow(.cf)));
    try std.testing.expectEqual(@as(u1, 1), @intCast(u1, cpu.getShadow(.nf)));
    try std.testing.expectEqual(@as(u1, 0), @intCast(u1, cpu.getShadow(.pv)));
    try std.testing.expectEqual(@as(u1, 0), @intCast(u1, cpu.getShadow(.xf)));
    try std.testing.expectEqual(@as(u1, 1), @intCast(u1, cpu.getShadow(.hc)));
    try std.testing.expectEqual(@as(u1, 0), @intCast(u1, cpu.getShadow(.yf)));
    try std.testing.expectEqual(@as(u1, 0), @intCast(u1, cpu.getShadow(.zf)));
    try std.testing.expectEqual(@as(u1, 0), @intCast(u1, cpu.getShadow(.sf)));

    try std.testing.expectEqual(@as(u8, 0x23), @intCast(u8, cpu.get(.f)));
    try std.testing.expectEqual(@as(u8, 0x01), @intCast(u8, cpu.get(.a)));
    try std.testing.expectEqual(@as(u8, 0x89), @intCast(u8, cpu.get(.c)));
    try std.testing.expectEqual(@as(u8, 0x67), @intCast(u8, cpu.get(.b)));
    try std.testing.expectEqual(@as(u8, 0x45), @intCast(u8, cpu.get(.bcu)));
    try std.testing.expectEqual(@as(u8, 0xEF), @intCast(u8, cpu.get(.e)));
    try std.testing.expectEqual(@as(u8, 0xCD), @intCast(u8, cpu.get(.d)));
    try std.testing.expectEqual(@as(u8, 0xAB), @intCast(u8, cpu.get(.deu)));
    try std.testing.expectEqual(@as(u8, 0x8A), @intCast(u8, cpu.get(.l)));
    try std.testing.expectEqual(@as(u8, 0x46), @intCast(u8, cpu.get(.h)));
    try std.testing.expectEqual(@as(u8, 0x02), @intCast(u8, cpu.get(.hlu)));
    try std.testing.expectEqual(@as(u8, 0x04), @intCast(u8, cpu.get(.ixl)));
    try std.testing.expectEqual(@as(u8, 0xBE), @intCast(u8, cpu.get(.ixh)));
    try std.testing.expectEqual(@as(u8, 0x58), @intCast(u8, cpu.get(.ixu)));
    try std.testing.expectEqual(@as(u8, 0x9D), @intCast(u8, cpu.get(.iyl)));
    try std.testing.expectEqual(@as(u8, 0x15), @intCast(u8, cpu.get(.iyh)));
    try std.testing.expectEqual(@as(u8, 0x8C), @intCast(u8, cpu.get(.iyu)));
    try std.testing.expectEqual(@as(u8, 0xD4), @intCast(u8, cpu.get(.r)));
    try std.testing.expectEqual(@as(u8, 0x9E), @intCast(u8, cpu.get(.mb)));

    try std.testing.expectEqual(@as(u8, 0x13), @intCast(u8, cpu.getShadow(.f)));
    try std.testing.expectEqual(@as(u8, 0xCE), @intCast(u8, cpu.getShadow(.a)));
    try std.testing.expectEqual(@as(u8, 0xCE), @intCast(u8, cpu.getShadow(.c)));
    try std.testing.expectEqual(@as(u8, 0x9A), @intCast(u8, cpu.getShadow(.b)));
    try std.testing.expectEqual(@as(u8, 0x57), @intCast(u8, cpu.getShadow(.bcu)));
    try std.testing.expectEqual(@as(u8, 0xCF), @intCast(u8, cpu.getShadow(.e)));
    try std.testing.expectEqual(@as(u8, 0x69), @intCast(u8, cpu.getShadow(.d)));
    try std.testing.expectEqual(@as(u8, 0x03), @intCast(u8, cpu.getShadow(.deu)));
    try std.testing.expectEqual(@as(u8, 0xD2), @intCast(u8, cpu.getShadow(.l)));
    try std.testing.expectEqual(@as(u8, 0x7A), @intCast(u8, cpu.getShadow(.h)));
    try std.testing.expectEqual(@as(u8, 0x14), @intCast(u8, cpu.getShadow(.hlu)));

    try std.testing.expectEqual(@as(u16, 0x0123), @intCast(u16, cpu.get(.af)));
    try std.testing.expectEqual(@as(u16, 0x6789), @intCast(u16, cpu.get(.bc)));
    try std.testing.expectEqual(@as(u16, 0xCDEF), @intCast(u16, cpu.get(.de)));
    try std.testing.expectEqual(@as(u16, 0x468A), @intCast(u16, cpu.get(.hl)));
    try std.testing.expectEqual(@as(u16, 0xBE04), @intCast(u16, cpu.get(.ix)));
    try std.testing.expectEqual(@as(u16, 0x159D), @intCast(u16, cpu.get(.iy)));
    try std.testing.expectEqual(@as(u16, 0x26AE), @intCast(u16, cpu.get(.sps)));
    try std.testing.expectEqual(@as(u16, 0x7C38), @intCast(u16, cpu.get(.ui)));

    try std.testing.expectEqual(@as(u16, 0xCE13), @intCast(u16, cpu.getShadow(.af)));
    try std.testing.expectEqual(@as(u16, 0x9ACE), @intCast(u16, cpu.getShadow(.bc)));
    try std.testing.expectEqual(@as(u16, 0x69CF), @intCast(u16, cpu.getShadow(.de)));
    try std.testing.expectEqual(@as(u16, 0x7AD2), @intCast(u16, cpu.getShadow(.hl)));

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
