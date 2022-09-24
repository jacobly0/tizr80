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
    mbase,

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
    mbaseui,
    pc,
};

pub const Adl = enum(u1) { z80, ez80 };

pub const Mode = packed struct(u2) {
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

    pub fn getCpu(cpu: *const Cpu) Flags {
        return .{
            .cf = cpu.cf,
            .nf = cpu.nf,
            .pv = cpu.pv,
            .xf = cpu.xf,
            .hc = cpu.hc,
            .yf = cpu.yf,
            .zf = cpu.zf,
            .sf = cpu.sf,
        };
    }
    pub fn setCpu(self: Flags, cpu: *Cpu) void {
        cpu.cf = self.cf;
        cpu.nf = self.nf;
        cpu.pv = self.pv;
        cpu.xf = self.xf;
        cpu.hc = self.hc;
        cpu.yf = self.yf;
        cpu.zf = self.zf;
        cpu.sf = self.sf;
    }
};

mode: Mode = .{ .adl = .z80, .madl = .z80 },

cf: bool = false,
nf: bool = false,
pv: bool = false,
xf: bool = false,
hc: bool = false,
yf: bool = false,
zf: bool = false,
sf: bool = false,

a: u8 = 0,
r: u8 = 0,

bc: u32 = 0,
de: u32 = 0,
hl: u32 = 0,

@"af'": u32 = 0,
@"bc'": u32 = 0,
@"de'": u32 = 0,
@"hl'": u32 = 0,

ix: u32 = 0,
iy: u32 = 0,

sps: u32 = 0,
spl: u32 = 0,
epc: u32 = 0,
pc: u32 = 0,
mbi: u32 = 0,

im: u2 = 1,
ief1: bool = false,
ief2: bool = false,

cycles: u64 = 0,

backend: *Backend,

pub fn RegisterType(comptime id: RegisterId) type {
    return switch (id) {
        .adl, .ief, .cf, .nf, .pv, .xf, .hc, .yf, .zf, .sf => u1,
        .im => u2,
        .f, .a, .c, .b, .bcu, .e, .d, .deu, .l, .h, .hlu, .ixl, .ixh, .ixu, .iyl, .iyh, .iyu, .i, .r, .mbase => u8,
        .af, .bc, .de, .hl, .ix, .iy, .sps, .ui => u16,
        .ubc, .ude, .uhl, .uix, .uiy, .spl, .mbaseui, .pc => u24,
    };
}

pub fn registerBitOffset(comptime id: RegisterId) comptime_int {
    return switch (id) {
        .adl, .ief, .im => 0,
        .cf, .nf, .pv, .xf, .hc, .yf, .zf, .sf => @bitOffsetOf(Flags, @tagName(id)),
        .f, .c, .e, .l, .ixl, .iyl, .r, .i => 0,
        .a, .b, .d, .h, .ixh, .iyh => 8,
        .bcu, .deu, .hlu, .ixu, .iyu, .mbase => 16,
        .af, .bc, .de, .hl, .ix, .iy, .sps, .ui => 0,
        .ubc, .ude, .uhl, .uix, .uiy, .spl, .mbaseui, .pc => 0,
    };
}

pub fn get(self: *const Cpu, comptime id: RegisterId) RegisterType(id) {
    return util.bit.extract(switch (id) {
        .adl => @enumToInt(self.mode.adl),
        .ief => @boolToInt(self.ief1),
        .im => self.im,
        .cf, .nf, .pv, .xf, .hc, .yf, .zf, .sf, .a, .f, .af => util.bit.concat(.{
            self.a,
            util.toBacking(Flags.getCpu(self)),
        }),
        .bcu, .b, .c, .bc, .ubc => self.bc,
        .deu, .d, .e, .de, .ude => self.de,
        .hlu, .h, .l, .hl, .uhl => self.hl,
        .ixu, .ixh, .ixl, .ix, .uix => self.ix,
        .iyu, .iyh, .iyl, .iy, .uiy => self.iy,
        .mbase, .i, .ui, .mbaseui => self.mbi,
        .r => std.math.rotr(u8, self.r, 1),
        .sps => self.sps,
        .spl => self.spl,
        .pc => self.epc,
    }, RegisterType(id), registerBitOffset(id));
}
pub fn getAny(self: *const Cpu, id: RegisterId) u24 {
    return inline for (@typeInfo(RegisterId).Enum.fields) |field| {
        const comptimeId = @field(RegisterId, field.name);
        if (id == comptimeId) break self.get(comptimeId);
    } else unreachable;
}

pub fn getShadow(self: *const Cpu, comptime id: RegisterId) RegisterType(id) {
    return util.bit.extract(switch (id) {
        .adl => return @enumToInt(self.mode.madl),
        .ief => return @boolToInt(self.ief2),
        .im => unreachable,
        .cf, .nf, .pv, .xf, .hc, .yf, .zf, .sf, .a, .f, .af => self.@"af'",
        .bcu, .b, .c, .bc, .ubc => self.@"bc'",
        .deu, .d, .e, .de, .ude => self.@"de'",
        .hlu, .h, .l, .hl, .uhl => self.@"hl'",
        .ixl, .ixh, .ixu, .iyu, .iyl, .iyh, .i, .r, .mbase => unreachable,
        .ix, .iy, .sps, .ui => unreachable,
        .uix, .uiy, .spl, .mbaseui => unreachable,
        .pc => self.pc,
    }, RegisterType(id), registerBitOffset(id));
}
pub fn getAnyShadow(self: *const Cpu, id: RegisterId) u24 {
    return inline for (@typeInfo(RegisterId).Enum.fields) |field| {
        const comptimeId = @field(RegisterId, field.name);
        if (id == comptimeId) break self.getShadow(comptimeId);
    } else unreachable;
}

pub fn set(self: *Cpu, comptime id: RegisterId, value: RegisterType(id)) void {
    util.bit.insert(&switch (id) {
        .adl => {
            self.mode.adl = @intToEnum(Adl, value);
            return;
        },
        .ief => {
            self.ief1 = value != 0;
            self.setShadow(id, value);
            return;
        },
        .im => {
            std.debug.assert(value <= 2);
            self.im = value;
            return;
        },
        .cf, .nf, .pv, .xf, .hc, .yf, .zf, .sf => {
            @field(self, @tagName(id)) = value != 0;
            return;
        },
        .f => {
            util.fromBacking(Flags, value).setCpu(self);
            return;
        },
        .a => {
            self.a = value;
            return;
        },
        .af => {
            self.set(.f, util.bit.extract(value, u8, 0));
            self.set(.a, util.bit.extract(value, u8, 8));
            return;
        },
        .bcu, .b, .c, .bc, .ubc => self.bc,
        .deu, .d, .e, .de, .ude => self.de,
        .hlu, .h, .l, .hl, .uhl => self.hl,
        .ixu, .ixh, .ixl, .ix, .uix => self.ix,
        .iyu, .iyh, .iyl, .iy, .uiy => self.iy,
        .mbase, .i, .ui, .mbaseui => self.mbi,
        .r => {
            self.r = std.math.rotl(u8, value, 1);
            return;
        },
        .sps => self.sps,
        .spl => self.spl,
        .pc => field: {
            self.backend.flush = true;
            break :field self.epc;
        },
    }, value, registerBitOffset(id));
}
pub fn setAny(self: *Cpu, id: RegisterId, value: u24) void {
    inline for (@typeInfo(RegisterId).Enum.fields) |field| {
        const comptimeId = @field(RegisterId, field.name);
        if (id == comptimeId)
            break self.set(comptimeId, @intCast(RegisterType(comptimeId), value));
    } else unreachable;
}

pub fn setShadow(self: *Cpu, comptime id: RegisterId, value: RegisterType(id)) void {
    util.bit.insert(&switch (id) {
        .adl => {
            self.mode.madl = @intToEnum(Adl, value);
            return;
        },
        .ief => {
            self.ief2 = @intCast(u1, value) != 0;
            return;
        },
        .im => unreachable,
        .cf, .nf, .pv, .xf, .hc, .yf, .zf, .sf, .a, .f, .af => self.@"af'",
        .bcu, .b, .c, .bc, .ubc => self.@"bc'",
        .deu, .d, .e, .de, .ude => self.@"de'",
        .hlu, .h, .l, .hl, .uhl => self.@"hl'",
        .ixl, .ixh, .ixu, .iyu, .iyl, .iyh, .i, .r, .mbase => unreachable,
        .ix, .iy, .sps, .ui => unreachable,
        .uix, .uiy, .spl, .mbaseui => unreachable,
        .pc => self.pc,
    }, value, registerBitOffset(id));
}
pub fn setAnyShadow(self: *Cpu, id: RegisterId, value: u24) void {
    inline for (@typeInfo(RegisterId).Enum.fields) |field| {
        const comptimeId = @field(RegisterId, field.name);
        if (id == comptimeId)
            break self.setShadow(comptimeId, @intCast(RegisterType(comptimeId), value));
    } else unreachable;
}

pub fn add(self: *Cpu, comptime id: RegisterId, comptime offset: comptime_int) void {
    const increment = @bitCast(
        RegisterType(id),
        @as(std.meta.Int(.signed, @typeInfo(RegisterType(id)).Int.bits), offset),
    );
    switch (id) {
        .r => self.r +%= increment << 1,
        else => self.set(id, self.get(id) +% increment),
    }
}

pub fn init(self: *Cpu, allocator: std.mem.Allocator) !void {
    self.* = Cpu{
        .backend = try Interpreter.create(allocator),
    };
}
pub fn deinit(self: *Cpu, allocator: std.mem.Allocator) void {
    self.backend.destroy(self.backend, allocator);
    self.* = undefined;
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
    cpu.set(.mbase, 0x9E);

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
    try std.testing.expectEqual(@as(u8, 0x9E), @intCast(u8, cpu.get(.mbase)));

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
