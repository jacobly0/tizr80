const std = @import("std");

const Cpu = @This();

pub const RegisterAddress = enum {
    // 1-bit flags
    CarryFlag,
    SubtractFlag,
    ParityOverflowFlag,
    XFlag,
    HalfCarryFlag,
    YFlag,
    ZeroFlag,
    SignFlag,

    // 8-bit registers
    F,
    A,
    C,
    B,
    BCU,
    E,
    D,
    DEU,
    L,
    H,
    HLU,
    IXL,
    IXH,
    IXU,
    IYL,
    IYH,
    IYU,
    R,
    MB,

    // 16-bit registers
    AF,
    BC,
    DE,
    HL,
    IX,
    IY,
    SPS,
    I,

    // 24-bit registers
    UBC,
    UDE,
    UHL,
    UIX,
    UIY,
    SPL,
    PC,
    RPC,
};

const Flags = packed struct(u8) {
    cf: u1,
    nf: u1,
    pv: u1,
    xf: u1,
    hc: u1,
    yf: u1,
    zf: u1,
    sf: u1,

    const Backing = @typeInfo(@This()).Struct.backing_integer.?;
    pub fn from(value: Backing) @This() {
        return @bitCast(@This(), value);
    }
    pub fn all(self: @This()) Backing {
        return @bitCast(Backing, self);
    }
};

const u8u8 = packed struct(u16) {
    low: u8,
    high: u8,

    const Backing = @typeInfo(@This()).Struct.backing_integer.?;
    pub fn from(value: Backing) @This() {
        return @bitCast(@This(), value);
    }
    pub fn all(self: @This()) Backing {
        return @bitCast(Backing, self);
    }
};

const u8u8u8 = packed struct(u24) {
    low: u8,
    high: u8,
    upper: u8,

    const Backing = @typeInfo(@This()).Struct.backing_integer.?;
    pub fn from(value: Backing) @This() {
        return @bitCast(@This(), value);
    }
    pub fn all(self: @This()) Backing {
        return @bitCast(Backing, self);
    }
};

const u8u16 = packed struct(u24) {
    short: u16,
    upper: u8,

    const Backing = @typeInfo(@This()).Struct.backing_integer.?;
    pub fn from(value: Backing) @This() {
        return @bitCast(@This(), value);
    }
    pub fn all(self: @This()) Backing {
        return @bitCast(Backing, self);
    }
};

cf: u1 = 0,
nf: u1 = 0,
pv: u1 = 0,
xf: u1 = 0,
hc: u1 = 0,
yf: u1 = 0,
zf: u1 = 0,
sf: u1 = 0,

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
r: u8 = 0,
mbi: u24 = 0,

pub fn RegisterType(address: RegisterAddress) type {
    return switch (address) {
        .CarryFlag, .SubtractFlag, .ParityOverflowFlag, .XFlag, .HalfCarryFlag, .YFlag, .ZeroFlag, .SignFlag => u1,
        .F, .A, .C, .B, .BCU, .E, .D, .DEU, .L, .H, .HLU, .IXL, .IXH, .IXU, .IYL, .IYH, .IYU, .R, .MB => u8,
        .AF, .BC, .DE, .HL, .IX, .IY, .SPS, .I => u16,
        .UBC, .UDE, .UHL, .UIX, .UIY, .SPL, .PC, .RPC => u24,
    };
}

pub fn get(self: *const Cpu, address: RegisterAddress) RegisterType(address) {
    return switch (address) {
        // 1-bit flags
        .CarryFlag => self.cf,
        .SubtractFlag => self.nf,
        .ParityOverflowFlag => self.pv,
        .XFlag => self.xf,
        .HalfCarryFlag => self.hc,
        .YFlag => self.yf,
        .ZeroFlag => self.zf,
        .SignFlag => self.sf,

        // 8-bit registers
        .F => Flags.all(.{
            .cf = self.get(.CarryFlag),
            .nf = self.get(.SubtractFlag),
            .pv = self.get(.ParityOverflowFlag),
            .xf = self.get(.XFlag),
            .hc = self.get(.HalfCarryFlag),
            .yf = self.get(.YFlag),
            .zf = self.get(.ZeroFlag),
            .sf = self.get(.SignFlag),
        }),
        .A => self.a,
        .C => u8u8u8.from(self.get(.UBC)).low,
        .B => u8u8u8.from(self.get(.UBC)).high,
        .BCU => u8u8u8.from(self.get(.UBC)).upper,
        .E => u8u8u8.from(self.get(.UDE)).low,
        .D => u8u8u8.from(self.get(.UDE)).high,
        .DEU => u8u8u8.from(self.get(.UDE)).upper,
        .L => u8u8u8.from(self.get(.UHL)).low,
        .H => u8u8u8.from(self.get(.UHL)).high,
        .HLU => u8u8u8.from(self.get(.UHL)).upper,
        .IXL => u8u8u8.from(self.get(.UIX)).low,
        .IXH => u8u8u8.from(self.get(.UIX)).high,
        .IXU => u8u8u8.from(self.get(.UIX)).upper,
        .IYL => u8u8u8.from(self.get(.UIY)).low,
        .IYH => u8u8u8.from(self.get(.UIY)).high,
        .IYU => u8u8u8.from(self.get(.UIY)).upper,
        .R => std.math.rotr(u8, self.r, 1),
        .MB => u8u8u8.from(self.mbi).upper,

        // 16-bit registers
        .AF => u8u8.all(.{ .low = self.get(.F), .high = self.get(.A) }),
        .BC => u8u16.from(self.get(.UBC)).short,
        .DE => u8u16.from(self.get(.UDE)).short,
        .HL => u8u16.from(self.get(.UHL)).short,
        .IX => u8u16.from(self.get(.UIX)).short,
        .IY => u8u16.from(self.get(.UIY)).short,
        .SPS => self.sps,
        .I => u8u16.from(self.mbi).short,

        // 24-bit registers
        .UBC => self.bc,
        .UDE => self.de,
        .UHL => self.hl,
        .UIX => self.ix,
        .UIY => self.iy,
        .SPL => self.spl,
        .PC => self.pc,
        .RPC => std.debug.todo("unimplemented"),
    };
}

pub fn getShadow(self: *const Cpu, address: RegisterAddress) RegisterType(address) {
    return switch (address) {
        // 1-bit flags
        .CarryFlag => Flags.from(self.getShadow(.F)).cf,
        .SubtractFlag => Flags.from(self.getShadow(.F)).nf,
        .ParityOverflowFlag => Flags.from(self.getShadow(.F)).pv,
        .XFlag => Flags.from(self.getShadow(.F)).xf,
        .HalfCarryFlag => Flags.from(self.getShadow(.F)).hc,
        .YFlag => Flags.from(self.getShadow(.F)).yf,
        .ZeroFlag => Flags.from(self.getShadow(.F)).zf,
        .SignFlag => Flags.from(self.getShadow(.F)).sf,

        // 8-bit registers
        .F => u8u8.from(self.getShadow(.AF)).low,
        .A => u8u8.from(self.getShadow(.AF)).high,
        .C => u8u8u8.from(self.getShadow(.UBC)).low,
        .B => u8u8u8.from(self.getShadow(.UBC)).high,
        .BCU => u8u8u8.from(self.getShadow(.UBC)).upper,
        .E => u8u8u8.from(self.getShadow(.UDE)).low,
        .D => u8u8u8.from(self.getShadow(.UDE)).high,
        .DEU => u8u8u8.from(self.getShadow(.UDE)).upper,
        .L => u8u8u8.from(self.getShadow(.UHL)).low,
        .H => u8u8u8.from(self.getShadow(.UHL)).high,
        .HLU => u8u8u8.from(self.getShadow(.UHL)).upper,
        .IXL, .IXH, .IXU, .IYU, .IYL, .IYH, .R, .MB => unreachable,

        // 16-bit registers
        .AF => self.@"af'",
        .BC => u8u16.from(self.getShadow(.UBC)).short,
        .DE => u8u16.from(self.getShadow(.UDE)).short,
        .HL => u8u16.from(self.getShadow(.UHL)).short,
        .IX, .IY, .SPS, .I => unreachable,

        // 24-bit registers
        .UBC => self.@"bc'",
        .UDE => self.@"de'",
        .UHL => self.@"hl'",
        .UIX, .UIY, .SPL, .PC, .RPC => unreachable,
    };
}

pub fn set(self: *Cpu, address: RegisterAddress, value: RegisterType(address)) void {
    switch (address) {
        // 1-bit flags
        .CarryFlag => self.cf = value,
        .SubtractFlag => self.nf = value,
        .ParityOverflowFlag => self.pv = value,
        .XFlag => self.xf = value,
        .HalfCarryFlag => self.hc = value,
        .YFlag => self.yf = value,
        .ZeroFlag => self.zf = value,
        .SignFlag => self.sf = value,

        // 8-bit registers
        .F => {
            const flags = Flags.from(value);
            self.set(.CarryFlag, flags.cf);
            self.set(.SubtractFlag, flags.nf);
            self.set(.ParityOverflowFlag, flags.pv);
            self.set(.XFlag, flags.xf);
            self.set(.HalfCarryFlag, flags.hc);
            self.set(.YFlag, flags.yf);
            self.set(.ZeroFlag, flags.zf);
            self.set(.SignFlag, flags.sf);
        },
        .A => self.a = value,
        .C => @ptrCast(*u8u8u8, &self.bc).low = value,
        .B => @ptrCast(*u8u8u8, &self.bc).high = value,
        .BCU => @ptrCast(*u8u8u8, &self.bc).upper = value,
        .E => @ptrCast(*u8u8u8, &self.de).low = value,
        .D => @ptrCast(*u8u8u8, &self.de).high = value,
        .DEU => @ptrCast(*u8u8u8, &self.de).upper = value,
        .L => @ptrCast(*u8u8u8, &self.hl).low = value,
        .H => @ptrCast(*u8u8u8, &self.hl).high = value,
        .HLU => @ptrCast(*u8u8u8, &self.hl).upper = value,
        .IXL => @ptrCast(*u8u8u8, &self.ix).low = value,
        .IXH => @ptrCast(*u8u8u8, &self.ix).high = value,
        .IXU => @ptrCast(*u8u8u8, &self.ix).upper = value,
        .IYL => @ptrCast(*u8u8u8, &self.iy).low = value,
        .IYH => @ptrCast(*u8u8u8, &self.iy).high = value,
        .IYU => @ptrCast(*u8u8u8, &self.iy).upper = value,
        .R => self.r = std.math.rotl(u8, value, 1),
        .MB => @ptrCast(*u8u16, &self.mbi).upper = value,

        // 16-bit registers
        .AF => {
            self.set(.F, u8u8.from(value).low);
            self.set(.A, u8u8.from(value).high);
        },
        .BC => @ptrCast(*u8u16, &self.bc).short = value,
        .DE => @ptrCast(*u8u16, &self.de).short = value,
        .HL => @ptrCast(*u8u16, &self.hl).short = value,
        .IX => @ptrCast(*u8u16, &self.ix).short = value,
        .IY => @ptrCast(*u8u16, &self.iy).short = value,
        .SPS => self.sps = value,
        .I => @ptrCast(*u8u16, &self.mbi).short = value,

        // 24-bit registers
        .UBC => self.bc = value,
        .UDE => self.de = value,
        .UHL => self.hl = value,
        .UIX => self.ix = value,
        .UIY => self.iy = value,
        .SPL => self.spl = value,
        .PC => self.pc = value,
        .RPC => std.debug.todo("unimplemented"),
    }
}

pub fn setShadow(self: *Cpu, address: RegisterAddress, value: RegisterType(address)) void {
    switch (address) {
        // 1-bit flags
        .CarryFlag => @ptrCast(*Flags, &@ptrCast(*u8u8, &self.@"af'").low).cf = value,
        .SubtractFlag => @ptrCast(*Flags, &@ptrCast(*u8u8, &self.@"af'").low).nf = value,
        .ParityOverflowFlag => @ptrCast(*Flags, &@ptrCast(*u8u8, &self.@"af'").low).pv = value,
        .XFlag => @ptrCast(*Flags, &@ptrCast(*u8u8, &self.@"af'").low).xf = value,
        .HalfCarryFlag => @ptrCast(*Flags, &@ptrCast(*u8u8, &self.@"af'").low).hc = value,
        .YFlag => @ptrCast(*Flags, &@ptrCast(*u8u8, &self.@"af'").low).yf = value,
        .ZeroFlag => @ptrCast(*Flags, &@ptrCast(*u8u8, &self.@"af'").low).zf = value,
        .SignFlag => @ptrCast(*Flags, &@ptrCast(*u8u8, &self.@"af'").low).sf = value,

        // 8-bit registers
        .F => @ptrCast(*u8u8, &self.@"af'").low = value,
        .A => @ptrCast(*u8u8, &self.@"af'").high = value,
        .C => @ptrCast(*u8u8u8, &self.@"bc'").low = value,
        .B => @ptrCast(*u8u8u8, &self.@"bc'").high = value,
        .BCU => @ptrCast(*u8u8u8, &self.@"bc'").upper = value,
        .E => @ptrCast(*u8u8u8, &self.@"de'").low = value,
        .D => @ptrCast(*u8u8u8, &self.@"de'").high = value,
        .DEU => @ptrCast(*u8u8u8, &self.@"de'").upper = value,
        .L => @ptrCast(*u8u8u8, &self.@"hl'").low = value,
        .H => @ptrCast(*u8u8u8, &self.@"hl'").high = value,
        .HLU => @ptrCast(*u8u8u8, &self.@"hl'").upper = value,
        .IXL, .IXH, .IXU, .IYU, .IYL, .IYH, .R, .MB => unreachable,

        // 16-bit registers
        .AF => self.@"af'" = value,
        .BC => @ptrCast(*u8u16, &self.@"bc'").short = value,
        .DE => @ptrCast(*u8u16, &self.@"de'").short = value,
        .HL => @ptrCast(*u8u16, &self.@"hl'").short = value,
        .IX, .IY, .SPS, .I => unreachable,

        // 24-bit registers
        .UBC => self.@"bc'" = value,
        .UDE => self.@"de'" = value,
        .UHL => self.@"hl'" = value,
        .UIX, .UIY, .SPL, .PC, .RPC => unreachable,
    }
}

test "registers" {
    var cpu: Cpu = undefined;

    cpu.set(.AF, 0x0123);
    cpu.set(.UBC, 0x456789);
    cpu.set(.UDE, 0xABCDEF);
    cpu.set(.UHL, 0x02468A);
    cpu.setShadow(.AF, 0xCE13);
    cpu.setShadow(.UBC, 0x579ACE);
    cpu.setShadow(.UDE, 0x0369CF);
    cpu.setShadow(.UHL, 0x147AD2);
    cpu.set(.UIX, 0x58BE04);
    cpu.set(.UIY, 0x8C159D);
    cpu.set(.SPS, 0x26AE);
    cpu.set(.SPL, 0x37BF05);
    cpu.set(.PC, 0xAF16B2);
    cpu.set(.I, 0x7C38);
    cpu.set(.R, 0xD4);
    cpu.set(.MB, 0x9E);

    try std.testing.expectEqual(@as(u1, 1), cpu.get(.CarryFlag));
    try std.testing.expectEqual(@as(u1, 1), cpu.get(.SubtractFlag));
    try std.testing.expectEqual(@as(u1, 0), cpu.get(.ParityOverflowFlag));
    try std.testing.expectEqual(@as(u1, 0), cpu.get(.XFlag));
    try std.testing.expectEqual(@as(u1, 0), cpu.get(.HalfCarryFlag));
    try std.testing.expectEqual(@as(u1, 1), cpu.get(.YFlag));
    try std.testing.expectEqual(@as(u1, 0), cpu.get(.ZeroFlag));
    try std.testing.expectEqual(@as(u1, 0), cpu.get(.SignFlag));

    try std.testing.expectEqual(@as(u1, 1), cpu.getShadow(.CarryFlag));
    try std.testing.expectEqual(@as(u1, 1), cpu.getShadow(.SubtractFlag));
    try std.testing.expectEqual(@as(u1, 0), cpu.getShadow(.ParityOverflowFlag));
    try std.testing.expectEqual(@as(u1, 0), cpu.getShadow(.XFlag));
    try std.testing.expectEqual(@as(u1, 1), cpu.getShadow(.HalfCarryFlag));
    try std.testing.expectEqual(@as(u1, 0), cpu.getShadow(.YFlag));
    try std.testing.expectEqual(@as(u1, 0), cpu.getShadow(.ZeroFlag));
    try std.testing.expectEqual(@as(u1, 0), cpu.getShadow(.SignFlag));

    try std.testing.expectEqual(@as(u8, 0x23), cpu.get(.F));
    try std.testing.expectEqual(@as(u8, 0x01), cpu.get(.A));
    try std.testing.expectEqual(@as(u8, 0x89), cpu.get(.C));
    try std.testing.expectEqual(@as(u8, 0x67), cpu.get(.B));
    try std.testing.expectEqual(@as(u8, 0x45), cpu.get(.BCU));
    try std.testing.expectEqual(@as(u8, 0xEF), cpu.get(.E));
    try std.testing.expectEqual(@as(u8, 0xCD), cpu.get(.D));
    try std.testing.expectEqual(@as(u8, 0xAB), cpu.get(.DEU));
    try std.testing.expectEqual(@as(u8, 0x8A), cpu.get(.L));
    try std.testing.expectEqual(@as(u8, 0x46), cpu.get(.H));
    try std.testing.expectEqual(@as(u8, 0x02), cpu.get(.HLU));
    try std.testing.expectEqual(@as(u8, 0x04), cpu.get(.IXL));
    try std.testing.expectEqual(@as(u8, 0xBE), cpu.get(.IXH));
    try std.testing.expectEqual(@as(u8, 0x58), cpu.get(.IXU));
    try std.testing.expectEqual(@as(u8, 0x9D), cpu.get(.IYL));
    try std.testing.expectEqual(@as(u8, 0x15), cpu.get(.IYH));
    try std.testing.expectEqual(@as(u8, 0x8C), cpu.get(.IYU));
    try std.testing.expectEqual(@as(u8, 0xD4), cpu.get(.R));
    try std.testing.expectEqual(@as(u8, 0x9E), cpu.get(.MB));

    try std.testing.expectEqual(@as(u8, 0x13), cpu.getShadow(.F));
    try std.testing.expectEqual(@as(u8, 0xCE), cpu.getShadow(.A));
    try std.testing.expectEqual(@as(u8, 0xCE), cpu.getShadow(.C));
    try std.testing.expectEqual(@as(u8, 0x9A), cpu.getShadow(.B));
    try std.testing.expectEqual(@as(u8, 0x57), cpu.getShadow(.BCU));
    try std.testing.expectEqual(@as(u8, 0xCF), cpu.getShadow(.E));
    try std.testing.expectEqual(@as(u8, 0x69), cpu.getShadow(.D));
    try std.testing.expectEqual(@as(u8, 0x03), cpu.getShadow(.DEU));
    try std.testing.expectEqual(@as(u8, 0xD2), cpu.getShadow(.L));
    try std.testing.expectEqual(@as(u8, 0x7A), cpu.getShadow(.H));
    try std.testing.expectEqual(@as(u8, 0x14), cpu.getShadow(.HLU));

    try std.testing.expectEqual(@as(u16, 0x0123), cpu.get(.AF));
    try std.testing.expectEqual(@as(u16, 0x6789), cpu.get(.BC));
    try std.testing.expectEqual(@as(u16, 0xCDEF), cpu.get(.DE));
    try std.testing.expectEqual(@as(u16, 0x468A), cpu.get(.HL));
    try std.testing.expectEqual(@as(u16, 0xBE04), cpu.get(.IX));
    try std.testing.expectEqual(@as(u16, 0x159D), cpu.get(.IY));
    try std.testing.expectEqual(@as(u16, 0x26AE), cpu.get(.SPS));
    try std.testing.expectEqual(@as(u16, 0x7C38), cpu.get(.I));

    try std.testing.expectEqual(@as(u16, 0xCE13), cpu.getShadow(.AF));
    try std.testing.expectEqual(@as(u16, 0x9ACE), cpu.getShadow(.BC));
    try std.testing.expectEqual(@as(u16, 0x69CF), cpu.getShadow(.DE));
    try std.testing.expectEqual(@as(u16, 0x7AD2), cpu.getShadow(.HL));

    try std.testing.expectEqual(@as(u24, 0x456789), cpu.get(.UBC));
    try std.testing.expectEqual(@as(u24, 0xABCDEF), cpu.get(.UDE));
    try std.testing.expectEqual(@as(u24, 0x02468A), cpu.get(.UHL));
    try std.testing.expectEqual(@as(u24, 0x58BE04), cpu.get(.UIX));
    try std.testing.expectEqual(@as(u24, 0x8C159D), cpu.get(.UIY));
    try std.testing.expectEqual(@as(u24, 0x37BF05), cpu.get(.SPL));
    try std.testing.expectEqual(@as(u24, 0xAF16B2), cpu.get(.PC));

    try std.testing.expectEqual(@as(u24, 0x579ACE), cpu.getShadow(.UBC));
    try std.testing.expectEqual(@as(u24, 0x0369CF), cpu.getShadow(.UDE));
    try std.testing.expectEqual(@as(u24, 0x147AD2), cpu.getShadow(.UHL));
}

pub fn init(self: *Cpu) !void {
    self.* = Cpu{};
}
pub fn deinit(self: *Cpu) void {
    _ = self;
}
