const std = @import("std");

const Cpu = @This();

pub const RegisterAddress = enum {
    // 1-bit flags
    CarryFlag,
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

sps: u16 = 0,
spl: u24 = 0,
pc: u24 = 0,
r: u8 = 0,
mbi: u24 = 0,

fn RegisterType(address: RegisterAddress) type {
    return switch (address) {
        .CarryFlag, .ParityOverflowFlag, .XFlag, .HalfCarryFlag, .YFlag, .ZeroFlag, .SignFlag => bool,
        .F, .A, .C, .B, .BCU, .E, .D, .DEU, .L, .H, .HLU, .IXL, .IXH, .IXU, .IYL, .IYH, .IYU, .R, .MB => u8,
        .AF, .BC, .DE, .HL, .IX, .IY, .SPS, .I => u16,
        .UBC, .UDE, .UHL, .UIX, .UIY, .SPL, .PC, .RPC => u24,
    };
}

pub fn get(self: *Cpu, address: RegisterAddress) RegisterType(address) {
    return switch (address) {
        .CarryFlag => self.cf,
    };
}

pub fn init(self: *Cpu) !void {
    self.* = Cpu{};
}
pub fn deinit(self: *Cpu) void {
    _ = self;
}
