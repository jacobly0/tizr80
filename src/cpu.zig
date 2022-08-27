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
        // 1-bit flags
        .CarryFlag => self.cf,
        .ParityOverflowFlag => self.pv,
        .XFlag => self.xf,
        .HalfCarryFlag => self.hc,
        .YFlag => self.yf,
        .ZeroFlag => self.zf,
        .SignFlag => self.sf,

        // 8-bit registers
        .F => std.debug.todo("unimplemented"),
        .A => std.debug.todo("unimplemented"),
        .C => std.debug.todo("unimplemented"),
        .B => std.debug.todo("unimplemented"),
        .BCU => std.debug.todo("unimplemented"),
        .E => std.debug.todo("unimplemented"),
        .D => std.debug.todo("unimplemented"),
        .DEU => std.debug.todo("unimplemented"),
        .L => std.debug.todo("unimplemented"),
        .H => std.debug.todo("unimplemented"),
        .HLU => std.debug.todo("unimplemented"),
        .IXL => std.debug.todo("unimplemented"),
        .IXH => std.debug.todo("unimplemented"),
        .IXU => std.debug.todo("unimplemented"),
        .IYL => std.debug.todo("unimplemented"),
        .IYH => std.debug.todo("unimplemented"),
        .IYU => std.debug.todo("unimplemented"),
        .R => std.debug.todo("unimplemented"),
        .MB => std.debug.todo("unimplemented"),

        // 16-bit registers
        .AF => std.debug.todo("unimplemented"),
        .BC => std.debug.todo("unimplemented"),
        .DE => std.debug.todo("unimplemented"),
        .HL => std.debug.todo("unimplemented"),
        .IX => std.debug.todo("unimplemented"),
        .IY => std.debug.todo("unimplemented"),
        .SPS => std.debug.todo("unimplemented"),
        .I => std.debug.todo("unimplemented"),

        // 24-bit registers
        .UBC => std.debug.todo("unimplemented"),
        .UDE => std.debug.todo("unimplemented"),
        .UHL => std.debug.todo("unimplemented"),
        .UIX => std.debug.todo("unimplemented"),
        .UIY => std.debug.todo("unimplemented"),
        .SPL => std.debug.todo("unimplemented"),
        .PC => std.debug.todo("unimplemented"),
        .RPC => std.debug.todo("unimplemented"),
    };
}

pub fn init(self: *Cpu) !void {
    self.* = Cpu{};
}
pub fn deinit(self: *Cpu) void {
    _ = self;
}
