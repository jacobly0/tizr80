const std = @import("std");

const Interrupt = @This();
const util = @import("../util.zig");

active: u22,
enable: u22,
latch: u22,

pub fn init(self: *Interrupt, _: std.mem.Allocator) std.mem.Allocator.Error!void {
    self.* = .{
        .active = 0,
        .enable = 0,
        .latch = 0,
    };
}
pub fn deinit(self: *Interrupt, _: std.mem.Allocator) void {
    self.* = undefined;
}

pub fn read(self: *Interrupt, address: u12, cycles: *u64) u8 {
    cycles.* +%= 3;
    return switch (@truncate(u8, address)) {
        0x04 => util.bit.extract(self.enable, u8, 0),
        0x05 => util.bit.extract(self.enable, u8, 8),
        0x06 => util.bit.extract(self.enable, u6, 16),
        0x07 => 0,
        0x0C => util.bit.extract(self.latch, u8, 0),
        0x0D => util.bit.extract(self.latch, u8, 8),
        0x0E => util.bit.extract(self.latch, u6, 16),
        0x0F => 0,
        else => std.debug.todo("Interrupt port read unimplemented"),
    };
}
pub fn write(self: *Interrupt, address: u12, value: u8, cycles: *u64) void {
    cycles.* +%= 3;
    switch (@truncate(u8, address)) {
        0x04 => util.bit.insert(&self.enable, value, 0),
        0x05 => util.bit.insert(&self.enable, value, 8),
        0x06 => util.bit.insert(&self.enable, @truncate(u6, value), 16),
        0x07 => {},
        0x0C => util.bit.insert(&self.latch, value, 0),
        0x0D => util.bit.insert(&self.latch, value, 8),
        0x0E => util.bit.insert(&self.latch, @truncate(u6, value), 16),
        0x0F => {},
        else => std.debug.todo("Interrupt port write unimplemented"),
    }
}
