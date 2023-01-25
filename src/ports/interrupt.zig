const std = @import("std");

const Interrupt = @This();
const util = @import("../util.zig");

active: u22,
enable: u22,
latch: u22,
invert: u22,

pub fn init(self: *Interrupt, _: std.mem.Allocator) std.mem.Allocator.Error!void {
    self.* = .{
        .active = 0,
        .enable = 0,
        .latch = 0,
        .invert = 0,
    };
}
pub fn deinit(self: *Interrupt, _: std.mem.Allocator) void {
    self.* = undefined;
}

pub fn read(self: *Interrupt, address: u12, cycles: *u64) u8 {
    cycles.* +%= 3;
    return switch (@truncate(u8, address)) {
        0x00 => util.bit.extract(self.active, u8, 0),
        0x01 => util.bit.extract(self.active, u8, 8),
        0x02 => util.bit.extract(self.active, u6, 16),
        0x03 => 0,
        0x04 => util.bit.extract(self.enable, u8, 0),
        0x05 => util.bit.extract(self.enable, u8, 8),
        0x06 => util.bit.extract(self.enable, u6, 16),
        0x07 => 0,
        0x0C => util.bit.extract(self.latch, u8, 0),
        0x0D => util.bit.extract(self.latch, u8, 8),
        0x0E => util.bit.extract(self.latch, u6, 16),
        0x0F => 0,
        0x10 => util.bit.extract(self.invert, u8, 0),
        0x11 => util.bit.extract(self.invert, u8, 8),
        0x12 => util.bit.extract(self.invert, u6, 16),
        0x13 => 0,
        else => {
            std.debug.print("read 0x{X}\n", .{address});
            util.todo("Interrupt port read unimplemented");
        },
    };
}
pub fn write(self: *Interrupt, address: u12, value: u8, cycles: *u64) void {
    cycles.* +%= 3;
    switch (@truncate(u8, address)) {
        0x00 => util.bit.insert(&self.active, value, 0),
        0x01 => util.bit.insert(&self.active, value, 8),
        0x02 => util.bit.insert(&self.active, @truncate(u6, value), 16),
        0x03 => {},
        0x04 => util.bit.insert(&self.enable, value, 0),
        0x05 => util.bit.insert(&self.enable, value, 8),
        0x06 => util.bit.insert(&self.enable, @truncate(u6, value), 16),
        0x07 => {},
        0x08...0x0B => {},
        0x0C => util.bit.insert(&self.latch, value, 0),
        0x0D => util.bit.insert(&self.latch, value, 8),
        0x0E => util.bit.insert(&self.latch, @truncate(u6, value), 16),
        0x0F => {},
        0x10 => util.bit.insert(&self.invert, value, 0),
        0x11 => util.bit.insert(&self.invert, value, 8),
        0x12 => util.bit.insert(&self.invert, @truncate(u6, value), 16),
        0x13 => {},
        else => {
            std.debug.print("write 0x{X}\n", .{address});
            util.todo("Interrupt port write unimplemented");
        },
    }
}
