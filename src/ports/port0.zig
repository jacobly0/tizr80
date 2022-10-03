const std = @import("std");

const Port0 = @This();
const util = @import("../util.zig");

port_access: u7,
gpio: [6]u8,
power: u4,
region: [9]u8,
stack_protector: [3]u8,

pub fn init(self: *Port0, _: std.mem.Allocator) std.mem.Allocator.Error!void {
    self.* = .{
        .port_access = 0,
        .gpio = .{0} ** self.gpio.len,
        .power = 0,
        .region = .{0} ** self.region.len,
        .stack_protector = .{0} ** self.stack_protector.len,
    };
}
pub fn deinit(self: *Port0, _: std.mem.Allocator) void {
    self.* = undefined;
}

pub fn read(self: *Port0, address: u12, cycles: *u64) u8 {
    cycles.* +%= 2;
    return switch (@truncate(u8, address)) {
        0x02 => 0,
        0x05 => self.port_access,
        0x07...0x0C => |addr| self.gpio[addr - 0x07],
        0x0D => util.bit.concat(.{ self.power, self.power }),
        0x0F => 0,
        0x1D...0x25 => |addr| self.region[addr - 0x1D],
        0x3A...0x3C => |addr| self.stack_protector[addr - 0x3A],
        else => std.debug.todo("Port0 port read unimplemented"),
    };
}
pub fn write(self: *Port0, address: u12, value: u8, cycles: *u64) void {
    cycles.* +%= 2;
    switch (@truncate(u8, address)) {
        0x00, 0x01, 0x06 => {},
        0x05 => self.port_access = util.bit.extract(value, u7, 0),
        0x07...0x0C => |addr| self.gpio[addr - 0x07] = value,
        0x0D => self.power = util.bit.extract(value, u4, 0),
        0x1D...0x25 => |addr| self.region[addr - 0x1D] = value,
        0x3A...0x3C => |addr| self.stack_protector[addr - 0x3A] = value,
        else => std.debug.todo("Port0 port write unimplemented"),
    }
}

test {
    _ = read;
    _ = write;
}
