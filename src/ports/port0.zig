const std = @import("std");

const Port0 = @This();
const Ports = @import("../ports.zig");
const TiZr80 = @import("../tizr80.zig");
const util = @import("../util.zig");

handler: Ports.Handler,
port_access: u7,
gpio: [6]u8,
power: u4,
region: [9]u8,
stack_protector: [3]u8,

pub fn create(allocator: std.mem.Allocator) !*Ports.Handler {
    const self = try allocator.create(Port0);
    errdefer allocator.destroy(self);

    self.* = .{
        .handler = .{ .read = read, .write = write, .destroy = destroy },
        .port_access = 0,
        .gpio = .{0} ** self.gpio.len,
        .power = 0,
        .region = .{0} ** self.region.len,
        .stack_protector = .{0} ** self.stack_protector.len,
    };
    return &self.handler;
}
fn destroy(handler: *Ports.Handler, allocator: std.mem.Allocator) void {
    const self = @fieldParentPtr(Port0, "handler", handler);
    allocator.destroy(self);
}

fn read(_: *TiZr80, handler: *Ports.Handler, address: u12, cycles: *u64) u8 {
    const self = @fieldParentPtr(Port0, "handler", handler);
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
fn write(_: *TiZr80, handler: *Ports.Handler, address: u12, value: u8, cycles: *u64) void {
    const self = @fieldParentPtr(Port0, "handler", handler);
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
