const std = @import("std");

const Interrupt = @This();
const Ports = @import("../ports.zig");
const TiZr80 = @import("../tizr80.zig");
const util = @import("../util.zig");

handler: Ports.Handler,
enable: u22,
latch: u22,

pub fn create(allocator: std.mem.Allocator) !*Ports.Handler {
    const self = try allocator.create(Interrupt);
    errdefer allocator.destroy(self);

    self.* = .{
        .handler = .{ .read = read, .write = write, .destroy = destroy },
        .enable = 0,
        .latch = 0,
    };
    return &self.handler;
}
fn destroy(handler: *Ports.Handler, allocator: std.mem.Allocator) void {
    const self = @fieldParentPtr(Interrupt, "handler", handler);
    allocator.destroy(self);
}

fn read(_: *TiZr80, handler: *Ports.Handler, address: u12, cycles: *u64) u8 {
    const self = @fieldParentPtr(Interrupt, "handler", handler);
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
fn write(_: *TiZr80, handler: *Ports.Handler, address: u12, value: u8, cycles: *u64) void {
    const self = @fieldParentPtr(Interrupt, "handler", handler);
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
