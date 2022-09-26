const std = @import("std");

const Ports = @import("../ports.zig");
const Spi = @This();
const TiZr80 = @import("../tizr80.zig");

handler: Ports.Handler,

pub fn create(allocator: std.mem.Allocator) !*Ports.Handler {
    const self = try allocator.create(Spi);
    errdefer allocator.destroy(self);

    self.* = .{
        .handler = .{ .read = read, .write = write, .destroy = destroy },
    };
    return &self.handler;
}
fn destroy(handler: *Ports.Handler, allocator: std.mem.Allocator) void {
    const self = @fieldParentPtr(Spi, "handler", handler);
    allocator.destroy(self);
}

fn read(_: *TiZr80, handler: *Ports.Handler, address: u12, cycles: *u64) u8 {
    const self = @fieldParentPtr(Spi, "handler", handler);
    _ = self;
    cycles.* +%= 3;
    return switch (@truncate(u7, address)) {
        0x0C...0x0D, 0x18 => 0,
        else => std.debug.todo("Spi port read unimplemented"),
    };
}
fn write(_: *TiZr80, handler: *Ports.Handler, address: u12, value: u8, cycles: *u64) void {
    const self = @fieldParentPtr(Spi, "handler", handler);
    _ = .{ self, value };
    cycles.* +%= 3;
    switch (@truncate(u7, address)) {
        0x00...0x09, 0x18 => {},
        else => std.debug.todo("Spi port write unimplemented"),
    }
}
