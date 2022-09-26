const std = @import("std");

const Backlight = @This();
const Ports = @import("../ports.zig");
const TiZr80 = @import("../tizr80.zig");

handler: Ports.Handler,

pub fn create(allocator: std.mem.Allocator) !*Ports.Handler {
    const self = try allocator.create(Backlight);
    errdefer allocator.destroy(self);

    self.* = .{
        .handler = .{ .read = read, .write = write, .destroy = destroy },
    };
    return &self.handler;
}
fn destroy(handler: *Ports.Handler, allocator: std.mem.Allocator) void {
    const self = @fieldParentPtr(Backlight, "handler", handler);
    allocator.destroy(self);
}

fn read(_: *TiZr80, handler: *Ports.Handler, address: u12, cycles: *u64) u8 {
    const self = @fieldParentPtr(Backlight, "handler", handler);
    _ = self;
    cycles.* +%= 3;
    return switch (@truncate(u12, address)) {
        else => std.debug.todo("Backlight port read unimplemented"),
    };
}
fn write(_: *TiZr80, handler: *Ports.Handler, address: u12, value: u8, cycles: *u64) void {
    const self = @fieldParentPtr(Backlight, "handler", handler);
    _ = .{ self, value };
    cycles.* +%= 3;
    switch (@truncate(u12, address)) {
        0x020...0x026 => {},
        else => std.debug.todo("Backlight port write unimplemented"),
    }
}
