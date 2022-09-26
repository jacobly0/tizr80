const std = @import("std");

const Flash = @This();
const Ports = @import("../ports.zig");
const TiZr80 = @import("../tizr80.zig");

handler: Ports.Handler,

pub fn create(allocator: std.mem.Allocator) !*Ports.Handler {
    const self = try allocator.create(Flash);
    errdefer allocator.destroy(self);

    self.* = .{
        .handler = .{ .read = read, .write = write, .destroy = destroy },
    };
    return &self.handler;
}
fn destroy(handler: *Ports.Handler, allocator: std.mem.Allocator) void {
    const self = @fieldParentPtr(Flash, "handler", handler);
    allocator.destroy(self);
}

fn read(core: *TiZr80, handler: *Ports.Handler, address: u12, cycles: *u64) u8 {
    const self = @fieldParentPtr(Flash, "handler", handler);
    _ = self;
    cycles.* +%= 2;
    return switch (@truncate(u8, address)) {
        0x05 => @intCast(u8, core.mem.flash_wait_states - 6),
        else => std.debug.todo("Flash port read unimplemented"),
    };
}
fn write(core: *TiZr80, handler: *Ports.Handler, address: u12, value: u8, cycles: *u64) void {
    const self = @fieldParentPtr(Flash, "handler", handler);
    _ = self;
    cycles.* +%= 2;
    switch (@truncate(u8, address)) {
        0x02 => {},
        0x05 => core.mem.flash_wait_states = 6 + value,
        else => std.debug.todo("Flash port write unimplemented"),
    }
}
