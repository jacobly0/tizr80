const std = @import("std");

const Ports = @import("../ports.zig");
const Port0 = @This();

handler: Ports.Handler,
region: [6]u8,

pub fn create(allocator: std.mem.Allocator) !*Ports.Handler {
    const self = try allocator.create(Port0);
    errdefer allocator.destroy(self);

    self.* = .{
        .handler = .{ .read = read, .write = write, .destroy = destroy },
        .region = [_]u8{0} ** self.region.len,
    };
    return &self.handler;
}
fn destroy(handler: *Ports.Handler, allocator: std.mem.Allocator) void {
    const self = @fieldParentPtr(Port0, "handler", handler);
    allocator.destroy(self);
}

fn read(handler: *Ports.Handler, address: u12, cycles: *u64) u8 {
    const self = @fieldParentPtr(Port0, "handler", handler);
    const addr = @truncate(u8, address);
    cycles.* +%= 2;
    return switch (addr) {
        0x20...0x25 => return self.region[addr - 0x20],
        else => std.debug.todo("Port0 port unimplemented"),
    };
}
fn write(handler: *Ports.Handler, address: u12, value: u8, cycles: *u64) void {
    const self = @fieldParentPtr(Port0, "handler", handler);
    const addr = @truncate(u8, address);
    cycles.* +%= 2;
    switch (addr) {
        0x20...0x25 => self.region[addr - 0x20] = value,
        else => std.debug.todo("Port0 port unimplemented"),
    }
}
