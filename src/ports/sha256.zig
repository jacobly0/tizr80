const std = @import("std");

const Ports = @import("../ports.zig");
const Sha256 = @This();
const TiZr80 = @import("../tizr80.zig");

handler: Ports.Handler,
data: [64]u8,
state: [32]u8,

pub fn create(allocator: std.mem.Allocator) !*Ports.Handler {
    const self = try allocator.create(Sha256);
    errdefer allocator.destroy(self);

    self.* = .{
        .handler = .{ .read = read, .write = write, .destroy = destroy },
        .data = .{0} ** self.data.len,
        .state = .{0} ** self.state.len,
    };
    return &self.handler;
}
fn destroy(handler: *Ports.Handler, allocator: std.mem.Allocator) void {
    const self = @fieldParentPtr(Sha256, "handler", handler);
    allocator.destroy(self);
}

fn read(_: *TiZr80, handler: *Ports.Handler, address: u12, cycles: *u64) u8 {
    const self = @fieldParentPtr(Sha256, "handler", handler);
    cycles.* +%= 2;
    return switch (@truncate(u8, address)) {
        0x10...0x4F => |addr| self.data[addr - 0x10],
        0x60...0x7F => |addr| self.state[addr - 0x60],
        else => std.debug.todo("Sha256 port read unimplemented"),
    };
}
fn write(_: *TiZr80, handler: *Ports.Handler, address: u12, value: u8, cycles: *u64) void {
    const self = @fieldParentPtr(Sha256, "handler", handler);
    cycles.* +%= 2;
    switch (@truncate(u8, address)) {
        0x10...0x4F => |addr| self.data[addr - 0x10] = value,
        0x60...0x7F => |addr| self.state[addr - 0x60] = value,
        else => std.debug.todo("Sha256 port write unimplemented"),
    }
}
