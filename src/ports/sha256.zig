const std = @import("std");

const Ports = @import("../ports.zig");
const Sha256 = @This();

handler: Ports.Handler,
data: [64]u8,
state: [32]u8,

pub fn create(allocator: std.mem.Allocator) !*Ports.Handler {
    const self = try allocator.create(Sha256);
    errdefer allocator.destroy(self);

    self.* = .{
        .handler = .{ .read = read, .write = write, .destroy = destroy },
        .data = [_]u8{0} ** self.data.len,
        .state = [_]u8{0} ** self.state.len,
    };
    return &self.handler;
}
fn destroy(handler: *Ports.Handler, allocator: std.mem.Allocator) void {
    const self = @fieldParentPtr(Sha256, "handler", handler);
    allocator.destroy(self);
}

fn read(handler: *Ports.Handler, address: u12, cycles: *u64) u8 {
    const self = @fieldParentPtr(Sha256, "handler", handler);
    const addr = @truncate(u8, address);
    cycles.* +%= 2;
    return switch (addr) {
        0x10...0x4F => self.data[addr - 0x10],
        0x60...0x7F => self.state[addr - 0x60],
        else => std.debug.todo("Sha256 port unimplemented"),
    };
}
fn write(handler: *Ports.Handler, address: u12, value: u8, cycles: *u64) void {
    const self = @fieldParentPtr(Sha256, "handler", handler);
    const addr = @truncate(u8, address);
    cycles.* +%= 2;
    switch (addr) {
        0x10...0x4F => self.data[addr - 0x10] = value,
        0x60...0x7F => self.state[addr - 0x60] = value,
        else => std.debug.todo("Sha256 port unimplemented"),
    }
}
