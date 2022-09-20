const std = @import("std");

const Ports = @import("../ports.zig");
const Lcd = @This();

handler: Ports.Handler,
cursor: [0x400]u8,

pub fn create(allocator: std.mem.Allocator) !*Ports.Handler {
    const self = try allocator.create(Lcd);
    errdefer allocator.destroy(self);

    self.* = .{
        .handler = .{ .read = read, .write = write, .destroy = destroy },
        .cursor = [_]u8{0} ** self.cursor.len,
    };
    return &self.handler;
}
fn destroy(handler: *Ports.Handler, allocator: std.mem.Allocator) void {
    const self = @fieldParentPtr(Lcd, "handler", handler);
    allocator.destroy(self);
}

fn read(handler: *Ports.Handler, address: u12, cycles: *u64) u8 {
    const self = @fieldParentPtr(Lcd, "handler", handler);
    const addr = @truncate(u12, address);
    cycles.* +%= 3;
    const value = switch (addr) {
        0x800...0xBFF => return self.cursor[addr - 0x800],
        else => std.debug.todo("Lcd port unimplemented"),
    };
    return value;
}
fn write(handler: *Ports.Handler, address: u12, value: u8, cycles: *u64) void {
    const self = @fieldParentPtr(Lcd, "handler", handler);
    const addr = @truncate(u12, address);
    cycles.* +%= 2;
    switch (addr) {
        0x800...0xBFF => self.cursor[addr - 0x800] = value,
        else => std.debug.todo("Lcd port unimplemented"),
    }
}
