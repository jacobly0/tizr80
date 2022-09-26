const std = @import("std");

const Lcd = @This();
const Ports = @import("../ports.zig");
const TiZr80 = @import("../tizr80.zig");

handler: Ports.Handler,
base: [8]u8,
control: [4]u8,
cursor: [0x400]u8,

pub fn create(allocator: std.mem.Allocator) !*Ports.Handler {
    const self = try allocator.create(Lcd);
    errdefer allocator.destroy(self);

    self.* = .{
        .handler = .{ .read = read, .write = write, .destroy = destroy },
        .base = .{0} ** self.base.len,
        .control = .{0} ** self.control.len,
        .cursor = .{0} ** self.cursor.len,
    };
    return &self.handler;
}
fn destroy(handler: *Ports.Handler, allocator: std.mem.Allocator) void {
    const self = @fieldParentPtr(Lcd, "handler", handler);
    allocator.destroy(self);
}

fn read(_: *TiZr80, handler: *Ports.Handler, address: u12, cycles: *u64) u8 {
    const self = @fieldParentPtr(Lcd, "handler", handler);
    cycles.* +%= 3;
    return switch (@truncate(u12, address)) {
        0x010...0x017 => |addr| self.base[addr - 0x010],
        0x018...0x01B => |addr| self.control[addr - 0x018],
        0x800...0xBFF => |addr| self.cursor[addr - 0x800],
        else => std.debug.todo("Lcd port read unimplemented"),
    };
}
fn write(_: *TiZr80, handler: *Ports.Handler, address: u12, value: u8, cycles: *u64) void {
    const self = @fieldParentPtr(Lcd, "handler", handler);
    cycles.* +%= 2;
    switch (@truncate(u12, address)) {
        0x000...0x00F => {},
        0x010...0x017 => |addr| self.base[addr - 0x010] = value,
        0x018...0x01B => |addr| self.control[addr - 0x018] = value,
        0x800...0xBFF => |addr| self.cursor[addr - 0x800] = value,
        else => std.debug.todo("Lcd port write unimplemented"),
    }
}
