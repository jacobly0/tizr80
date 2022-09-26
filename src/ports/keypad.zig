const std = @import("std");

const Keypad = @This();
const Ports = @import("../ports.zig");
const TiZr80 = @import("../tizr80.zig");
const util = @import("../util.zig");

pub const Key = packed struct(u8) {
    column: u4,
    row: u4,
};

handler: Ports.Handler,
keys: [16]u16,
events: [16][2]std.atomic.Atomic(u16),

pub fn create(allocator: std.mem.Allocator) !*Ports.Handler {
    const self = try allocator.create(Keypad);
    errdefer allocator.destroy(self);

    self.* = .{
        .handler = .{ .read = read, .write = write, .destroy = destroy },
        .keys = .{0} ** self.keys.len,
        .events = .{.{.{ .value = 0 }} ** self.events[0].len} ** self.events.len,
    };
    return &self.handler;
}
fn destroy(handler: *Ports.Handler, allocator: std.mem.Allocator) void {
    const self = @fieldParentPtr(Keypad, "handler", handler);
    allocator.destroy(self);
}

fn read(_: *TiZr80, handler: *Ports.Handler, address: u12, cycles: *u64) u8 {
    const self = @fieldParentPtr(Keypad, "handler", handler);
    _ = .{self};
    cycles.* +%= 3;
    return switch (@truncate(u7, address)) {
        else => std.debug.todo("Keypad port read unimplemented"),
    };
}
fn write(_: *TiZr80, handler: *Ports.Handler, address: u12, value: u8, cycles: *u64) void {
    const self = @fieldParentPtr(Keypad, "handler", handler);
    _ = .{ self, value };
    cycles.* +%= 3;
    switch (@truncate(u7, address)) {
        0x0...0x5 => {},
        else => std.debug.todo("Keypad port write unimplemented"),
    }
}

pub fn getKey(self: *const Keypad, key: Key) u1 {
    return util.bit.extract(self.keys[key.row], u1, key.column);
}
pub fn setKey(self: *Keypad, key: Key, value: u1) void {
    _ = self.events[key.row][value].fetchOr(@as(u16, 1) << key.column, .SeqCst);
}
