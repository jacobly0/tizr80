const std = @import("std");

const Keypad = @This();
const util = @import("../util.zig");

pub const Key = packed struct(u8) {
    column: u4,
    row: u4,
};

keys: [16]u16,
events: [16][2]std.atomic.Atomic(u16),

pub fn init(self: *Keypad, _: std.mem.Allocator) std.mem.Allocator.Error!void {
    self.* = .{
        .keys = .{0} ** self.keys.len,
        .events = .{.{.{ .value = 0 }} ** self.events[0].len} ** self.events.len,
    };
}
pub fn deinit(self: *Keypad, _: std.mem.Allocator) void {
    self.* = undefined;
}

pub fn read(_: *Keypad, address: u12, cycles: *u64) u8 {
    cycles.* +%= 3;
    return switch (@truncate(u7, address)) {
        else => util.todo("Keypad port read unimplemented"),
    };
}
pub fn write(_: *Keypad, address: u12, _: u8, cycles: *u64) void {
    cycles.* +%= 3;
    switch (@truncate(u7, address)) {
        0x0...0x5 => {},
        else => util.todo("Keypad port write unimplemented"),
    }
}

pub fn getKey(self: *const Keypad, key: Key) u1 {
    return util.bit.extract(self.keys[key.row], u1, key.column);
}
pub fn setKey(self: *Keypad, key: Key, value: u1) void {
    _ = self.events[key.row][value].fetchOr(@as(u16, 1) << key.column, .SeqCst);
}
