const std = @import("std");

const Keypad = @This();
const util = @import("util.zig");

pub const Key = packed struct(u8) {
    column: u4,
    row: u4,
};

keys: [16]u16,
events: [16][2]std.atomic.Atomic(u16),

pub fn init(self: *Keypad) !void {
    self.* = std.mem.zeroes(Keypad);
}
pub fn deinit(self: *Keypad) void {
    self.* = undefined;
}

pub fn getKey(self: *const Keypad, key: Key) u1 {
    return util.bit.extract(self.keys[key.row], u1, key.column);
}
pub fn setKey(self: *Keypad, key: Key, value: u1) void {
    _ = self.events[key.row][value].fetchOr(@as(u16, 1) << key.column, .SeqCst);
}
