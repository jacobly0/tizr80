const std = @import("std");

const Keypad = @This();

const Key = packed struct(u8) {
    column: u4,
    row: u4,

    const Backing = @typeInfo(@This()).Struct.backing_integer.?;
    pub fn from(value: Backing) @This() {
        return @bitCast(@This(), value);
    }
    pub fn all(self: @This()) Backing {
        return @bitCast(Backing, self);
    }
};

keys: [16]u16,
events: [16][2]std.atomic.Atomic(u16),
gpio_enable: u32,

pub fn init(self: *Keypad) !void {
    self.* = std.mem.zeroes(Keypad);
}
pub fn deinit(self: *Keypad) void {
    _ = self;
}

pub fn getKey(self: *const Keypad, index: u8) u1 {
    const key = Key.from(index);
    return @truncate(u1, self.keys[key.row] >> key.column);
}
pub fn setKey(self: *Keypad, index: u8, value: u1) void {
    const key = Key.from(index);
    _ = self.events[key.row][value].fetchOr(@as(u16, 1) << key.column, .SeqCst);
}

pub fn getGpio(self: *const Keypad, index: u5) u1 {
    return @truncate(u1, self.gpio_enable >> index);
}
pub fn setGpio(self: *Keypad, index: u5, value: u1) void {
    self.gpio_enable = self.gpio_enable & ~(@as(u32, 1) << index) | @as(u32, value) << index;
}
