const std = @import("std");

const Lcd = @This();
const util = @import("../util.zig");

base: [8]u8,
control: [4]u8,
cursor: [0x400]u8,

pub fn init(self: *Lcd, _: std.mem.Allocator) std.mem.Allocator.Error!void {
    self.* = .{
        .base = .{0} ** self.base.len,
        .control = .{0} ** self.control.len,
        .cursor = .{0} ** self.cursor.len,
    };
}
pub fn deinit(self: *Lcd, _: std.mem.Allocator) void {
    self.* = undefined;
}

pub fn read(self: *Lcd, address: u12, cycles: *u64) u8 {
    cycles.* +%= 3;
    return switch (@truncate(u12, address)) {
        0x010...0x017 => |addr| self.base[addr - 0x010],
        0x018...0x01B => |addr| self.control[addr - 0x018],
        0x800...0xBFF => |addr| self.cursor[addr - 0x800],
        else => util.todo("Lcd port read unimplemented"),
    };
}
pub fn write(self: *Lcd, address: u12, value: u8, cycles: *u64) void {
    cycles.* +%= 2;
    switch (@truncate(u12, address)) {
        0x000...0x00F => {},
        0x010...0x017 => |addr| self.base[addr - 0x010] = value,
        0x018...0x01B => |addr| self.control[addr - 0x018] = value,
        0x800...0xBFF => |addr| self.cursor[addr - 0x800] = value,
        else => util.todo("Lcd port write unimplemented"),
    }
}
