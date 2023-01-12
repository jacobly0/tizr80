const std = @import("std");

const Backlight = @This();
const util = @import("../util.zig");

pub fn init(self: *Backlight, _: std.mem.Allocator) std.mem.Allocator.Error!void {
    self.* = .{};
}
pub fn deinit(self: *Backlight, _: std.mem.Allocator) void {
    self.* = undefined;
}

pub fn read(_: *Backlight, address: u12, cycles: *u64) u8 {
    cycles.* +%= 3;
    return switch (@truncate(u12, address)) {
        else => util.todo("Backlight port read unimplemented"),
    };
}
pub fn write(_: *Backlight, address: u12, _: u8, cycles: *u64) void {
    cycles.* +%= 3;
    switch (@truncate(u12, address)) {
        0x020...0x026 => {},
        else => util.todo("Backlight port write unimplemented"),
    }
}
