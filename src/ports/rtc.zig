const std = @import("std");

const Rtc = @This();
const util = @import("../util.zig");

pub fn init(self: *Rtc, _: std.mem.Allocator) std.mem.Allocator.Error!void {
    self.* = .{};
}
pub fn deinit(self: *Rtc, _: std.mem.Allocator) void {
    self.* = undefined;
}

pub fn read(_: *Rtc, address: u12, cycles: *u64) u8 {
    cycles.* +%= 3;
    return switch (@truncate(u7, address)) {
        0x00 => 0,
        0x04 => 0,
        0x08 => 0,
        0x20 => 0,
        0x40...0x41 => 0,
        else => {
            std.debug.print("read 0x{X}\n", .{address});
            util.todo("Rtc port read unimplemented");
        },
    };
}
pub fn write(_: *Rtc, address: u12, _: u8, cycles: *u64) void {
    cycles.* +%= 3;
    switch (@truncate(u7, address)) {
        0x20 => {},
        0x24 => {},
        0x28 => {},
        0x2C => {},
        0x30...0x31 => {},
        0x34 => {},
        else => {
            std.debug.print("write 0x{X}\n", .{address});
            util.todo("Rtc port write unimplemented");
        },
    }
}
