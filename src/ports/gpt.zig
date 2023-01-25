const std = @import("std");

const Gpt = @This();
const util = @import("../util.zig");

pub fn init(self: *Gpt, _: std.mem.Allocator) std.mem.Allocator.Error!void {
    self.* = .{};
}
pub fn deinit(self: *Gpt, _: std.mem.Allocator) void {
    self.* = undefined;
}

pub fn read(_: *Gpt, address: u12, cycles: *u64) u8 {
    cycles.* +%= 3;
    return switch (@truncate(u8, address)) {
        0x30 => 0,
        else => {
            std.debug.print("read 0x{X}\n", .{address});
            util.todo("Gpt port read unimplemented");
        },
    };
}
pub fn write(_: *Gpt, address: u12, _: u8, cycles: *u64) void {
    cycles.* +%= 3;
    switch (@truncate(u8, address)) {
        0x20...0x30 => {},
        else => {
            std.debug.print("write 0x{X}\n", .{address});
            util.todo("Gpt port write unimplemented");
        },
    }
}
