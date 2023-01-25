const std = @import("std");

const Usb = @This();
const util = @import("../util.zig");

pub fn init(self: *Usb, _: std.mem.Allocator) std.mem.Allocator.Error!void {
    self.* = .{};
}
pub fn deinit(self: *Usb, _: std.mem.Allocator) void {
    self.* = undefined;
}

pub fn read(_: *Usb, address: u12, cycles: *u64) u8 {
    cycles.* +%= 4;
    return switch (@truncate(u9, address)) {
        0x010 => 0,
        0x014...0x015 => 0,
        0x030 => 0,
        0x040 => 0,
        0x080...0x082 => 0,
        0x088...0x089 => 0,
        0x0C4 => 0,
        0x100 => 0,
        0x114 => 0,
        0x13C...0x13D => 0,
        0x14C...0x14D => 0,
        0x1CB => 0,
        else => {
            std.debug.print("read 0x{X}\n", .{address});
            util.todo("Usb port read unimplemented");
        },
    };
}
pub fn write(_: *Usb, address: u12, _: u8, cycles: *u64) void {
    cycles.* +%= 4;
    switch (@truncate(u9, address)) {
        0x010 => {},
        0x014...0x015 => {},
        0x040 => {},
        0x080...0x082 => {},
        0x088...0x089 => {},
        0x0C4 => {},
        0x100 => {},
        0x114 => {},
        0x13C...0x13D => {},
        0x144 => {},
        0x14C...0x14D => {},
        0x1CB => {},
        else => {
            std.debug.print("write 0x{X}\n", .{address});
            util.todo("Usb port write unimplemented");
        },
    }
}
