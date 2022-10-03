const std = @import("std");

const Spi = @This();

pub fn init(self: *Spi, _: std.mem.Allocator) std.mem.Allocator.Error!void {
    self.* = .{};
}
pub fn deinit(self: *Spi, _: std.mem.Allocator) void {
    self.* = undefined;
}

pub fn read(_: *Spi, address: u12, cycles: *u64) u8 {
    cycles.* +%= 3;
    return switch (@truncate(u7, address)) {
        0x0C...0x0D, 0x18 => 0,
        else => std.debug.todo("Spi port read unimplemented"),
    };
}
pub fn write(_: *Spi, address: u12, _: u8, cycles: *u64) void {
    cycles.* +%= 3;
    switch (@truncate(u7, address)) {
        0x00...0x09, 0x18 => {},
        else => std.debug.todo("Spi port write unimplemented"),
    }
}
