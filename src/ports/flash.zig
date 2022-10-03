const std = @import("std");

const Flash = @This();
const Ports = @import("../ports.zig");

pub fn init(self: *Flash, _: std.mem.Allocator) std.mem.Allocator.Error!void {
    self.* = .{};
}
pub fn deinit(self: *Flash, _: std.mem.Allocator) void {
    self.* = undefined;
}

fn ports(self: *Flash) *Ports {
    return @fieldParentPtr(Ports, "flash", self);
}

pub fn read(self: *Flash, address: u12, cycles: *u64) u8 {
    cycles.* +%= 2;
    return switch (@truncate(u8, address)) {
        0x05 => @intCast(u8, self.ports().core().mem.flash_wait_states - 6),
        else => std.debug.todo("Flash port read unimplemented"),
    };
}
pub fn write(self: *Flash, address: u12, value: u8, cycles: *u64) void {
    cycles.* +%= 2;
    switch (@truncate(u8, address)) {
        0x02 => {},
        0x05 => self.ports().core().mem.flash_wait_states = 6 + value,
        else => std.debug.todo("Flash port write unimplemented"),
    }
}
