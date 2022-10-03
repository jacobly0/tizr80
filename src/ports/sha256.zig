const std = @import("std");

const Sha256 = @This();

data: [64]u8,
state: [32]u8,

pub fn init(self: *Sha256, _: std.mem.Allocator) std.mem.Allocator.Error!void {
    self.* = .{
        .data = .{0} ** self.data.len,
        .state = .{0} ** self.state.len,
    };
}
pub fn deinit(self: *Sha256, _: std.mem.Allocator) void {
    self.* = undefined;
}

pub fn read(self: *Sha256, address: u12, cycles: *u64) u8 {
    cycles.* +%= 2;
    return switch (@truncate(u8, address)) {
        0x10...0x4F => |addr| self.data[addr - 0x10],
        0x60...0x7F => |addr| self.state[addr - 0x60],
        else => std.debug.todo("Sha256 port read unimplemented"),
    };
}
pub fn write(self: *Sha256, address: u12, value: u8, cycles: *u64) void {
    cycles.* +%= 2;
    switch (@truncate(u8, address)) {
        0x10...0x4F => |addr| self.data[addr - 0x10] = value,
        0x60...0x7F => |addr| self.state[addr - 0x60] = value,
        else => std.debug.todo("Sha256 port write unimplemented"),
    }
}
