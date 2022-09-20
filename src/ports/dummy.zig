const std = @import("std");

const Ports = @import("../ports.zig");

handler: Ports.Handler = .{ .read = read, .write = write, .destroy = destroy },

fn read(_: *Ports.Handler, _: u12, _: *u64) u8 {
    std.debug.todo("unimplemented port");
}
fn write(_: *Ports.Handler, _: u12, _: u8, _: *u64) void {
    std.debug.todo("unimplemented port");
}
fn destroy(_: *Ports.Handler, _: std.mem.Allocator) void {}
