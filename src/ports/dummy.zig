const std = @import("std");

const Ports = @import("../ports.zig");
const TiZr80 = @import("../tizr80.zig");

handler: Ports.Handler = .{ .read = read, .write = write, .destroy = destroy },

fn read(_: *TiZr80, _: *Ports.Handler, _: u12, _: *u64) u8 {
    std.debug.todo("unimplemented port read");
}
fn write(_: *TiZr80, _: *Ports.Handler, _: u12, _: u8, _: *u64) void {
    std.debug.todo("unimplemented port write");
}
fn destroy(_: *Ports.Handler, _: std.mem.Allocator) void {}
