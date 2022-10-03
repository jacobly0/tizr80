const std = @import("std");

const Dummy = @This();
const Ports = @import("../ports.zig");
const TiZr80 = @import("../tizr80.zig");

pub fn read(_: *Dummy, _: u12, _: *u64) u8 {
    std.debug.todo("unimplemented port read");
}
pub fn write(_: *Dummy, _: u12, _: u8, _: *u64) void {
    std.debug.todo("unimplemented port write");
}
