const std = @import("std");

const Dummy = @This();
const Ports = @import("../ports.zig");
const TiZr80 = @import("../tizr80.zig");
const util = @import("../util.zig");

pub fn read(_: *Dummy, address: u12, _: *u64) u8 {
    std.debug.print("read 0x{X}\n", .{address});
    util.todo("unimplemented port read");
}
pub fn write(_: *Dummy, _: u12, _: u8, _: *u64) void {
    util.todo("unimplemented port write");
}
