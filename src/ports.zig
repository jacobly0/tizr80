const std = @import("std");

const CEmuCore = @import("cemucore.zig");
const Dummy = @import("ports/dummy.zig");
const Lcd = @import("ports/lcd.zig");
const Port0 = @import("ports/port0.zig");
const Ports = @This();
const Sha256 = @import("ports/sha256.zig");
const util = @import("util.zig");

pub const Handler = struct {
    read: *const fn (*Handler, u12, *u64) u8,
    write: *const fn (*Handler, u12, u8, *u64) void,
    destroy: *const fn (*Handler, std.mem.Allocator) void,
};

dummy: Dummy,
handlers: [0x10]*Handler,

pub fn init(self: *Ports, allocator: std.mem.Allocator) !void {
    self.* = .{
        .dummy = .{},
        .handlers = [_]*Handler{&self.dummy.handler} ** self.handlers.len,
    };

    self.handlers[0x0] = try Port0.create(allocator);
    self.handlers[0x2] = try Sha256.create(allocator);
    self.handlers[0x4] = try Lcd.create(allocator);
}
pub fn deinit(self: *Ports, allocator: std.mem.Allocator) void {
    for (self.handlers) |handler| handler.destroy(handler, allocator);
    self.* = undefined;
}

pub fn read(self: *Ports, address: u16, cycles: *u64) u8 {
    const handler = self.handlers[util.bit.extract(address, u4, 12)];
    return handler.read(handler, util.bit.extract(address, u12, 0), cycles);
}

pub fn write(self: *Ports, address: u16, value: u8, cycles: *u64) void {
    const handler = self.handlers[util.bit.extract(address, u4, 12)];
    handler.write(handler, util.bit.extract(address, u12, 0), value, cycles);
}

pub fn peek(self: *Ports, address: u16) u8 {
    var cycles: u64 = undefined;
    return self.read(address, &cycles);
}
pub fn poke(self: *Ports, address: u16, value: u8) void {
    var cycles: u64 = undefined;
    self.write(address, value, &cycles);
}
