const std = @import("std");

const Backlight = @import("ports/backlight.zig");
const Dummy = @import("ports/dummy.zig");
const Flash = @import("ports/flash.zig");
const Interrupt = @import("ports/interrupt.zig");
const Keypad = @import("ports/keypad.zig");
const Lcd = @import("ports/lcd.zig");
const Port0 = @import("ports/port0.zig");
const Ports = @This();
const Sha256 = @import("ports/sha256.zig");
const Spi = @import("ports/spi.zig");
const TiZr80 = @import("tizr80.zig");
const util = @import("util.zig");

pub const Handler = struct {
    read: *const fn (*TiZr80, *Handler, u12, *u64) u8,
    write: *const fn (*TiZr80, *Handler, u12, u8, *u64) void,
    destroy: *const fn (*Handler, std.mem.Allocator) void,
};

dummy: Dummy,
handlers: [0x10]*Handler,

fn core(self: *Ports) *TiZr80 {
    return @fieldParentPtr(TiZr80, "ports", self);
}

pub fn init(self: *Ports, allocator: std.mem.Allocator) !void {
    self.* = .{
        .dummy = .{},
        .handlers = .{&self.dummy.handler} ** self.handlers.len,
    };

    self.handlers[0x0] = try Port0.create(allocator);
    self.handlers[0x1] = try Flash.create(allocator);
    self.handlers[0x2] = try Sha256.create(allocator);
    self.handlers[0x4] = try Lcd.create(allocator);
    self.handlers[0x5] = try Interrupt.create(allocator);
    self.handlers[0xA] = try Keypad.create(allocator);
    self.handlers[0xB] = try Backlight.create(allocator);
    self.handlers[0xD] = try Spi.create(allocator);
}
pub fn deinit(self: *Ports, allocator: std.mem.Allocator) void {
    for (self.handlers) |handler| handler.destroy(handler, allocator);
    self.* = undefined;
}

pub fn read(self: *Ports, address: u16, cycles: *u64) u8 {
    const handler = self.handlers[util.bit.extract(address, u4, 12)];
    return handler.read(self.core(), handler, util.bit.extract(address, u12, 0), cycles);
}

pub fn write(self: *Ports, address: u16, value: u8, cycles: *u64) void {
    const handler = self.handlers[util.bit.extract(address, u4, 12)];
    handler.write(self.core(), handler, util.bit.extract(address, u12, 0), value, cycles);
}

pub fn peek(self: *Ports, address: u16) u8 {
    var cycles: u64 = undefined;
    return self.read(self.core(), address, &cycles);
}
pub fn poke(self: *Ports, address: u16, value: u8) void {
    var cycles: u64 = undefined;
    self.write(self.core(), address, value, &cycles);
}
