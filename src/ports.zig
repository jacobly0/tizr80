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
    ptr: *anyopaque,
    read: *const fn (*Handler, u12, *u64) u8,
    write: *const fn (*Handler, u12, u8, *u64) void,

    pub fn init(pointer: anytype) Handler {
        const Pointer = @TypeOf(pointer);
        const info = @typeInfo(Pointer).Pointer;
        comptime std.debug.assert(info.size == .One);
        const alignment = if (info.alignment >= 1) info.alignment else 1;
        const T = info.child;
        const Impl = struct {
            fn unwrap(handler: *Handler) Pointer {
                return @ptrCast(Pointer, @alignCast(alignment, handler.ptr));
            }
            fn read(handler: *Handler, address: u12, cycles: *u64) u8 {
                return T.read(unwrap(handler), address, cycles);
            }
            fn write(handler: *Handler, address: u12, value: u8, cycles: *u64) void {
                T.write(unwrap(handler), address, value, cycles);
            }
        };
        return .{ .ptr = pointer, .read = Impl.read, .write = Impl.write };
    }
};

handlers: [0x10]Handler,
port0: Port0,
flash: Flash,
sha256: Sha256,
lcd: Lcd,
interrupt: Interrupt,
keypad: Keypad,
backlight: Backlight,
spi: Spi,

pub fn init(self: *Ports, allocator: std.mem.Allocator) !void {
    comptime std.debug.assert(@sizeOf(Dummy) == 0);
    self.handlers = .{Handler.init(@as(*Dummy, undefined))} ** self.handlers.len;

    try self.port0.init(allocator);
    errdefer self.port0.deinit(allocator);
    self.handlers[0x0] = Handler.init(&self.port0);

    try self.flash.init(allocator);
    errdefer self.flash.deinit(allocator);
    self.handlers[0x1] = Handler.init(&self.flash);

    try self.sha256.init(allocator);
    errdefer self.sha256.deinit(allocator);
    self.handlers[0x2] = Handler.init(&self.sha256);

    try self.lcd.init(allocator);
    errdefer self.lcd.deinit(allocator);
    self.handlers[0x4] = Handler.init(&self.lcd);

    try self.interrupt.init(allocator);
    errdefer self.interrupt.deinit(allocator);
    self.handlers[0x5] = Handler.init(&self.interrupt);

    try self.keypad.init(allocator);
    errdefer self.keypad.deinit(allocator);
    self.handlers[0xA] = Handler.init(&self.keypad);

    try self.backlight.init(allocator);
    errdefer self.backlight.deinit(allocator);
    self.handlers[0xB] = Handler.init(&self.backlight);

    try self.spi.init(allocator);
    errdefer self.spi.deinit(allocator);
    self.handlers[0xD] = Handler.init(&self.spi);
}
pub fn deinit(self: *Ports, allocator: std.mem.Allocator) void {
    self.spi.deinit(allocator);
    self.backlight.deinit(allocator);
    self.keypad.deinit(allocator);
    self.interrupt.deinit(allocator);
    self.lcd.deinit(allocator);
    self.sha256.deinit(allocator);
    self.flash.deinit(allocator);
    self.port0.deinit(allocator);
    self.* = undefined;
}

pub fn core(self: *Ports) *TiZr80 {
    return @fieldParentPtr(TiZr80, "ports", self);
}

pub fn read(self: *Ports, address: u16, cycles: *u64) u8 {
    const handler = &self.handlers[util.bit.extract(address, u4, 12)];
    return handler.read(handler, util.bit.extract(address, u12, 0), cycles);
}

pub fn write(self: *Ports, address: u16, value: u8, cycles: *u64) void {
    const handler = &self.handlers[util.bit.extract(address, u4, 12)];
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
