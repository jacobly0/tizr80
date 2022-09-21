const std = @import("std");

const CEmuCore = @import("cemucore.zig");
const Memory = @This();
const Ports = @import("ports.zig");

pub const ram_start = 0xD00000;
pub const ram_end = ram_start + @as(Memory, undefined).ram.len - 1;

flash: []u8,
ram: *[0x40000 + 320 * 240 * 2]u8,

pub fn init(self: *Memory, allocator: std.mem.Allocator) !void {
    self.flash = try allocator.alloc(u8, 0x400000);
    errdefer allocator.free(self.flash);

    self.ram = try allocator.create(@TypeOf(self.ram.*));
    errdefer allocator.destroy(self.ram);
}
pub fn deinit(self: *Memory, allocator: std.mem.Allocator) void {
    allocator.destroy(self.ram);
    allocator.free(self.flash);
    self.* = undefined;
}

fn ports(self: *Memory) *Ports {
    return &@fieldParentPtr(CEmuCore, "mem", self).ports;
}
fn mmio(address: u24, cycles: *u64) ?u16 {
    const addr = @intCast(u8, address >> 16);
    return @as(u16, switch (addr) {
        else => unreachable,
        0xE0...0xE3 => 0x1 + addr - 0xE0,
        0xE4, 0xFF => {
            cycles.* +%= 2;
            return null;
        },
        0xF0...0xFA => 0x5 + addr - 0xF0,
        0xFB, 0xFE => {
            cycles.* +%= 3;
            return null;
        },
    }) << 12 | @truncate(u12, address);
}

pub fn read(self: *Memory, address: u24, cycles: *u64) u8 {
    switch (@truncate(u4, address >> 20)) {
        0x0...0xC => if (address < self.flash.len) {
            cycles.* +%= 10;
            return self.flash[address];
        } else {
            cycles.* +%= 258;
            return undefined;
        },
        ram_start >> 20...ram_end >> 20 => {
            cycles.* +%= 4;
            return switch (address) {
                ram_start...ram_end => self.ram[address - ram_start],
                else => undefined,
            };
        },
        0xE...0xF => return self.ports().read(mmio(address, cycles) orelse return 0, cycles),
    }
}
pub fn write(self: *Memory, address: u24, value: u8, cycles: *u64) void {
    switch (@truncate(u4, address >> 20)) {
        0x0...0xC => if (address < self.flash.len) {
            cycles.* +%= 10;
            self.flash[address] = value;
        } else {
            cycles.* +%= 258;
        },
        ram_start >> 20...ram_end >> 20 => {
            cycles.* +%= 2;
            switch (address) {
                ram_start...ram_end => self.ram[address - ram_start] = value,
                else => {},
            }
        },
        0xE...0xF => return self.ports().write(mmio(address, cycles) orelse return, value, cycles),
    }
}

pub fn peek(self: *Memory, address: u24) u8 {
    var cycles: u64 = undefined;
    return self.read(address, &cycles);
}
pub fn poke(self: *Memory, address: u24, value: u8) void {
    var cycles: u64 = undefined;
    self.write(address, value, &cycles);
}
