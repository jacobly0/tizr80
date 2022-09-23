const std = @import("std");

const CEmuCore = @import("cemucore.zig");
const Memory = @This();
const Ports = @import("ports.zig");
const util = @import("util.zig");

pub const ram_start: u24 = 0xD00000;
pub const ram_len: u19 = 0x40000 + 320 * 240 * 2;
pub const ram_end: u24 = ram_start + ram_len - 1;

flash: []u8,
ram: *[ram_len]u8,

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
    return util.bit.concat(.{ @intCast(u4, switch (util.bit.extract(address, u8, 16)) {
        else => unreachable,
        0xE0...0xE3 => |addr| 0x1 + addr - 0xE0,
        0xE4, 0xFF => {
            cycles.* +%= 2;
            return null;
        },
        0xF0...0xFA => |addr| 0x5 + addr - 0xF0,
        0xFB, 0xFE => {
            cycles.* +%= 3;
            return null;
        },
    }), util.bit.extract(address, u12, 0) });
}

pub fn read(self: *Memory, address: u24, cycles: *u64) u8 {
    switch (util.bit.extract(address, u4, 20)) {
        0x0...0xC => if (address < self.flash.len) {
            cycles.* +%= 10;
            return self.flash[address];
        } else {
            cycles.* +%= 258;
            return undefined;
        },
        util.bit.extract(ram_start, u4, 20)...util.bit.extract(ram_end, u4, 20) => {
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
    switch (util.bit.extract(address, u4, 20)) {
        0x0...0xC => if (address < self.flash.len) {
            cycles.* +%= 10;
            self.flash[address] = value;
        } else {
            cycles.* +%= 258;
        },
        util.bit.extract(ram_start, u4, 20)...util.bit.extract(ram_end, u4, 20) => {
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
