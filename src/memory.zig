const std = @import("std");

const CEmuCore = @import("cemucore.zig");
const Memory = @This();

pub const ram_start = 0xD00000;
pub const ram_end = ram_start + (Memory{}).ram.len;

flash: []u8,
ram: *[0x40000 + 320 * 240 * 2]u8,
port0: [6]u8,
sha256_data: [64]u8,
sha256_state: [32]u8,
cursor: *[0x400]u8,

pub fn init(self: *Memory, allocator: std.mem.Allocator) !void {
    self.flash = try allocator.alloc(u8, 0x400000);
    errdefer allocator.free(self.flash);

    self.ram = try allocator.create(@TypeOf(self.ram.*));
    errdefer allocator.destroy(self.ram);

    self.cursor = try allocator.create(@TypeOf(self.cursor.*));
    errdefer allocator.destroy(self.cursor);
}
pub fn deinit(self: *Memory, allocator: std.mem.Allocator) void {
    allocator.destroy(self.cursor);
    allocator.destroy(self.ram);
    allocator.free(self.flash);
}

pub fn loadByte(self: *Memory, address: u24) u8 {
    return switch (address) {
        0x000000...0xCFFFFF => if (address < self.flash.len) self.flash[address] else undefined,
        ram_start...ram_start + self.ram.len - 1 => self.ram[address - ram_start],
        0xE30800...0xE30BFF => self.cursor[address - 0xE30800],
        else => {
            std.debug.print("\n0x{X:0>6}\n", .{address});
            std.debug.todo("unimplemented");
        },
    };
}
pub fn storeByte(self: *Memory, address: u24, value: u8) void {
    switch (address) {
        ram_start...ram_start + self.ram.len - 1 => self.ram[address - ram_start] = value,
        0xE30800...0xE30BFF => self.cursor[address - 0xE30800] = value,
        else => {
            std.debug.print("\n0x{X:0>6}\n", .{address});
            std.debug.todo("unimplemented");
        },
    }
}

pub fn loadCpuByte(self: *Memory, address: u24) u8 {
    @fieldParentPtr(CEmuCore, "mem", self).cpu.cycles += switch (address) {
        0x000000...0xCFFFFF => if (address < self.flash.len) 10 else 258,
        ram_start...ram_start + self.ram.len - 1 => 4,
        0xE30800...0xE30BFF => 3,
        else => {
            std.debug.print("\n0x{X:0>6}\n", .{address});
            std.debug.todo("unimplemented");
        },
    };
    return self.loadByte(address);
}
pub fn storeCpuByte(self: *Memory, address: u24, value: u8) void {
    @fieldParentPtr(CEmuCore, "mem", self).cpu.cycles += switch (address) {
        ram_start...ram_start + self.ram.len - 1 => 2,
        0xE30800...0xE30BFF => 2,
        else => {
            std.debug.print("\n0x{X:0>6}\n", .{address});
            std.debug.todo("unimplemented");
        },
    };
    self.storeByte(address, value);
}
