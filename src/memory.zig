const std = @import("std");

const CEmuCore = @import("cemucore.zig");
const Memory = @This();

pub const ram_start = 0xD00000;

flash: []u8,
ram: *[0x40000 + 320 * 240 * 2]u8,
port0: [6]u8,
sha256_data: [64]u8,
sha256_state: [32]u8,
cursor: *[0x400]u8,

pub fn init(self: *Memory, allocator: *std.mem.Allocator) !void {
    self.flash = try allocator.alloc(u8, 0x400000);
    errdefer allocator.free(self.flash);

    self.ram = try allocator.create(@TypeOf(self.ram.*));
    errdefer allocator.destroy(self.ram);

    self.cursor = try allocator.create(@TypeOf(self.cursor.*));
    errdefer allocator.destroy(self.cursor);
}
pub fn deinit(self: *Memory, allocator: *std.mem.Allocator) void {
    allocator.destroy(self.cursor);
    allocator.destroy(self.ram);
    allocator.free(self.flash);
}

pub fn readByte(self: *Memory, address: u24) u8 {
    if (address >= 0xE30800 and address < 0xE30C00) {
        return self.cursor[address - 0xE30800];
    } else if (address >= ram_start and address < ram_start + self.ram.len) {
        return self.ram[address - ram_start];
    } else {
        std.debug.todo("unimplemented");
    }
}
pub fn writeByte(self: *Memory, address: u24, value: u8) void {
    if (address >= 0xE30800 and address < 0xE30C00) {
        self.cursor[address - 0xE30800] = value;
    } else if (address >= ram_start and address < ram_start + self.ram.len) {
        self.ram[address - ram_start] = value;
    } else {
        std.debug.todo("unimplemented");
    }
}

pub fn readCpuByte(self: *Memory, address: u24) u8 {
    const cycles = &@fieldParentPtr(CEmuCore, "mem", self).cpu.cycles;
    if (address >= 0xE30800 and address < 0xE30C00) {
        cycles.* += 3;
    } else if (address >= ram_start and address < ram_start + self.ram.len) {
        cycles.* += 4;
    } else {
        std.debug.todo("unimplemented");
    }
    return self.readByte(address);
}
pub fn writeCpuByte(self: *Memory, address: u24, value: u8) void {
    const cycles = &@fieldParentPtr(CEmuCore, "mem", self).cpu.cycles;
    if (address >= 0xE30800 and address < 0xE30C00) {
        cycles.* += 2;
    } else if (address >= ram_start and address < ram_start + self.ram.len) {
        cycles.* += 2;
    } else {
        std.debug.todo("unimplemented");
    }
    self.writeByte(address, value);
}
