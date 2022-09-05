const std = @import("std");

const CEmuCore = @import("cemucore.zig");
const Memory = @This();

pub const ram_start = 0xD00000;

flash: []u8,
ram: *[0x40000 + 320 * 240 * 2]u8,

pub fn init(self: *Memory, allocator: *std.mem.Allocator) !void {
    const flash = try allocator.alloc(u8, 0x400000);
    errdefer allocator.free(flash);

    const ram = try allocator.create(@TypeOf(self.ram.*));
    errdefer allocator.destroy(ram);

    self.* = Memory{
        .flash = flash,
        .ram = ram,
    };
}
pub fn deinit(self: *Memory, allocator: *std.mem.Allocator) void {
    allocator.destroy(self.ram);
    allocator.free(self.flash);
}

pub fn readByte(self: *Memory, address: u24) u8 {
    if (address >= ram_start and address < ram_start + self.ram.len) {
        return self.ram[address - ram_start];
    } else {
        std.debug.todo("unimplemented");
    }
}
pub fn writeByte(self: *Memory, address: u24, value: u8) void {
    if (address >= ram_start and address < ram_start + self.ram.len) {
        self.ram[address - ram_start] = value;
    } else {
        std.debug.todo("unimplemented");
    }
}
