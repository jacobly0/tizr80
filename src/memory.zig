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

pub fn readByte(self: *Memory, address: u24) u8 {
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
pub fn writeByte(self: *Memory, address: u24, value: u8) void {
    switch (address) {
        ram_start...ram_start + self.ram.len - 1 => self.ram[address - ram_start] = value,
        0xE30800...0xE30BFF => self.cursor[address - 0xE30800] = value,
        else => {
            std.debug.print("\n0x{X:0>6}\n", .{address});
            std.debug.todo("unimplemented");
        },
    }
}
pub fn readPortByte(self: *Memory, address: u16) u8 {
    return switch (address) {
        0x0020...0x0025 => self.port0[address - 0x0020],
        0x0120...0x0125 => self.port0[address - 0x0120],
        0x0220...0x0225 => self.port0[address - 0x0220],
        0x0320...0x0325 => self.port0[address - 0x0320],
        0x0420...0x0425 => self.port0[address - 0x0420],
        0x0520...0x0525 => self.port0[address - 0x0520],
        0x0620...0x0625 => self.port0[address - 0x0620],
        0x0720...0x0725 => self.port0[address - 0x0720],
        0x4800...0x4BFF => self.cursor[address - 0x4800],
        else => {
            std.debug.print("\n0x{X:0>4}\n", .{address});
            std.debug.todo("unimplemented");
        },
    };
}
pub fn writePortByte(self: *Memory, address: u16, value: u8) void {
    switch (address) {
        0x0020...0x0025 => self.port0[address - 0x0020] = value,
        0x0120...0x0125 => self.port0[address - 0x0120] = value,
        0x0220...0x0225 => self.port0[address - 0x0220] = value,
        0x0320...0x0325 => self.port0[address - 0x0320] = value,
        0x0420...0x0425 => self.port0[address - 0x0420] = value,
        0x0520...0x0525 => self.port0[address - 0x0520] = value,
        0x0620...0x0625 => self.port0[address - 0x0620] = value,
        0x0720...0x0725 => self.port0[address - 0x0720] = value,
        0x4800...0x4BFF => self.cursor[address - 0x4800] = value,
        else => {
            std.debug.print("\n0x{X:0>4}\n", .{address});
            std.debug.todo("unimplemented");
        },
    }
}

fn addCycles(self: *Memory, increment: u64) void {
    @fieldParentPtr(CEmuCore, "mem", self).cpu.cycles +%= increment;
}
pub fn readCpuByte(self: *Memory, address: u24) u8 {
    self.addCycles(switch (address) {
        0x000000...0xCFFFFF => if (address < self.flash.len) 10 else 258,
        ram_start...ram_start + self.ram.len - 1 => 4,
        0xE30800...0xE30BFF => 3,
        else => {
            std.debug.print("\n0x{X:0>6}\n", .{address});
            std.debug.todo("unimplemented");
        },
    });
    return self.readByte(address);
}
pub fn writeCpuByte(self: *Memory, address: u24, value: u8) void {
    self.addCycles(switch (address) {
        ram_start...ram_start + self.ram.len - 1 => 2,
        0xE30800...0xE30BFF => 2,
        else => {
            std.debug.print("\n0x{X:0>6}\n", .{address});
            std.debug.todo("unimplemented");
        },
    });
    self.writeByte(address, value);
}
pub fn readCpuPortByte(self: *Memory, address: u16) u8 {
    self.addCycles(switch (address) {
        0x0000...0x0FFF => 2,
        0x4800...0x4BFF => 3,
        else => {
            std.debug.print("\n0x{X:0>4}\n", .{address});
            std.debug.todo("unimplemented");
        },
    });
    return self.readPortByte(address);
}
pub fn writeCpuPortByte(self: *Memory, address: u16, value: u8) void {
    self.addCycles(switch (address) {
        0x0000...0x0FFF => 2,
        0x4800...0x4BFF => 2,
        else => {
            std.debug.print("\n0x{X:0>4}\n", .{address});
            std.debug.todo("unimplemented");
        },
    });
    self.writePortByte(address, value);
}
