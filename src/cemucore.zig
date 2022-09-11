const builtin = @import("builtin");
const std = @import("std");

const Sync = @import("sync.zig");
const Memory = @import("memory.zig");
const Cpu = @import("cpu.zig");
const Keypad = @import("keypad.zig");

pub const options = @import("options");

pub const Signal = enum {
    DevChanged,
    FlashSizeChanged,
    TransferTotal,
    TransferProgress,
    TransferComplete,
    LcdFrame,
    SoftCmd,
};

pub const CreateOptions = struct {
    allocator: std.mem.Allocator,
    threading: enum { SingleThreaded, MultiThreaded } = if (builtin.single_threaded)
        .SingleThreaded
    else
        .MultiThreaded,
    signal_handler: SignalHandler = null,
};

pub const Property = if (options.debugger) enum {
    Dev,
    Reg,
    RegShadow,
    Key,
    FlashSize,
    MemZ80,
    MemAdl,
    Flash,
    Ram,
    Port,
    GpioEnable,
    Transfer,

    Watch,
    WatchAddr,
    WatchSize,
    WatchFlags,
    MemZ80WatchFlags,
    MemAdlWatchFlags,
    FlashWatchFlags,
    RamWatchFlags,
    PortWatchFlags,
} else enum {
    Dev,
    Reg,
    RegShadow,
    Key,
    FlashSize,
    MemZ80,
    MemAdl,
    Flash,
    Ram,
    Port,
    GpioEnable,
    Transfer,
};

pub const Device = enum {
    Unknown,
    TI84PCE,
    TI84PCEPE,
    TI83PCE,
    TI83PCEEP,
    TI84PCET,
    TI84PCETPE,
};

pub const TransferAddress = enum {
    Total,
    Progress,
    Remaining,
    Error,
};

pub const RegisterAddress = Cpu.RegisterAddress;

pub const SignalHandler = ?*const fn (*CEmuCore, Signal) void;

const CEmuCore = @This();

allocator: std.mem.Allocator,
signal_handler: ?*const fn (*CEmuCore, Signal) void,
sync: ?Sync = undefined,
mem: Memory = undefined,
cpu: Cpu = undefined,
keypad: Keypad = undefined,
thread: ?std.Thread = null,

pub fn create(create_options: CreateOptions) !*CEmuCore {
    const self = try create_options.allocator.create(CEmuCore);
    errdefer create_options.allocator.destroy(self);

    try self.init(create_options);
    return self;
}
pub fn init(self: *CEmuCore, create_options: CreateOptions) !void {
    errdefer self.deinit();
    self.* = CEmuCore{
        .allocator = create_options.allocator,
        .signal_handler = create_options.signal_handler,
    };
    switch (create_options.threading) {
        .SingleThreaded => self.sync = null,
        .MultiThreaded => {
            std.debug.assert(!builtin.single_threaded);
            try self.sync.?.init();
        },
    }
    try self.mem.init(self.allocator);
    try self.cpu.init(self.allocator);
    try self.keypad.init();
    if (self.sync) |*sync| {
        const thread = try std.Thread.spawn(
            .{},
            runLoop,
            .{self},
        );
        const name = "cemucore";
        thread.setName(name[0..std.math.min(name.len, std.Thread.max_name_len)]) catch {};
        self.thread = thread;
        sync.start();
    }
}

pub fn deinit(self: *CEmuCore) void {
    if (self.sync) |*sync| sync.stop();
    if (self.thread) |thread| thread.join();
    self.keypad.deinit();
    self.cpu.deinit(self.allocator);
    self.mem.deinit(self.allocator);
    if (self.sync) |*sync| sync.deinit();
}
pub fn destroy(self: *CEmuCore) void {
    self.deinit();
    self.allocator.destroy(self);
}

fn IntTypeForBuffer(buffer: []const u8) type {
    return std.meta.Int(.unsigned, std.math.min(buffer.len, 3) * 8);
}

fn getRaw(self: *CEmuCore, property: Property, address: u24) ?u24 {
    return switch (property) {
        .Reg => inline for (@typeInfo(RegisterAddress).Enum.fields) |field| {
            if (address == field.value) {
                break self.cpu.get(@field(RegisterAddress, field.name));
            }
        } else unreachable,
        .RegShadow => inline for (@typeInfo(RegisterAddress).Enum.fields) |field| {
            if (address == field.value) {
                break self.cpu.getShadow(@field(RegisterAddress, field.name));
            }
        } else unreachable,
        .Key => self.keypad.getKey(@intCast(u8, address)),
        .Ram => self.mem.readByte(Memory.ram_start + @as(u24, @intCast(u19, address))),
        .GpioEnable => self.keypad.getGpio(@intCast(u5, address)),
        else => std.debug.todo("unimplemented"),
    };
}
pub fn get(self: *CEmuCore, property: Property, address: u24) ?u24 {
    const needs_sync = switch (property) {
        else => true,
    };
    if (needs_sync) if (self.sync) |*sync| sync.enter();
    defer if (needs_sync) if (self.sync) |*sync| sync.leave();

    return self.getRaw(property, address);
}
pub fn getSlice(self: *CEmuCore, property: Property, address: u24, buffer: []u8) void {
    const needs_sync = switch (property) {
        else => true,
    };
    if (needs_sync) if (self.sync) |*sync| sync.enter();
    defer if (needs_sync) if (self.sync) |*sync| sync.leave();

    switch (property) {
        else => if (self.getRaw(property, address)) |value|
            inline for (.{ 3, 2, 1, 0 }) |len| {
                const IntType = std.meta.Int(.unsigned, len * 8);
                if (len <= buffer.len)
                    break std.mem.writeIntSliceLittle(IntType, buffer, @intCast(IntType, value));
            } else unreachable,
    }
}
fn setRaw(self: *CEmuCore, property: Property, address: u24, value: ?u24) void {
    switch (property) {
        .Reg => inline for (@typeInfo(RegisterAddress).Enum.fields) |field| {
            if (address == field.value) {
                const register_address = @field(RegisterAddress, field.name);
                const truncated_value = @intCast(Cpu.RegisterType(register_address), value.?);
                switch (register_address) {
                    .pc => if (truncated_value != self.cpu.get(register_address))
                        self.cpu.needFlush(),
                    else => {},
                }
                self.cpu.set(register_address, truncated_value);
                break;
            }
        } else unreachable,
        .RegShadow => inline for (@typeInfo(RegisterAddress).Enum.fields) |field| {
            if (address == field.value) {
                const register_address = @field(RegisterAddress, field.name);
                const truncated_value = @intCast(Cpu.RegisterType(register_address), value.?);
                self.cpu.setShadow(register_address, truncated_value);
                break;
            }
        },
        .Key => self.keypad.setKey(@intCast(u8, address), @intCast(u1, value.?)),
        .Ram => self.mem.writeByte(Memory.ram_start + @as(u24, @intCast(u19, address)), @intCast(u8, value.?)),
        .GpioEnable => self.keypad.setGpio(@intCast(u5, address), @intCast(u1, value.?)),
        else => std.debug.todo("unimplemented"),
    }
}
pub fn set(self: *CEmuCore, property: Property, address: u24, value: ?u24) void {
    const needs_sync = switch (property) {
        .Key => false,
        else => true,
    };
    if (needs_sync) if (self.sync) |*sync| sync.enter();
    defer if (needs_sync) if (self.sync) |*sync| sync.leave();

    self.setRaw(property, address, value);
}
pub fn setSlice(self: *CEmuCore, property: Property, address: u24, buffer: []const u8) void {
    const needs_sync = switch (property) {
        .Key => false,
        else => true,
    };
    if (needs_sync) if (self.sync) |*sync| sync.enter();
    defer if (needs_sync) if (self.sync) |*sync| sync.leave();

    switch (property) {
        .Port => {
            var offset: usize = 0;
            while (offset < buffer.len) : (offset += 1) {
                const current = @truncate(u16, address) +% offset;
                if (current >= 0x0020 and current < 0x0026)
                    self.mem.port0[current - 0x0020] = buffer[offset]
                else if (current >= 0x2010 and current < 0x2050)
                    self.mem.sha256_data[current - 0x2010] = buffer[offset]
                else if (current >= 0x4800 and current < 0x4C00)
                    self.mem.cursor[current - 0x4800] = buffer[offset]
                else
                    std.debug.todo("unimplemented");
            }
        },
        else => {
            const value = inline for (.{ 3, 2, 1, 0 }) |len| {
                if (len <= buffer.len)
                    break std.mem.readIntSliceLittle(std.meta.Int(.unsigned, len * 8), buffer);
            };
            self.setRaw(property, address, value);
        },
    }
}

pub fn doCommand(self: *CEmuCore, arguments: [:null]?[*:0]u8) i32 {
    _ = self;
    _ = arguments;
    return 0;
}

pub fn runLoop(self: *CEmuCore) void {
    while (self.sync.?.loop()) {
        if (!self.sync.?.delay(std.time.ns_per_s / 10)) break;

        self.cpu.step();
    } else return;
    std.debug.assert(!self.sync.?.loop());
}

pub fn sleep(self: *CEmuCore) bool {
    return if (self.sync) |*sync| sync.sleep() else false;
}

pub fn wake(self: *CEmuCore) bool {
    return if (self.sync) |*sync| sync.wake() else false;
}

test "create" {
    const core = try CEmuCore.create(.{ .allocator = std.testing.allocator });
    defer core.destroy();
}

test "init" {
    const core = try std.testing.allocator.create(CEmuCore);
    defer std.testing.allocator.destroy(core);

    try core.init(.{ .allocator = std.testing.allocator });
    defer core.deinit();
}

test "sleep/wake" {
    const core = try CEmuCore.create(.{ .allocator = std.testing.allocator });
    defer core.destroy();

    try std.testing.expect(!core.sleep());
    try std.testing.expect(core.wake());
    try std.testing.expect(!core.wake());
    try std.testing.expect(core.sleep());
    try std.testing.expect(!core.sleep());
}

test "single threaded" {
    const core = try CEmuCore.create(.{ .allocator = std.testing.allocator, .threading = .SingleThreaded });
    defer core.destroy();
}

test {
    std.testing.refAllDecls(CEmuCore);
}
