const builtin = @import("builtin");
const std = @import("std");

const Sync = @import("sync.zig");
const Memory = @import("memory.zig");
const Cpu = @import("cpu.zig");
const Keypad = @import("keypad.zig");
const Ports = @import("ports.zig");

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

pub const Property = enum {
    device,
    register,
    shadow_register,
    key,
    flash_size,
    memory_z80,
    memory_adl,
    flash,
    ram,
    port,
    transfer,

    // options.debugger only
    watch,
    watch_address,
    watch_size,
    watch_flags,
    memory_z80_watch_flags,
    memory_adl_watch_flags,
    flash_watch_flags,
    ram_watch_flags,
    port_watch_flags,

    pub const Key = union(Property) {
        device: Device,
        register: Cpu.RegisterId,
        shadow_register: Cpu.RegisterId,
        key: Keypad.Key,
        flash_size: u24,
        memory_z80: u16,
        memory_adl: u24,
        flash: u23,
        ram: u19,
        port: u16,
        transfer: Transfer,

        watch: ?u24,
        watch_address: u24,
        watch_size: u24,
        watch_flags: u24,
        memory_z80_watch_flags: u24,
        memory_adl_watch_flags: u24,
        flash_watch_flags: u24,
        ram_watch_flags: u24,
        port_watch_flags: u24,
    };
};

pub const Device = enum {
    unknown,
    ti84pce,
    ti84pcepe,
    ti83pce,
    ti83pceep,
    ti84pcet,
    ti84pcetpe,
};

pub const Transfer = enum {
    total,
    progress,
    remaining,
    @"error",
};

pub const SignalHandler = ?*const fn (*TiZr80, Signal) void;

const TiZr80 = @This();

allocator: std.mem.Allocator,
signal_handler: ?*const fn (*TiZr80, Signal) void,
sync: ?Sync = undefined,
mem: Memory = undefined,
ports: Ports = undefined,
cpu: Cpu = undefined,
keypad: Keypad = undefined,
thread: ?std.Thread = null,

pub fn create(create_options: CreateOptions) !*TiZr80 {
    const self = try create_options.allocator.create(TiZr80);
    errdefer create_options.allocator.destroy(self);

    try self.init(create_options);
    return self;
}
pub fn init(self: *TiZr80, create_options: CreateOptions) !void {
    errdefer self.deinit();
    self.* = TiZr80{
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
    try self.ports.init(self.allocator);
    try self.cpu.init(self.allocator);
    try self.keypad.init();
    if (self.sync) |*sync| {
        const thread = try std.Thread.spawn(
            .{},
            runLoop,
            .{self},
        );
        const name = "TiZr80";
        thread.setName(name[0..std.math.min(name.len, std.Thread.max_name_len)]) catch {};
        self.thread = thread;
        sync.start();
    }
}

pub fn deinit(self: *TiZr80) void {
    if (self.sync) |*sync| sync.stop();
    if (self.thread) |thread| thread.join();
    self.keypad.deinit();
    self.cpu.deinit(self.allocator);
    self.ports.deinit(self.allocator);
    self.mem.deinit(self.allocator);
    if (self.sync) |*sync| sync.deinit();
    self.* = undefined;
}
pub fn destroy(self: *TiZr80) void {
    const allocator = self.allocator;
    self.deinit();
    allocator.destroy(self);
}

fn IntTypeForBuffer(buffer: []const u8) type {
    return std.meta.Int(.unsigned, std.math.min(buffer.len, 3) * 8);
}

fn getRaw(self: *TiZr80, key: Property.Key) ?u24 {
    return switch (key) {
        .register => |id| self.cpu.getAny(id),
        .shadow_register => |id| self.cpu.getAnyShadow(id),
        .key => |key| self.keypad.getKey(key),
        .flash => |address| self.mem.peek(address),
        .ram => |address| self.mem.peek(Memory.ram_start + @as(u24, address)),
        .port => |address| self.ports.peek(address),
        else => std.debug.todo("unimplemented"),
    };
}
pub fn get(self: *TiZr80, key: Property.Key) ?u24 {
    const needs_sync = switch (key) {
        else => true,
    };
    if (needs_sync) if (self.sync) |*sync| sync.enter();
    defer if (needs_sync) if (self.sync) |*sync| sync.leave();

    return self.getRaw(key);
}
pub fn getSlice(self: *TiZr80, key: Property.Key, buffer: []u8) void {
    const needs_sync = switch (key) {
        else => true,
    };
    if (needs_sync) if (self.sync) |*sync| sync.enter();
    defer if (needs_sync) if (self.sync) |*sync| sync.leave();

    switch (key) {
        .flash => |address| for (buffer) |*value, offset| {
            value.* = @intCast(u8, self.getRaw(.{ .flash = address + @intCast(u23, offset) }).?);
        },
        .ram => |address| for (buffer) |*value, offset| {
            value.* = @intCast(u8, self.getRaw(.{ .ram = address + @intCast(u19, offset) }).?);
        },
        .port => |address| for (buffer) |*value, offset| {
            value.* = @intCast(u8, self.getRaw(.{ .port = address + @intCast(u16, offset) }).?);
        },
        else => if (self.getRaw(key)) |value|
            inline for (.{ 3, 2, 1, 0 }) |len| {
                const IntType = std.meta.Int(.unsigned, len * 8);
                if (len <= buffer.len)
                    break std.mem.writeIntSliceLittle(IntType, buffer, @intCast(IntType, value));
            } else unreachable,
    }
}
fn setRaw(self: *TiZr80, key: Property.Key, value: ?u24) void {
    switch (key) {
        .register => |id| self.cpu.setAny(id, value.?),
        .shadow_register => |id| self.cpu.setAnyShadow(id, value.?),
        .key => |key| self.keypad.setKey(key, @intCast(u1, value.?)),
        .ram => |address| self.mem.poke(Memory.ram_start + @as(u24, address), @intCast(u8, value.?)),
        .port => |address| self.ports.poke(address, @intCast(u8, value.?)),
        else => std.debug.todo("unimplemented"),
    }
}
pub fn set(self: *TiZr80, key: Property.Key, value: ?u24) void {
    const needs_sync = switch (key) {
        .key => false,
        else => true,
    };
    if (needs_sync) if (self.sync) |*sync| sync.enter();
    defer if (needs_sync) if (self.sync) |*sync| sync.leave();

    self.setRaw(key, value);
}
pub fn setSlice(self: *TiZr80, key: Property.Key, buffer: []const u8) void {
    const needs_sync = switch (key) {
        .key => false,
        else => true,
    };
    if (needs_sync) if (self.sync) |*sync| sync.enter();
    defer if (needs_sync) if (self.sync) |*sync| sync.leave();

    switch (key) {
        .ram => |address| for (buffer) |value, offset| {
            self.setRaw(.{ .ram = address + @intCast(u19, offset) }, value);
        },
        .port => |address| for (buffer) |value, offset| {
            self.setRaw(.{ .port = address + @intCast(u16, offset) }, value);
        },
        else => {
            self.setRaw(key, inline for (.{ 3, 2, 1, 0 }) |len| {
                if (len <= buffer.len)
                    break std.mem.readIntSliceLittle(std.meta.Int(.unsigned, len * 8), buffer);
            });
        },
    }
}

pub const CommandError = error{
    InvalidCommand,
} || std.mem.Allocator.Error || std.fs.File.OpenError;

pub fn commandPointers(self: *TiZr80, arguments: [*:null]const ?[*:0]const u8) CommandError!i32 {
    const slices = try self.allocator.allocSentinel(?[:0]const u8, std.mem.len(arguments), null);
    defer self.allocator.free(slices);
    for (slices) |*slice, index| slice.* = std.mem.span(arguments[index]);
    return self.commandSlices(slices);
}

pub fn commandSlices(self: *TiZr80, arguments: [:null]const ?[:0]const u8) CommandError!i32 {
    _ = self;
    if (arguments.len == 0) return CommandError.InvalidCommand;
    if (std.mem.eql(u8, arguments[0].?, "load")) {
        if (arguments.len != 3) return CommandError.InvalidCommand;
        if (std.mem.eql(u8, arguments[1].?, "rom")) {
            const file = try std.fs.cwd().openFileZ(arguments[2].?, .{});
            defer file.close();
        }
    } else if (std.mem.eql(u8, arguments[0].?, "store")) {
        if (arguments.len != 3) return CommandError.InvalidCommand;
    }
    return CommandError.InvalidCommand;
}

pub fn runLoop(self: *TiZr80) void {
    while (self.sync.?.loop()) {
        if (!self.sync.?.delay(std.time.ns_per_s / 10)) break;

        self.cpu.step();
    } else return;
    std.debug.assert(!self.sync.?.loop());
}

pub fn sleep(self: *TiZr80) bool {
    return if (self.sync) |*sync| sync.sleep() else false;
}

pub fn wake(self: *TiZr80) bool {
    return if (self.sync) |*sync| sync.wake() else false;
}

test "create" {
    const core = try TiZr80.create(.{ .allocator = std.testing.allocator });
    defer core.destroy();
}

test "init" {
    const core = try std.testing.allocator.create(TiZr80);
    defer std.testing.allocator.destroy(core);

    try core.init(.{ .allocator = std.testing.allocator });
    defer core.deinit();
}

test "sleep/wake" {
    const core = try TiZr80.create(.{ .allocator = std.testing.allocator });
    defer core.destroy();

    try std.testing.expect(!core.sleep());
    try std.testing.expect(core.wake());
    try std.testing.expect(!core.wake());
    try std.testing.expect(core.sleep());
    try std.testing.expect(!core.sleep());
}

test "single threaded" {
    const core = try TiZr80.create(.{ .allocator = std.testing.allocator, .threading = .SingleThreaded });
    defer core.destroy();
}

test {
    std.testing.refAllDecls(TiZr80);
    _ = @import("as.zig");
}
