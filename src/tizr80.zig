const builtin = @import("builtin");
const std = @import("std");

const Commands = @import("commands.zig");
const Cpu = @import("cpu.zig");
const Keypad = @import("ports/keypad.zig");
const Memory = @import("memory.zig");
const Ports = @import("ports.zig");
const Sync = @import("sync.zig");
const util = @import("util.zig");

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
commands: Commands = undefined,
thread: ?std.Thread = null,

pub fn create(create_options: CreateOptions) !*TiZr80 {
    const self = try create_options.allocator.create(TiZr80);
    errdefer create_options.allocator.destroy(self);

    try self.init(create_options);
    return self;
}
pub fn init(self: *TiZr80, create_options: CreateOptions) !void {
    errdefer self.* = undefined;

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
    errdefer if (self.sync) |*sync| sync.deinit();

    try self.mem.init(self.allocator);
    errdefer self.mem.deinit(self.allocator);

    try self.ports.init(self.allocator);
    errdefer self.ports.deinit(self.allocator);

    try self.cpu.init(self.allocator);
    errdefer self.cpu.deinit(self.allocator);

    try self.commands.init(self.allocator);
    errdefer self.commands.deinit(self.allocator);

    errdefer if (self.thread) |thread| thread.join();
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
    errdefer if (self.sync) |*sync| sync.stop();
}

pub fn deinit(self: *TiZr80) void {
    if (self.sync) |*sync| sync.stop();
    if (self.thread) |thread| thread.join();
    self.commands.deinit(self.allocator);
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

fn getRaw(self: *TiZr80, property: Property.Key) ?u24 {
    return switch (property) {
        .register => |id| self.cpu.getAny(id),
        .shadow_register => |id| self.cpu.getAnyShadow(id),
        .key => |key| self.ports.keypad.getKey(key),
        .flash => |address| self.mem.peek(address),
        .ram => |address| self.mem.peek(Memory.ram_start + @as(u24, address)),
        .port => |address| self.ports.peek(address),
        else => std.debug.todo("unimplemented"),
    };
}
pub fn get(self: *TiZr80, property: Property.Key) ?u24 {
    const needs_sync = switch (property) {
        else => true,
    };
    if (needs_sync) if (self.sync) |*sync| sync.enter();
    defer if (needs_sync) if (self.sync) |*sync| sync.leave();

    return self.getRaw(property);
}
pub fn getSlice(self: *TiZr80, property: Property.Key, buffer: []u8) void {
    const needs_sync = switch (property) {
        else => true,
    };
    if (needs_sync) if (self.sync) |*sync| sync.enter();
    defer if (needs_sync) if (self.sync) |*sync| sync.leave();

    switch (property) {
        .flash => |address| for (buffer) |*value, offset| {
            value.* = @intCast(u8, self.getRaw(.{ .flash = address + @intCast(u23, offset) }).?);
        },
        .ram => |address| for (buffer) |*value, offset| {
            value.* = @intCast(u8, self.getRaw(.{ .ram = address + @intCast(u19, offset) }).?);
        },
        .port => |address| for (buffer) |*value, offset| {
            value.* = @intCast(u8, self.getRaw(.{ .port = address + @intCast(u16, offset) }).?);
        },
        else => if (self.getRaw(property)) |value|
            inline for (.{ 3, 2, 1, 0 }) |len| {
                const IntType = std.meta.Int(.unsigned, len * 8);
                if (len <= buffer.len)
                    break std.mem.writeIntSliceLittle(IntType, buffer, @intCast(IntType, value));
            } else unreachable,
    }
}
fn setRaw(self: *TiZr80, property: Property.Key, value: ?u24) void {
    switch (property) {
        .register => |id| self.cpu.setAny(id, value.?),
        .shadow_register => |id| self.cpu.setAnyShadow(id, value.?),
        .key => |key| self.ports.keypad.setKey(key, @intCast(u1, value.?)),
        .ram => |address| self.mem.poke(Memory.ram_start + @as(u24, address), @intCast(u8, value.?)),
        .port => |address| self.ports.poke(address, @intCast(u8, value.?)),
        else => std.debug.todo("unimplemented"),
    }
}
pub fn set(self: *TiZr80, property: Property.Key, value: ?u24) void {
    const needs_sync = switch (property) {
        .key => false,
        else => true,
    };
    if (needs_sync) if (self.sync) |*sync| sync.enter();
    defer if (needs_sync) if (self.sync) |*sync| sync.leave();

    self.setRaw(property, value);
}
pub fn setSlice(self: *TiZr80, property: Property.Key, buffer: []const u8) void {
    const needs_sync = switch (property) {
        .key => false,
        else => true,
    };
    if (needs_sync) if (self.sync) |*sync| sync.enter();
    defer if (needs_sync) if (self.sync) |*sync| sync.leave();

    switch (property) {
        .ram => |address| for (buffer) |value, offset| {
            self.setRaw(.{ .ram = address + @intCast(u19, offset) }, value);
        },
        .port => |address| for (buffer) |value, offset| {
            self.setRaw(.{ .port = address + @intCast(u16, offset) }, value);
        },
        else => {
            self.setRaw(property, inline for (.{ 3, 2, 1, 0 }) |len| {
                if (len <= buffer.len)
                    break std.mem.readIntSliceLittle(std.meta.Int(.unsigned, len * 8), buffer);
            });
        },
    }
}

pub const CommandSplitError = Commands.Error || util.ArgumentSplitter.Error;

pub fn commandSplit(self: *TiZr80, line: []const u8) CommandSplitError!i32 {
    var argument_splitter = util.ArgumentSplitter.init(self.allocator, line);
    defer argument_splitter.deinit();

    const arguments = try argument_splitter.restOwned();
    defer self.allocator.free(arguments);

    return self.commandSlices(arguments);
}

pub fn commandPointers(self: *TiZr80, arguments: [*:null]const ?[*:0]const u8) Commands.Error!i32 {
    const slices = try self.allocator.alloc([:0]const u8, std.mem.len(arguments));
    defer self.allocator.free(slices);
    for (slices) |*slice, index| slice.* = std.mem.span(arguments[index].?);
    return self.commandSlices(slices);
}

pub fn commandSlices(self: *TiZr80, arguments: []const [:0]const u8) Commands.Error!i32 {
    if (self.sync) |*sync| sync.enter();
    defer if (self.sync) |*sync| sync.leave();

    return self.commands.run(arguments);
}

pub fn runLoop(self: *TiZr80) void {
    while (self.sync.?.loop()) self.cpu.step() else return;
    std.debug.assert(!self.sync.?.loop());
}

pub fn sleep(self: *TiZr80) bool {
    return if (self.sync) |*sync| sync.sleep() else false;
}

pub fn wake(self: *TiZr80) bool {
    return if (self.sync) |*sync| sync.wake() else false;
}

fn testCreate(allocator: std.mem.Allocator) !void {
    const core = try TiZr80.create(.{ .allocator = allocator });
    defer core.destroy();
}
fn testInit(allocator: std.mem.Allocator) !void {
    const core = try allocator.create(TiZr80);
    defer allocator.destroy(core);

    try core.init(.{ .allocator = allocator });
    defer core.deinit();
}
test "tizr80 create/init" {
    try testCreate(std.testing.allocator);
    try testInit(std.testing.allocator);
}
test "tizr80 create/init allocation failures" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testCreate, .{});
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testInit, .{});
}

test "tizr80 sleep/wake" {
    const core = try TiZr80.create(.{ .allocator = std.testing.allocator });
    defer core.destroy();

    try std.testing.expect(!core.sleep());
    try std.testing.expect(core.wake());
    try std.testing.expect(!core.wake());
    try std.testing.expect(core.sleep());
    try std.testing.expect(!core.sleep());
}

test "tizr80 single threaded" {
    const core = try TiZr80.create(.{ .allocator = std.testing.allocator, .threading = .SingleThreaded });
    defer core.destroy();
}

test {
    std.testing.refAllDecls(TiZr80);
    _ = @import("as.zig");
}
