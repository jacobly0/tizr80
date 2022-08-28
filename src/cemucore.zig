const builtin = @import("builtin");
const std = @import("std");

const Sync = @import("sync.zig");
const Mem = @import("mem.zig");
const Cpu = @import("cpu.zig");

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
    TI83PCE,
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
sync: Sync = undefined,
mem: Mem = undefined,
cpu: Cpu = undefined,
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
    try self.sync.init();
    try self.mem.init();
    try self.cpu.init();
    switch (create_options.threading) {
        .SingleThreaded => {},
        .MultiThreaded => {
            std.debug.assert(!builtin.single_threaded);
            const thread = try std.Thread.spawn(
                .{},
                runLoop,
                .{self},
            );
            const name = "cemucore";
            thread.setName(name[0..std.math.min(name.len, std.Thread.max_name_len)]) catch {};
            self.thread = thread;
        },
    }
    self.sync.start();
}

pub fn deinit(self: *CEmuCore) void {
    self.sync.stop();
    if (self.thread) |thread| thread.join();
    self.cpu.deinit();
    self.mem.deinit();
    self.sync.deinit();
}
pub fn destroy(self: *CEmuCore) void {
    self.deinit();
    self.allocator.destroy(self);
}

fn property_needs_sync(property: Property) bool {
    return switch (property) {
        .Key, .GpioEnable => false,
        else => true,
    };
}

pub fn get(self: *CEmuCore, property: Property, address: u24) ?u24 {
    const needs_sync = property_needs_sync(property);
    if (needs_sync) self.sync.enter();
    defer if (needs_sync) self.sync.leave();

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
        else => std.debug.todo("unimplemented"),
    };
}
pub fn getBuffer(self: *CEmuCore, property: Property, address: u24, buffer: []u8) void {
    _ = self;
    _ = property;
    _ = address;
    _ = buffer;
}
pub fn set(self: *CEmuCore, property: Property, address: u24, value: ?u24) void {
    const needs_sync = property_needs_sync(property);
    if (needs_sync) self.sync.enter();
    defer if (needs_sync) self.sync.leave();

    switch (property) {
        .Reg => inline for (@typeInfo(RegisterAddress).Enum.fields) |field| {
            if (address == field.value) {
                const register_address = @field(RegisterAddress, field.name);
                const register_type = Cpu.RegisterType(register_address);
                self.cpu.set(register_address, @intCast(register_type, value.?));
                break;
            }
        } else unreachable,
        .RegShadow => inline for (@typeInfo(RegisterAddress).Enum.fields) |field| {
            if (address == field.value) {
                const register_address = @field(RegisterAddress, field.name);
                const register_type = Cpu.RegisterType(register_address);
                self.cpu.setShadow(register_address, @intCast(register_type, value.?));
                break;
            }
        } else unreachable,
        else => std.debug.todo("unimplemented"),
    }
}
pub fn setBuffer(self: *CEmuCore, property: Property, address: u24, buffer: []const u8) void {
    _ = self;
    _ = property;
    _ = address;
    _ = buffer;
}
pub fn doCommand(self: *CEmuCore, arguments: [:null]?[*:0]u8) i32 {
    _ = self;
    _ = arguments;
    return 0;
}

pub fn runLoop(self: *CEmuCore) void {
    while (self.sync.loop()) {
        if (!self.sync.delay(std.time.ns_per_s / 10)) break;
    } else return;
    std.debug.assert(!self.sync.loop());
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
