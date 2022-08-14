const std = @import("std");

const Sync = @import("sync.zig");
const Mem = @import("mem.zig");
const Cpu = @import("cpu.zig");

pub const nodebug = std.meta.globalOption("nodebug", bool) orelse false;

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
    threading: Sync.Threading = if (@import("builtin").single_threaded)
        .SingleThreaded
    else
        .MultiThreaded,
    signal_handler: ?SignalHandler = null,
};

pub const Property = if (nodebug) enum {
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

    Watch,
    WatchAddr,
    WatchSize,
    WatchFlags,
    MemZ80WatchFlags,
    MemAdlWatchFlags,
    FlashWatchFlags,
    RamWatchFlags,
    PortWatchFlags,
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

pub const SignalHandler = fn (*CEmuCore, Signal) void;

const CEmuCore = @This();

allocator: std.mem.Allocator,
signal_handler: ?SignalHandler,
sync: Sync = undefined,
mem: Mem = undefined,
cpu: Cpu = undefined,

pub fn create(options: CreateOptions) !*CEmuCore {
    const self = try options.allocator.create(CEmuCore);
    errdefer options.allocator.destroy(self);

    try self.init(options);
    return self;
}
pub fn init(self: *CEmuCore, options: CreateOptions) !void {
    self.* = CEmuCore{
        .allocator = options.allocator,
        .signal_handler = options.signal_handler,
    };
    try self.sync.init(options.threading);
    try self.mem.init();
    try self.cpu.init();
}
pub fn deinit(self: *CEmuCore) void {
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
    _ = self;
    _ = property;
    _ = address;

    const needs_sync = property_needs_sync(property);
    if (needs_sync) self.sync.enter();
    defer if (needs_sync) self.sync.leave();

    return switch (property) {
        else => unreachable,
    };
}
pub fn getBuffer(self: *CEmuCore, property: Property, address: u24, buffer: []u8) void {
    _ = self;
    _ = property;
    _ = address;
    _ = buffer;
}
pub fn set(self: *CEmuCore, property: Property, address: u24, value: ?u24) void {
    _ = self;
    _ = property;
    _ = address;
    _ = value;
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

test {
    const core = try CEmuCore.create(.{ .allocator = std.testing.allocator });
    defer core.destroy();
}

test {
    const core = try std.testing.allocator.create(CEmuCore);
    defer std.testing.allocator.destroy(core);

    try core.init(.{ .allocator = std.testing.allocator });
    defer core.deinit();
}
