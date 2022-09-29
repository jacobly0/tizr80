const std = @import("std");

const Cpu = @import("cpu.zig");
const Keypad = @import("ports/keypad.zig");
const TiZr80 = @import("tizr80.zig");
const util = @import("util.zig");

pub const allow_todo_in_release = true;

pub const TIZR80_DEBUGGER = TiZr80.options.debugger;

pub const tizr80_sig = enum(c_int) {
    TIZR80_SIG_DEV_CHANGED,
    TIZR80_SIG_FLASH_SIZE_CHANGED,
    TIZR80_SIG_TRANSFER_TOTAL,
    TIZR80_SIG_TRANSFER_PROGRESS,
    TIZR80_SIG_TRANSFER_COMPLETE,
    TIZR80_SIG_LCD_FRAME,
    TIZR80_SIG_SOFT_CMD,
};

pub const tizr80_create_flags = c_int;
pub const TIZR80_CREATE_FLAG_THREADED = @as(tizr80_create_flags, 1 << 0);

pub const tizr80_prop = enum(c_int) {
    TIZR80_PROP_DEV,
    TIZR80_PROP_REG,
    TIZR80_PROP_REG_SHADOW,
    TIZR80_PROP_KEY,
    TIZR80_PROP_FLASH_SIZE,
    TIZR80_PROP_MEM_Z80,
    TIZR80_PROP_MEM_ADL,
    TIZR80_PROP_FLASH,
    TIZR80_PROP_RAM,
    TIZR80_PROP_PORT,
    TIZR80_PROP_TRANSFER,

    TIZR80_PROP_WATCH,
    TIZR80_PROP_WATCH_ADDR,
    TIZR80_PROP_WATCH_SIZE,
    TIZR80_PROP_WATCH_FLAGS,
    TIZR80_PROP_MEM_Z80_WATCH_FLAGS,
    TIZR80_PROP_MEM_ADL_WATCH_FLAGS,
    TIZR80_PROP_FLASH_WATCH_FLAGS,
    TIZR80_PROP_RAM_WATCH_FLAGS,
    TIZR80_PROP_PORT_WATCH_FLAGS,

    pub fn unwrapKey(self: tizr80_prop, addr: i32) TiZr80.Property.Key {
        return switch (self) {
            .TIZR80_PROP_DEV => .{ .device = @intToEnum(tizr80_dev, addr).unwrap() },
            .TIZR80_PROP_REG => .{ .register = @intToEnum(tizr80_reg, addr).unwrap() },
            .TIZR80_PROP_REG_SHADOW => .{ .shadow_register = @intToEnum(tizr80_reg, addr).unwrap() },
            .TIZR80_PROP_KEY => .{ .key = util.fromBacking(Keypad.Key, @intCast(u8, addr)) },
            .TIZR80_PROP_FLASH_SIZE => .{ .flash_size = @intCast(u24, addr) },
            .TIZR80_PROP_MEM_Z80 => .{ .memory_z80 = @intCast(u16, addr) },
            .TIZR80_PROP_MEM_ADL => .{ .memory_adl = @intCast(u16, addr) },
            .TIZR80_PROP_FLASH => .{ .flash = @intCast(u23, addr) },
            .TIZR80_PROP_RAM => .{ .ram = @intCast(u19, addr) },
            .TIZR80_PROP_PORT => .{ .port = @intCast(u16, addr) },
            .TIZR80_PROP_TRANSFER => .{ .transfer = @intToEnum(tizr80_transfer, addr).unwrap() },
            .TIZR80_PROP_WATCH => .{ .watch = switch (addr) {
                else => @intCast(u24, addr),
                -1 => null,
            } },
            .TIZR80_PROP_WATCH_ADDR => .{ .watch_address = @intCast(u24, addr) },
            .TIZR80_PROP_WATCH_SIZE => .{ .watch_size = @intCast(u24, addr) },
            .TIZR80_PROP_WATCH_FLAGS => .{ .watch_flags = @intCast(u24, addr) },
            .TIZR80_PROP_MEM_Z80_WATCH_FLAGS => .{ .memory_z80_watch_flags = @intCast(u24, addr) },
            .TIZR80_PROP_MEM_ADL_WATCH_FLAGS => .{ .memory_adl_watch_flags = @intCast(u24, addr) },
            .TIZR80_PROP_FLASH_WATCH_FLAGS => .{ .flash_watch_flags = @intCast(u24, addr) },
            .TIZR80_PROP_RAM_WATCH_FLAGS => .{ .ram_watch_flags = @intCast(u24, addr) },
            .TIZR80_PROP_PORT_WATCH_FLAGS => .{ .port_watch_flags = @intCast(u24, addr) },
        };
    }
};

pub const tizr80_dev = enum(c_int) {
    TIZR80_DEV_UNKNOWN,
    TIZR80_DEV_TI84PCE,
    TIZR80_DEV_TI84PCEPE,
    TIZR80_DEV_TI83PCE,
    TIZR80_DEV_TI83PCEEP,
    TIZR80_DEV_TI84PCET,
    TIZR80_DEV_TI84PCETPE,

    pub fn unwrap(self: tizr80_dev) TiZr80.Device {
        return switch (self) {
            .TIZR80_DEV_UNKNOWN => TiZr80.Device.unknown,
            .TIZR80_DEV_TI84PCE => TiZr80.Device.ti84pce,
            .TIZR80_DEV_TI84PCEPE => TiZr80.Device.ti84pcepe,
            .TIZR80_DEV_TI83PCE => TiZr80.Device.ti83pce,
            .TIZR80_DEV_TI83PCEEP => TiZr80.Device.ti83pceep,
            .TIZR80_DEV_TI84PCET => TiZr80.Device.ti84pcet,
            .TIZR80_DEV_TI84PCETPE => TiZr80.Device.ti84pcetpe,
        };
    }
};

pub const tizr80_transfer = enum(c_int) {
    TIZR80_TRANSFER_TOTAL,
    TIZR80_TRANSFER_PROGRESS,
    TIZR80_TRANSFER_REMAINING,
    TIZR80_TRANSFER_ERROR,

    pub fn unwrap(self: tizr80_transfer) TiZr80.Transfer {
        return switch (self) {
            .TIZR80_TRANSFER_TOTAL => .total,
            .TIZR80_TRANSFER_PROGRESS => .progress,
            .TIZR80_TRANSFER_REMAINING => .remaining,
            .TIZR80_TRANSFER_ERROR => .@"error",
        };
    }
};

pub const tizr80_reg = enum(c_int) {
    // state
    TIZR80_STATE_ADL,
    TIZR80_STATE_IEF,
    TIZR80_STATE_IM,

    // 1-bit flags
    TIZR80_FLAG_C,
    TIZR80_FLAG_N,
    TIZR80_FLAG_PV,
    TIZR80_FLAG_X,
    TIZR80_FLAG_H,
    TIZR80_FLAG_Y,
    TIZR80_FLAG_Z,
    TIZR80_FLAG_S,

    // 8-bit registers
    TIZR80_REG_F,
    TIZR80_REG_A,
    TIZR80_REG_C,
    TIZR80_REG_B,
    TIZR80_REG_BCU,
    TIZR80_REG_E,
    TIZR80_REG_D,
    TIZR80_REG_DEU,
    TIZR80_REG_L,
    TIZR80_REG_H,
    TIZR80_REG_HLU,
    TIZR80_REG_IXL,
    TIZR80_REG_IXH,
    TIZR80_REG_IXU,
    TIZR80_REG_IYL,
    TIZR80_REG_IYH,
    TIZR80_REG_IYU,
    TIZR80_REG_I,
    TIZR80_REG_R,
    TIZR80_REG_MBASE,

    // 16-bit registers
    TIZR80_REG_AF,
    TIZR80_REG_BC,
    TIZR80_REG_DE,
    TIZR80_REG_HL,
    TIZR80_REG_IX,
    TIZR80_REG_IY,
    TIZR80_REG_SPS,
    TIZR80_REG_UI,

    // 24-bit registers
    TIZR80_REG_UBC,
    TIZR80_REG_UDE,
    TIZR80_REG_UHL,
    TIZR80_REG_UIX,
    TIZR80_REG_UIY,
    TIZR80_REG_SPL,
    TIZR80_REG_MBASEUI,
    TIZR80_REG_PC,

    pub fn unwrap(self: tizr80_reg) Cpu.RegisterId {
        return switch (self) {
            .TIZR80_STATE_ADL => .adl,
            .TIZR80_STATE_IEF => .ief,
            .TIZR80_STATE_IM => .im,
            .TIZR80_FLAG_C => .cf,
            .TIZR80_FLAG_N => .nf,
            .TIZR80_FLAG_PV => .pv,
            .TIZR80_FLAG_X => .xf,
            .TIZR80_FLAG_H => .hc,
            .TIZR80_FLAG_Y => .yf,
            .TIZR80_FLAG_Z => .zf,
            .TIZR80_FLAG_S => .sf,
            .TIZR80_REG_F => .f,
            .TIZR80_REG_A => .a,
            .TIZR80_REG_C => .c,
            .TIZR80_REG_B => .b,
            .TIZR80_REG_BCU => .bcu,
            .TIZR80_REG_E => .e,
            .TIZR80_REG_D => .d,
            .TIZR80_REG_DEU => .deu,
            .TIZR80_REG_L => .l,
            .TIZR80_REG_H => .h,
            .TIZR80_REG_HLU => .hlu,
            .TIZR80_REG_IXL => .ixl,
            .TIZR80_REG_IXH => .ixh,
            .TIZR80_REG_IXU => .ixu,
            .TIZR80_REG_IYL => .iyl,
            .TIZR80_REG_IYH => .iyh,
            .TIZR80_REG_IYU => .iyu,
            .TIZR80_REG_I => .i,
            .TIZR80_REG_R => .r,
            .TIZR80_REG_MBASE => .mbase,
            .TIZR80_REG_AF => .af,
            .TIZR80_REG_BC => .bc,
            .TIZR80_REG_DE => .de,
            .TIZR80_REG_HL => .hl,
            .TIZR80_REG_IX => .ix,
            .TIZR80_REG_IY => .iy,
            .TIZR80_REG_SPS => .sps,
            .TIZR80_REG_UI => .ui,
            .TIZR80_REG_UBC => .ubc,
            .TIZR80_REG_UDE => .ude,
            .TIZR80_REG_UHL => .uhl,
            .TIZR80_REG_UIX => .uix,
            .TIZR80_REG_UIY => .uiy,
            .TIZR80_REG_SPL => .spl,
            .TIZR80_REG_MBASEUI => .mbaseui,
            .TIZR80_REG_PC => .pc,
        };
    }
};

pub const tizr80_watch_flags = c_int;

pub const TIZR80_WATCH_AREA_PORT = @as(tizr80_watch_flags, 0 << 0);
pub const TIZR80_WATCH_AREA_MEM = @as(tizr80_watch_flags, 1 << 0);
pub const TIZR80_WATCH_AREA_FLASH = @as(tizr80_watch_flags, 2 << 0);
pub const TIZR80_WATCH_AREA_RAM = @as(tizr80_watch_flags, 3 << 0);
pub const TIZR80_WATCH_AREA_MASK = TIZR80_WATCH_AREA_PORT | TIZR80_WATCH_AREA_MEM |
    TIZR80_WATCH_AREA_FLASH | TIZR80_WATCH_AREA_RAM;

pub const TIZR80_WATCH_MODE_PORT = @as(tizr80_watch_flags, 0 << 2);
pub const TIZR80_WATCH_MODE_Z80 = @as(tizr80_watch_flags, 1 << 2);
pub const TIZR80_WATCH_MODE_ADL = @as(tizr80_watch_flags, 2 << 2);
pub const TIZR80_WATCH_MODE_ANY = TIZR80_WATCH_MODE_Z80 | TIZR80_WATCH_MODE_ADL;

pub const TIZR80_WATCH_TYPE_READ = @as(tizr80_watch_flags, 1 << 4);
pub const TIZR80_WATCH_TYPE_WRITE = @as(tizr80_watch_flags, 1 << 5);
pub const TIZR80_WATCH_TYPE_READ_WRITE = TIZR80_WATCH_TYPE_READ | TIZR80_WATCH_TYPE_WRITE;
pub const TIZR80_WATCH_TYPE_EXECUTE = @as(tizr80_watch_flags, 1 << 6);
pub const TIZR80_WATCH_TYPE_ALL = TIZR80_WATCH_TYPE_READ_WRITE | TIZR80_WATCH_TYPE_EXECUTE;

pub const TIZR80_WATCH_ENABLE = @as(tizr80_watch_flags, 1 << 7);

pub const tizr80 = opaque {
    fn unwrap(core: *tizr80) *TiZr80 {
        return @ptrCast(*TiZr80, @alignCast(@alignOf(TiZr80), core));
    }
    fn wrap(core: *TiZr80) *tizr80 {
        return @ptrCast(*tizr80, core);
    }
};
pub const tizr80_sig_handler = *const fn (tizr80_sig, ?*anyopaque) callconv(.C) void;

fn AllocatorWrapper(comptime _: struct {}) type {
    return struct {
        backing_allocator: std.mem.Allocator,

        fn allocator(self: *@This()) std.mem.Allocator {
            return self.backing_allocator;
        }
        fn deinit(self: *@This()) bool {
            self.* = undefined;
            return false;
        }
    };
}

const CoreWrapper = struct {
    const backing_allocator = std.heap.c_allocator;
    const Allocator = if (std.debug.runtime_safety)
        std.heap.GeneralPurposeAllocator
    else
        AllocatorWrapper;
    const allocator_init = Allocator(.{}){ .backing_allocator = backing_allocator };

    allocator: @TypeOf(allocator_init) = allocator_init,
    sig: struct {
        handler: tizr80_sig_handler,
        data: ?*anyopaque,
    },
    core: TiZr80 = undefined,

    fn coreToSelf(core: *TiZr80) *CoreWrapper {
        return @fieldParentPtr(CoreWrapper, "core", core);
    }
    fn handlerWrapper(core: *TiZr80, signal: TiZr80.Signal) void {
        const self = coreToSelf(core).sig;
        return self.handler(@intToEnum(tizr80_sig, @enumToInt(signal)), self.data);
    }

    fn create(
        create_flags: tizr80_create_flags,
        sig_handler: tizr80_sig_handler,
        sig_handler_data: ?*anyopaque,
    ) !*tizr80 {
        const self = try backing_allocator.create(CoreWrapper);
        errdefer backing_allocator.destroy(self);

        self.* = CoreWrapper{ .sig = .{ .handler = sig_handler, .data = sig_handler_data } };
        try self.core.init(.{
            .allocator = self.allocator.allocator(),
            .threading = if (create_flags & TIZR80_CREATE_FLAG_THREADED != 0)
                .MultiThreaded
            else
                .SingleThreaded,
            .signal_handler = handlerWrapper,
        });
        return tizr80.wrap(&self.core);
    }
    fn destroy(core: *TiZr80) void {
        const self = coreToSelf(core);
        core.deinit();
        if (self.allocator.deinit()) std.c.abort();
        backing_allocator.destroy(self);
    }
};

export fn tizr80_create(
    create_flags: tizr80_create_flags,
    sig_handler: tizr80_sig_handler,
    sig_handler_data: ?*anyopaque,
) callconv(.C) ?*tizr80 {
    return CoreWrapper.create(create_flags, sig_handler, sig_handler_data) catch null;
}
export fn tizr80_destroy(core: *tizr80) callconv(.C) void {
    return CoreWrapper.destroy(core.unwrap());
}

export fn tizr80_get(core: *tizr80, prop: tizr80_prop, addr: i32) callconv(.C) i32 {
    return core.unwrap().get(prop.unwrapKey(addr)) orelse -1;
}
export fn tizr80_get_buffer(
    core: *tizr80,
    prop: tizr80_prop,
    addr: i32,
    buf: [*]u8,
    len: u32,
) callconv(.C) void {
    return core.unwrap().getSlice(prop.unwrapKey(addr), buf[0..len]);
}
export fn tizr80_set(core: *tizr80, prop: tizr80_prop, addr: i32, val: i32) callconv(.C) void {
    return core.unwrap().set(prop.unwrapKey(addr), switch (val) {
        else => @intCast(u24, val),
        -1 => null,
    });
}
export fn tizr80_set_buffer(
    core: *tizr80,
    prop: tizr80_prop,
    addr: i32,
    buf: [*]const u8,
    len: u32,
) callconv(.C) void {
    return core.unwrap().setSlice(prop.unwrapKey(addr), buf[0..len]);
}

fn wrapCommandResult(result: TiZr80.CommandSplitError!i32) c_int {
    return result catch |err|
        return -@as(c_int, @enumToInt(switch (err) {
        error.InvalidEscape,
        error.UnterminatedString,
        error.InvalidCommand,
        error.BadPathName,
        error.InvalidUtf8,
        => std.c.E.INVAL,
        error.OutOfMemory => std.c.E.NOMEM,
        error.AccessDenied, error.LockViolation => std.c.E.ACCES,
        error.DeviceBusy => std.c.E.BUSY,
        error.FileBusy => std.c.E.TXTBSY,
        error.FileLocksNotSupported => std.c.E.OPNOTSUPP,
        error.FileNotFound => std.c.E.NOENT,
        error.FileTooBig => std.c.E.FBIG,
        error.InvalidHandle, error.NotOpenForReading, error.NotOpenForWriting => std.c.E.BADF,
        error.IsDir => std.c.E.ISDIR,
        error.NameTooLong => std.c.E.NAMETOOLONG,
        error.NoDevice => std.c.E.NODEV,
        error.NoSpaceLeft => std.c.E.NOSPC,
        error.NotDir => std.c.E.NOTDIR,
        error.PathAlreadyExists => std.c.E.EXIST,
        error.BrokenPipe, error.PipeBusy => std.c.E.PIPE,
        error.ProcessFdQuotaExceeded => std.c.E.MFILE,
        error.SharingViolation => std.c.E.PERM,
        error.SymLinkLoop => std.c.E.LOOP,
        error.SystemFdQuotaExceeded => std.c.E.NFILE,
        error.SystemResources => std.c.E.NOBUFS,
        error.WouldBlock => std.c.E.AGAIN,
        error.ConnectionResetByPeer => std.c.E.CONNRESET,
        error.ConnectionTimedOut => std.c.E.TIMEDOUT,
        error.DiskQuota => std.c.E.DQUOT,
        error.InputOutput => std.c.E.IO,
        error.OperationAborted => std.c.E.CANCELED,
        error.Unexpected => std.c.E.NOTRECOVERABLE,
    }));
}
export fn tizr80_command_split(core: *tizr80, command: [*:0]const u8) callconv(.C) c_int {
    return wrapCommandResult(core.unwrap().commandSplit(std.mem.span(command)));
}
export fn tizr80_command(core: *tizr80, command: [*:null]const ?[*:0]const u8) callconv(.C) c_int {
    return wrapCommandResult(core.unwrap().commandPointers(command));
}

export fn tizr80_sleep(core: *tizr80) bool {
    return core.unwrap().sleep();
}
export fn tizr80_wake(core: *tizr80) bool {
    return core.unwrap().wake();
}
