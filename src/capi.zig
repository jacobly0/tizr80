const std = @import("std");

const CEmuCore = @import("cemucore.zig");

pub const allow_todo_in_release = true;

pub const CEMUCORE_DEBUGGER = CEmuCore.options.debugger;

pub const cemucore_sig = enum(c_int) {
    CEMUCORE_SIG_DEV_CHANGED,
    CEMUCORE_SIG_FLASH_SIZE_CHANGED,
    CEMUCORE_SIG_TRANSFER_TOTAL,
    CEMUCORE_SIG_TRANSFER_PROGRESS,
    CEMUCORE_SIG_TRANSFER_COMPLETE,
    CEMUCORE_SIG_LCD_FRAME,
    CEMUCORE_SIG_SOFT_CMD,
};

pub const cemucore_create_flags = c_int;
pub const CEMUCORE_CREATE_FLAG_THREADED = @as(cemucore_create_flags, 1 << 0);

pub const cemucore_prop = if (CEMUCORE_DEBUGGER) enum(c_int) {
    CEMUCORE_PROP_DEV,
    CEMUCORE_PROP_REG,
    CEMUCORE_PROP_REG_SHADOW,
    CEMUCORE_PROP_KEY,
    CEMUCORE_PROP_FLASH_SIZE,
    CEMUCORE_PROP_MEM_Z80,
    CEMUCORE_PROP_MEM_ADL,
    CEMUCORE_PROP_FLASH,
    CEMUCORE_PROP_RAM,
    CEMUCORE_PROP_PORT,
    CEMUCORE_PROP_GPIO_ENABLE,
    CEMUCORE_PROP_TRANSFER,

    CEMUCORE_PROP_WATCH,
    CEMUCORE_PROP_WATCH_ADDR,
    CEMUCORE_PROP_WATCH_SIZE,
    CEMUCORE_PROP_WATCH_FLAGS,
    CEMUCORE_PROP_MEM_Z80_WATCH_FLAGS,
    CEMUCORE_PROP_MEM_ADL_WATCH_FLAGS,
    CEMUCORE_PROP_FLASH_WATCH_FLAGS,
    CEMUCORE_PROP_RAM_WATCH_FLAGS,
    CEMUCORE_PROP_PORT_WATCH_FLAGS,
} else enum(c_int) {
    CEMUCORE_PROP_DEV,
    CEMUCORE_PROP_REG,
    CEMUCORE_PROP_REG_SHADOW,
    CEMUCORE_PROP_KEY,
    CEMUCORE_PROP_FLASH_SIZE,
    CEMUCORE_PROP_MEM_Z80,
    CEMUCORE_PROP_MEM_ADL,
    CEMUCORE_PROP_FLASH,
    CEMUCORE_PROP_RAM,
    CEMUCORE_PROP_PORT,
    CEMUCORE_PROP_GPIO_ENABLE,
    CEMUCORE_PROP_TRANSFER,
};

pub const cemucore_dev = enum(c_int) {
    CEMUCORE_DEV_UNKNOWN,
    CEMUCORE_DEV_TI84PCE,
    CEMUCORE_DEV_TI84PCEPE,
    CEMUCORE_DEV_TI83PCE,
    CEMUCORE_DEV_TI83PCEEP,
    CEMUCORE_DEV_TI84PCET,
    CEMUCORE_DEV_TI84PCETPE,
};

pub const cemucore_transfer = enum(c_int) {
    CEMUCORE_TRANSFER_TOTAL,
    CEMUCORE_TRANSFER_PROGRESS,
    CEMUCORE_TRANSFER_REMAINING,
    CEMUCORE_TRANSFER_ERROR,
};

pub const cemucore_reg = enum(c_int) {
    // 1-bit state
    CEMUCORE_STATE_ADL,
    CEMUCORE_STATE_MADL,
    CEMUCORE_STATE_IEF,

    // 1-bit flags
    CEMUCORE_FLAG_C,
    CEMUCORE_FLAG_N,
    CEMUCORE_FLAG_PV,
    CEMUCORE_FLAG_X,
    CEMUCORE_FLAG_HC,
    CEMUCORE_FLAG_Y,
    CEMUCORE_FLAG_Z,
    CEMUCORE_FLAG_S,

    // 8-bit registers
    CEMUCORE_REG_F,
    CEMUCORE_REG_A,
    CEMUCORE_REG_C,
    CEMUCORE_REG_B,
    CEMUCORE_REG_BCU,
    CEMUCORE_REG_E,
    CEMUCORE_REG_D,
    CEMUCORE_REG_DEU,
    CEMUCORE_REG_L,
    CEMUCORE_REG_H,
    CEMUCORE_REG_HLU,
    CEMUCORE_REG_IXL,
    CEMUCORE_REG_IXH,
    CEMUCORE_REG_IXU,
    CEMUCORE_REG_IYL,
    CEMUCORE_REG_IYH,
    CEMUCORE_REG_IYU,
    CEMUCORE_REG_R,
    CEMUCORE_REG_MB,

    // 16-bit registers
    CEMUCORE_REG_AF,
    CEMUCORE_REG_BC,
    CEMUCORE_REG_DE,
    CEMUCORE_REG_HL,
    CEMUCORE_REG_IX,
    CEMUCORE_REG_IY,
    CEMUCORE_REG_SPS,
    CEMUCORE_REG_I,

    // 24-bit registers
    CEMUCORE_REG_UBC,
    CEMUCORE_REG_UDE,
    CEMUCORE_REG_UHL,
    CEMUCORE_REG_UIX,
    CEMUCORE_REG_UIY,
    CEMUCORE_REG_SPL,
    CEMUCORE_REG_PC,
    CEMUCORE_REG_RPC,
};

pub usingnamespace if (CEMUCORE_DEBUGGER) struct {
    pub const cemucore_watch_flags = c_int;

    pub const CEMUCORE_WATCH_AREA_PORT = @as(cemucore_watch_flags, 0 << 0);
    pub const CEMUCORE_WATCH_AREA_MEM = @as(cemucore_watch_flags, 1 << 0);
    pub const CEMUCORE_WATCH_AREA_FLASH = @as(cemucore_watch_flags, 2 << 0);
    pub const CEMUCORE_WATCH_AREA_RAM = @as(cemucore_watch_flags, 3 << 0);
    pub const CEMUCORE_WATCH_AREA_MASK = CEMUCORE_WATCH_AREA_PORT | CEMUCORE_WATCH_AREA_MEM |
        CEMUCORE_WATCH_AREA_FLASH | CEMUCORE_WATCH_AREA_RAM;

    pub const CEMUCORE_WATCH_MODE_PORT = @as(cemucore_watch_flags, 0 << 2);
    pub const CEMUCORE_WATCH_MODE_Z80 = @as(cemucore_watch_flags, 1 << 2);
    pub const CEMUCORE_WATCH_MODE_ADL = @as(cemucore_watch_flags, 2 << 2);
    pub const CEMUCORE_WATCH_MODE_ANY = CEMUCORE_WATCH_MODE_Z80 | CEMUCORE_WATCH_MODE_ADL;

    pub const CEMUCORE_WATCH_TYPE_READ = @as(cemucore_watch_flags, 1 << 4);
    pub const CEMUCORE_WATCH_TYPE_WRITE = @as(cemucore_watch_flags, 1 << 5);
    pub const CEMUCORE_WATCH_TYPE_READ_WRITE = CEMUCORE_WATCH_TYPE_READ | CEMUCORE_WATCH_TYPE_WRITE;
    pub const CEMUCORE_WATCH_TYPE_EXECUTE = @as(cemucore_watch_flags, 1 << 6);
    pub const CEMUCORE_WATCH_TYPE_ALL = CEMUCORE_WATCH_TYPE_READ_WRITE | CEMUCORE_WATCH_TYPE_EXECUTE;

    pub const CEMUCORE_WATCH_ENABLE = @as(cemucore_watch_flags, 1 << 7);
} else struct {};

pub const cemucore = anyopaque;
pub const cemucore_sig_handler = *const fn (cemucore_sig, ?*anyopaque) callconv(.C) void;

fn AllocatorWrapper(backing_allocator: std.mem.Allocator) type {
    return struct {
        fn allocator(_: *@This()) std.mem.Allocator {
            return backing_allocator;
        }
        fn deinit(_: *@This()) bool {
            return false;
        }
    };
}

const CoreWrapper = struct {
    const backing_allocator = std.heap.c_allocator;
    const allocator_init = if (std.debug.runtime_safety)
        std.heap.GeneralPurposeAllocator(.{}){ .backing_allocator = backing_allocator }
    else
        AllocatorWrapper(backing_allocator){};

    allocator: @TypeOf(allocator_init) = allocator_init,
    sig: struct {
        handler: cemucore_sig_handler,
        data: ?*anyopaque,
    },
    core: CEmuCore = undefined,

    fn coreToSelf(core: *CEmuCore) *CoreWrapper {
        return @fieldParentPtr(CoreWrapper, "core", core);
    }
    fn handlerWrapper(core: *CEmuCore, signal: CEmuCore.Signal) void {
        const self = coreToSelf(core).sig;
        return self.handler(@intToEnum(cemucore_sig, @enumToInt(signal)), self.data);
    }

    fn create(
        create_flags: cemucore_create_flags,
        sig_handler: cemucore_sig_handler,
        sig_handler_data: ?*anyopaque,
    ) !*cemucore {
        const self = try backing_allocator.create(CoreWrapper);
        errdefer backing_allocator.destroy(self);

        self.* = CoreWrapper{ .sig = .{ .handler = sig_handler, .data = sig_handler_data } };
        try self.core.init(.{
            .allocator = self.allocator.allocator(),
            .threading = if (create_flags & CEMUCORE_CREATE_FLAG_THREADED != 0)
                .MultiThreaded
            else
                .SingleThreaded,
            .signal_handler = handlerWrapper,
        });
        return &self.core;
    }
    fn destroy(core: *CEmuCore) void {
        const self = coreToSelf(core);
        core.deinit();
        if (self.allocator.deinit()) std.c.abort();
        backing_allocator.destroy(self);
    }
};

export fn cemucore_create(
    create_flags: cemucore_create_flags,
    sig_handler: cemucore_sig_handler,
    sig_handler_data: ?*anyopaque,
) callconv(.C) ?*cemucore {
    return CoreWrapper.create(create_flags, sig_handler, sig_handler_data) catch null;
}
export fn cemucore_destroy(core: *cemucore) callconv(.C) void {
    return CoreWrapper.destroy(@ptrCast(*CEmuCore, core));
}

export fn cemucore_get(
    core: *cemucore,
    prop: cemucore_prop,
    addr: i32,
) callconv(.C) i32 {
    return @ptrCast(*CEmuCore, core).get(
        @intToEnum(CEmuCore.Property, @enumToInt(prop)),
        std.math.cast(u24, addr) orelse return -1,
    ) orelse -1;
}
export fn cemucore_get_buffer(
    core: *cemucore,
    prop: cemucore_prop,
    addr: i32,
    buf: *anyopaque,
    len: u32,
) callconv(.C) void {
    return @ptrCast(*CEmuCore, core).getSlice(
        @intToEnum(CEmuCore.Property, @enumToInt(prop)),
        std.math.cast(u24, addr) orelse return,
        @ptrCast([*]u8, buf)[0..len],
    );
}
export fn cemucore_set(
    core: *cemucore,
    prop: cemucore_prop,
    addr: i32,
    val: i32,
) callconv(.C) void {
    return @ptrCast(*CEmuCore, core).set(
        @intToEnum(CEmuCore.Property, @enumToInt(prop)),
        std.math.cast(u24, addr) orelse return,
        std.math.cast(u24, val),
    );
}
export fn cemucore_set_buffer(
    core: *cemucore,
    prop: cemucore_prop,
    addr: i32,
    buf: *const anyopaque,
    len: u32,
) callconv(.C) void {
    return @ptrCast(*CEmuCore, core).setSlice(
        @intToEnum(CEmuCore.Property, @enumToInt(prop)),
        std.math.cast(u24, addr) orelse return,
        @ptrCast([*]const u8, buf)[0..len],
    );
}
export fn cemucore_command(
    core: *cemucore,
    command: [*:null]?[*:0]u8,
) callconv(.C) c_int {
    return @ptrCast(*CEmuCore, core).doCommand(std.mem.sliceTo(command, null));
}

export fn cemucore_sleep(core: *cemucore) bool {
    return @ptrCast(*CEmuCore, core).sleep();
}
export fn cemucore_wake(core: *cemucore) bool {
    return @ptrCast(*CEmuCore, core).wake();
}
