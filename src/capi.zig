const std = @import("std");

const CEmuCore = @import("cemucore.zig");
const Cpu = @import("cpu.zig");
const Keypad = @import("keypad.zig");
const util = @import("util.zig");

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

pub const cemucore_prop = enum(c_int) {
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

    pub fn unwrapKey(self: cemucore_prop, addr: i32) CEmuCore.Property.Key {
        return switch (self) {
            .CEMUCORE_PROP_DEV => .{ .device = @intToEnum(cemucore_dev, addr).unwrap() },
            .CEMUCORE_PROP_REG => .{ .register = @intToEnum(cemucore_reg, addr).unwrap() },
            .CEMUCORE_PROP_REG_SHADOW => .{ .shadow_register = @intToEnum(cemucore_reg, addr).unwrap() },
            .CEMUCORE_PROP_KEY => .{ .key = util.fromBacking(Keypad.Key, @intCast(u8, addr)) },
            .CEMUCORE_PROP_FLASH_SIZE => .{ .flash_size = @intCast(u24, addr) },
            .CEMUCORE_PROP_MEM_Z80 => .{ .memory_z80 = @intCast(u16, addr) },
            .CEMUCORE_PROP_MEM_ADL => .{ .memory_adl = @intCast(u16, addr) },
            .CEMUCORE_PROP_FLASH => .{ .flash = @intCast(u23, addr) },
            .CEMUCORE_PROP_RAM => .{ .ram = @intCast(u19, addr) },
            .CEMUCORE_PROP_PORT => .{ .port = @intCast(u16, addr) },
            .CEMUCORE_PROP_TRANSFER => .{ .transfer = @intToEnum(cemucore_transfer, addr).unwrap() },
            .CEMUCORE_PROP_WATCH => .{ .watch = switch (addr) {
                else => @intCast(u24, addr),
                -1 => null,
            } },
            .CEMUCORE_PROP_WATCH_ADDR => .{ .watch_address = @intCast(u24, addr) },
            .CEMUCORE_PROP_WATCH_SIZE => .{ .watch_size = @intCast(u24, addr) },
            .CEMUCORE_PROP_WATCH_FLAGS => .{ .watch_flags = @intCast(u24, addr) },
            .CEMUCORE_PROP_MEM_Z80_WATCH_FLAGS => .{ .memory_z80_watch_flags = @intCast(u24, addr) },
            .CEMUCORE_PROP_MEM_ADL_WATCH_FLAGS => .{ .memory_adl_watch_flags = @intCast(u24, addr) },
            .CEMUCORE_PROP_FLASH_WATCH_FLAGS => .{ .flash_watch_flags = @intCast(u24, addr) },
            .CEMUCORE_PROP_RAM_WATCH_FLAGS => .{ .ram_watch_flags = @intCast(u24, addr) },
            .CEMUCORE_PROP_PORT_WATCH_FLAGS => .{ .port_watch_flags = @intCast(u24, addr) },
        };
    }
};

pub const cemucore_dev = enum(c_int) {
    CEMUCORE_DEV_UNKNOWN,
    CEMUCORE_DEV_TI84PCE,
    CEMUCORE_DEV_TI84PCEPE,
    CEMUCORE_DEV_TI83PCE,
    CEMUCORE_DEV_TI83PCEEP,
    CEMUCORE_DEV_TI84PCET,
    CEMUCORE_DEV_TI84PCETPE,

    pub fn unwrap(self: cemucore_dev) CEmuCore.Device {
        return switch (self) {
            .CEMUCORE_DEV_UNKNOWN => CEmuCore.Device.unknown,
            .CEMUCORE_DEV_TI84PCE => CEmuCore.Device.ti84pce,
            .CEMUCORE_DEV_TI84PCEPE => CEmuCore.Device.ti84pcepe,
            .CEMUCORE_DEV_TI83PCE => CEmuCore.Device.ti83pce,
            .CEMUCORE_DEV_TI83PCEEP => CEmuCore.Device.ti83pceep,
            .CEMUCORE_DEV_TI84PCET => CEmuCore.Device.ti84pcet,
            .CEMUCORE_DEV_TI84PCETPE => CEmuCore.Device.ti84pcetpe,
        };
    }
};

pub const cemucore_transfer = enum(c_int) {
    CEMUCORE_TRANSFER_TOTAL,
    CEMUCORE_TRANSFER_PROGRESS,
    CEMUCORE_TRANSFER_REMAINING,
    CEMUCORE_TRANSFER_ERROR,

    pub fn unwrap(self: cemucore_transfer) CEmuCore.Transfer {
        return switch (self) {
            .CEMUCORE_TRANSFER_TOTAL => .total,
            .CEMUCORE_TRANSFER_PROGRESS => .progress,
            .CEMUCORE_TRANSFER_REMAINING => .remaining,
            .CEMUCORE_TRANSFER_ERROR => .@"error",
        };
    }
};

pub const cemucore_reg = enum(c_int) {
    // 1-bit state
    CEMUCORE_STATE_ADL,
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

    pub fn unwrap(self: cemucore_reg) Cpu.RegisterId {
        return switch (self) {
            .CEMUCORE_STATE_ADL => .adl,
            .CEMUCORE_STATE_IEF => .ief,
            .CEMUCORE_FLAG_C => .cf,
            .CEMUCORE_FLAG_N => .nf,
            .CEMUCORE_FLAG_PV => .pv,
            .CEMUCORE_FLAG_X => .xf,
            .CEMUCORE_FLAG_HC => .hc,
            .CEMUCORE_FLAG_Y => .yf,
            .CEMUCORE_FLAG_Z => .zf,
            .CEMUCORE_FLAG_S => .sf,
            .CEMUCORE_REG_F => .f,
            .CEMUCORE_REG_A => .a,
            .CEMUCORE_REG_C => .c,
            .CEMUCORE_REG_B => .b,
            .CEMUCORE_REG_BCU => .bcu,
            .CEMUCORE_REG_E => .e,
            .CEMUCORE_REG_D => .d,
            .CEMUCORE_REG_DEU => .deu,
            .CEMUCORE_REG_L => .l,
            .CEMUCORE_REG_H => .h,
            .CEMUCORE_REG_HLU => .hlu,
            .CEMUCORE_REG_IXL => .ixl,
            .CEMUCORE_REG_IXH => .ixh,
            .CEMUCORE_REG_IXU => .ixu,
            .CEMUCORE_REG_IYL => .iyl,
            .CEMUCORE_REG_IYH => .iyh,
            .CEMUCORE_REG_IYU => .iyu,
            .CEMUCORE_REG_R => .r,
            .CEMUCORE_REG_MB => .mb,
            .CEMUCORE_REG_AF => .af,
            .CEMUCORE_REG_BC => .bc,
            .CEMUCORE_REG_DE => .de,
            .CEMUCORE_REG_HL => .hl,
            .CEMUCORE_REG_IX => .ix,
            .CEMUCORE_REG_IY => .iy,
            .CEMUCORE_REG_SPS => .sps,
            .CEMUCORE_REG_I => .i,
            .CEMUCORE_REG_UBC => .ubc,
            .CEMUCORE_REG_UDE => .ude,
            .CEMUCORE_REG_UHL => .uhl,
            .CEMUCORE_REG_UIX => .uix,
            .CEMUCORE_REG_UIY => .uiy,
            .CEMUCORE_REG_SPL => .spl,
            .CEMUCORE_REG_PC => .pc,
        };
    }
};

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

pub const cemucore = opaque {
    fn unwrap(core: *cemucore) *CEmuCore {
        return @ptrCast(*CEmuCore, core);
    }
    fn wrap(core: *CEmuCore) *cemucore {
        return @ptrCast(*cemucore, core);
    }
};
pub const cemucore_sig_handler = *const fn (cemucore_sig, ?*anyopaque) callconv(.C) void;

fn AllocatorWrapper(comptime _: struct {}) type {
    return struct {
        backing_allocator: std.mem.Allocator,

        fn allocator(self: *@This()) std.mem.Allocator {
            return self.backing_allocator;
        }
        fn deinit(_: *@This()) bool {
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
        return cemucore.wrap(&self.core);
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

export fn cemucore_get(core: *cemucore, prop: cemucore_prop, addr: i32) callconv(.C) i32 {
    return core.unwrap().get(prop.unwrapKey(addr)) orelse -1;
}
export fn cemucore_get_buffer(
    core: *cemucore,
    prop: cemucore_prop,
    addr: i32,
    buf: [*]u8,
    len: u32,
) callconv(.C) void {
    return core.unwrap().getSlice(prop.unwrapKey(addr), buf[0..len]);
}
export fn cemucore_set(core: *cemucore, prop: cemucore_prop, addr: i32, val: i32) callconv(.C) void {
    return @ptrCast(*CEmuCore, core).set(prop.unwrapKey(addr), switch (val) {
        else => @intCast(u24, val),
        -1 => null,
    });
}
export fn cemucore_set_buffer(
    core: *cemucore,
    prop: cemucore_prop,
    addr: i32,
    buf: [*]const u8,
    len: u32,
) callconv(.C) void {
    return @ptrCast(*CEmuCore, core).setSlice(prop.unwrapKey(addr), buf[0..len]);
}
export fn cemucore_command(core: *cemucore, command: [*:null]?[*:0]u8) callconv(.C) c_int {
    return @ptrCast(*CEmuCore, core).doCommand(std.mem.sliceTo(command, null));
}

export fn cemucore_sleep(core: *cemucore) bool {
    return @ptrCast(*CEmuCore, core).sleep();
}
export fn cemucore_wake(core: *cemucore) bool {
    return @ptrCast(*CEmuCore, core).wake();
}
