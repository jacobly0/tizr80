const std = @import("std");

const CEmuCore = @import("../cemucore.zig");
const Cpu = @import("../cpu.zig");
const decode = @import("decode.zig").decode;
const Interpreter = @This();
const util = @import("../util.zig");

backend: Cpu.Backend,
halted: bool,
prefetch: u8,

pub fn create(allocator: *std.mem.Allocator) !*Cpu.Backend {
    const self = try allocator.create(Interpreter);
    errdefer allocator.destroy(self);

    self.* = .{
        .backend = .{
            .execute = execute,
            .destroy = destroy,
        },
        .halted = false,
        .prefetch = 0,
    };
    return &self.backend;
}

fn destroy(backend: *Cpu.Backend, allocator: *std.mem.Allocator) void {
    const self = @fieldParentPtr(Interpreter, "backend", backend);
    allocator.destroy(self);
}

fn execute(backend: *Cpu.Backend, core: *CEmuCore) void {
    const self = @fieldParentPtr(Interpreter, "backend", backend);
    if (self.halted) return std.debug.print("Doing nothing, halted!\n", .{});
    var state = State{ .interp = self, .core = core, .temp = undefined };
    decode(&state);
    std.debug.print(
        \\AF {X:0>4}     {X:0>4} AF'
        \\BC {X:0>6} {X:0>6} BC'
        \\DE {X:0>6} {X:0>6} DE'
        \\HL {X:0>6} {X:0>6} HL'
        \\IX {X:0>6} {X:0>6} IY
        \\PC {X:0>6} {X:0>2}
        \\
        \\
    , .{
        core.cpu.get(.af),
        core.cpu.getShadow(.af),
        core.cpu.get(.ubc),
        core.cpu.getShadow(.ubc),
        core.cpu.get(.ude),
        core.cpu.getShadow(.ude),
        core.cpu.get(.uhl),
        core.cpu.getShadow(.uhl),
        core.cpu.get(.uix),
        core.cpu.get(.uiy),
        core.cpu.get(.pc),
        self.prefetch,
    });
}

const State = struct {
    interp: *Interpreter,
    core: *CEmuCore,
    temp: u32,

    pub fn fetchByte(self: *State) void {
        const pc = switch (self.core.cpu.mode.adl) {
            .z80 => util.toBacking(Cpu.u8u16{ .short = @truncate(u16, self.core.cpu.get(.pc)), .upper = self.core.cpu.get(.mb) }),
            .ez80 => self.core.cpu.get(.pc),
        };
        self.temp = self.interp.prefetch;
        self.interp.prefetch = self.core.mem.readByte(pc);
    }

    pub fn addPC(self: *State, comptime increment: comptime_int) void {
        self.core.cpu.set(.pc, switch (self.core.cpu.mode.adl) {
            .z80 => util.toBacking(Cpu.u8u16{ .short = @truncate(u16, self.core.cpu.get(.pc)) +% increment, .upper = self.core.cpu.get(.mb) }),
            .ez80 => self.core.cpu.get(.pc) +% increment,
        });
    }

    pub fn halt(self: *State) void {
        self.interp.halted = true;
    }

    pub fn addR(self: *State, comptime increment: comptime_int) void {
        self.core.cpu.r +%= increment << 1;
    }

    pub fn dispatch(self: *State, comptime dispatcher: fn (anytype, comptime u8) void) void {
        comptime var opcode: u8 = 0;
        inline while (true) : (opcode += 1) {
            if (opcode == self.temp) return dispatcher(self, opcode);
            if (opcode == std.math.maxInt(u8)) unreachable;
        }
    }

    pub fn readRegister(self: *State, comptime address: Cpu.RegisterAddress) void {
        self.temp = self.core.cpu.get(address);
    }
    pub fn writeRegister(self: *State, comptime address: Cpu.RegisterAddress) void {
        self.core.cpu.set(address, @intCast(Cpu.RegisterType(address), self.temp));
    }
};
