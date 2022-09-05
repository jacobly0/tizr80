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
}

const State = struct {
    interp: *Interpreter,
    core: *CEmuCore,
    temp: u32,

    pub fn fetchByte(self: *State) void {
        const pc = switch (self.core.cpu.mode.adl) {
            .z80 => util.toBacking(Cpu.u8u16{ .short = @truncate(u16, self.core.cpu.get(.PC)), .upper = self.core.cpu.get(.MB) }),
            .ez80 => self.core.cpu.get(.PC),
        };
        self.temp = self.interp.prefetch;
        self.interp.prefetch = self.core.mem.readByte(pc);
        std.debug.print("Fetching 0x{X:0>2} from 0x{X:0>6}\n", .{ self.interp.prefetch, pc });
    }

    pub fn addPC(self: *State, comptime increment: comptime_int) void {
        self.core.cpu.set(.PC, switch (self.core.cpu.mode.adl) {
            .z80 => util.toBacking(Cpu.u8u16{ .short = @truncate(u16, self.core.cpu.get(.PC)) +% increment, .upper = self.core.cpu.get(.MB) }),
            .ez80 => self.core.cpu.get(.PC) +% increment,
        });
    }

    pub fn halt(self: *State) void {
        self.interp.halted = true;
    }

    pub fn addR(self: *State, comptime increment: comptime_int) void {
        self.core.cpu.r +%= increment;
    }

    pub fn dispatch(self: *State, comptime dispatcher: fn (anytype, comptime u8) void) void {
        comptime var opcode: u8 = 0;
        inline while (true) : (opcode += 1) {
            if (opcode == self.temp) return dispatcher(self, opcode);
            if (opcode == std.math.maxInt(u8)) unreachable;
        }
    }
};
