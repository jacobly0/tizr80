const std = @import("std");

const CEmuCore = @import("../cemucore.zig");
const Cpu = @import("../cpu.zig");

pub fn create(allocator: std.mem.Allocator) !*Cpu.Backend {
    const backend = try allocator.create(Cpu.Backend);
    errdefer allocator.destroy(backend);

    backend.* = .{
        .flush = flush,
        .step = step,
        .destroy = destroy,
    };
    return backend;
}

fn flush(_: *Cpu.Backend, _: *CEmuCore) void {}

fn step(_: *Cpu.Backend, _: *CEmuCore) void {}

fn destroy(backend: *Cpu.Backend, allocator: std.mem.Allocator) void {
    allocator.destroy(backend);
}
