const std = @import("std");

const CEmuCore = @import("../cemucore.zig");
const Cpu = @import("../cpu.zig");

pub fn create(allocator: std.mem.Allocator) !*Cpu.Backend {
    const backend = try allocator.create(Cpu.Backend);
    errdefer allocator.destroy(backend);

    backend.* = .{
        .execute = execute,
        .destroy = destroy,
    };
    return backend;
}

fn execute(_: *Cpu.Backend, _: *CEmuCore, _: Cpu.ExecuteMode) void {}

fn destroy(backend: *Cpu.Backend, allocator: std.mem.Allocator) void {
    allocator.destroy(backend);
}
