const CEmuCore = @import("cemucore.zig");
const Mem = @This();

flash: []u8,
ram: *[0x40000 + 320 * 240 * 2]u8,

pub fn init(self: *Mem) !void {
    const allocator = @fieldParentPtr(CEmuCore, "mem", self).allocator;

    const flash = try allocator.alloc(u8, 0x400000);
    errdefer allocator.free(flash);

    const ram = try allocator.create(@TypeOf(self.ram.*));
    errdefer allocator.destroy(ram);

    self.* = Mem{
        .flash = flash,
        .ram = ram,
    };
}
pub fn deinit(self: *Mem) void {
    const allocator = @fieldParentPtr(CEmuCore, "mem", self).allocator;

    allocator.destroy(self.ram);
    allocator.free(self.flash);
}
