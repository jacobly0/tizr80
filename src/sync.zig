const Sync = @This();

pub const Threading = enum {
    SingleThreaded,
    MultiThreaded,
};

threading: Threading,

pub fn init(self: *Sync, threading: Threading) !void {
    self.* = Sync{ .threading = threading };
}
pub fn deinit(self: *Sync) void {
    _ = self;
}

pub fn enter(self: *Sync) void {
    _ = self;
}
pub fn leave(self: *Sync) void {
    _ = self;
}
