const Mem = @This();

pub fn init(self: *Mem) !void {
    self.* = Mem{};
}
pub fn deinit(self: *Mem) void {
    _ = self;
}
