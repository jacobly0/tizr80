const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn SmallArrayList(comptime T: type, comptime small_capacity: usize) type {
    return struct {
        const Self = @This();

        contents: union { small: [small_capacity]T, big: std.ArrayListUnmanaged(T) } = undefined,
        small_len: std.meta.Int(.unsigned, std.math.log2_int_ceil(usize, small_capacity + 1)) = 0,

        pub fn isSmall(self: *Self) bool {
            return self.small_len <= small_capacity;
        }

        pub fn deinit(self: *Self, allocator: Allocator) void {
            if (!self.isSmall()) self.contents.big.deinit(allocator);
            self.* = undefined;
        }

        pub fn items(self: *Self) []T {
            return if (self.isSmall())
                self.contents.small[0..self.small_len]
            else
                self.contents.big.items;
        }

        pub fn append(self: *Self, allocator: Allocator, item: T) Allocator.Error!void {
            if (self.isSmall()) {
                self.small_len += 1;
                if (self.isSmall())
                    self.contents.small[self.small_len - 1] = item
                else {
                    var big = try std.ArrayListUnmanaged(T).initCapacity(allocator, self.small_len);
                    big.appendSliceAssumeCapacity(&self.contents.small);
                    big.appendAssumeCapacity(item);
                    self.contents.big = big;
                }
            } else try self.contents.big.append(allocator, item);
        }
    };
}
