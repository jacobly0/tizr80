const std = @import("std");
const assert = std.debug.assert;
const minInt = std.math.minInt;
const maxInt = std.math.maxInt;
const testing = std.testing;

pub fn Backing(comptime T: type) type {
    return @typeInfo(T).Struct.backing_integer.?;
}

pub fn fromBacking(comptime T: type, value: Backing(T)) T {
    return @bitCast(T, value);
}

pub fn toBacking(value: anytype) Backing(@TypeOf(value)) {
    return @bitCast(Backing(@TypeOf(value)), value);
}

pub fn pow(comptime base: comptime_int, comptime exponent: comptime_int) comptime_int {
    var power = base;
    var bits = exponent;
    var result = 1;
    while (bits != 0) {
        if (bits & 1 != 0) result *= power;
        power *= power;
        bits >>= 1;
    }
    return result;
}

test "pow" {
    try std.testing.expectEqual(-8, pow(-2, 3));
    try std.testing.expectEqual(64, pow(2, 6));
    try std.testing.expectEqual(1_000_000, pow(10, 6));
    try std.testing.expectEqual(1_000_000_000_000, pow(100, 6));
}
