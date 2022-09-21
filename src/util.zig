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

/// Cast an integer to a different integer type. If the value doesn't fit,
/// return null.
pub fn cast(comptime T: type, x: anytype) ?T {
    if (T == comptime_int) return x; // only works if x is comptime_int
    comptime assert(@typeInfo(T) == .Int); // must pass an integer
    const is_comptime = @TypeOf(x) == comptime_int;
    comptime assert(is_comptime or @typeInfo(@TypeOf(x)) == .Int); // must pass an integer
    if ((is_comptime or maxInt(@TypeOf(x)) > maxInt(T)) and x > maxInt(T)) {
        return null;
    } else if ((is_comptime or minInt(@TypeOf(x)) < minInt(T)) and x < minInt(T)) {
        return null;
    } else {
        return @intCast(T, x);
    }
}

test "cast" {
    try testing.expect(cast(u8, 300) == null);
    try testing.expect(cast(u8, @as(u32, 300)) == null);
    try testing.expect(cast(i8, -200) == null);
    try testing.expect(cast(i8, @as(i32, -200)) == null);
    try testing.expect(cast(u8, -1) == null);
    try testing.expect(cast(u8, @as(i8, -1)) == null);
    try testing.expect(cast(u64, -1) == null);
    try testing.expect(cast(u64, @as(i8, -1)) == null);

    try testing.expect(cast(comptime_int, 12345).? == 12345);
    try testing.expect(cast(u8, 255).? == @as(u8, 255));
    try testing.expect(cast(u8, @as(u32, 255)).? == @as(u8, 255));
    try testing.expect(@TypeOf(cast(u8, 255).?) == u8);
    try testing.expect(@TypeOf(cast(u8, @as(u32, 255)).?) == u8);
}
