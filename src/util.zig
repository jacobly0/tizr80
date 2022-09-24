const std = @import("std");
const Type = std.builtin.Type;

pub fn Backing(comptime T: type) type {
    return @typeInfo(T).Struct.backing_integer.?;
}

pub fn fromBacking(comptime T: type, value: Backing(T)) T {
    return @bitCast(T, value);
}

pub fn toBacking(value: anytype) Backing(@TypeOf(value)) {
    return @bitCast(Backing(@TypeOf(value)), value);
}
pub const bit = struct {
    fn check(comptime Value: type, comptime Field: type, bit_offset: std.math.Log2Int(Value)) void {
        const value_info = @typeInfo(Value).Int;
        const field_info = @typeInfo(Field).Int;
        std.debug.assert(value_info.signedness == .unsigned);
        std.debug.assert(field_info.signedness == .unsigned);
        std.debug.assert(@as(std.math.Log2IntCeil(Value), bit_offset) +
            field_info.bits <= value_info.bits);
    }

    pub fn extract(
        value: anytype,
        comptime Field: type,
        bit_offset: std.math.Log2Int(@TypeOf(value)),
    ) Field {
        check(@TypeOf(value), Field, bit_offset);
        return @truncate(Field, value >> bit_offset);
    }

    pub fn inserted(
        value: anytype,
        field: anytype,
        bit_offset: std.math.Log2Int(@TypeOf(value)),
    ) @TypeOf(value) {
        const Value = @TypeOf(value);
        const Field = @TypeOf(field);
        check(Value, Field, bit_offset);
        return value & ~(@as(Value, std.math.maxInt(Field)) << bit_offset) |
            @as(Value, field) << bit_offset;
    }

    pub fn insert(value: anytype, field: anytype, bit_offset: std.math.Log2Int(@TypeOf(value.*))) void {
        value.* = inserted(value.*, field, bit_offset);
    }

    fn Concat(comptime Tuple: type) type {
        var bits = 0;
        for (@typeInfo(Tuple).Struct.fields) |field| bits += @bitSizeOf(field.field_type);
        return std.meta.Int(.unsigned, bits);
    }

    pub fn concat(tuple: anytype) Concat(@TypeOf(tuple)) {
        const Tuple = @TypeOf(tuple);
        const Result = Concat(Tuple);
        var result: Result = 0;
        comptime var bit_offset = @bitSizeOf(Result);
        inline for (tuple) |field| {
            bit_offset -= @bitSizeOf(@TypeOf(field));
            insert(&result, field, bit_offset);
        }
        return result;
    }
};

test "bit" {
    try std.testing.expectEqual(@as(u12, 0x123), bit.extract(@as(u24, 0xAB123C), u12, 4));
    try std.testing.expectEqual(
        @as(u24, 0xAB987C),
        bit.inserted(@as(u24, 0xAB123C), @as(u12, 0x987), 4),
    );
    try std.testing.expectEqual(
        @as(u15, 0b101_11101_1_0010_11),
        bit.concat(.{ @as(u3, 0b101), @as(u5, 0b11101), @as(u1, 0b1), @as(u4, 0b0010), @as(u2, 0b11) }),
    );
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
