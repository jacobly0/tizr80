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

pub const ArgumentSplitter = struct {
    pub const Error = error{
        UnterminatedString,
        InvalidEscape,
    } || std.mem.Allocator.Error;

    allocator: std.mem.Allocator,
    line: []const u8,
    storage: std.ArrayListUnmanaged(u8) = .{},
    arguments: std.ArrayListUnmanaged([:0]const u8) = .{},

    pub fn init(allocator: std.mem.Allocator, line: []const u8) ArgumentSplitter {
        return .{ .allocator = allocator, .line = line };
    }
    pub fn deinit(self: *ArgumentSplitter) void {
        self.storage.deinit(self.allocator);
        self.arguments.deinit(self.allocator);
    }

    fn appendNext(self: *ArgumentSplitter) Error!?[:0]const u8 {
        const start = self.storage.items.len;

        const line = std.mem.trimLeft(u8, self.line, "\t\n\r ");
        var index: usize = 0;
        defer self.line = line[index..];
        if (line.len == 0) return null;

        const State = enum { default, escape, single, double, double_escape };
        var state: State = .default;

        while (index < line.len) : (index += 1) {
            const char = line[index];
            const transition: struct { copy: bool, state: State } = switch (state) {
                .default => switch (char) {
                    '\t', '\n', '\r', ' ' => break,
                    '"' => .{ .copy = false, .state = .double },
                    '\'' => .{ .copy = false, .state = .single },
                    '\\' => .{ .copy = false, .state = .escape },
                    else => .{ .copy = true, .state = .default },
                },
                .escape => .{ .copy = true, .state = .default },
                .single => switch (char) {
                    '\'' => .{ .copy = false, .state = .default },
                    else => .{ .copy = true, .state = .single },
                },
                .double => switch (char) {
                    '"' => .{ .copy = false, .state = .default },
                    '\\' => .{ .copy = false, .state = .double_escape },
                    else => .{ .copy = true, .state = .double },
                },
                .double_escape => .{ .copy = true, .state = .double },
            };
            if (transition.copy) try self.storage.append(self.allocator, char);
            state = transition.state;
        }

        switch (state) {
            .default => {
                const end = self.storage.items.len;
                try self.storage.append(self.allocator, 0);
                return self.storage.items[start..end :0];
            },
            .escape, .double_escape => return error.InvalidEscape,
            .single, .double => return error.UnterminatedString,
        }
    }
    pub fn next(self: *ArgumentSplitter) Error!?[:0]const u8 {
        self.storage.clearRetainingCapacity();
        return self.appendNext();
    }
    pub fn restOwned(self: *ArgumentSplitter) Error![][:0]const u8 {
        var arguments: std.ArrayListUnmanaged([:0]const u8) = .{};
        defer arguments.deinit(self.allocator);

        while (try self.appendNext()) |argument| try arguments.append(self.allocator, argument);

        // The pointers in arguments may have been invalidated, so recompute them all.
        var storage = self.storage.items;
        for (arguments.items) |*argument| {
            argument.* = storage[0..argument.len :0];
            storage = storage[argument.len + 1 ..];
        }

        return arguments.toOwnedSlice(self.allocator);
    }
};

fn testArgumentSplitter(
    allocator: std.mem.Allocator,
    expected_arguments: []const []const u8,
    line: []const u8,
) !void {
    {
        var argument_splitter = ArgumentSplitter.init(allocator, line);
        defer argument_splitter.deinit();

        for (expected_arguments) |expected_argument| {
            const actual_argument = try argument_splitter.next();
            try std.testing.expect(actual_argument != null);
            try std.testing.expectEqualStrings(expected_argument, actual_argument.?);
        }
        try std.testing.expectEqual(null, try argument_splitter.next());
    }

    {
        var argument_splitter = ArgumentSplitter.init(allocator, line);
        defer argument_splitter.deinit();

        const actual_arguments = try argument_splitter.restOwned();
        defer allocator.free(actual_arguments);

        try std.testing.expectEqual(expected_arguments.len, actual_arguments.len);
        for (expected_arguments) |expected_argument, argument_index|
            try std.testing.expectEqualStrings(expected_argument, actual_arguments[argument_index]);
    }
}

test "ArgumentSplitter" {
    inline for (.{
        .{ &.{}, "" },
        .{ &.{ "one", "two" }, " \tone\ntwo\r" },
        .{ &.{"single quoted"}, "\'single quoted\'" },
        .{ &.{"double quoted"}, "\"double quoted\"" },
        .{
            &.{
                \\first argument
                ,
                \\'second argument'
                ,
                \\"third argument"
            },
            \\first\ argument
            \\\''second argument'\'
            \\"\"third argument\""
        },
    }) |case|
        try std.testing.checkAllAllocationFailures(std.testing.allocator, testArgumentSplitter, case);
}
