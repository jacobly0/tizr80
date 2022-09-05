pub fn Backing(comptime T: type) type {
    return @typeInfo(T).Struct.backing_integer.?;
}

pub fn fromBacking(comptime T: type, value: Backing(T)) T {
    return @bitCast(T, value);
}

pub fn toBacking(value: anytype) Backing(@TypeOf(value)) {
    return @bitCast(Backing(@TypeOf(value)), value);
}
