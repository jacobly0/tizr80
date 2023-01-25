const std = @import("std");

const Commands = @This();
const TiZr80 = @import("tizr80.zig");

pub const Error = error{
    InvalidCommand,
    InvalidArguments,
} || std.mem.Allocator.Error ||
    std.fs.File.OpenError || std.fs.File.ReadError || std.fs.File.WriteError;

pub const Command = union(enum) {
    const Group = struct {
        subcommands: std.StringArrayHashMapUnmanaged(Command) = .{},

        pub fn deinit(self: *Group, allocator: std.mem.Allocator) void {
            var iterator = self.subcommands.iterator();
            while (iterator.next()) |entry| {
                allocator.free(entry.key_ptr.*);
                entry.value_ptr.deinit(allocator);
            }
            self.subcommands.deinit(allocator);
        }

        pub fn lookup(self: *Group, commands: *Commands, arguments: *[]const [:0]const u8) ?*Command {
            if (arguments.len == 0)
                return null
            else if (self.subcommands.getPtr(arguments.*[0])) |command| {
                arguments.* = arguments.*[1..];
                return command.lookup(commands, arguments);
            } else return null;
        }

        pub fn run(self: *Group, commands: *Commands, arguments: []const [:0]const u8) Error!i32 {
            if (arguments.len == 0) {
                self.usage();
                return 1;
            } else if (self.subcommands.getPtr(arguments[0])) |command| {
                return command.run(commands, arguments[1..]);
            } else {
                self.usage();
                return Error.InvalidCommand;
            }
        }

        pub fn usage(_: *Group) void {}

        pub fn createCommand(
            self: *Group,
            allocator: std.mem.Allocator,
            name: []const u8,
        ) std.mem.Allocator.Error!*Command {
            const owned_name = try allocator.dupe(u8, name);
            errdefer allocator.free(owned_name);

            const result = try self.subcommands.getOrPut(allocator, owned_name);
            std.debug.assert(!result.found_existing);
            return result.value_ptr;
        }

        pub fn addCommand(
            self: *Group,
            allocator: std.mem.Allocator,
            name: []const u8,
            comptime tag: std.meta.Tag(Command),
        ) std.mem.Allocator.Error!*std.meta.TagPayload(Command, tag) {
            const command = try self.createCommand(allocator, name);
            command.* = @unionInit(Command, @tagName(tag), .{});
            return &@field(command, @tagName(tag));
        }
    };

    const Alias = struct {
        target: []const [:0]const u8 = undefined,

        pub fn init(self: *Alias, allocator: std.mem.Allocator, target: []const []const u8) !void {
            var owned_target = try allocator.alloc([:0]const u8, target.len);
            errdefer allocator.free(owned_target);

            var index: usize = 0;
            errdefer for (owned_target[0..index]) |argument| allocator.free(argument);
            while (index < target.len) : (index += 1)
                owned_target[index] = try allocator.dupeZ(u8, target[index]);

            self.* = .{ .target = owned_target };
        }
        pub fn deinit(self: *Alias, allocator: std.mem.Allocator) void {
            for (self.target) |argument| allocator.free(argument);
            allocator.free(self.target);
        }

        pub fn lookup(self: *Alias, commands: *Commands, arguments: *[]const [:0]const u8) ?*Command {
            var target_arguments = self.target;
            return if (commands.root.lookup(commands, &target_arguments)) |target|
                if (target_arguments.len == 0) target.lookup(commands, arguments) else null
            else
                null;
        }

        pub fn run(self: *Alias, commands: *Commands, arguments: []const [:0]const u8) Error!i32 {
            var target_arguments = self.target;
            const target = commands.root.lookup(commands, &target_arguments) orelse
                return Error.InvalidCommand;
            if (target_arguments.len != 0) return Error.InvalidCommand;
            return target.run(commands, arguments);
        }
    };

    const Native = struct {
        handler: *const fn (*Commands, []const [:0]const u8) Error!i32 = undefined,

        pub fn deinit(_: *Native, _: std.mem.Allocator) void {}

        pub fn run(self: *Native, commands: *Commands, arguments: []const [:0]const u8) Error!i32 {
            return self.handler(commands, arguments);
        }
    };

    group: Group,
    alias: Alias,
    native: Native,

    pub fn deinit(self: *Command, allocator: std.mem.Allocator) void {
        switch (self.*) {
            inline else => |*command| command.deinit(allocator),
        }
    }

    pub fn lookup(self: *Command, commands: *Commands, arguments: *[]const [:0]const u8) ?*Command {
        return switch (self.*) {
            .native => self,
            inline else => |*command| command.lookup(commands, arguments),
        };
    }

    pub fn run(self: *Command, commands: *Commands, arguments: []const [:0]const u8) Error!i32 {
        switch (self.*) {
            inline else => |*command| return command.run(commands, arguments),
        }
    }

    pub fn usage(self: *Command) void {
        switch (self.*) {
            inline else => |*command| command.usage(),
        }
    }
};

root: Command,

pub fn init(self: *Commands, allocator: std.mem.Allocator) !void {
    self.* = .{ .root = .{ .group = .{} } };

    const root_group = &self.root.group;
    errdefer root_group.deinit(allocator);

    const set_group = try root_group.addCommand(allocator, "set", .group);
    (try set_group.addCommand(allocator, "debug", .native)).* = .{ .handler = &setDebug };

    (try root_group.addCommand(allocator, "resume", .native)).* = .{ .handler = &@"resume" };
    try (try root_group.addCommand(allocator, "run", .alias)).init(allocator, &.{"resume"});
    try (try root_group.addCommand(allocator, "r", .alias)).init(allocator, &.{"resume"});
    try (try root_group.addCommand(allocator, "continue", .alias)).init(allocator, &.{"resume"});
    try (try root_group.addCommand(allocator, "c", .alias)).init(allocator, &.{"resume"});

    (try root_group.addCommand(allocator, "pause", .native)).* = .{ .handler = &pause };

    (try root_group.addCommand(allocator, "step", .native)).* = .{ .handler = &step };
    try (try root_group.addCommand(allocator, "s", .alias)).init(allocator, &.{"step"});

    const load_group = try root_group.addCommand(allocator, "load", .group);
    (try load_group.addCommand(allocator, "rom", .native)).* = .{ .handler = &loadRom };

    const save_group = try root_group.addCommand(allocator, "save", .group);
    (try save_group.addCommand(allocator, "lcd", .native)).* = .{ .handler = &saveLcd };
    (try save_group.addCommand(allocator, "rom", .native)).* = .{ .handler = &saveRom };
}
pub fn deinit(self: *Commands, allocator: std.mem.Allocator) void {
    self.root.deinit(allocator);
}

pub fn run(self: *Commands, arguments: []const [:0]const u8) Error!i32 {
    return self.root.run(self, arguments);
}

fn core(self: *Commands) *TiZr80 {
    return @fieldParentPtr(TiZr80, "commands", self);
}

fn setDebug(self: *Commands, arguments: []const [:0]const u8) Error!i32 {
    if (arguments.len != 1) return Error.InvalidArguments;

    self.core().cpu.backend.debug = if (std.mem.eql(u8, arguments[0], "on"))
        true
    else if (std.mem.eql(u8, arguments[0], "off"))
        false
    else
        return Error.InvalidArguments;
    return 0;
}

fn @"resume"(self: *Commands, arguments: []const [:0]const u8) Error!i32 {
    if (arguments.len != 0) return Error.InvalidArguments;

    return @boolToInt(!self.core().wake());
}

fn pause(self: *Commands, arguments: []const [:0]const u8) Error!i32 {
    if (arguments.len != 0) return Error.InvalidArguments;

    return @boolToInt(!self.core().sleep());
}

fn step(self: *Commands, arguments: []const [:0]const u8) Error!i32 {
    if (arguments.len != 0) return Error.InvalidArguments;

    _ = self.core().sleep();
    self.core().cpu.step();
    return 0;
}

fn loadRom(self: *Commands, arguments: []const [:0]const u8) Error!i32 {
    if (arguments.len != 1) return Error.InvalidArguments;

    const file = try std.fs.cwd().openFileZ(arguments[0], .{});
    defer file.close();

    return @boolToInt(try file.readAll(self.core().mem.flash) < self.core().mem.flash.len);
}

fn saveLcd(self: *Commands, arguments: []const [:0]const u8) Error!i32 {
    if (arguments.len != 1) return Error.InvalidArguments;

    const file = try std.fs.cwd().createFileZ(arguments[0], .{});
    defer file.close();

    try file.writeAll(self.core().mem.ram[0x40000..0x65800]);
    return 0;
}

fn saveRom(self: *Commands, arguments: []const [:0]const u8) Error!i32 {
    if (arguments.len != 1) return Error.InvalidArguments;

    const file = try std.fs.cwd().createFileZ(arguments[0], .{});
    defer file.close();

    try file.writeAll(self.core().mem.flash);
    return 0;
}

fn testInit(allocator: std.mem.Allocator) !void {
    var commands: Commands = undefined;
    try commands.init(allocator);
    defer commands.deinit(allocator);
}
test "commands init" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testInit, .{});
}
