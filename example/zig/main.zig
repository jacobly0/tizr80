const std = @import("std");
const TiZr80 = @import("tizr80");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(!gpa.deinit());
    const allocator = gpa.allocator();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    const core = try TiZr80.create(.{ .allocator = allocator });
    defer core.destroy();

    _ = args.skip();
    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--one-line")) {
            if (args.next()) |command|
                _ = try core.commandSplit(command)
            else {
                try stderr.print("Missing {s} argument\n", .{arg});
                std.process.exit(1);
            }
        } else {
            try stderr.print("Unknown argument: \"{s}\"\n", .{arg});
            std.process.exit(1);
        }
    }

    var last_line = std.ArrayList(u8).init(allocator);
    defer last_line.deinit();

    var line = std.ArrayList(u8).init(allocator);
    defer line.deinit();

    while (true) {
        try stdout.print("tizr80> ", .{});
        stdin.readUntilDelimiterArrayList(&line, '\n', 1024 * 1024) catch |err| switch (err) {
            error.EndOfStream => {
                try stdout.print("\n", .{});
                break;
            },
            else => |e| return e,
        };

        if (std.mem.eql(u8, line.items, "q") or
            std.mem.eql(u8, line.items, "quit") or
            std.mem.eql(u8, line.items, "exit"))
            break;
        if (line.items.len != 0)
            std.mem.swap(std.ArrayList(u8), &last_line, &line);
        _ = try core.commandSplit(last_line.items);
    }
}
