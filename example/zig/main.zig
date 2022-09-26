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
        if (std.mem.eql(u8, arg, "--rom")) {
            if (args.next()) |rom| {
                // FIXME: use tuple when that doesn't trigger a segfault
                if (try core.commandSlices(&[_:null]?[:0]const u8{ "load", "rom", rom }) != 0) {
                    try stderr.print("Unable to load rom \"{s}\"\n", .{rom});
                    std.process.exit(1);
                }
            } else {
                try stderr.print("Missing {s} argument\n", .{arg});
                std.process.exit(1);
            }
        } else {
            try stderr.print("Unknown argument: \"{s}\"\n", .{arg});
            std.process.exit(1);
        }
    }

    var line = std.ArrayList(u8).init(allocator);
    defer line.deinit();

    const Command = enum {
        unknown,
        help,
        pause,
        quit,
        run,
        step,
    };
    var last_command: ?Command = null;

    while (true) {
        try stdout.print("> ", .{});
        stdin.readUntilDelimiterArrayList(&line, '\n', 1024 * 1024) catch |err| switch (err) {
            error.EndOfStream => {
                try stdout.print("\n", .{});
                break;
            },
            else => |e| return e,
        };

        const command: Command = if (line.items.len == 0)
            if (last_command) |last| last else .help
        else if (std.mem.eql(u8, line.items, "q") or
            std.mem.eql(u8, line.items, "quit") or
            std.mem.eql(u8, line.items, "exit"))
            .quit
        else if (std.mem.eql(u8, line.items, "?") or
            std.mem.eql(u8, line.items, "h") or
            std.mem.eql(u8, line.items, "help"))
            .help
        else if (std.mem.eql(u8, line.items, "r") or
            std.mem.eql(u8, line.items, "c") or
            std.mem.eql(u8, line.items, "run") or
            std.mem.eql(u8, line.items, "continue"))
            .run
        else if (std.mem.eql(u8, line.items, "p") or
            std.mem.eql(u8, line.items, "pause"))
            .pause
        else if (std.mem.eql(u8, line.items, "s") or
            std.mem.eql(u8, line.items, "step"))
            .step
        else
            .unknown;

        switch (command) {
            .unknown => try stdout.print("unknown command: {s}\n", .{line.items}),
            .help => try stdout.print(
                \\ Commands:
                \\   help    print this help
                \\   pause   stop executing instructions
                \\   quit    terminate the program
                \\   run     start executing instructions
                \\   step    execute one instruction
                \\
            , .{}),
            .pause => _ = core.sleep(),
            .quit => break,
            .run => _ = core.wake(),
            .step => {
                _ = core.sleep();
                core.cpu.step();
            },
        }

        last_command = command;
    }
}
