const std = @import("std");

pub fn build(b: *std.build.Builder) !void {
    const project_name = std.fs.path.basename(b.build_root);

    const mode = b.standardReleaseOptions();
    const debugger = b.option(
        bool,
        "debugger",
        "Enable support for debugger features. (default: true)",
    ) orelse true;
    const strip = b.option(bool, "strip", "Omit debug symbols. (default: false)") orelse false;
    const test_filter = b.option([]const u8, "test-filter", "Skip tests that do not match filter.");

    const options = b.addOptions();
    options.addOption(bool, "debugger", debugger);

    const zig_fmt = b.addFmt(&.{ @src().file, "src" });

    const clang_format = if (b.findProgram(&.{"clang-format"}, &.{})) |executable| command: {
        const command = b.addSystemCommand(&.{ executable, "-i" });
        const initial_command_len = command.argv.items.len;

        const filter = std.ComptimeStringMap(void, .{ .{ ".h", {} }, .{ ".c", {} } });
        for ([_][]const u8{ "include", "src" }) |dir| {
            var iterable_dir = try std.fs.cwd().openIterableDir(b.pathFromRoot(dir), .{});
            defer iterable_dir.close();

            var src_walker = try iterable_dir.walk(b.allocator);
            defer src_walker.deinit();

            while (try src_walker.next()) |entry|
                if (entry.kind == .File and filter.has(std.fs.path.extension(entry.basename)))
                    command.addArg(b.pathFromRoot(b.pathJoin(&.{ dir, entry.path })));
        }

        break :command if (command.argv.items.len > initial_command_len) command else null;
    } else |err| command: {
        std.debug.print("Could not find clang-format because of {}, skipping formatting.\n", .{err});
        break :command null;
    };

    const fmt_step = b.step("fmt", "Format code");
    fmt_step.dependOn(&zig_fmt.step);
    if (clang_format) |command| fmt_step.dependOn(&command.step);

    b.installDirectory(.{
        .source_dir = "include",
        .install_dir = .header,
        .install_subdir = project_name,
        .exclude_extensions = &.{"~"},
    });

    const main_tests = b.addTest(b.fmt("src/{s}.zig", .{project_name}));
    main_tests.setBuildMode(mode);
    main_tests.setFilter(test_filter);
    main_tests.addOptions("options", options);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(fmt_step);
    test_step.dependOn(&main_tests.step);

    const src_capi = "src/capi.zig";
    const libraries = .{
        .static = b.addStaticLibrary(project_name, src_capi),
        .shared = b.addSharedLibrary(project_name, src_capi, .unversioned),
    };
    inline for (@typeInfo(@TypeOf(libraries)).Struct.fields) |field| {
        const library = @field(libraries, field.name);
        library.strip = strip;
        library.setBuildMode(mode);
        library.addOptions("options", options);
        library.linkLibC();
        library.install();
        library.step.dependOn(fmt_step);

        const test_api = b.addExecutable("test-capi-" ++ field.name, null);
        switch (mode) {
            .Debug => {},
            .ReleaseSafe, .ReleaseFast, .ReleaseSmall => test_api.defineCMacro("NDEBUG", null),
        }
        test_api.defineCMacro(b.fmt("{s}_DEBUGGER", .{
            try std.ascii.allocUpperString(b.allocator, project_name),
        }), b.fmt("{}", .{@boolToInt(debugger)}));
        test_api.defineCMacro(b.fmt("{s}_{s}", .{
            try std.ascii.allocUpperString(b.allocator, project_name),
            try std.ascii.allocUpperString(b.allocator, field.name),
        }), null);
        test_api.addIncludePath("include");
        test_api.addCSourceFile("src/test-capi.c", &.{"-fvisibility=internal"});
        test_api.linkLibrary(library);
        test_api.linkLibC();

        test_step.dependOn(&test_api.run().step);
    }
}
