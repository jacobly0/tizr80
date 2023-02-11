const std = @import("std");
const Build = std.Build;
const FileSource = Build.FileSource;

pub fn build(b: *Build) !void {
    const project_name = std.fs.path.basename(b.build_root);

    const max_supported_glibc_version = std.builtin.Version{ .major = 2, .minor = 34, .patch = 0 };
    const target = if (b.host.target.isGnuLibC() and
        b.host.target.os.version_range.linux.glibc.order(max_supported_glibc_version).compare(.gt))
    target: {
        break :target std.zig.CrossTarget{ .glibc_version = max_supported_glibc_version };
    } else std.zig.CrossTarget{};

    const optimize = b.standardOptimizeOption(.{});
    const debugger = b.option(
        bool,
        "debugger",
        "Enable support for debugger features. (default: true)",
    ) orelse true;
    const strip = b.option(bool, "strip", "Omit debug symbols. (default: false)") orelse false;
    const test_filter = b.option([]const u8, "test-filter", "Skip tests that do not match filter.");
    b.verbose = b.option(bool, "verbose", "Verbose build output. (default: false)") orelse false;

    const options = b.addOptions();
    options.addOption(bool, "debugger", debugger);

    const zig_fmt = b.addFmt(&.{ @src().file, "src", "example" });

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

    const main_src = FileSource.relative(b.fmt("src/{s}.zig", .{project_name}));
    b.addModule(.{
        .name = project_name,
        .source_file = main_src,
    });

    const main_tests = b.addTest(.{
        .root_source_file = main_src,
        .optimize = optimize,
        .target = target,
    });
    main_tests.setFilter(test_filter);
    main_tests.addOptions("options", options);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(fmt_step);
    test_step.dependOn(&main_tests.step);

    const src_capi = FileSource.relative("src/capi.zig");
    const libraries = .{
        .static = b.addStaticLibrary(.{
            .name = project_name,
            .root_source_file = src_capi,
            .target = target,
            .optimize = optimize,
        }),
        .shared = b.addSharedLibrary(.{
            .name = project_name,
            .root_source_file = src_capi,
            .target = target,
            .optimize = optimize,
        }),
    };
    inline for (@typeInfo(@TypeOf(libraries)).Struct.fields) |field| {
        const library = @field(libraries, field.name);
        library.strip = strip;
        library.addOptions("options", options);
        library.linkLibC();
        library.install();
        library.step.dependOn(fmt_step);

        const test_api = b.addExecutable(.{
            .name = "test-capi-" ++ field.name,
            .target = target,
            .optimize = optimize,
        });
        switch (optimize) {
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

        if (test_filter == null or std.mem.indexOf(u8, "capi-" ++ field.name, test_filter.?) != null)
            test_step.dependOn(&test_api.run().step);
    }

    const zig_example = b.addExecutable(.{
        .name = b.fmt("{s}-example-zig", .{project_name}),
        .root_source_file = FileSource.relative("example/zig/main.zig"),
        .optimize = optimize,
    });
    zig_example.addOptions("options", options);
    zig_example.addModule(project_name, b.modules.get(project_name).?);
    zig_example.install();
    zig_example.step.dependOn(fmt_step);

    const zig_example_run = zig_example.run();
    if (b.args) |args| zig_example_run.addArgs(args);

    const run_step = b.step("run", "Run example");
    run_step.dependOn(&zig_example_run.step);
}
