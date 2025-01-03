const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast,-emscripten and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const is_wasm = target.query.cpu_arch != null and target.query.cpu_arch.?.isWasm();

    const out = exe: {
        if (is_wasm) {
            var lib = b.addExecutable(.{
                .name = "tyvm",
                .root_source_file = .{ .cwd_relative = "vm/main_wasm.zig" },
                .target = target,
                .optimize = optimize,
                .link_libc = true,
            });
            // lib.target = std.zig.CrossTarget{
            //     .cpu_arch = .wasm64,
            //     .os_tag = .emscripten,
            // };
            lib.root_module.export_symbol_names = &.{ "init", "get_function", "get_global_function", "run", "alloc", "dealloc", "is_game", "jump", "reset" };
            lib.initial_memory = 65536 * 65536;
            lib.max_memory = 65536 * 65536;
            lib.root_module.dwarf_format = .@"32";
            break :exe lib;
        }
        const exe = b.addExecutable(.{
            .name = "tyvm",
            .root_source_file = .{ .cwd_relative = "vm/main.zig" },
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        });
        break :exe exe;
    };

    out.linkSystemLibrary("tyvm_compiler");
    out.addIncludePath(.{ .cwd_relative = "./include" });
    if (is_wasm) {
        if (optimize == .Debug) {
            out.addLibraryPath(.{ .cwd_relative = "./target/wasm32-wasi/debug" });
        } else {
            out.addLibraryPath(.{ .cwd_relative = "./target/wasm32-wasi/release" });
        }
    } else {
        if (optimize == .Debug) {
            out.addLibraryPath(.{ .cwd_relative = "./target/debug" });
        } else {
            out.addLibraryPath(.{ .cwd_relative = "./target/release" });
        }
    }

    const ENABLE_DEBUG_SYMBOLS = true;
    if (comptime ENABLE_DEBUG_SYMBOLS) {
        out.root_module.strip = false;
        out.export_table = true;
        // out.symb
        out.linkLibC();
    }

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(out);

    if (!is_wasm) {
        // This *creates* a Run step in the build graph, to be executed when another
        // step is evaluated that depends on it. The next line below will establish
        // such a dependency.
        const run_cmd = b.addRunArtifact(out);

        // By making the run step depend on the install step, it will be run from the
        // installation directory rather than directly from within the cache directory.
        // This is not necessary, however, if the application depends on other installed
        // files, this ensures they will be present and in the expected location.
        run_cmd.step.dependOn(b.getInstallStep());

        // This allows the user to pass arguments to the application in the build
        // command itself, like this: `zig build run -- arg1 arg2 etc`
        if (b.args) |args| {
            run_cmd.addArgs(args);
        }

        // This creates a build step. It will be visible in the `zig build --help` menu,
        // and can be selected like this: `zig build run`
        // This will evaluate the `run` step rather than the default, which is "install".
        const run_step = b.step("run", "Run the app");
        run_step.dependOn(&run_cmd.step);
    }

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const unit_tests = b.addTest(.{
        .root_source_file = .{ .cwd_relative = "vm/vm.zig" },
        .target = target,
        .optimize = optimize,
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
