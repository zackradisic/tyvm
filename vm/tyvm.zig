const std = @import("std");

const builtin = @import("builtin");

pub const BuildTarget = enum { native, wasm, wasi };
pub const build_target: BuildTarget = brk: {
    if (@import("builtin").target.isWasm()) {
        break :brk BuildTarget.wasm;
    } else {
        break :brk BuildTarget.native;
    }
};

pub const isWasm = build_target == .wasm;
pub const isNative = build_target == .native;
pub const isWasi = build_target == .wasi;
pub const isMac = build_target == .native and @import("builtin").target.os.tag == .macos;
pub const isBrowser = !isWasi and isWasm;
pub const isWindows = @import("builtin").target.os.tag == .windows;
pub const isPosix = !isWindows and !isWasm;
pub const isDebug = std.builtin.Mode.Debug == @import("builtin").mode;
pub const isRelease = std.builtin.Mode.Debug != @import("builtin").mode and !isTest;
pub const isTest = @import("builtin").is_test;
pub const isLinux = @import("builtin").target.os.tag == .linux;
pub const isAarch64 = @import("builtin").target.cpu.arch.isAARCH64();
pub const isX86 = @import("builtin").target.cpu.arch.isX86();
pub const isX64 = @import("builtin").target.cpu.arch == .x86_64;
pub const allow_assert = isDebug or isTest or std.builtin.Mode.ReleaseSafe == @import("builtin").mode;

pub const debug_assert = if (allow_assert) std.debug.assert else ___debug_assert;
pub fn __debug_assert(aok: bool) void {
    std.debug.assert(aok);
}
pub fn ___debug_assert(_: bool) void {}

pub inline fn oom(e: anyerror) noreturn {
    if (comptime allow_assert) {
        if (e != std.mem.Allocator.Error.OutOfMemory) @panic("Expected OOM");
    }
    @panic("Out of memory");
}

const _log_fn = fn (comptime fmt: []const u8, args: anytype) void;
pub fn logger(comptime tag: anytype, comptime disabled: bool) _log_fn {
    const tagname = switch (@TypeOf(tag)) {
        @Type(.EnumLiteral) => @tagName(tag),
        []const u8 => tag,
        else => @compileError("Output.scoped expected @Type(.EnumLiteral) or []const u8, you gave: " ++ @typeName(@Type(tag))),
    };

    if (comptime !isDebug or !isNative) {
        return struct {
            pub fn log(comptime _: []const u8, _: anytype) void {}
        }.log;
    }

    return struct {
        var evaluated_disable = false;
        var really_disable = disabled;
        const BufferedWriter = std.io.BufferedWriter(4096, std.fs.File.Writer);
        var buffered_writer: BufferedWriter = undefined;
        var out: BufferedWriter.Writer = undefined;
        var out_set = false;
        var lock = std.Thread.Mutex{};

        pub fn log(comptime fmt: []const u8, args: anytype) void {
            if (fmt.len == 0 or fmt[fmt.len - 1] != '\n') {
                return log(fmt ++ "\n", args);
            }

            if (!evaluated_disable) {
                evaluated_disable = true;
                if (std.os.getenv("TYVM_DEBUG_" ++ tagname)) |val| {
                    really_disable = std.mem.eql(u8, val, "0");
                } else if (std.os.getenv("TYVM_DEBUG_LOGS")) |val| {
                    really_disable = really_disable or std.mem.eql(u8, val, "0");
                }
            }

            if (really_disable) return;
            if (!out_set) {
                out_set = true;
                const stderr_writer = std.io.getStdErr().writer();
                buffered_writer = .{
                    .unbuffered_writer = stderr_writer,
                };
                out = buffered_writer.writer();
                out_set = true;
            }
            lock.lock();
            defer lock.unlock();
            out.print("[" ++ tagname ++ "] " ++ fmt, args) catch @panic("Failed to write!");
            out.context.flush() catch @panic("Failed to write!");
        }
    }.log;
}
