const std = @import("std");
const VM = @import("./vm.zig");

const Source = extern struct {
    ptr: [*]const u8,
    len: usize,
};

const Bytecode = extern struct {
    ptr: [*] u8,
    len: usize,
    cap: usize,
};

extern fn tyvm_compile(source: Source) Bytecode;
extern fn tyvm_bytecode_free(bytecode: Bytecode) void;

pub fn main() !void {
    var file = try std.fs.cwd().openFile("./test/fib.ts", .{});
    defer file.close();
    const source_bytes = try file.readToEndAlloc(std.heap.c_allocator, std.math.maxInt(u32));
    defer std.heap.c_allocator.free(source_bytes);

    const source: Source = .{
        .ptr = source_bytes.ptr,
        .len = source_bytes.len
    };

    const bytecode: Bytecode = tyvm_compile(source);
    defer tyvm_bytecode_free(bytecode);

    var vm = try VM.init(std.heap.c_allocator, bytecode.ptr[0..bytecode.len]);
    try vm.run();
}