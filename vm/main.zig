const std = @import("std");
const VM = @import("./vm.zig");
const Compiler = @import("./compiler.zig");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("./test/head-tail.ts", .{});
    defer file.close();
    const source_bytes = try file.readToEndAlloc(std.heap.c_allocator, std.math.maxInt(u32));
    defer std.heap.c_allocator.free(source_bytes);

    const source: Compiler.Source = .{
        .ptr = source_bytes.ptr,
        .len = source_bytes.len
    };

    const bytecode: Compiler.Bytecode = Compiler.compile(source);
    defer Compiler.bytecode_free(bytecode);

    var vm = try VM.new(std.heap.c_allocator, bytecode.ptr[0..bytecode.len]);
    try vm.init();
    const global_function = vm.get_global_function();
    try vm.run(global_function);
    // try vm.run(global_function);
}