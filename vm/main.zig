const std = @import("std");
const tyvm = @import("./tyvm.zig");
const VM = @import("./vm.zig");
const Compiler = @import("./compiler.zig");

var gc: VM.GC = undefined;

pub fn main() !void {
    const arg = std.mem.span(std.os.argv[1]);
    gc = VM.GC.init(undefined) catch |e| tyvm.oom(e);
    var file = try std.fs.cwd().openFile(arg, .{});
    defer file.close();
    const source_bytes = try file.readToEndAlloc(std.heap.c_allocator, std.math.maxInt(u32));
    defer std.heap.c_allocator.free(source_bytes);

    const source: Compiler.Source = .{ .ptr = source_bytes.ptr, .len = source_bytes.len };

    const bytecode: Compiler.Bytecode = Compiler.compile(source);
    defer Compiler.bytecode_free(bytecode);

    var vm = try VM.new(&gc, bytecode.ptr[0..bytecode.len]);
    gc.vm = &vm;
    try vm.init();
    const global_function = vm.get_global_function();
    try vm.run(global_function);
    // try vm.run(global_function);
}
