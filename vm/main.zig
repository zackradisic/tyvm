const std = @import("std");
const VM = @import("./vm.zig");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("fib.tsb", .{});
    const bytecode = try file.readToEndAlloc(std.heap.c_allocator, std.math.maxInt(u32));

    var vm = try VM.init(std.heap.c_allocator, bytecode);
    try vm.run();
}