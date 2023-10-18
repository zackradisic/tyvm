const std = @import("std");
const VM = @import("./vm.zig");
const Compiler = @import("./compiler.zig");

// pub export fn main(argc: i32, argv: i32) i32 {
//     _ = argv;
//     _ = argc;
//     return 0;
// }

pub  fn main() void {
}

pub export fn init(source_ptr: [*]u8, len: usize) *VM {
    const bytecode: Compiler.Bytecode = Compiler.compile(.{ .ptr = source_ptr, .len = len });

    var vm = VM.new(std.heap.c_allocator, bytecode.ptr[0..bytecode.len]) catch @panic("OOM");
    vm.init() catch @panic("OOM");

    var vm_ptr = std.heap.c_allocator.create(VM) catch @panic("OOM");
    vm_ptr.* = vm;

    return vm_ptr;
}

pub export fn get_function(vm: *VM, fn_name_ptr: [*]u8, fn_name_len: usize) ?*const VM.Function {
    return vm.get_function(fn_name_ptr[0..fn_name_len]);
}

pub export fn get_global_function(vm: *VM) *const VM.Function {
    return vm.get_global_function();
}

pub export fn get_main_function(vm: *VM) ?*const VM.Function {
    return vm.get_main_function();
}

pub export fn run(vm: *VM, function: *const VM.Function) void {
    vm.run(function) catch @panic("OOM");
}

pub export fn jump(vm: *VM) void {
    if (!vm.is_game) @panic("Can only call if is a game");
    const initial_state: *VM.Value = vm.globals.getPtr(vm.initial_state_index.?).?;
    const jump_input = vm.make_string_from_slice("jumpInput") catch @panic("OOM");
    initial_state.Object.update_field(jump_input, VM.Value.boolean(true));
}

pub export fn reset(vm: *VM) void {
    if (!vm.is_game) @panic("Can only call if is a game");
    const initial_state: *VM.Value = vm.globals.getPtr(vm.initial_state_index.?).?;
    const reset_val = vm.make_string_from_slice("reset") catch @panic("OOM");
    initial_state.Object.update_field(reset_val, VM.Value.boolean(true));
}

pub export fn is_game(vm: *VM) bool {
    return vm.is_game;
}

pub export fn alloc(len: usize) [*]u8 {
    const mem = std.heap.c_allocator.alloc(u8, len) catch @panic("OOM");
    return mem.ptr;
}

pub export fn dealloc(ptr: [*]u8, len: usize) void {
    std.heap.c_allocator.free(ptr[0..len]);
}