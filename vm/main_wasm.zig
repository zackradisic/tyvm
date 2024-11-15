const std = @import("std");
const VM = @import("./vm.zig");
const Compiler = @import("./compiler.zig");
const tyvm = @import("./tyvm.zig");

// pub export fn main(argc: i32, argv: i32) i32 {
//     _ = argv;
//     _ = argc;
//     return 0;
// }

var gc: VM.GC = undefined;

pub fn main() void {}

pub export fn init(source_ptr: [*]u8, len: usize) *VM {
    gc = VM.GC.init(undefined) catch |e| tyvm.oom(e);
    const bytecode: Compiler.Bytecode = Compiler.compile(.{ .ptr = source_ptr, .len = len });

    var vm = VM.new(&gc, bytecode.ptr[0..bytecode.len]) catch @panic("OOM");
    vm.init() catch @panic("OOM");

    const vm_ptr = std.heap.c_allocator.create(VM) catch @panic("OOM");
    vm_ptr.* = vm;

    gc.vm = vm_ptr;

    return vm_ptr;
}

pub export fn get_function(vm: *VM, fn_name_ptr: [*]align(8) u8, fn_name_len: usize) ?*const VM.Function {
    tyvm.debug_assert(@intFromPtr(fn_name_ptr) % 8 == 0);
    const str = VM.String.from_literal(fn_name_ptr[0..fn_name_len]);
    return vm.get_function(str);
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

const KeydownEvent = struct {
    code: []const u8,
};

pub export fn keydown(vm: *VM, ptr: [*]const u8, len: usize) bool {
    const json_str = ptr[0..len];
    defer std.heap.c_allocator.free(json_str);

    const json = std.json.parseFromSlice(KeydownEvent, std.heap.c_allocator, json_str, .{}) catch @panic("FUCK");
    defer json.deinit();

    const keydown_event = json.value;

    const keydown_fn_str = vm.intern_string(VM.String.from_literal(VM.const_str("OnKeydown"))) catch |e| tyvm.oom(e);
    const keydown_fn = vm.get_function(keydown_fn_str) orelse @panic("No function");
    const keydown_value = VM.Value.derive(vm, KeydownEvent, keydown_event) catch |e| tyvm.oom(e);
    const game_state = vm.game_state orelse @panic("Called OnKeydown without game state");
    vm.run_with_args(keydown_fn, &.{ keydown_value, game_state }) catch |e| tyvm.oom(e);
    tyvm.debug_assert(@as(VM.ValueKind, (vm.stack_top - 1)[0]) == VM.ValueKind.Array);
    const return_value: VM.Value = (vm.stack_top - 1)[0];

    const state_value = return_value.Array.item_at_index(0);
    const prevent_default = return_value.Array.item_at_index(1);
    vm.game_state = state_value;
    return prevent_default.isTruthy();
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
