const std = @import("std");
const mem = std.mem;
const raf = @import("raf.zig");
const tyvm = @import("./tyvm.zig");

const print = std.debug.print;
// const trace = tyvm.logger(.TRACE, true);
const trace = tyvm.logger(.TRACE, !TRACING);
const stacktrace = tyvm.logger(.STACK_TRACE, !TRACING and STACKTRACING);

const HashMap = std.AutoHashMap;
pub fn StringMap(comptime V: type) type {
    return std.ArrayHashMap(String, V, struct {
        pub fn hash(self: @This(), s: String) u32 {
            _ = self; // autofix
            return std.array_hash_map.hashString(s.as_str());
        }
        pub fn eql(self: @This(), a: String, b: String, b_index: usize) bool {
            _ = self; // autofix
            _ = b_index; // autofix

            return std.array_hash_map.eqlString(a.as_str(), b.as_str());
        }
    }, true);
}
const Allocator = std.mem.Allocator;

const VM = @This();

extern "c" fn memset(*anyopaque, c_int, usize) *anyopaque;

// const TRACING: bool = true;
const TRACING: bool = false;
const STACKTRACING: bool = false;

var rnd = std.rand.DefaultPrng.init(0);

stack: [1024]Value = [_]Value{.{ .Number = 0.0 }} ** 1024,
stack_top: [*]Value = undefined,

call_frames: [1024]CallFrame = [_]CallFrame{CallFrame.uninit()} ** 1024,
call_frames_count: usize = 0,

globals: HashMap(ConstantTableIdx, Value),

gc: *GC,

constant_strings: StringMap(ConstantTableIdx),
interned_strings: StringMap(String),
native_functions: StringMap(NativeFunction),

/// Constant data loaded from the bytecode
///
/// TODO: Should we make `function` indexed by pointer instead of constant table index?
/// As it is now, to look up a function we need to do: name (String) -> constant table idx -> function
/// What if it were just: name -> function?
functions: HashMap(ConstantTableIdx, Function),
constant_table: []const ConstantTableEntry align(8),
constant_pool: []const u8 align(8),
is_game: bool = false,
game_state: ?Value = null,
length_string: ?ConstantTableIdx = null,
length_string_ptr: ?[*]const u8 = null,

pub fn new(gc: *GC, bytecode: []const u8) !VM {
    var vm: VM = .{
        .globals = HashMap(ConstantTableIdx, Value).init(gc.as_allocator_no_gc()),
        .constant_strings = StringMap(ConstantTableIdx).init(gc.as_allocator_no_gc()),
        // .interned_strings = std.StringArrayHashMap(String).init(gc.as_allocator_no_gc()),
        .interned_strings = StringMap(String).init(gc.as_allocator_no_gc()),
        .native_functions = StringMap(NativeFunction).init(gc.as_allocator_no_gc()),

        .functions = HashMap(ConstantTableIdx, Function).init(gc.as_allocator_no_gc()),
        .constant_table = undefined,
        .constant_pool = undefined,

        .gc = gc,
    };

    try vm.load_bytecode(bytecode);
    try vm.load_native_functions();

    // if (comptime TRACING) {
    var iter = vm.functions.valueIterator();
    while (iter.next()) |function| {
        function.disassemble(&vm);
        print("\n", .{});
    }
    // }

    return vm;
}

fn load_bytecode(self: *VM, bytecode: []const u8) !void {
    const header: *const Bytecode.Header = @ptrCast(@alignCast(bytecode.ptr));
    if (header.magic != 69420) {
        @panic("Bad header value.");
    }
    const constants_header: *const Bytecode.ConstantsHeader = @ptrCast(@alignCast(bytecode.ptr + header.constants_offset));
    const functions_header: *const Bytecode.FunctionsHeader = @ptrCast(@alignCast(bytecode.ptr + header.functions_offset));
    tyvm.debug_assert(functions_header.function_code_padding == 0);

    const constant_table_raw_ptr: [*]const ConstantTableEntry = @ptrCast(@alignCast(bytecode.ptr + header.constants_offset + @sizeOf(Bytecode.ConstantsHeader)));
    const constant_table: []const ConstantTableEntry = constant_table_raw_ptr[0 .. constants_header.table_len / @sizeOf(ConstantTableEntry)];
    const constant_pool_raw_ptr: [*]const u8 = (bytecode.ptr + header.constants_offset + @sizeOf(Bytecode.ConstantsHeader) + constants_header.table_len);
    const constant_pool: []const u8 = constant_pool_raw_ptr[0..(constants_header.pool_size - constants_header.pool_padding)];

    const function_table: []const Bytecode.FunctionTableEntry = @as([*]const Bytecode.FunctionTableEntry, @ptrCast(@alignCast(bytecode.ptr + functions_header.table_offset)))[0..functions_header.table_len];
    for (function_table) |*entry_| {
        const entry: *const Bytecode.FunctionTableEntry = entry_;
        const base: [*]const u8 = bytecode.ptr + functions_header.table_offset + functions_header.table_len * @sizeOf(Bytecode.FunctionTableEntry);
        const function: Function = .{ .name = entry.name_constant_idx, .code = base[entry.offset .. entry.offset + entry.size] };
        try self.functions.put(entry.name_constant_idx, function);
    }

    if (TRACING) {
        var iter = self.functions.iterator();
        while (iter.next()) |val| {
            std.debug.print("IDX: {d}\n", .{val.key_ptr.*.v});
        }
        std.debug.print("Printed functions\n", .{});
    }

    self.is_game = header.is_game == 1;
    self.constant_pool = constant_pool;
    self.constant_table = constant_table;

    for (self.constant_table, 0..) |entry, i| {
        trace("PROCESS ENTRY: {d} {any}\n", .{ i, entry });
        if (entry.kind == .String) {
            trace("Reading constant\n", .{});
            const str = self.read_constant_string(entry.idx);
            trace("Read the constant: {any} {s}\n", .{ str, str.as_str() });
            trace("Read the constant2: {any} {s}\n", .{ str, str.as_str() });
            if (std.mem.eql(u8, str.as_str(), "length")) {
                self.length_string = ConstantTableIdx.new(@intCast(i));
                self.length_string_ptr = str.as_str().ptr;
            }
            trace("Putting in interned stirngs: {}\n", .{str});
            try self.interned_strings.put(str, str);
            trace("Putting in constant stirngs: {}\n", .{str});
            try self.constant_strings.put(str, ConstantTableIdx.new(@intCast(i)));
        }
    }
    trace("Load bytecode done\n", .{});
}

pub fn const_str(comptime str: anytype) []align(8) const u8 {
    // const buf = comptime brk: {
    //     var buf: [str.len + 8]u8 = undefined;
    //     @memcpy(buf[8..], str);
    //     break :brk buf;
    // };
    // return buf[8..];

    const DummyStruct = extern struct {
        len: u32,
        actualstr: [str.len]u8 align(8),

        const DUMMY: @This() = .{ .len = @intCast(str.len), .actualstr = str.* };
        // actualstr: [str.len]u8 = str.*,
    };

    return &DummyStruct.DUMMY.actualstr;
}

fn load_native_functions(self: *VM) !void {
    const native_fns = [_]NativeFunction{
        .{ .name = comptime const_str("AssertEq"), .fn_ptr = NativeFunction.assert_eq_impl, .arg_types = &[_]Value{ .Any, .Any } },
        .{ .name = comptime const_str("Print"), .fn_ptr = NativeFunction.print_impl, .arg_types = &[_]Value{
            .Any,
        } },
        .{ .name = comptime const_str("WriteFile"), .fn_ptr = NativeFunction.write_file_impl, .arg_types = &[_]Value{
            .StringKeyword,
            .StringKeyword,
        } },
        .{ .name = comptime const_str("ToTypescriptSource"), .fn_ptr = NativeFunction.to_typescript_source_impl, .arg_types = &[_]Value{
            .StringKeyword,
            .Any,
        } },
        .{ .name = comptime const_str("ParseInt"), .fn_ptr = NativeFunction.parse_int_impl, .arg_types = &[_]Value{
            .StringKeyword,
        } },
        .{ .name = comptime const_str("Panic"), .fn_ptr = NativeFunction.panic_impl, .arg_types = &[_]Value{
            .Any,
        } },
        .{ .name = comptime const_str("RequestAnimFrame"), .fn_ptr = NativeFunction.request_anim_frame_impl, .arg_types = &[_]Value{
            .Any,
        } },
        .{ .name = comptime const_str("Rand"), .fn_ptr = NativeFunction.rand_impl, .arg_types = &[_]Value{
            .NumberKeyword,
            .NumberKeyword,
        } },
    };

    for (native_fns) |native_fn| {
        const name_str = String.from_literal(native_fn.name);
        const name_slice = name_str.as_str();
        trace("PUTTING: {s}", .{name_slice});
        try self.native_functions.put(name_str, native_fn);
    }
}

pub fn get_function(self: *VM, name: String) ?*const Function {
    const table_idx = self.constant_strings.get(name) orelse return null;
    const function = self.functions.getPtr(table_idx) orelse return null;
    return function;
}

const GLOBAL_CONSTANT_IDX: ConstantTableIdx = ConstantTableIdx.new(0);
const MAIN_CONSTANT_IDX: ConstantTableIdx = ConstantTableIdx.new(2);
pub fn get_global_function(self: *VM) *const Function {
    if (comptime TRACING) {
        var iter = self.functions.iterator();
        while (iter.next()) |val| {
            trace("IDX get_global: {d}\n", .{val.key_ptr.*.v});
        }
    }
    return self.functions.getPtr(GLOBAL_CONSTANT_IDX).?;
}

pub fn get_main_function(self: *VM) ?*const Function {
    return self.functions.getPtr(MAIN_CONSTANT_IDX);
}

pub fn init(self: *VM) !void {
    self.stack_top = self.stack[0..];
}

pub fn run(self: *VM, function: *const Function) !void {
    self.stack_top = self.stack[0..];

    self.push_call_frame(.{
        .func = function,
        .ip = function.code.ptr,
        .slots = self.stack_top,
    });
    return self.run_impl();
}

pub fn run_game(self: *VM, function: *const Function) !void {
    _ = self; // autofix
    _ = function; // autofix

}

pub fn run_with_args(self: *VM, function: *const Function, args: []const Value) !void {
    self.stack_top = @ptrCast(&self.stack);
    @memset(self.stack[0..], Value{ .Any = {} });
    @memcpy(self.stack_top[0..args.len], args);
    self.push_call_frame(.{
        .func = function,
        .ip = function.code.ptr,
        .slots = self.stack_top,
    });
    self.stack_top += args.len;
    return self.run_impl();
}

pub fn run_impl(self: *VM) !void {
    var frame: *CallFrame = &self.call_frames[self.call_frames_count - 1];

    while (true) {
        const op: Op = @enumFromInt(frame.read_byte());
        if (comptime TRACING) {
            print("FUNCTION NAME IDX: {d}\n", .{self.cur_call_frame().func.name.v});
            const fn_name = self.read_constant(self.cur_call_frame().func.name).String;
            if (comptime STACKTRACING) {
                print("STACK:\n", .{});
                const stack_top_int: usize = @intFromPtr(self.stack_top);
                const stack_bot_int: usize = @intFromPtr(self.stack[0..]);
                const stack_len = stack_top_int / @sizeOf(Value) - stack_bot_int / @sizeOf(Value);
                for (0..stack_len) |i| {
                    print("    {?}\n", .{self.stack[i]});
                    // self.stack[i].debug(debug_alloc.allocator(), "    ") catch @panic("OOM");
                }
            }
            print("({s}) OP: {s}\n", .{ fn_name.as_str(), @tagName(op) });
        }

        switch (op) {
            .Add => {
                const b = self.pop();
                const a = self.pop();
                self.push(Value.number(a.Number + b.Number));
            },
            .Sub => {
                const b = self.pop();
                const a = self.pop();
                self.push(Value.number(a.Number - b.Number));
            },
            .Mul => {
                const b = self.pop();
                const a = self.pop();
                self.push(Value.number(a.Number * b.Number));
            },
            .Div => {
                const b = self.pop();
                const a = self.pop();
                self.push(Value.number(a.Number / b.Number));
            },
            .Exp => {
                const b = self.pop();
                const a = self.pop();
                self.push(Value.number(std.math.pow(f64, a.Number, b.Number)));
            },
            .Floor => {
                const a = self.peek_ptr(0);
                a.* = Value.number(@floor(a.Number));
            },
            .Mod => {
                const b = self.pop();
                const a = self.pop();
                self.push(Value.number(@mod(a.Number, b.Number)));
            },
            .Eq => {
                const b = self.pop();
                const a = self.pop();
                self.push(Value.boolean(a.Number == b.Number));
            },
            .Lte => {
                const b = self.pop();
                const a = self.pop();
                self.push(Value.boolean(a.Number <= b.Number));
            },
            .Lt => {
                const b = self.pop();
                const a = self.pop();
                self.push(Value.boolean(a.Number < b.Number));
            },
            .Gte => {
                const b = self.pop();
                const a = self.pop();
                self.push(Value.boolean(a.Number >= b.Number));
            },
            .And => {
                const b = self.pop();
                const a = self.pop();
                self.push(Value.boolean(a.Bool and b.Bool));
            },
            .Or => {
                const b = self.pop();
                const a = self.pop();
                self.push(Value.boolean(a.Bool or b.Bool));
            },
            .CallMain => {
                const main_name = frame.read_constant_idx();
                try self.call_main(main_name);
                frame = self.cur_call_frame();
            },
            .Call => {
                const count = frame.read_byte();
                const fn_name = frame.read_constant_idx();
                self.call(count, fn_name, false);
                frame = self.cur_call_frame();
            },
            .TailCall => {
                const count = frame.read_byte();
                const fn_name = frame.read_constant_idx();
                self.call(count, fn_name, true);
                frame = self.cur_call_frame();
            },
            .GetLocal => {
                const slot = frame.read_byte();
                const val = frame.read_local(slot);
                if (comptime TRACING) {
                    trace("Local idx: {d}\n", .{slot});
                    try val.debug(std.heap.c_allocator, "Got local");
                }
                self.push(val);
            },
            .Number => {
                self.push(.NumberKeyword);
            },
            .Boolean => {
                self.push(.BoolKeyword);
            },
            .String => {
                self.push(.StringKeyword);
            },
            .Object => {
                self.push(.ObjectKeyword);
            },
            .Any => {
                self.push(.Any);
            },
            .EmptyTuple => {
                self.push(Value.array(try Array.empty_tuple(self.gc)));
            },
            .MakeArray => {
                try self.make_array(false, 1);
            },
            .MakeTuple => {
                const count = frame.read_byte();
                try self.make_array(true, count);
            },
            .MakeTupleSpread => {
                const count = frame.read_byte();
                const spread_bitfield = frame.read_u256();
                try self.make_array_spread(count, spread_bitfield);
            },
            .Extends => {
                const b = self.pop();
                const a = self.pop();
                const skip_then_branch_offset = frame.read_u16();
                if (!self.extends(a, b)) {
                    frame.ip = @ptrCast(&frame.func.code[skip_then_branch_offset]);
                }
            },
            .ExtendsTrue => {
                const a = self.pop();
                const skip_then_branch_offset = frame.read_u16();
                // If not true
                if (!(@as(ValueKind, a) == .Bool and a.Bool)) {
                    frame.ip = @ptrCast(&frame.func.code[skip_then_branch_offset]);
                }
            },
            .ExtendsNoPopLeft => {
                const a = self.peek(1);
                const b = self.pop();
                const skip_then_branch_offset = frame.read_u16();
                if (!self.extends(a, b)) {
                    _ = self.pop();
                    frame.ip = @ptrCast(&frame.func.code[skip_then_branch_offset]);
                }
            },
            .PanicExtends => {
                const b = self.pop();
                const a = self.pop();
                if (!self.extends(a, b)) {
                    if (comptime TRACING) {
                        try a.debug(std.heap.c_allocator, "A type:");
                        try b.debug(std.heap.c_allocator, "B type:");
                    }
                    @panic("a does not extend b");
                }
            },
            .Constant => {
                const constant_idx = frame.read_constant_idx();
                const constant = self.read_constant(constant_idx);
                self.push(constant);
            },
            .GetGlobal => {
                const constant_idx = frame.read_constant_idx();
                const global = self.globals.get(constant_idx).?;
                if (comptime TRACING) {
                    trace("Constant idx: {d}\n", .{constant_idx.v});
                    try global.debug(std.heap.c_allocator, "Global");
                }
                self.push(global);
            },
            .SetGlobal => {
                const constant_idx = frame.read_constant_idx();
                const value = self.pop();
                try self.globals.put(constant_idx, value);
            },
            .SetInitialState => {
                const constant_idx = frame.read_constant_idx();
                const value = self.pop();
                if (self.globals.contains(constant_idx)) continue;
                try self.globals.put(constant_idx, value);
            },
            .Jump => {
                const offset = frame.read_u16();
                frame.ip = @ptrCast(&frame.func.code[offset]);
            },
            .JumpGameState => {
                const offset = frame.read_u16();
                if (self.game_state) |val| {
                    self.push(val);
                    self.game_state = null;
                    frame.ip = @ptrCast(&frame.func.code[offset]);
                }
            },
            .PopCallFrame => {
                // Set the value at the top of the stack on the return slot
                const return_val = self.peek(0);
                const return_slot = frame.slots;
                return_slot[0] = return_val;
                self.stack_top = return_slot + 1;

                self.call_frames_count -= 1;
                if (self.call_frames_count == 0) {
                    return;
                }
                frame = self.cur_call_frame();
            },
            .FormatString => {
                const args_count = frame.read_byte();
                const start: [*]Value = self.stack_top - args_count;
                var str_array = std.ArrayListUnmanaged(u8){};
                defer str_array.deinit(std.heap.c_allocator);
                for (start[0..args_count]) |v_| {
                    try v_.encode_as_string(std.heap.c_allocator, &str_array, true);
                }
                str_array.shrinkAndFree(std.heap.c_allocator, str_array.items.len);
                const bytes_ptr = try Bytes.alloc_heap(self.gc, str_array.items[0..]);
                // defer bytes_ptr.deinit(self)
                const str = try self.intern_string(String{
                    .ptr = bytes_ptr,
                    .len = @intCast(str_array.items.len),
                });
                // self.interned_strings.getO(str)
                const str_value = Value.string(str);
                self.pop_n(args_count);
                self.push(str_value);
            },
            .Negate => {
                self.push(self.pop().negate());
            },
            .MakeObj => {
                const count = frame.read_byte();
                const fields_base: [*]Value = self.stack_top - count * 2;
                const fields = fields_base[0 .. count * 2];

                const obj = try Object.new(self.gc, fields);
                self.pop_n(count * 2);
                self.push(Value.object(obj));
            },
            .Index => {
                const index = self.pop();
                const object = self.pop();
                const ret = self.index_value(object, index);
                self.push(ret);
            },
            .IndexLit => {
                const constant_idx = frame.read_constant_idx();
                const constant = self.read_constant(constant_idx);
                const object = self.pop();
                const ret = self.index_value(object, constant);
                self.push(ret);
            },
            .Update => {
                const addition = self.pop();
                const object = self.pop();
                if (!self.extends(object, .ObjectKeyword) and !self.extends(addition, .ObjectKeyword)) {
                    @panic("Both LHS and RHS of Update<...> must extend `obect`");
                }
                const new_object = try self.update_object(self.gc, object.Object, addition.Object);
                self.push(new_object);
            },
            .SetArray => {
                const index = self.pop();
                const value = self.peek(0);
                const arr = self.peek(1);
                if (index != .Number) {
                    @panic("Third argument to `SetArray` must be a number literal.");
                }
                if (arr != .Array) {
                    @panic("First argument to `SetArray` must be an array.");
                }

                const idx: u32 = @intFromFloat(@floor(index.Number));

                const new_arr = try arr.Array.dupe(self.gc);

                const ptr = new_arr.item_at_index_mut(idx) orelse {
                    std.debug.panic("Index out of bounds: {d}. Length = {d}", .{ @floor(index.Number), arr.Array.len });
                };
                ptr.* = value;
                _ = self.pop();
                _ = self.pop();
                self.push(Value.array(new_arr));
            },
            .Fill => {
                const value = self.pop();
                const arr = self.peek(0);
                if (arr != .Array) {
                    @panic("First argument to `Fill` must be an array.");
                }
                const new_arr = try arr.Array.dupe(self.gc);
                for (new_arr.items_mutable()) |*item| {
                    item.* = value;
                }
                _ = self.pop();
                self.push(Value.array(new_arr));
            },
            .NewArray => {
                const length_val = self.pop();
                if (length_val != .Number) {
                    @panic("Second argument to `NewArray` must be a number.");
                }
                const length: u32 = @intFromFloat(@floor(length_val.Number));
                if (length == 0) {
                    _ = self.pop();
                    self.push(Value.array(try Array.empty_tuple(self.gc)));
                } else {
                    var array = try Array.new_with_capacity(self.gc, length);
                    const initial_value = self.pop();
                    @memset(array.items_mutable(), initial_value);
                    self.push(Value.array(array));
                }
            },
            .Union => {
                const count = frame.read_u8();
                const args_base = self.stack_top - count;
                const args = args_base[0..count];
                const union_val = try self.make_union(std.heap.c_allocator, args);
                self.push(union_val);
            },
            .CallNative => {
                const arg_count = frame.read_u8();
                const name_idx = frame.read_constant_idx();
                const name = self.read_constant(name_idx);

                const args = (self.stack_top - arg_count)[0..arg_count];

                const name_str = name.String.as_str();
                trace("NAME: {s}", .{name_str});

                var iter = self.native_functions.iterator();
                while (iter.next()) |entry| {
                    trace("iter NAME: {s}", .{entry.key_ptr.as_str()});
                }
                const native_fn = self.native_functions.getPtr(name.String) orelse @panic("Unknown native function");
                const value = try native_fn.call(self, args);

                self.pop_n(arg_count);
                self.push(value);
            },
            .Exit => {
                self.call_frames_count -= 1;
                return;
            },
            else => {
                print("Unhandled op name: {s}\n", .{@tagName(op)});
                @panic("Unhandled op.");
            },
        }
    }
}

pub fn intern_string(self: *VM, str_: String) !String {
    var str = str_;
    var vroot = false;
    var maybe_objptr = str.ptr.asObjPtr();
    if (maybe_objptr) |*objptr| {
        self.gc.push_vroot(objptr);
        vroot = true;
    }
    defer if (vroot) {
        defer self.gc.pop_vroot();
    };
    const result = try self.interned_strings.getOrPut(str);
    if (result.found_existing) return result.value_ptr.*;
    result.value_ptr.* = str;
    return str;
}

fn eq(self: *VM, a: Value, b: Value) bool {
    // Doing very simple thing of checking A extends B and B extends A for equality
    return self.extends(a, b) and self.extends(b, a);
}

fn call_native(self: *VM, fn_name: ConstantTableIdx, args: []const Value) void {
    _ = args;
    _ = fn_name;
    _ = self;
}

/// Typescript's "extends" is a terrible name, but kept here for consistency's sake.
/// A better term is "subtype". First think of types as sets. For example,
/// `string` is a set of every possible string type: "foo", "bar", "sldsad", and
/// so on.
///
/// When we say `A subtypes B`, we are actually saying `A is a subset of
/// B`. For example: "foo" subtypes `string`.
fn extends(self: *VM, a: Value, b: Value) bool {
    if (b == .Any or a == .Any) {
        return true;
    }
    if (b == .Undefined) return a == .Undefined;
    if (b == .NumberKeyword) return @as(ValueKind, a) == ValueKind.Number or @as(ValueKind, a) == ValueKind.NumberKeyword;
    if (b == .BoolKeyword) return @as(ValueKind, a) == ValueKind.Bool or @as(ValueKind, a) == ValueKind.BoolKeyword;
    if (b == .StringKeyword) return @as(ValueKind, a) == ValueKind.String or @as(ValueKind, a) == ValueKind.StringKeyword;
    if (b == .ObjectKeyword) return @as(ValueKind, a) == ValueKind.Object or @as(ValueKind, a) == ValueKind.ObjectKeyword;

    if (@as(ValueKind, b) == .Number) return @as(ValueKind, a) == .Number and a.Number == b.Number;
    if (@as(ValueKind, b) == .Bool) return @as(ValueKind, a) == .Bool and a.Bool == b.Bool;
    // `a.String.ptr == b.String.ptr` should be sufficient because all strings
    // are interned, meaning that string equality is synonymous to pointer
    // equality. However, if we allow strings to have the same pointer (e.g.
    // "foobar" and "foo" share the same base pointer), this will obviously
    // break without a length check.
    if (@as(ValueKind, b) == .String) return @as(ValueKind, a) == .String and @as(usize, @bitCast(a.String.ptr)) == @as(usize, @bitCast(b.String.ptr));

    if (@as(ValueKind, b) == .Array) return @as(ValueKind, a) == .Array and self.extends_array(a.Array, b.Array);
    if (@as(ValueKind, b) == .Object) return @as(ValueKind, a) == .Object and self.extends_object(a.Object, b.Object);

    // TODO: Discriminant unions have slightly different subtyping logic that
    // need to be supported.
    if (@as(ValueKind, b) == .Union) return self.extends_any_of(a, b.Union.variants_slice());
    if (@as(ValueKind, a) == .Union) return self.extends_many_all(a.Union.variants_slice(), b);

    return false;
}

fn extends_object(self: *VM, a: *Object, b: *Object) bool {
    // Everything extends the empty object: `{}`
    if (b.len == 0) return true;
    // If `a` has less keys than `b` then it cannot possibly subtype `b`
    if (a.len < b.len) return false;

    // Now we know `a` either has the same keys as `b` or more:
    // 1. Check that each key in `b` exists in `a`
    // 2. Check that the value in `a[key]` subtypes the value in `b[key]`
    for (b.fields_slice()) |*bfield| {
        const afield = a.get_field(bfield.name) orelse return false;
        if (!self.extends(afield.value, bfield.value)) return false;
    }

    return true;
}

fn extends_array(self: *VM, a: *Array, b: *Array) bool {
    // Empty tuple
    if (b.len == 0) return a.len == 0;

    // `b` is an `Array<T>`, so either:
    // 1. `a` is an empty tuple [] which extends any Array<T>
    // 2. `a` is either a tuple or regular Array<T>, in either case
    //     all of its `items` need to extend `b`'s Array type
    if (!b.is_tuple_array()) return a.len == 0 or self.extends_many_all(a.items(), b.ptr.?[0]);

    // Now we know `b` is a tuple. Then `a` extends `b` if `a` is a tuple of the same size
    // with each item extending the corresponding item in `b`

    if (b.len != a.len) return false;

    for (a.items(), b.items()) |av, bv| {
        if (!self.extends(av, bv)) return false;
    }

    return true;
}

/// Returns true when all of `a_items` extends `b_item`
fn extends_many_all(self: *VM, a_items: []const Value, b_item: Value) bool {
    for (a_items) |item| {
        if (!self.extends(item, b_item)) return false;
    }
    return true;
}

/// Returns true when `a` extends any of `b_items`
fn extends_any_of(self: *VM, a: Value, b_items: []const Value) bool {
    for (b_items) |item| {
        if (self.extends(a, item)) return true;
    }
    return false;
}

fn index_value(self: *VM, object: Value, index: Value) Value {
    switch (object) {
        .Object => |obj| {
            const field = obj.get_field(index.String) orelse return .Any;
            return field.value;
        },
        .Array => |arr| {
            switch (index) {
                .Number => |v| {
                    if (std.math.floor(v) != v) return .Undefined;
                    return arr.item_at_index(@intFromFloat(v));
                },
                .String => |s| {
                    if (self.length_string_ptr != null and self.length_string_ptr.? == s.ptr.ptrToSlice()) return Value.number(@floatFromInt(arr.len));
                    // TODO:
                    unreachable;
                },
                // TODO:
                else => unreachable,
            }
        },
        .String => |str| {
            _ = str;
            if (@as(ValueKind, index) == ValueKind.Number) return .StringKeyword;
            // TODO:
            unreachable;
        },
        .Union => |uni| {
            _ = uni;
            // TODO: Only properties available to all variants can be indexed
            // If the value of the property differs among variants, then the
            // result of indexing it is a union of all possible values:
            // ```typescript
            // type Union = { type: 'foo', data: string } | { type: 'bar', data: number }
            //
            //
            // type result = Union['type']  // 'foo' | 'bar'
            // type result2 = Union['data'] // string | number
            // ```
            //
            unreachable;
        },
        else => return .Any,
    }
}

fn update_object(self: *VM, gc: *GC, base_: *Object, additional_: *Object) !Value {
    _ = self; // autofix
    var base = base_;
    var additional = additional_;
    if (base.len == 0 and additional.len == 0) @panic("TODO: Empty object");
    gc.push_vroot(@ptrCast(&base));
    gc.push_vroot(@ptrCast(&additional));
    defer {
        gc.pop_vroot();
        gc.pop_vroot();
    }
    var object = try gc.as_allocator().create(Object);
    object.* = .{
        .fields = null,
        .len = 0,
    };
    gc.push_vroot(@ptrCast(&object));
    defer {
        gc.pop_vroot();
    }

    var actual_len: usize = base.len + additional.len;
    var new_fields = try gc.as_allocator().alloc(Object.Field, actual_len);
    if (base.len == 0) {
        @memcpy(new_fields[0..additional.len], additional.fields_slice());
    } else if (additional.len == 0) {
        @memcpy(new_fields[0..base.len], base.fields_slice());
    } else {
        // trace("BASE {}", .{base});
        @memcpy(new_fields[0..base.len], base.fields_slice());
        var i: u32 = base.len;
        for (additional.fields_slice()) |update_field| {
            const dummy_field: Object.Field = .{
                .name = update_field.name,
                .value = .Any,
            };
            if (binary_search(Object.Field, new_fields, &dummy_field, Object.Field.cmp_key)) |idx| {
                new_fields[idx] = update_field;
                continue;
            }
            new_fields[i] = update_field;
            i += 1;
        }

        actual_len = i;
    }
    const actual_new_fields = new_fields[0..actual_len];
    std.sort.block(Object.Field, actual_new_fields, {}, Object.Field.less_than_key);

    object.* = .{
        .fields = actual_new_fields.ptr,
        .len = @intCast(actual_len),
    };

    object.panic_on_duplicate_keys();

    return Value.object(object);
}

/// This creates a union from the given list of variants. The variants may
/// normalize to a single value, so it is not guaranteed that the resultant
/// Value is a Union.
fn make_union(self: *VM, alloc: Allocator, variants: []const Value) !Value {
    var variants_list = try std.ArrayListUnmanaged(Value).initCapacity(alloc, variants.len);

    // Flatten/normalize the union by only allowing the largest types.
    // TODO: This is broken, just use the O(N^2) algo for now, but we can make it smarter by using some heuristics (e.g. if a string is present in the union, we can omit all string literals)
    for (variants) |variant| {
        if (@as(ValueKind, variant) == .Union) {
            for (variant.Union.variants_slice()) |nested_variant| {
                try self.make_union_impl(alloc, nested_variant, &variants_list);
            }
            continue;
        }
        try self.make_union_impl(alloc, variant, &variants_list);
    }

    // If the normalized union is just a single type, return it
    if (variants_list.items.len == 1) {
        defer variants_list.deinit(alloc);
        return variants_list.items[0];
    }

    if (variants_list.items.len != variants.len) {
        variants_list.shrinkAndFree(alloc, variants_list.items.len);
    }

    // Detect if the union is a tagged union
    // 1. Every variant must be an object
    // 2. There must be a common key among them (the "discriminant" key)
    // 3. The value of this discriminant key must be a literal
    // 4. There can only be one discriminant key/value pair

    // Check 1: Every variant must be an object
    const all_objects = all_objects: {
        for (variants_list.items) |variant| {
            if (@as(ValueKind, variant) != .Object) break :all_objects false;
        }
        break :all_objects true;
    };

    const discriminant_key: ?String = discriminant_key: {
        if (all_objects) {
            // Check 2-4
            const variants_len = variants_list.items.len;

            var key_counts = StringMap(struct { count: u8, len: u32 }).init(alloc);
            defer key_counts.deinit();

            for (variants_list.items) |variant| {
                const obj: *Object = variant.Object;
                for (obj.fields_slice()) |*field| {
                    const value = key_counts.getPtr(field.name) orelse {
                        if (field.value.as_literal() == null) continue;
                        try key_counts.put(field.name, .{ .count = 1, .len = field.name.len });
                        continue;
                    };

                    if (field.value.as_literal() == null) {
                        _ = key_counts.swapRemove(field.name);
                    } else {
                        value.count += 1;
                    }
                }
            }

            var possible_tag_key: ?struct { ptr: String, len: u32 } = null;
            var iter = key_counts.iterator();
            while (iter.next()) |entry| {
                if (entry.value_ptr.count == variants_len) {
                    // If there is more than one key that matches the conditions for
                    // a discriminant key, then all fail.
                    if (possible_tag_key != null) break :discriminant_key null;
                    possible_tag_key = .{ .ptr = entry.key_ptr.*, .len = entry.value_ptr.len };
                }
            }

            break :discriminant_key if (possible_tag_key) |tag| tag.ptr else null;
        }
        break :discriminant_key null;
    };

    const union_ptr = try alloc.create(Union);
    union_ptr.* = .{
        .variants = variants_list.items.ptr,
        .len = @intCast(variants_list.items.len),
        .discriminant_key = discriminant_key,
    };

    return Value.make_union(union_ptr);
}

fn make_union_impl(self: *VM, alloc: Allocator, potential_variant: Value, existing_variants: *std.ArrayListUnmanaged(Value)) !void {
    for (existing_variants.items) |existing_variant| {
        if (self.extends(potential_variant, existing_variant)) return;
    }
    try existing_variants.append(alloc, potential_variant);
}

fn make_array_spread(self: *VM, count: u32, spread_bitfield: u256) !void {
    const spread_count = @popCount(spread_bitfield);
    tyvm.debug_assert(spread_count > 0);

    var bitset = std.bit_set.IntegerBitSet(256).initEmpty();
    bitset.mask = spread_bitfield;

    const array_args_base = self.stack_top - count;

    const total_size = size: {
        var total: u32 = @as(u32, @intCast(count)) - @as(u32, @intCast(spread_count));

        var iter = bitset.iterator(.{});
        while (iter.next()) |idx| {
            const val: Value = array_args_base[idx];
            tyvm.debug_assert(@as(ValueKind, val) == ValueKind.Array);
            total += val.Array.len;
        }

        break :size total;
    };

    if (total_size == 0) {
        self.pop_n(count);
        self.push(Value.array(try Array.empty_tuple(self.gc)));
        return;
    }

    var array = try self.gc.as_allocator().create(Array);
    array.* = .{
        .ptr = null,
        .len = 0,
        .flags = .{},
    };
    const ptrptr: **ObjHeader = @ptrCast(&array);
    self.gc.push_vroot(ptrptr);
    defer self.gc.pop_vroot();

    trace("PTR: {d}", .{@intFromPtr(ptrptr)});
    if (comptime tyvm.allow_assert) {
        const cast: *GC.AnyObjHeaderPtr = @ptrCast(ptrptr);
        tyvm.debug_assert(cast.getPtr() == @intFromPtr(array));
        const from_space_ptr: *ObjHeader = @ptrFromInt(cast.getPtr());
        tyvm.debug_assert(from_space_ptr.tag.tytag == .arr);
    }

    trace("STACK first: {}", .{self.stack[0]});
    var ptr = try self.gc.as_allocator().alloc(Value, total_size);

    trace("STACK first: {}", .{self.stack[0]});

    var prev: usize = 0;
    var iter = bitset.iterator(.{});
    var i: usize = 0;
    while (iter.next()) |idx| {
        const diff = idx - prev;
        // + => regular array item
        // ^ => spread array item
        //
        // diff >= 1 means we need to push regular array items:
        // 0 1 2 3 4 5 6
        // + ^
        if (diff >= 1) {
            const start = prev;
            // const end = idx - 1;
            const end = idx;

            const len = end - start;
            @memcpy(ptr[i .. i + len], array_args_base[start..end]);
            i += len;
        }

        const val: Value = array_args_base[idx];
        tyvm.debug_assert(@as(ValueKind, val) == ValueKind.Array);
        @memcpy(ptr[i .. i + val.Array.len], val.Array.items());

        i += val.Array.len;
        prev = idx + 1;
    }

    // Process the rest
    // prev = 4
    // count = 7
    // 0 1 2 3 4 5 6
    // + ^ + + ^ + +
    if (prev < count) {
        const len = count - prev;
        @memcpy(ptr[i .. i + len], array_args_base[prev..count]);
    }

    array.* = Array{
        .ptr = ptr.ptr,
        .len = total_size,
        .flags = Array.Flags{
            .is_tuple = true,
        },
    };

    self.pop_n(count);
    self.push(Value.array(array));
}

fn make_array(self: *VM, comptime is_tuple: bool, count: u32) !void {
    if (!is_tuple) tyvm.debug_assert(count == 1);
    const items_ptr = self.stack_top - count;
    const items = items_ptr[0..count];
    const array = try Array.new(self.gc, is_tuple, items, &[_]Value{});

    self.pop_n(count);
    self.push(Value.array(array));
}

fn call_main(self: *VM, main_name: ConstantTableIdx) !void {
    const count: u8 = if (self.is_game) 2 else 1;
    // self.push(.Any);
    // // TODO: read args from stdout
    // try self.make_array(true, count);
    if (comptime tyvm.isDebug) {
        if (self.is_game) {
            tyvm.debug_assert(@as(ValueKind, (self.stack_top - 2)[0]) == ValueKind.Array);
        } else {
            tyvm.debug_assert(@as(ValueKind, (self.stack_top - 1)[0]) == ValueKind.Array);
        }
    }
    self.call(count, main_name, false);
}

/// Call a function. This assumes the top of the stack has the N values for the function's arguments.
fn call(self: *VM, arg_count: u8, fn_name: ConstantTableIdx, tail_call: bool) void {
    {
        const function_name_constant = self.functions.getPtr(fn_name).?.name;
        const constant_idx = self.constant_table[function_name_constant.v].idx;
        const function_name = self.read_constant_string(constant_idx);
        std.debug.print("Call function: {s}\n", .{function_name.as_str()});
    }
    // Reuse the call frame
    if (tail_call) {
        const current_call_frame = self.cur_call_frame();
        // If the function is the same as the current function we just need to:
        // 1. Reset the instruction pointer
        // 2. memcpy the arguments into the call frames argument stack slots
        if (fn_name.v == current_call_frame.func.name.v) {
            current_call_frame.ip = current_call_frame.func.code.ptr;
            if (arg_count > 0) {
                const args_start = self.stack_top - arg_count;
                const src = args_start[0..arg_count];
                const dest = current_call_frame.slots[0..arg_count];
                // `src` and `dest` won't overlap unless arg_count == 0
                @memcpy(dest, src);
            }
            self.stack_top = current_call_frame.slots + arg_count;
            return;
        }

        // Otherise, replace current call frame.
        const new_func: *const Function = self.functions.getPtr(fn_name).?;
        const old_slots = current_call_frame.slots;

        if (arg_count > 0) {
            const args_start = self.stack_top - arg_count;
            const src = args_start[0..arg_count];
            const dest = current_call_frame.slots[0..arg_count];
            std.mem.copyForwards(Value, dest, src);
        }

        self.stack_top = old_slots + arg_count;

        current_call_frame.* = .{ .func = new_func, .ip = new_func.code.ptr, .slots = old_slots };
        return;
    }

    // 0 1 2 3 (4) 5 6 7 8 (9)
    if (comptime TRACING) {
        trace("PRINTING FUNCTIONS!\n", .{});
        var iter = self.functions.iterator();
        while (iter.next()) |val| {
            const function = val.value_ptr;
            const tableidx = self.constant_table[function.name.v];
            const foo = self.read_constant_string(tableidx.idx);
            trace("SLICE: {s}\n", .{foo.as_str()});
        }
    }
    const func: *const Function = self.functions.getPtr(fn_name).?;
    self.push_call_frame(.{ .func = func, .ip = func.code.ptr, .slots = self.stack_top - arg_count });
}

inline fn cur_call_frame(self: *VM) *CallFrame {
    return &self.call_frames[self.call_frames_count - 1];
}

inline fn peek(self: *VM, distance: usize) Value {
    return (self.stack_top - 1 - distance)[0];
}

inline fn peek_ptr(self: *VM, distance: usize) *Value {
    return &(self.stack_top - 1 - distance)[0];
}

inline fn push(self: *VM, value: Value) void {
    // trace("Stack push: {}", .{value});
    self.stack_top[0] = value;
    self.stack_top += 1;
}

inline fn pop(self: *VM) Value {
    // trace("Stack pop", .{});
    self.stack_top -= 1;
    return self.stack_top[0];
}

inline fn pop_n(self: *VM, n: usize) void {
    self.stack_top -= n;
}

fn push_call_frame(self: *VM, call_frame: CallFrame) void {
    self.call_frames[self.call_frames_count] = call_frame;
    self.call_frames_count += 1;
}

fn read_constant(self: *const VM, idx: ConstantTableIdx) Value {
    const constant_entry = self.constant_table[idx.v];
    return switch (constant_entry.kind) {
        .Bool => .{ .Bool = self.read_constant_boolean(constant_entry.idx) },
        .Number => .{ .Number = self.read_constant_number(constant_entry.idx) },
        .String => .{ .String = self.read_constant_string(constant_entry.idx) },
        .Bytes => .{ .Bytes = self.read_constant_bytes(constant_entry.idx) },
    };
}

fn read_constant_boolean(self: *const VM, constant_idx: ConstantIdx) bool {
    return self.constant_pool[constant_idx.v] == 1;
}

fn read_constant_number(self: *const VM, constant_idx: ConstantIdx) f64 {
    const ptr: *const f64 = @ptrCast(@alignCast(&self.constant_pool[constant_idx.v]));
    return ptr.*;
}

fn read_constant_string(self: *const VM, constant_idx: ConstantIdx) String {
    const ptr: *const extern struct { idx: u32 align(8), len: u32 } = @ptrCast(@alignCast(&self.constant_pool[constant_idx.v]));
    const constant_bytes_idx: u32 = ptr.idx;
    const slice_len: u32 = ptr.len;
    const bytesptr = self.read_constant_bytes(ConstantIdx{ .v = constant_bytes_idx });
    return String{
        .ptr = bytesptr,
        .len = slice_len,
    };
}

fn read_constant_bytes(self: *const VM, constant_idx: ConstantIdx) BytesPtr {
    const entry = self.constant_table[constant_idx.v];
    const constant_bytes_ptr: [*]const u8 = @ptrCast(&self.constant_pool[entry.idx.v]);
    const actual_ptr: [*]const u8 = constant_bytes_ptr;
    tyvm.debug_assert((@intFromPtr(actual_ptr) & 0b111) == 0);
    return BytesPtr{
        .impl = BytesPtr.Impl.init(.constant, actual_ptr),
    };
}

/// The bytecode format is laid out like so:
/// 1. Header
///    - magic bytes
///    - constant pool start offset
///    - functions bytecode start offset
///
/// 2. Constant pool
///    - comprised of a) table, b) an array of bytes which represent constants
///    - each constant is accessed by a "constant idx", which is index into the table
///    - the table tells you:
///        - what type of value the constant has
///        - where to find the constant's value in the array of bytes (the offset at which it begins)
///
/// 3. Functions
///    - also contains a table (an array of FunctionTableEntry)
///    - and then an array of functions
///    - the table is just there to mark where to find each function and how large its bytecode is
const Bytecode = struct {
    const Header = extern struct {
        magic: u32 align(8),
        constants_offset: u32,
        functions_offset: u32,
        is_game: u32,
    };

    const ConstantsHeader = extern struct {
        table_len: u32 align(8),
        pool_size: u32,
        pool_padding: u32,
    };

    const FunctionsHeader = extern struct {
        table_len: u32 align(8),
        table_offset: u32,
        function_code_size: u32,
        function_code_padding: u32,
    };

    const FunctionTableEntry = struct {
        name_constant_idx: ConstantTableIdx align(8),
        offset: u32,
        size: u32,
    };
};

const ConstantBytes = extern struct {
    len: u32 align(8),
    _pad: u32 = 0,
};
const ConstantString = extern struct { len: u32 align(8), ptr: [*]const u8 };

const ConstantTableIdx = extern struct {
    v: u32,

    pub inline fn new(v: u32) ConstantTableIdx {
        return .{ .v = v };
    }
};
const ConstantIdx = extern struct {
    v: u32,

    pub inline fn new(v: u32) ConstantIdx {
        return .{ .v = v };
    }
};

pub const ConstantKind = enum(u32) { Bool, Number, String, Bytes };

const ConstantTableEntry = extern struct {
    idx: ConstantIdx align(8),
    kind: ConstantKind,
};

const CallFrame = struct {
    ip: [*]const u8,
    /// Pointer into VM's value stack at the first slot this function can run
    slots: [*]Value,
    func: *const Function,

    fn uninit() CallFrame {
        return .{
            .ip = undefined,
            .slots = undefined,
            .func = undefined,
        };
    }

    fn read_local(self: *CallFrame, local: u8) Value {
        return self.slots[local];
    }

    fn read_int(self: *CallFrame, comptime T: type) T {
        const byte_amount = comptime @divExact(@typeInfo(T).Int.bits, 8);
        const val = std.mem.readInt(T, @ptrCast(self.ip[0..byte_amount]), .little);
        self.ip += byte_amount;
        return val;
    }

    fn read_byte(self: *CallFrame) u8 {
        return self.read_int(u8);
    }

    fn read_u8(self: *CallFrame) u8 {
        return self.read_int(u8);
    }

    fn read_u16(self: *CallFrame) u16 {
        return self.read_int(u16);
    }

    fn read_u32(self: *CallFrame) u32 {
        return self.read_int(u32);
    }

    fn read_u256(self: *CallFrame) u256 {
        return self.read_int(u256);
    }

    fn read_constant_idx(self: *CallFrame) ConstantTableIdx {
        return ConstantTableIdx.new(self.read_u32());
    }
};

pub const Function = struct {
    name: ConstantTableIdx,
    code: []const u8,

    pub fn read_int(self: *const Function, comptime T: type, i: *usize) T {
        const byte_amount = comptime @divExact(@typeInfo(T).Int.bits, 8);
        const val = std.mem.readInt(T, @ptrCast(self.code[i.* .. i.* + byte_amount]), .little);
        i.* = i.* + byte_amount;
        return val;
    }

    pub fn read_u8(self: *const Function, i: *usize) u8 {
        return self.read_int(u8, i);
    }

    pub fn read_u32(self: *const Function, i: *usize) u32 {
        return self.read_int(u32, i);
    }

    pub fn read_u128(self: *const Function, i: *usize) u128 {
        return self.read_int(u128, i);
    }

    pub fn read_u256(self: *const Function, i: *usize) u256 {
        return self.read_int(u256, i);
    }

    pub fn read_constant_table_idx(self: *const Function, i: *usize) ConstantTableIdx {
        return ConstantTableIdx.new(self.read_u32(i));
    }

    pub fn disassemble(self: *const Function, vm: *const VM) void {
        const name = vm.read_constant(self.name).String;
        print("== {s} ==\n", .{name.as_str()});
        var i: usize = 0;
        while (i < self.code.len) {
            const op: Op = @enumFromInt(self.code[i]);
            const j = i;
            i += 1;
            switch (op) {
                .CallMain => {
                    const idx = self.read_constant_table_idx(&i);
                    std.debug.print("{} CallMain {s}\n", .{ j, vm.read_constant(idx).String.as_str() });
                },
                .Lte => {
                    std.debug.print("{} Lte\n", .{j});
                },
                .Lt => {
                    std.debug.print("{} Lt\n", .{j});
                },
                .Gte => {
                    std.debug.print("{} Gte\n", .{j});
                },
                .And => {
                    std.debug.print("{} And\n", .{j});
                },
                .Or => {
                    std.debug.print("{} Or\n", .{j});
                },
                .Eq => {
                    std.debug.print("{} EQ\n", .{j});
                },
                .Number => {
                    std.debug.print("{} Number\n", .{j});
                },
                .Boolean => {
                    std.debug.print("{} Boolean\n", .{j});
                },
                .String => {
                    std.debug.print("{} String\n", .{j});
                },
                .Object => {
                    std.debug.print("{} Object\n", .{j});
                },
                .Any => {
                    std.debug.print("{} Any\n", .{j});
                },
                .Add => {
                    std.debug.print("{} ADD\n", .{j});
                },
                .Sub => {
                    std.debug.print("{} SUB\n", .{j});
                },
                .Mul => {
                    std.debug.print("{} Mul\n", .{j});
                },
                .Div => {
                    std.debug.print("{} Div\n", .{j});
                },
                .Exp => {
                    std.debug.print("{} Exp\n", .{j});
                },
                .Floor => {
                    std.debug.print("{} Floor\n", .{j});
                },
                .Mod => {
                    std.debug.print("{} Mod\n", .{j});
                },
                .Intersect => {
                    const count = self.code[i];
                    i += 1;
                    std.debug.print("{} INTERSECT: {}\n", .{ j, count });
                },
                .Union => {
                    const count = self.read_u8(&i);
                    std.debug.print("{} Union: {d}\n", .{ j, count });
                },
                .Constant => {
                    const idx = self.read_constant_table_idx(&i);
                    std.debug.print("{} CONST: {any}\n", .{ j, vm.read_constant(idx) });
                },
                .Pop => {
                    std.debug.print("{} POP\n", .{j});
                },
                .TailCall, .Call => {
                    const count = self.code[i];
                    i += 1;
                    const name_idx = self.read_constant_table_idx(&i);
                    std.debug.print("{} {?} {?} {s}\n", .{ j, op, count, vm.read_constant(name_idx).String.as_str() });
                },
                .SetLocal => {
                    const idx = self.code[i];
                    i += 1;
                    std.debug.print("{} Set local {?}\n", .{ j, idx });
                },
                .GetLocal => {
                    const idx = self.code[i];
                    i += 1;
                    std.debug.print("{} Get local {?}\n", .{ j, idx });
                },
                .SetGlobal => {
                    const idx = self.read_constant_table_idx(&i);
                    std.debug.print("{} SET GLOBAL {s}\n", .{ j, vm.read_constant(idx).String.as_str() });
                },
                .SetInitialState => {
                    const idx = self.read_constant_table_idx(&i);
                    std.debug.print("{} SetInitialState {s}\n", .{ j, vm.read_constant(idx).String.as_str() });
                },
                .GetGlobal => {
                    const idx = self.read_constant_table_idx(&i);
                    std.debug.print("{} GET GLOBAL {s}\n", .{ j, vm.read_constant(idx).String.as_str() });
                },
                .PanicExtends => {
                    std.debug.print("{} PanicExtends\n", .{j});
                },
                .Extends, .ExtendsTrue, .ExtendsNoPopLeft => {
                    const skip_then = @as(u16, @intCast(self.code[i + 1])) << 8 | @as(u16, @intCast(self.code[i]));
                    i += 2;
                    std.debug.print("{} {?} (skip_then={})\n", .{ j, op, skip_then });
                },
                .Jump => {
                    const offset = @as(u16, @intCast(self.code[i + 1])) << 8 | @as(u16, @intCast(self.code[i]));
                    i += 2;
                    std.debug.print("{} JUMP {}\n", .{ j, offset });
                },
                .PopCallFrame => {
                    std.debug.print("{} POP CALL FRAME\n", .{j});
                },
                .MakeObj => {
                    const count = self.read_u8(&i);
                    std.debug.print("{} Make obj {?}\n", .{ j, count });
                },
                .MakeArray => {
                    std.debug.print("{} MakeArray\n", .{j});
                },
                .EmptyTuple => {
                    std.debug.print("{} EmptyTuple\n", .{j});
                },
                .MakeTuple => {
                    const count = self.read_u8(&i);
                    std.debug.print("{} MakeTuple {d}\n", .{ j, count });
                },
                .MakeTupleSpread => {
                    const count = self.read_u8(&i);
                    const spread_bitfield = self.read_u256(&i);
                    std.debug.print("{} MakeTupleSpread {d} {d}\n", .{ j, count, spread_bitfield });
                },
                .Index => {
                    std.debug.print("{} Index\n", .{j});
                },
                .IndexLit => {
                    const constant = self.read_constant_table_idx(&i);
                    std.debug.print("{} IndexLit {?}\n", .{ j, constant });
                },
                .FormatString => {
                    const count = self.code[i];
                    i += 1;
                    std.debug.print("{} FormatString {?}\n", .{ j, count });
                },
                .Negate => {
                    std.debug.print("{} Negate\n", .{j});
                },
                .Exit => {
                    std.debug.print("{} Exit\n", .{j});
                },
                .Update => {
                    std.debug.print("{} Update\n", .{j});
                },
                .SetArray => {
                    std.debug.print("{} SetArray\n", .{j});
                },
                .Fill => {
                    std.debug.print("{} Fill\n", .{j});
                },
                .NewArray => {
                    std.debug.print("{} NewArray\n", .{j});
                },
                .Panic => {
                    std.debug.print("{} Panic\n", .{j});
                },
                .Length => {
                    std.debug.print("{} Length\n", .{j});
                },
                .CallNative => {
                    const count = self.read_u8(&i);
                    const name_idx = self.read_constant_table_idx(&i);
                    const str = vm.read_constant(name_idx);
                    std.debug.print("{} CallNative {s} {d}\n", .{ j, str.String.as_str(), count });
                },
                .JumpGameState => {
                    const offset = @as(u16, @intCast(self.code[i + 1])) << 8 | @as(u16, @intCast(self.code[i]));
                    i += 2;
                    std.debug.print("{} JumpGameState {}\n", .{ j, offset });
                },
            }
        }
        print("== end {s} ==\n", .{name.as_str()});
    }

    // pub fn disassemble(self: *const Function, constant_table: []const ConstantTableEntry, constant_pool: []const u8) void {
    //     const name = self.constant_string(self.name);
    //     print("== {s} ==\n", .{name.as_str()});
    //     var i: usize = 0;
    //     while (i < self.code.len) {
    //         i = self.disassemble_instruction(i, constant_table, constant_pool);
    //     }
    //     print("== end {s} ==\n", .{name.as_str()});
    // }

    // pub fn disassemble_instruction(self: *const Function, offset: usize, constant_table: []const ConstantTableEntry, constant_pool: []const u8) usize {
    //     _ = constant_pool;
    //     _ = constant_table;
    //     print("{d:04} ", .{offset});
    //     if (offset > 0 and self.lines.items[offset] == self.lines.items[offset - 1]) {
    //         print("   | ", .{});
    //     } else {
    //         print("{d:4} ", .{self.lines.items[offset]});
    //     }
    //     const op: Op = @enumFromInt(self.code[offset]);
    //     switch (op) {
    //         .Add, .Sub, .Eq, .Lte, .Intersect, .Union => return self.disassemble_simple_instruction(op, offset),
    //     }
    // }

    // pub fn disassemble_simple_instruction(self: *const Function, op: Op, offset: usize) usize {
    //     _ = self;
    //     print("{s}\n", .{@tagName(op)});
    //     return offset + 1;
    // }

    // pub fn disasseble_instruction_with_operands(self: *const Function, op: Op, offset: usize, comptime operands: usize) usize {

    // }
};

const NativeFunction = struct {
    name: []align(8) const u8,
    fn_ptr: *const fn (vm: *VM, []const Value) anyerror!Value,
    arg_types: []const Value,

    pub fn call(self: *const NativeFunction, vm: *VM, args: []const Value) !Value {
        if (self.arg_types.len != args.len) @panic("Invalid args");
        for (self.arg_types, args) |check_arg, arg| {
            if (!vm.extends(arg, check_arg)) @panic("Arg a does not extend arg b");
        }
        return try self.fn_ptr(vm, args);
    }

    pub fn assert_eq_impl(vm: *VM, args: []const Value) !Value {
        if (!vm.eq(args[0], args[1])) @panic("Not equal");
        return .Undefined;
    }

    pub fn print_impl(vm: *VM, args: []const Value) !Value {
        _ = vm;
        const val = args[0];
        if (@as(ValueKind, val) == ValueKind.String) {
            print("{s}\n", .{val.String.as_str()});
        } else {
            var buf = std.ArrayListUnmanaged(u8){};
            defer buf.deinit(std.heap.c_allocator);
            try val.encode_as_string(std.heap.c_allocator, &buf, false);
            print("{s}\n", .{buf.items.ptr[0..buf.items.len]});
        }
        return args[0];
    }

    pub fn write_file_impl(vm: *VM, args: []const Value) !Value {
        _ = args;
        _ = vm;
        @panic("Unimplemented: WriteFile<...>");
    }

    pub fn to_typescript_source_impl(vm: *VM, args: []const Value) !Value {
        _ = args;
        _ = vm;
        @panic("Unimplemented: ToTypescriptSource<...>");
    }

    pub fn parse_int_impl(vm: *VM, args: []const Value) !Value {
        _ = args;
        _ = vm;
        @panic("Unimplemented: ParseInt<...>");
    }

    pub fn panic_impl(vm: *VM, args: []const Value) !Value {
        _ = vm;
        const val = args[0];
        if (@as(ValueKind, val) == ValueKind.String) {
            print("{s}\n", .{val.String.as_str()});
        } else {
            var buf = std.ArrayListUnmanaged(u8){};
            defer buf.deinit(std.heap.c_allocator);
            try val.encode_as_string(std.heap.c_allocator, &buf, false);
            print("{s}\n", .{buf.items.ptr[0..buf.items.len]});
        }
        @panic("");
    }

    pub fn request_anim_frame_impl(vm: *VM, args: []const Value) !Value {
        var arg = args[0];
        vm.game_state = arg;
        const drawCommandsString = vm.interned_strings.get(String.from_literal(const_str("drawCommands"))).?;
        const drawCommandsField = arg.Object.get_field(drawCommandsString).?;
        const drawCommandsValue = drawCommandsField.value;

        var buf = std.ArrayListUnmanaged(u8){};
        // defer buf.deinit(std.heap.c_allocator);
        // var vroot = false;
        // if (arg.as_anyobjheaderptr()) |obj_ptr| {
        //     vm.gc.push_vroot(@ptrCast(obj_ptr));
        //     vroot = true;
        // }
        // defer if (vroot) vm.gc.pop_vroot();
        try drawCommandsValue.serialize_to_json(std.heap.c_allocator, &buf);

        raf.request_anim_frame(buf.items.ptr, buf.items.len, buf.capacity);

        return Value.Any;
    }

    pub fn rand_impl(vm: *VM, args: []const Value) !Value {
        _ = vm;
        const min = args[0].Number;
        const max = args[1].Number;

        const space_width = max - min;

        const random = rnd.random().float(f64) * space_width + min;
        return Value.number(random);
    }
};

const Op = enum(u8) {
    // 2 stack operands
    Add = 0,
    Sub,
    Mul,
    Div,
    Exp,
    Floor,
    Mod,
    Eq,
    Lte,
    Lt,
    Gte,
    And,
    Or,
    Intersect,
    Union,
    Constant,
    Pop,
    Call,
    TailCall,
    CallMain,
    CallNative,
    Extends,
    ExtendsTrue,
    ExtendsNoPopLeft,
    PanicExtends,
    Jump,
    JumpGameState,
    Number,
    Boolean,
    String,
    Object,
    PopCallFrame,
    MakeObj,
    EmptyTuple,
    MakeArray,
    MakeTuple,
    MakeTupleSpread,

    Index,
    IndexLit,

    Panic,

    SetLocal,
    GetLocal,
    SetGlobal,
    GetGlobal,
    SetInitialState,

    FormatString,
    Any,
    Length,

    Negate,
    Update,
    SetArray,
    Fill,
    NewArray,

    Exit,
};

pub const ValueKind = enum {
    Any,
    Undefined,
    NumberKeyword,
    BoolKeyword,
    StringKeyword,
    ObjectKeyword,

    Number,
    Bool,
    String,
    Array,
    Object,
    Union,
    Bytes,
};

const Literal = union(enum) {
    Number: f64,
    Bool: bool,
    String: String,
};

pub const ObjHeader = struct {
    tag: ObjTag,

    pub inline fn setForwarded(this: *ObjHeader, forwarded: bool) void {
        this.tag.lowtag = if (forwarded) .forwarded else .none;
    }

    pub inline fn isForwarded(this: *ObjHeader) bool {
        return this.tag.lowtag == .forwarded;
    }

    pub inline fn clearForwarded(this: ObjHeader) ObjHeader {
        var out = this;
        out.tag.lowtag = .none;
        return out;
    }

    pub fn narrow(this: *ObjHeader, comptime tytag: ObjTy) *tytag.asTy() {
        tyvm.debug_assert(this.tag.tytag == tytag);
        const ptr: [*]u8 = @ptrCast(@alignCast(this));
        return @ptrCast(@alignCast(ptr));
    }

    pub fn gc_move(this: *ObjHeader, gc: *GC, worklist: *GC.WorkList) !*ObjHeader {
        return try switch (this.tag.tytag) {
            .bytes => this.narrow(.bytes).gc_move(gc, worklist),
            .arr => this.narrow(.arr).gc_move(gc, worklist),
            .obj => this.narrow(.obj).gc_move(gc, worklist),
            .@"union" => this.narrow(.@"union").gc_move(gc, worklist),
        };
    }

    pub fn gc_scan(this: *ObjHeader, gc: *GC, worklist: *GC.WorkList) !void {
        return try switch (this.tag.tytag) {
            .bytes => this.narrow(.bytes).gc_scan(gc, worklist),
            .arr => this.narrow(.arr).gc_scan(gc, worklist),
            .obj => this.narrow(.obj).gc_scan(gc, worklist),
            .@"union" => this.narrow(.@"union").gc_scan(gc, worklist),
        };
    }
};

pub const ObjTy = enum(u16) {
    bytes = 0,
    arr = 1,
    obj = 2,
    @"union" = 3,

    pub fn asTy(comptime tag: ObjTy) type {
        return switch (tag) {
            .bytes => Bytes,
            .arr => Array,
            .obj => Object,
            .@"union" => Union,
        };
    }
};

/// Object type tag used in ObjHeader
/// This is word sized because it will be casted to a pointer to store the
/// forwarding ref in GC.collect()
/// TODO: Fix this for wasm / 32 bit targets
pub const ObjTag = packed struct(u64) {
    /// All underlying objects will have alignment of 8 meaning we have these
    /// extra 3 bits to play with
    lowtag: enum(u1) {
        none = 0,
        forwarded = 1,
    },
    is_constant: bool = false,
    __unused: u1 = 0,
    /// When lowtag == .none, these bits are meaningless and zeroed
    /// When lowtag == .forwarded, these bits represent the pointer of the
    /// allocation that has been moved
    ptrbits: u45 = 0,
    tytag: ObjTy,

    pub fn format(this: ObjTag, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("ObjTag(lowtag={s}, is_constant={}, ptr={d}, tytag={s})", .{ @tagName(this.lowtag), this.is_constant, @as(u64, @intCast(this.ptrbits)) << 3, @tagName(this.tytag) });
    }

    pub fn forwarded_ptr(this: ObjTag) *ObjHeader {
        tyvm.debug_assert(this.lowtag == .forwarded);
        return @ptrFromInt(@as(usize, @intCast(this.ptrbits)) << 3);
    }

    /// size excluding the obj tag
    pub fn size(this: ObjTag) usize {
        return @sizeOf(ObjHeader) - switch (this.tytag) {
            .bytes => @sizeOf(Bytes),
            .arr => @sizeOf(Array),
            .obj => @sizeOf(Object),
            .@"union" => @sizeOf(Union),
        };
    }
};

pub const Value = union(ValueKind) {
    Any,
    Undefined,
    NumberKeyword,
    BoolKeyword,
    StringKeyword,
    ObjectKeyword,

    Number: f64,
    Bool: bool,
    String: String,
    Array: *Array,
    Object: *Object,
    Union: *Union,
    Bytes: BytesPtr,

    // pub fn format(this: *const Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    //     try writer.print("Value({})", .{this});
    // }

    pub fn isTruthy(this: Value) bool {
        return switch (this) {
            .Any, .Undefined, .NumberKeyword, .BoolKeyword, .StringKeyword, .ObjectKeyword => false,
            .Number => this.Number != 0,
            .Bool => this.Bool,
            .String => this.String.len != 0,
            .Array, .Object, .Union => true,
            .Bytes => this.Bytes.getSlice().len != 0,
        };
    }

    pub fn derive(vm: *VM, comptime T: type, val: T) !Value {
        if (T == []const u8) {
            const bytes_ptr = try Bytes.alloc_heap(vm.gc, val);
            const str = try vm.intern_string(String{
                .ptr = bytes_ptr,
                .len = @intCast(val.len),
            });
            const str_value = Value.string(str);
            return str_value;
        }
        const tyinfo = @typeInfo(T);
        return switch (tyinfo) {
            .Struct => Value.object(try Object.derive_struct(vm, T, val)),
            .Bool => Value.boolean(val),
            .Int => Value.number(@floatFromInt(val)),
            .Float => Value.number(@floatCast(val)),
            else => @compileError("Only structs, bools, ints and floats are supported. Got: " ++ @typeName(T)),
        };
    }

    fn as_anyobjheaderptr(self: *Value) ?*GC.AnyObjHeaderPtr {
        const ptr = self.as_anyobjheaderptr_impl();
        tyvm.debug_assert(ptr == null or @intFromPtr(ptr) % 8 == 0);
        return ptr;
    }

    fn as_anyobjheaderptr_impl(self: *Value) ?*GC.AnyObjHeaderPtr {
        return switch (self.*) {
            .String => {
                if (self.String.ptr.impl.meta == .heap) return @ptrCast(&self.String.ptr);
                return null;
            },
            .Array => |*v| {
                trace("VALUE=0x{x} ARRAY=0x{x}", .{ @intFromPtr(self), @intFromPtr(v) });
                return @ptrCast(v);
            },
            .Object => @ptrCast(&self.Object),
            .Union => @ptrCast(&self.Union),
            .Bytes => @ptrCast(&self.Bytes),
            .Any, .Undefined, .NumberKeyword, .BoolKeyword, .StringKeyword, .ObjectKeyword, .Number, .Bool => null,
        };
    }

    fn as_objheader(self: Value) ?*ObjHeader {
        return switch (self) {
            .String => @ptrCast(self.String),
            .Array => @ptrCast(self.Array),
            .Object => @ptrCast(self.Object),
            .Union => @ptrCast(self.Union),
            .Bytes => self.Bytes.asObjPtr(),
            .Any, .Undefined, .NumberKeyword, .BoolKeyword, .StringKeyword, .ObjectKeyword, .Number, .Bool => null,
        };
    }

    fn as_literal(self: Value) ?Literal {
        return switch (self) {
            .Number => |v| .{ .Number = v },
            .Bool => |b| .{ .Bool = b },
            .String => |s| .{ .String = s },
            else => null,
        };
    }

    fn negate(self: Value) Value {
        switch (self) {
            .Number => |v| return Value.number(-v),
            else => @panic("Invalid negation"),
        }
    }

    fn make_union(value: *Union) Value {
        return .{
            .Union = value,
        };
    }

    fn object(value: *Object) Value {
        return .{
            .Object = value,
        };
    }

    fn array(value: *Array) Value {
        return .{ .Array = value };
    }

    fn number(value: f64) Value {
        return .{ .Number = value };
    }

    pub fn boolean(value: bool) Value {
        return .{ .Bool = value };
    }

    fn string(value: String) Value {
        return .{ .String = value };
    }

    pub fn debug(self: Value, alloc: Allocator, comptime str: []const u8) !void {
        // alloc.alloc;
        var buf = std.ArrayListUnmanaged(u8){};
        defer buf.deinit(alloc);
        try self.encode_as_string(alloc, &buf, true);
        print("{s}: {s}\n", .{ str, buf.items.ptr[0..buf.items.len] });
    }

    fn serialize_to_json(self: Value, alloc: Allocator, buf: *std.ArrayListUnmanaged(u8)) !void {
        switch (self) {
            .Any => {
                try write_str_expand(alloc, buf, "{{\"type\": \"keyword\",\"value\":\"any\"}}", .{});
            },
            .Undefined => {
                try write_str_expand(alloc, buf, "{{\"type\": \"keyword\",\"value\":\"undefined\"}}", .{});
            },
            .NumberKeyword => {
                try write_str_expand(alloc, buf, "{{\"type\": \"keyword\",\"value\":\"number\"}}", .{});
            },
            .BoolKeyword => {
                try write_str_expand(alloc, buf, "{{\"type\": \"keyword\",\"value\":\"boolean\"}}", .{});
            },
            .StringKeyword => {
                try write_str_expand(alloc, buf, "{{\"type\": \"keyword\",\"value\":\"string\"}}", .{});
            },
            .ObjectKeyword => {
                try write_str_expand(alloc, buf, "{{\"type\": \"keyword\",\"value\":\"object\"}}", .{});
            },
            .String => |v| {
                var str_buf = std.ArrayList(u8).init(alloc);
                defer str_buf.deinit();
                try write_str_expand(alloc, buf, "\"{s}\"", .{try json_escaped(v.as_str(), &str_buf)});
            },
            .Bool => |v| {
                if (v) {
                    try write_str_expand(alloc, buf, "true", .{});
                } else {
                    try write_str_expand(alloc, buf, "false", .{});
                }
            },
            .Number => |v| {
                try write_str_expand(alloc, buf, "{d}", .{v});
            },
            .Object => |v| {
                try write_str_expand(alloc, buf, "{{\n", .{});
                if (v.fields) |fields| {
                    const last = v.len -| 1;
                    for (fields[0..v.len], 0..) |field_, i| {
                        const field: Object.Field = field_;
                        try write_str_expand(alloc, buf, "    ", .{});
                        try Value.string(field.name).serialize_to_json(alloc, buf);
                        try write_str_expand(alloc, buf, ": ", .{});
                        try field.value.serialize_to_json(alloc, buf);
                        if (i != last) try write_str_expand(alloc, buf, ",\n", .{});
                    }
                }
                try write_str_expand(alloc, buf, "\n}}\n", .{});
            },
            .Array => |v| {
                if (v.is_tuple_array()) {
                    try write_str_expand(alloc, buf, "[", .{});
                    if (v.ptr) |ptr| {
                        const last = v.len -| 1;
                        for (ptr[0..v.len], 0..) |val, i| {
                            try val.serialize_to_json(alloc, buf);
                            if (i != last) try write_str_expand(alloc, buf, ", ", .{});
                        }
                    }
                    try write_str_expand(alloc, buf, "]", .{});
                } else {
                    try write_str_expand(alloc, buf, "Array<", .{});
                    try write_str_expand(alloc, buf, ">", .{});
                }
            },
            .Union => |v| {
                const last = v.len -| 1;
                for (v.variants[0..v.len], 0..) |variant, i| {
                    try variant.serialize_to_json(alloc, buf);
                    if (i != last) {
                        try write_str_expand(alloc, buf, " | ", .{});
                    }
                }
            },
            .Bytes => {
                // try write_str_expand(alloc, buf, "Bytes", args: anytype)
                @panic("UNIMPLEMENTED");
            },
        }
    }

    fn encode_as_string(self: Value, alloc: Allocator, buf: *std.ArrayListUnmanaged(u8), is_format: bool) !void {
        switch (self) {
            .Any => {
                try write_str_expand(alloc, buf, "any", .{});
            },
            .Undefined => {
                try write_str_expand(alloc, buf, "undefined", .{});
            },
            .NumberKeyword => {
                try write_str_expand(alloc, buf, "number", .{});
            },
            .BoolKeyword => {
                try write_str_expand(alloc, buf, "bool", .{});
            },
            .StringKeyword => {
                try write_str_expand(alloc, buf, "string", .{});
            },
            .ObjectKeyword => {
                try write_str_expand(alloc, buf, "object", .{});
            },
            .String => |v| {
                const str = v.as_str();
                if (is_format) {
                    try write_str_expand(alloc, buf, "{s}", .{str});
                } else {
                    try write_str_expand(alloc, buf, "\"{s}\"", .{str});
                }
            },
            .Bool => |v| {
                if (v) {
                    try write_str_expand(alloc, buf, "true", .{});
                } else {
                    try write_str_expand(alloc, buf, "false", .{});
                }
            },
            .Number => |v| {
                try write_str_expand(alloc, buf, "{d}", .{v});
            },
            .Object => |v| {
                try write_str_expand(alloc, buf, "{{\n", .{});
                if (v.fields) |fields| {
                    const last = v.len -| 1;
                    for (fields[0..v.len], 0..) |field_, i| {
                        const field: Object.Field = field_;
                        try write_str_expand(alloc, buf, "    ", .{});
                        try Value.string(field.name).encode_as_string(alloc, buf, is_format);
                        try write_str_expand(alloc, buf, ": ", .{});
                        try field.value.encode_as_string(alloc, buf, is_format);
                        if (i != last) try write_str_expand(alloc, buf, ",\n", .{});
                    }
                }
                try write_str_expand(alloc, buf, "\n}}\n", .{});
            },
            .Array => |v| {
                if (v.is_tuple_array()) {
                    try write_str_expand(alloc, buf, "[", .{});
                } else {
                    try write_str_expand(alloc, buf, "Array<", .{});
                }
                if (v.ptr) |ptr| {
                    const last = v.len -| 1;
                    for (ptr[0..v.len], 0..) |val, i| {
                        try val.encode_as_string(alloc, buf, is_format);
                        if (i != last) try write_str_expand(alloc, buf, ", ", .{});
                    }
                }
                if (v.is_tuple_array()) {
                    try write_str_expand(alloc, buf, "]", .{});
                } else {
                    try write_str_expand(alloc, buf, ">", .{});
                }
            },
            .Union => |v| {
                const last = v.len -| 1;
                for (v.variants[0..v.len], 0..) |variant, i| {
                    try variant.encode_as_string(alloc, buf, is_format);
                    if (i != last) {
                        try write_str_expand(alloc, buf, " | ", .{});
                    }
                }
            },
            .Bytes => |v| {
                const slice = v.getSlice();
                const base64_len = std.base64.standard.Encoder.calcSize(slice.len);
                const base64_str = try std.heap.c_allocator.alloc(u8, base64_len);
                defer std.heap.c_allocator.free(base64_str);

                const base64 = std.base64.standard.Encoder.encode(base64_str, slice);

                // copy pasted from above
                if (is_format) {
                    try write_str_expand(alloc, buf, "{s}", .{base64});
                } else {
                    try write_str_expand(alloc, buf, "\"{s}\"", .{base64});
                }
            },
        }
    }
};

fn write_str_expand(alloc: Allocator, buf: *std.ArrayListUnmanaged(u8), comptime fmt: []const u8, args: anytype) !void {
    const len: usize = @intCast(std.fmt.count(fmt, args));
    try buf.ensureUnusedCapacity(alloc, len);
    const insertion_slice = buf.items.ptr[buf.items.len .. buf.items.len + len];
    _ = try std.fmt.bufPrint(insertion_slice, fmt, args);
    buf.items.len += len;
}

/// Pointer to a `Bytes` object
/// This *cannot* be cast into *Bytes or *ObjHeader.
/// But you can get the pointer to both of these using member functions.
const BytesPtr = packed struct(usize) {
    const Meta = enum(u3) { constant = 0b100, literal = 0b110, heap = 0b010 };
    const Impl64 = packed struct(u64) {
        meta: Meta,
        ptrbits: u45 = 0,
        ___unused: u16 = 0,

        pub fn getPtrUsize(this: Impl64) usize {
            return @as(usize, this.ptrbits) << 3;
        }

        pub fn init(meta: Meta, ptr: [*]const u8) Impl64 {
            return Impl64{
                .meta = meta,
                .ptrbits = @intCast(@intFromPtr(ptr) >> 3),
            };
        }
    };
    const Impl32 = packed struct(u32) {
        meta: Meta,
        ptrbits: u29 = 0,

        pub fn getPtrUsize(this: Impl32) usize {
            return @as(usize, this.ptrbits) << 3;
        }

        pub fn init(meta: Meta, ptr: [*]const u8) Impl32 {
            return Impl32{
                .meta = meta,
                .ptrbits = @intCast(@intFromPtr(ptr) >> 3),
            };
        }
    };
    const Impl = if (tyvm.is64Bit) Impl64 else Impl32;
    impl: Impl,

    pub fn deinit(this: BytesPtr, allocator: Allocator) void {
        switch (this.meta) {
            .constant, .literal => {},
            .heap => {
                const bytes: *Bytes = @ptrFromInt(this.impl.getPtrUsize());
                const dataptr: [*]const u8 = @ptrFromInt(this.impl.getPtrUsize());
                const bytes_len = bytes.len;
                const len = @sizeOf(Bytes) + bytes_len;
                const data = dataptr[0..len];
                allocator.free(data);
            },
        }
    }

    pub fn format(this: *const BytesPtr, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("BytesPtr(.meta={s}, .ptr={d})", .{ @tagName(this.impl.meta), this.impl.getPtrUsize() });
    }

    pub fn newHeap(ptr: [*]const u8) BytesPtr {
        tyvm.debug_assert((@intFromPtr(ptr) & 0b111) == 0);
        return BytesPtr{ .impl = Impl.init(.heap, ptr) };
    }

    pub fn newLiteral(ptr: [*]const u8) BytesPtr {
        trace("PTR: {d}", .{@intFromPtr(ptr)});
        // Assert that the pointer is 8-byte aligned
        tyvm.debug_assert((@intFromPtr(ptr) & 0b111) == 0);
        return BytesPtr{ .impl = Impl.init(.literal, ptr) };
    }

    pub fn newConstant(ptr: [*]const u8) BytesPtr {
        trace("PTR: {d}", .{@intFromPtr(ptr)});
        tyvm.debug_assert((@intFromPtr(ptr) & 0b111) == 0);
        return BytesPtr{ .impl = Impl.init(.constant, ptr) };
    }

    pub fn setForwarded(this: *BytesPtr, forwarded: bool) void {
        this.meta = forwarded;
    }

    pub fn asObjPtr(this: BytesPtr) ?*ObjHeader {
        if (this.impl.meta == .heap) return @ptrCast(this.__ptr(ObjHeader));
        return null;
    }

    pub inline fn anyPtr(this: BytesPtr) *anyopaque {
        return @ptrFromInt(this.impl.getPtrUsize());
    }

    pub inline fn __ptr(this: BytesPtr, comptime T: type) *T {
        return @ptrFromInt(this.impl.getPtrUsize());
    }

    pub inline fn ptrToSlice(this: BytesPtr) [*]const u8 {
        return this.ptrToSliceImpl(
            false,
        );
    }

    pub inline fn getSlice(this: BytesPtr) []const u8 {
        return this.ptrToSliceImpl(true);
    }

    pub inline fn ptrToSliceImpl(this: BytesPtr, comptime slice: bool) if (slice) []const u8 else [*]const u8 {
        switch (this.impl.meta) {
            .constant => {
                const constant: *ConstantBytes = this.__ptr(ConstantBytes);
                const bytes_ptr: [*]const u8 = @as([*]const u8, @ptrCast(constant)) + @sizeOf(ConstantBytes);
                tyvm.debug_assert(@intFromPtr(bytes_ptr) % 8 == 0);
                if (comptime slice) return bytes_ptr[0..constant.len];
                return bytes_ptr;
            },
            .heap => {
                const bytes: *Bytes = this.__ptr(Bytes);
                if (comptime slice) return bytes.as_slice();
                return bytes.as_slice_ptr();
            },
            .literal => {
                const theptr: [*]const u8 = @ptrCast(this.__ptr(u8));
                if (comptime slice) {
                    // - 8 because we store length and the string bytes are align(8)
                    // see `cons_str()` function
                    const lenptr: *const u32 = @ptrCast(@alignCast(theptr - 8));
                    return theptr[0..lenptr.*];
                }
                return theptr;
            },
        }
    }
};

const Bytes = struct {
    obj: ObjHeader = .{
        .tag = .{ .lowtag = .none, .ptrbits = 0, .tytag = .bytes },
    },
    len: u32,
    _pad: u32 = 0,
    // bytes are stored after the above fields

    const Encoding = enum(u16) {
        // latin1,
        utf8,
    };

    fn alloc_heap(gc: *GC, slice: []const u8) !BytesPtr {
        const heap = try gc.as_allocator().alloc(u8, @sizeOf(Bytes) + slice.len);
        const bytes: *Bytes = @ptrCast(@alignCast(heap.ptr));
        bytes.* = .{
            .len = @intCast(slice.len),
        };
        const heap_slice: [*]u8 = @ptrCast(heap.ptr + @sizeOf(Bytes));
        @memcpy(heap_slice[0..slice.len], slice);
        tyvm.debug_assert((@intFromPtr(bytes) & 0b111) == 0);
        return BytesPtr{
            .impl = BytesPtr.Impl.init(.heap, @ptrCast(bytes)),
        };
    }

    inline fn as_slice_ptr(this: *Bytes) [*]const u8 {
        const ptr_start = @as([*]const u8, @ptrCast(this)) + @sizeOf(Bytes);
        return ptr_start;
    }

    fn as_slice(this: *Bytes) []const u8 {
        const ptr_start = this.as_slice_ptr();
        return ptr_start[0..this.len];
    }

    fn gc_move(this: *Bytes, gc: *GC, _: *GC.WorkList) !*ObjHeader {
        const addr = try gc.create_impl(Bytes, false);
        addr.* = this.*;
        addr.obj = addr.obj.clearForwarded();
        const bytes_addr = try gc.allocImpl(this.len, 8, false);
        const to_space_slice = bytes_addr[0..this.len];
        const from_space_slice = this.as_slice();
        @memcpy(to_space_slice, from_space_slice);
        return @ptrCast(addr);
    }

    fn gc_scan(this: *Bytes, gc: *GC, _: *GC.WorkList) !void {
        _ = this; // autofix
        _ = gc; // autofix
        @panic("This should never be called");
    }
};

/// Internally this is a lot like a slice. It is a pointer to Bytes and a length.
///
/// INVARIANTS:
/// - Strings are always interned, so two strings are equal if their pointer's are equal
pub const String = struct {
    ptr: BytesPtr,
    len: u32 align(8),

    pub fn format(this: *const String, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("String(\"{s}\", .ptr={})", .{ this.as_str(), this.ptr });
    }

    /// INVARIANTS:
    /// - `s.ptr` is 8-byte aligned
    /// - `s` will not be GC'ed
    ///
    /// You MUST ensure you use `const_str(...)` to make the ptr aligned
    pub fn from_literal(s: []align(8) const u8) String {
        return .{
            .ptr = BytesPtr.newLiteral(s.ptr),
            .len = @intCast(s.len),
        };
    }

    pub fn from_slice(s: []align(8) const u8) String {
        return .{
            .ptr = BytesPtr.newHeap(s.ptr),
            .len = @intCast(s.len),
        };
    }

    pub fn from_constant(constant: ConstantString) String {
        return .{
            .ptr = constant.ptr,
            .len = constant.len,
        };
    }

    pub fn as_str(self: String) []const u8 {
        return self.ptr.getSlice()[0..self.len];
    }

    pub fn cmp(a: *const String, b: *const String) Order {
        if (@as(usize, @intFromPtr(a.ptr.anyPtr())) < @as(usize, @intFromPtr(b.ptr.anyPtr()))) return .Less;
        if (@as(usize, @intFromPtr(a.ptr.anyPtr())) > @as(usize, @intFromPtr(b.ptr.anyPtr()))) return .Greater;
        if (a.len < b.len) return .Less;
        if (a.len > b.len) return .Greater;
        return .Equal;
    }

    pub fn widen(this: *String) *ObjHeader {
        return @ptrCast(this);
    }
};

/// TODO: Make this a slice over data like String
const Array = struct {
    obj: ObjHeader = .{
        .tag = .{
            .lowtag = .none,
            .ptrbits = 0,
            .tytag = .arr,
        },
    },
    ptr: ?[*]Value,
    len: u32,
    flags: Flags,

    const Flags = packed struct {
        is_tuple: bool = false,
        pad: u31 = 0,
    };

    pub fn new_with_capacity(gc: *GC, length: u32) !*Array {
        const alloc = gc.as_allocator();
        var arr = try alloc.create(Array);
        arr.* = .{
            .ptr = null,
            .len = length,
            .flags = Flags{
                .is_tuple = false,
            },
        };
        gc.push_vroot(@ptrCast(&arr));
        defer gc.pop_vroot();
        const slc = try gc.as_allocator().alloc(Value, length);
        arr.ptr = slc.ptr;
        return arr;
    }

    pub fn dupe(self: *Array, gc: *GC) !*Array {
        const alloc = gc.as_allocator();
        var arr = try alloc.create(Array);
        arr.* = .{
            .ptr = null,
            .len = self.len,
            .flags = Flags{
                .is_tuple = self.flags.is_tuple,
            },
        };
        gc.push_vroot(@ptrCast(&arr));
        defer gc.pop_vroot();
        const slc = try alloc.dupe(Value, self.items());
        arr.ptr = slc.ptr;
        return arr;
    }

    pub fn new(gc: *GC, is_tuple: bool, values: []const Value, spread: []const Value) !*Array {
        const alloc = gc.as_allocator();
        var arr = try alloc.create(Array);
        arr.* = .{
            .ptr = null,
            .len = 0,
            .flags = .{},
        };
        gc.push_vroot(@ptrCast(&arr));
        defer gc.pop_vroot();
        var ptr = try gc.as_allocator().alloc(Value, values.len + spread.len);

        if (values.len > 0) @memcpy(ptr[0..values.len], values);
        if (spread.len > 0) @memcpy(ptr[values.len .. values.len + spread.len], spread);

        arr.* = .{
            .ptr = @ptrCast(ptr),
            .len = @intCast(values.len + spread.len),
            .flags = Flags{
                .is_tuple = is_tuple,
            },
        };
        return arr;
    }

    pub fn is_tuple_array(self: *const Array) bool {
        return self.flags.is_tuple;
    }

    pub fn item_at_index_mut(self: *const Array, idx: u32) ?*Value {
        if (idx >= self.len) return null;
        return &self.ptr.?[idx];
    }

    pub fn item_at_index(self: *const Array, idx: u32) Value {
        if (idx >= self.len) return .Undefined;
        return self.ptr.?[idx];
    }

    pub fn items(self: *const Array) []const Value {
        if (self.ptr) |ptr| {
            return ptr[0..self.len];
        }
        return &[_]Value{};
    }

    pub fn items_mutable(self: *Array) []Value {
        if (self.ptr) |ptr| {
            return ptr[0..self.len];
        }
        return &[_]Value{};
    }

    /// TODO: Tagged pointer so we don't allocate?
    pub fn empty_tuple(gc: *GC) !*Array {
        const tuple = try gc.as_allocator().create(Array);
        tuple.* = .{
            .ptr = null,
            .len = 0,
            .flags = Flags{
                .is_tuple = true,
            },
        };
        return tuple;
    }

    fn gc_move(this: *Array, gc: *GC, worklist: *GC.WorkList) !*ObjHeader {
        tyvm.debug_assert(this.obj.tag.tytag == .arr);
        const addr = try gc.create_impl(Array, false);
        addr.* = this.*;
        addr.obj = addr.obj.clearForwarded();
        if (this.ptr) |ptr| {
            const values = ptr[0..this.len];
            const new_values = try gc.as_allocator_no_gc().alloc(Value, values.len);
            @memcpy(new_values, values);
            addr.ptr = new_values.ptr;
        }
        tyvm.debug_assert(addr.obj.tag.tytag == .arr);
        try worklist.append(@ptrCast(addr));
        return @ptrCast(addr);
    }

    fn gc_scan(this: *Array, gc: *GC, worklist: *GC.WorkList) !void {
        for (this.items_mutable()) |*item| {
            if (item.as_anyobjheaderptr()) |ptrptr| {
                try gc.process(worklist, ptrptr);
            }
        }
    }
};

/// INVARIANTS:
/// - `fields` are ordered lexographically by key
const Object = struct {
    obj: ObjHeader = .{
        .tag = .{
            .lowtag = .none,
            .ptrbits = 0,
            .tytag = .obj,
        },
    },
    fields: ?[*]Field,
    len: u32,

    pub fn format(this: *const Object, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("ObjTag(tag={}, fields={any})", .{ this.obj.tag, if (this.fields) |f| f[0..this.len] else &[_]Field{} });
    }

    const Field = struct {
        name: String,
        value: Value,

        pub fn cmp_key(a: *const Field, b: *const Field) Order {
            return String.cmp(&a.name, &b.name);
        }

        pub fn less_than_key(_: void, a: Field, b: Field) bool {
            return Field.cmp_key(&a, &b) == .Less;
        }
    };

    /// INVARIANT: The fields of the object are sorted
    pub fn panic_on_duplicate_keys(self: *const Object) void {
        if (self.len <= 1) return;
        const fields: []const Field = self.fields.?[0..self.len];

        // Since the object is sorted, duplicate keys will be adjacent to each other
        var i: usize = 0;
        var j: usize = 1;
        while (j < self.len) {
            if (fields[i].name.ptr.impl.ptrbits == fields[j].name.ptr.impl.ptrbits) {
                print("{d}: {s} == {d}: {s}\n", .{ i, fields[i].name.as_str(), j, fields[j].name.as_str() });
                @panic("Duplicate keys!");
            }
            i += 1;
            j += 1;
        }
    }

    fn gc_move(this: *Object, gc: *GC, worklist: *GC.WorkList) !*ObjHeader {
        tyvm.debug_assert(this.obj.tag.tytag == .obj);
        const addr = try gc.create_impl(Object, false);
        addr.* = this.*;
        addr.obj = addr.obj.clearForwarded();
        if (this.fields) |fields| {
            const values = fields[0..this.len];
            const new_values = try gc.as_allocator_no_gc().alloc(Field, values.len);
            @memcpy(new_values, values);
            addr.fields = new_values.ptr;
        }
        tyvm.debug_assert(addr.obj.tag.tytag == .obj);
        try worklist.append(@ptrCast(addr));
        return @ptrCast(addr);
    }

    fn gc_scan(this: *Object, gc: *GC, worklist: *GC.WorkList) !void {
        for (this.fields_slice_mutable()) |*fld| {
            if (fld.name.ptr.impl.meta == .heap) {
                try gc.process(worklist, @ptrCast(&fld.name.ptr));
            }

            if (fld.value.as_anyobjheaderptr()) |ptrptr| {
                try gc.process(worklist, ptrptr);
            }
        }
    }

    /// Compile-time generate code which converts a struct `T` into an object.
    ///
    /// This assumes that this function has ownership of `val.
    pub fn derive_struct(vm: *VM, comptime T: type, val: T) !*Object {
        const tyinfo = @typeInfo(T);
        if (tyinfo != .Struct) @compileError("Only structs are supported");
        const ty_fields = tyinfo.Struct.fields;
        var fields: []Value = try vm.gc.as_allocator().alloc(Value, ty_fields.len * 2);
        var j: usize = 0;
        inline for (ty_fields) |field| {
            const str = try vm.intern_string(String.from_literal(const_str(field.name[0..])));
            fields[j] = Value.string(str);
            fields[j + 1] = try Value.derive(vm, field.type, @field(val, field.name));
            j += 2;
        }
        return try Object.new(vm.gc, fields[0..]);
    }

    pub fn new(gc: *GC, fields: []const Value) !*Object {
        tyvm.debug_assert(fields.len % 2 == 0);

        var object = try gc.as_allocator().create(Object);
        object.* = .{
            .fields = null,
            .len = 0,
        };
        gc.push_vroot(@ptrCast(&object));
        defer gc.pop_vroot();

        const object_fields = try gc.as_allocator().alloc(Field, fields.len / 2);

        var i: usize = 0;
        for (object_fields) |*f| {
            f.name = fields[i].String;
            f.value = fields[i + 1];
            i += 2;
        }

        std.sort.block(Object.Field, object_fields, {}, Field.less_than_key);

        object.* = .{ .fields = object_fields.ptr, .len = @intCast(fields.len / 2) };

        object.panic_on_duplicate_keys();

        return object;
    }

    pub inline fn fields_slice(self: *const Object) []const Field {
        return if (self.fields) |fs| fs[0..self.len] else &[_]Field{};
    }

    pub inline fn fields_slice_mutable(self: *const Object) []Field {
        return if (self.fields) |fs| fs[0..self.len] else &[_]Field{};
    }

    pub fn get_field(self: *const Object, name: String) ?*const Field {
        const dummy_field = .{
            .name = name,
            .value = Value.number(0),
        };
        if (self.fields) |fields| {
            const idx = binary_search(Object.Field, fields[0..self.len], &dummy_field, Object.Field.cmp_key) orelse return null;
            return &fields[idx];
        }
        return null;
    }

    pub fn update_field(self: *Object, name: String, new_value: Value) void {
        const dummy_field: Object.Field = .{
            .name = name,
            .value = Value.number(0),
        };
        if (self.fields) |fields| {
            const idx = binary_search(Object.Field, fields[0..self.len], &dummy_field, Object.Field.cmp_key) orelse return;
            fields[idx].value = new_value;
        }
        return;
    }
};

/// INVARIANTS:
/// - `len` must always be >= 2, otherwise its not a union
/// - `variants` is always **flat**, meaning no Value in `variants` will be a Union.
///    This ensures that normalizing unions is cheap.
const Union = struct {
    obj: ObjHeader = .{
        .tag = .{
            .lowtag = .none,
            .ptrbits = 0,
            .tytag = .@"union",
        },
    },
    variants: [*]Value,
    len: u32,
    discriminant_key: ?String,

    pub fn variants_slice(self: *const Union) []const Value {
        return self.variants[0..self.len];
    }

    pub fn variants_slice_mutable(self: *const Union) []Value {
        return self.variants[0..self.len];
    }

    fn gc_move(this: *Union, gc: *GC, worklist: *GC.WorkList) !*ObjHeader {
        const addr = try gc.create_impl(Union, false);
        addr.* = this.*;
        addr.obj = addr.obj.clearForwarded();
        const values = this.variants[0..this.len];
        const new_values = try gc.as_allocator_no_gc().alloc(Value, values.len);
        @memcpy(new_values, values);
        addr.variants = new_values.ptr;
        try worklist.append(@ptrCast(addr));
        return @ptrCast(addr);
    }

    fn gc_scan(this: *Union, gc: *GC, worklist: *GC.WorkList) !void {
        for (this.variants_slice_mutable()) |*item| {
            if (item.as_anyobjheaderptr()) |ptrptr| {
                try gc.process(worklist, ptrptr);
            }
        }
    }
};

// Simple bump allocated semi-space collector
pub const GC = struct {
    from_space: [*]u8,

    to_space: [*]u8,
    bump: usize = 0,

    bytes_allocated: usize = 0,
    moved_strings: u32 = 0,

    vm: *VM,

    __virtual_roots: [VROOT_MAX]**ObjHeader = [_]**ObjHeader{undefined} ** VROOT_MAX,
    __virtual_roots_len: u8 = 0,

    const gctrace = tyvm.logger(.GC, false);

    /// This can either be:
    /// - *ObjHeader
    /// - BytesPtr
    ///
    /// All of the above have the following layout
    const AnyObjHeaderPtr = packed struct(usize) {
        const Impl64 = packed struct(u64) {
            /// We align every allocation to 8 bytes, so we know for certain the
            /// lower 3 bits will be zeroed
            _: u3 = 0,
            // The actual pointer value
            __ptr: u45 = 0,
            /// Pointers only use 48 bits of address space
            __: u16 = 0,
        };
        const Impl32 = packed struct(u32) {
            _: u3 = 0,
            __ptr: u29 = 0,
        };
        const Impl = if (tyvm.is64Bit) Impl64 else Impl32;
        impl: Impl,

        pub inline fn getPtr(this: AnyObjHeaderPtr) usize {
            return @as(usize, this.impl.__ptr) << 3;
        }

        pub inline fn setPtr(this: *AnyObjHeaderPtr, ptr: *ObjHeader) void {
            this.impl.__ptr = @intCast(@intFromPtr(ptr) >> 3);
        }
    };

    const WorkList = std.ArrayList(*ObjHeader);

    const VROOT_MAX = 8;

    var alloc_vtable: Allocator.VTable = .{
        .alloc = alloc,
        .resize = resize,
        .free = free,
    };

    var alloc_no_gc_table: Allocator.VTable = .{
        .alloc = allocNoGC,
        .resize = resize,
        .free = free,
    };

    const SPACE_SIZE = 150 * 1024 * 1024;
    // This seems to be a good number to test collection on fib.ts
    // const SPACE_SIZE = 1448;
    // This seems to be a good number to test collection on everything else
    // const SPACE_SIZE = 2048 + 1024;
    // union.ts
    // const SPACE_SIZE = 4096;
    comptime {
        std.debug.assert(SPACE_SIZE % 8 == 0);
    }
    // const SPACE_SIZE = 2049;

    pub fn init(vm: *VM) !GC {
        const both: []u8 = brk: {
            if (tyvm.isPosix) break :brk try std.posix.mmap(null, SPACE_SIZE * 2, std.posix.PROT.READ | std.posix.PROT.WRITE, std.posix.MAP{ .TYPE = .PRIVATE, .ANONYMOUS = true }, -1, 0);
            break :brk try std.heap.c_allocator.alignedAlloc(u8, 8, SPACE_SIZE * 2);
        };
        return .{
            .from_space = both.ptr,
            .to_space = @ptrCast(&both.ptr[SPACE_SIZE]),
            .vm = vm,
        };
    }

    pub inline fn push_vroot(this: *GC, obj: **ObjHeader) void {
        if (this.__virtual_roots_len == VROOT_MAX) @panic("vroot stack overflow");
        tyvm.debug_assert(@intFromPtr(obj.*) % 8 == 0);
        this.__virtual_roots[this.__virtual_roots_len] = obj;
        this.__virtual_roots_len += 1;
    }

    pub inline fn pop_vroot(this: *GC) void {
        tyvm.debug_assert(this.__virtual_roots_len > 0);
        this.__virtual_roots_len -= 1;
    }

    pub fn as_allocator(this: *GC) Allocator {
        return .{
            .ptr = @ptrCast(this),
            .vtable = &alloc_vtable,
        };
    }

    pub fn as_allocator_no_gc(this: *GC) Allocator {
        return .{
            .ptr = @ptrCast(this),
            .vtable = &alloc_no_gc_table,
        };
    }

    fn flip(this: *GC) void {
        const to_space = this.to_space;
        this.to_space = this.from_space;
        this.from_space = to_space;
        this.bump = 0;
    }

    pub fn collect(this: *GC) !void {
        gctrace("Collecting", .{});
        this.moved_strings = 0;
        this.flip();

        const stack = this.vm.stack[0 .. (@intFromPtr(this.vm.stack_top) - @intFromPtr(&this.vm.stack)) / @sizeOf(Value)];
        const globals: *HashMap(ConstantTableIdx, Value) = &this.vm.globals;

        const STACK_VALUE_MAX = 1000;
        const STACK_SIZE = @sizeOf(Value) * STACK_VALUE_MAX;
        var stackfb = std.heap.stackFallback(STACK_SIZE, std.heap.c_allocator);
        var worklist = std.ArrayList(*ObjHeader).initCapacity(stackfb.get(), STACK_VALUE_MAX) catch |e| tyvm.oom(e);
        defer worklist.deinit();

        try this.process_std_containers(&worklist);

        // Start by processing roots
        for (this.__virtual_roots[0..this.__virtual_roots_len]) |ptrptr| {
            gctrace("Process roots: {d}", .{@intFromPtr(ptrptr)});
            try this.process(&worklist, @ptrCast(ptrptr));
        }
        gctrace("STACK LEN: {d}", .{stack.len});
        for (stack) |*val| {
            gctrace("Process stack: {d}", .{@intFromPtr(val)});
            if (val.as_anyobjheaderptr()) |ptrptr| {
                try this.process(&worklist, ptrptr);
            }
        }
        var iter = globals.valueIterator();
        while (iter.next()) |val| {
            gctrace("Process global: {d}", .{@intFromPtr(val)});
            if (val.as_anyobjheaderptr()) |ptrptr| {
                try this.process(&worklist, ptrptr);
            }
        }
        if (this.vm.game_state) |*val| {
            if (val.as_anyobjheaderptr()) |ptrptr| {
                try this.process(&worklist, ptrptr);
            }
        }

        while (worklist.popOrNull()) |val| {
            gctrace("Process worklist item: {d}", .{@intFromPtr(val)});
            try this.scan(&worklist, val);
        }

        _ = memset(@ptrCast(this.from_space), 0, SPACE_SIZE);
    }

    /// The VM uses certain std container types like std.AutoHashMap and std.StringArrayHashMap
    fn process_std_containers(this: *GC, worklist: *WorkList) !void {

        // TODO: std.ArrayHashMap.clone() does a for-loop over entries instead of memcpy
        // could pose a performance problem
        // we could probably do the memcpy ourself
        //
        // TODO: should we even copy `constant_strings` and `native_functions`?
        // these live for the entire lifetime of the program, so seems wasteful
        this.vm.constant_strings = try this.vm.constant_strings.cloneWithAllocator(this.as_allocator_no_gc());
        this.vm.native_functions = try this.vm.native_functions.cloneWithAllocator(this.as_allocator_no_gc());
        this.vm.functions = try this.vm.functions.cloneWithAllocator(this.as_allocator_no_gc());
        this.vm.globals = try this.vm.globals.cloneWithAllocator(this.as_allocator_no_gc());

        this.vm.interned_strings = try this.vm.interned_strings.cloneWithAllocator(this.as_allocator_no_gc());

        var iter = this.vm.interned_strings.iterator();
        while (iter.next()) |entry| {
            if (entry.key_ptr.ptr.impl.meta == .heap) {
                try this.process(worklist, @ptrCast(&entry.key_ptr.ptr));
            }
            if (entry.value_ptr.ptr.impl.meta == .heap) {
                try this.process(worklist, @ptrCast(&entry.value_ptr.ptr));
            }
        }
    }

    fn process(this: *GC, worklist: *WorkList, ptrptr: *AnyObjHeaderPtr) !void {
        tyvm.debug_assert(ptrptr.getPtr() != 0);
        const from_space_ptr: *ObjHeader = @ptrFromInt(ptrptr.getPtr());
        // Already forwarded
        if (from_space_ptr.tag.lowtag == .forwarded) {
            tyvm.debug_assert((@intFromPtr(from_space_ptr.tag.forwarded_ptr()) & 0b111) == 0);
            ptrptr.setPtr(from_space_ptr.tag.forwarded_ptr());
        }
        // Need to forward it
        else {
            const to_space_ptr = try this.copy(worklist, from_space_ptr);
            from_space_ptr.tag.lowtag = .forwarded;
            tyvm.debug_assert((@intFromPtr(to_space_ptr) & 0b111) == 0);
            from_space_ptr.tag.ptrbits = @intCast(@intFromPtr(to_space_ptr) >> 3);
            ptrptr.setPtr(to_space_ptr);
        }
    }

    /// Copies an object in from_space into to_space, and returns a pointer to the object in to_space
    fn copy(this: *GC, worklist: *WorkList, from_space_ptr: *ObjHeader) !*ObjHeader {
        return from_space_ptr.gc_move(this, worklist);
    }

    fn scan(this: *GC, worklist: *WorkList, val: *ObjHeader) !void {
        gctrace("Scan: {d}", .{@intFromPtr(val)});
        try val.gc_scan(this, worklist);
        // switch (val.tag.tytag) {
        //     .bytes => unreachable,
        //     .arr => {
        //         const narrow_addr: *Array = val.narrow(.arr);
        //         for (narrow_addr.items_mutable()) |*item| {
        //             if (item.as_anyobjheaderptr()) |ptrptr| {
        //                 try this.process(worklist, ptrptr);
        //             }
        //         }
        //     },
        //     .obj => {
        //         const narrow_addr: *Object = val.narrow(.obj);
        //         for (narrow_addr.fields_slice_mutable()) |*fld| {
        //             if (fld.name.ptr.meta == .heap) {
        //                 try this.process(worklist, @ptrCast(&fld.name.ptr));
        //             }

        //             if (fld.value.as_anyobjheaderptr()) |ptrptr| {
        //                 try this.process(worklist, ptrptr);
        //             }
        //         }
        //     },
        //     .@"union" => {
        //         const narrow_addr: *Union = val.narrow(.@"union");
        //         for (narrow_addr.variants_slice_mutable()) |*item| {
        //             if (item.as_anyobjheaderptr()) |ptrptr| {
        //                 try this.process(worklist, ptrptr);
        //             }
        //         }
        //     },
        // }
    }

    fn alloc(ctx: *anyopaque, n: usize, log2_ptr_align: u8, _: usize) ?[*]u8 {
        const this: *GC = @ptrCast(@alignCast(ctx));
        return this.__allocImpl(n, log2_ptr_align, 0, true);
    }

    fn allocNoGC(ctx: *anyopaque, n: usize, log2_ptr_align: u8, _: usize) ?[*]u8 {
        const this: *GC = @ptrCast(@alignCast(ctx));
        return this.__allocImpl(n, log2_ptr_align, 0, false);
    }

    fn __allocImpl(this: *GC, n: usize, log2_ptr_align: u29, _: usize, comptime allow_collect: bool) ?[*]u8 {
        const ptr_align = @as(usize, 1) << @as(Allocator.Log2Align, @intCast(log2_ptr_align));
        const addr = this.bump;
        const adjusted_addr = mem.alignForward(usize, addr, ptr_align);
        const adjusted_index = this.bump + (adjusted_addr - addr);
        const new_end_index = adjusted_index + n;

        gctrace("Alloc: len={d} newbump={}\n", .{ new_end_index - this.bump, new_end_index });

        if (new_end_index > SPACE_SIZE) {
            if (comptime allow_collect) {
                this.collect() catch return null;
                return __allocImpl(this, n, log2_ptr_align, 0, false);
            }
            return null;
        }

        this.bump = new_end_index;

        return this.to_space + adjusted_index;
    }

    fn allocImpl(this: *GC, size: usize, @"align": usize, comptime allow_collect: bool) ![*]u8 {
        const log2_ptr_align = std.math.log2(@"align");
        const ret = this.__allocImpl(size, @intCast(log2_ptr_align), 0, allow_collect) orelse std.mem.Allocator.Error.OutOfMemory;
        return ret;
    }

    fn create_impl(this: *GC, comptime T: type, comptime allow_collect: bool) !*T {
        const addr: *T = @ptrCast(@alignCast(try this.allocImpl(@sizeOf(T), @alignOf(T), allow_collect)));
        return addr;
    }

    fn resize(ctx: *anyopaque, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
        _ = ctx; // autofix
        _ = buf; // autofix
        _ = buf_align; // autofix
        _ = new_len; // autofix
        _ = ret_addr; // autofix

        return false;
    }

    fn free(ctx: *anyopaque, buf: []u8, log2_buf_align: u8, ret_addr: usize) void {
        _ = ctx; // autofix
        _ = buf; // autofix
        _ = log2_buf_align; // autofix
        _ = ret_addr; // autofix

    }
};

pub const Order = enum {
    Less,
    Equal,
    Greater,
};

pub fn binary_search(comptime T: type, arr: []const T, search_elem: *const T, comptime cmp: fn (*const T, *const T) Order) ?usize {
    var size: usize = arr.len;
    var left: usize = 0;
    var right: usize = size;

    while (left < right) {
        const mid = left + size / 2;
        const item: *const T = &arr[mid];
        switch (cmp(search_elem, item)) {
            .Equal => return mid,
            .Less => {
                right = mid;
            },
            .Greater => {
                left = mid + 1;
            },
        }
        size = right - left;
    }

    return null;
}

pub fn json_escaped(str: []const u8, buf: *std.ArrayList(u8)) ![]const u8 {
    for (str) |ch| {
        switch (ch) {
            '"' => {
                try buf.appendSlice("\\\"");
            },
            '\\' => {
                try buf.appendSlice("\\\\");
            },
            // '\b' => {
            //     try buf.appendSlice("\\b");
            // },
            // '\f' => {
            //     try buf.appendSlice("\\f");
            // },
            '\n' => {
                try buf.appendSlice("\\n");
            },
            '\r' => {
                try buf.appendSlice("\\r");
            },
            '\t' => {
                try buf.appendSlice("\\t");
            },
            else => {
                try buf.append(ch);
            },
        }
    }
    return buf.items[0..buf.items.len];
}

test "count spread" {
    const args = &[_]u8{ 1, 1, 1, 1, 12, 1, 42, 1, 69 };
    const spread_bitfield: u256 = 0b101010000;

    const count = args.len;
    const spread_count = @popCount(spread_bitfield);
    const total_size = size: {
        if (spread_count == 0) break :size count;

        var total: u32 = @as(u32, @intCast(count)) - @as(u32, @intCast(spread_count));

        var bitset = std.bit_set.IntegerBitSet(256).initEmpty();
        bitset.mask = spread_bitfield;

        var iter = bitset.iterator(.{});
        while (iter.next()) |idx| {
            const items = args[idx];
            total += items;
        }

        break :size total;
    };
    std.debug.print("SIZE: {d}\n", .{total_size});
    const manual_size = manual: {
        var i: usize = 0;
        for (args) |a| {
            i += a;
        }
        break :manual i;
    };
    try std.testing.expectEqual(manual_size, total_size);
}
