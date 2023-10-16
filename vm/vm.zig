const std = @import("std");

const print = std.debug.print;

const HashMap = std.AutoHashMap;
const StringHashMap = std.StringArrayHashMap;
const Allocator = std.mem.Allocator;

const VM = @This();

const TRACING: bool = true;

stack: [1024]Value = [_]Value{.{.Number = 0.0}} ** 1024,
stack_top: [*]Value = undefined,

call_frames: [1024]CallFrame = [_]CallFrame{CallFrame.uninit()} ** 1024,
call_frames_count: usize = 0,

globals: HashMap(ConstantTableIdx, Value),
interned_strings: std.StringArrayHashMap(String),

/// Constant data loaded from the bytecode
functions: HashMap(ConstantTableIdx, Function),
constant_table: []const ConstantTableEntry align(8),
constant_pool: []const u8 align(8),

pub fn init(alloc: Allocator, bytecode: []const u8) !VM {
    var vm: VM = .{
        .globals = HashMap(ConstantTableIdx, Value).init(alloc),
        .interned_strings = std.StringArrayHashMap(String).init(alloc),
        .functions = HashMap(ConstantTableIdx, Function).init(alloc),
        .constant_table = undefined,
        .constant_pool = undefined,
    };

    try vm.load_bytecode(bytecode);

    var iter = vm.functions.valueIterator();
    while (iter.next()) |function| {
        function.disassemble(&vm);
    }

    return vm;
}

fn load_bytecode(self: *VM, bytecode: []const u8) !void {
    const header: *const Bytecode.Header = @ptrCast(@alignCast(bytecode.ptr));
    if (header.magic != 69420) {
        @panic("Bad header value.");
    }
    const constants_header: *const Bytecode.ConstantsHeader = @ptrCast(@alignCast(bytecode.ptr + header.constants_offset));
    const functions_header: *const Bytecode.FunctionsHeader = @ptrCast(@alignCast(bytecode.ptr + header.functions_offset));
    std.debug.assert(functions_header.function_code_padding == 0);

    const constant_table_raw_ptr: [*]const ConstantTableEntry = @ptrCast(@alignCast(bytecode.ptr + header.constants_offset + @sizeOf(Bytecode.ConstantsHeader)));
    const constant_table: []const ConstantTableEntry = constant_table_raw_ptr[0..constants_header.table_len / @sizeOf(ConstantTableEntry)];
    const constant_pool_raw_ptr: [*]const u8 = (bytecode.ptr + header.constants_offset + @sizeOf(Bytecode.ConstantsHeader) + constants_header.table_len);
    const constant_pool: []const u8 = constant_pool_raw_ptr[0..(constants_header.pool_size - constants_header.pool_padding)];

    const function_table: []const Bytecode.FunctionTableEntry = @as([*]const Bytecode.FunctionTableEntry, @ptrCast(@alignCast(bytecode.ptr + functions_header.table_offset)))[0..functions_header.table_len];
    for (function_table) |*entry_| {
        const entry: *const Bytecode.FunctionTableEntry = entry_;
        const base: [*]const u8 = bytecode.ptr + functions_header.table_offset + functions_header.table_len * @sizeOf(Bytecode.FunctionTableEntry);
        const function: Function = .{
            .name = entry.name_constant_idx,
            .code = base[entry.offset..entry.offset + entry.size]
        };
        try self.functions.put(entry.name_constant_idx, function);
    }
    
    self.constant_pool = constant_pool;
    self.constant_table = constant_table;

    for (self.constant_table, 0..) |entry, i| {
        _ = i;
        if (entry.kind == .String) {
            const str = self.read_constant_string(entry.idx);
            try self.interned_strings.put(str.as_str(), str);
        }
    }
}

pub fn run(self: *VM) !void {
    self.stack_top = self.stack[0..];

    const global_fn: *const Function = self.functions.getPtr(ConstantTableIdx.new(0)).?;

    self.push_call_frame(.{
        .func = global_fn,
        .ip = global_fn.code.ptr,
        .slots = self.stack_top,
    });

    var frame: *CallFrame = &self.call_frames[self.call_frames_count - 1];
    
    while (true) {
        const op: Op = @enumFromInt(frame.read_byte());
        if (comptime TRACING) {
            const fn_name = self.read_constant(self.cur_call_frame().func.name).String;
            print("STACK:\n", .{});
            const stack_top_int: usize = @intFromPtr(self.stack_top);
            const stack_bot_int: usize = @intFromPtr(self.stack[0..]);
            const stack_len = stack_top_int / @sizeOf(Value) - stack_bot_int / @sizeOf(Value);
            for (0..stack_len) |i| {
                print("    {?}\n", .{self.stack[i]});
            }
            print("({s}) OP: {s}\n", .{fn_name.as_str(), @tagName(op)});
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
                self.push(Value.array(Array.empty_tuple()));
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
            .PanicExtends => {
                const b = self.pop();
                const a = self.pop();
                if (!self.extends(a, b)) {
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
                self.push(global);
            },
            .SetGlobal => {
                const constant_idx = frame.read_constant_idx();
                const value = self.pop();
                try self.globals.put(constant_idx, value);
            },
            .Jump => {
                const offset = frame.read_u16();
                frame.ip = @ptrCast(&frame.func.code[offset]);
            },
            .PopCallFrame => {
                const return_val = self.peek(0);
                const return_slot = frame.slots;
                return_slot[0] = return_val;
                self.stack_top = return_slot + 1;
                self.call_frames_count -= 1;
                frame = self.cur_call_frame();
            },
            .Print => {
                const val = self.peek(0);
                if (@as(ValueKind, val) == ValueKind.String) {
                    print("{s}\n", .{val.String.as_str()});
                } else {
                    var buf = std.ArrayListUnmanaged(u8){};
                    defer buf.deinit(std.heap.c_allocator);
                    try val.encode_as_string(std.heap.c_allocator, &buf, false);
                    print("{s}\n", .{buf.items.ptr[0..buf.items.len]});
                }
            },
            .FormatString => {
                const args_count = frame.read_byte();
                const start = self.stack_top - args_count;
                var str_array = std.ArrayListUnmanaged(u8){};
                for (start[0..args_count]) |v_| {
                    try v_.encode_as_string(std.heap.c_allocator, &str_array, true);
                }
                str_array.shrinkAndFree(std.heap.c_allocator, str_array.items.len);
                self.pop_n(args_count);
                self.push(Value.string(self.make_string_from_slice(str_array.items)));
            },
            .Negate => {
                self.push(self.pop().negate());
            },
            .MakeObj => {
                const count = frame.read_byte();
                const fields_base: [*]Value = self.stack_top - count * 2;
                const fields = fields_base[0..count*2];

                const obj = try Object.new(std.heap.c_allocator, fields);
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
                const new_object = try self.update_object(std.heap.c_allocator, object.Object, addition.Object);
                self.push(new_object);
            },
            .AssertEq => {
                const b = self.pop();
                const a = self.pop();
                if (!self.eq(a, b)) @panic("Not equal");
            },
            .Union => {
                const count = frame.read_u8();
                const args_base = self.stack_top - count;
                const args = args_base[0..count];
                const union_val = try self.make_union(std.heap.c_allocator, args);
                var union_ptr = try std.heap.c_allocator.create(Union);
                union_ptr.* = union_val;
                self.push(Value.make_union(union_ptr));
            },
            .Exit => return,
            else => { 
                print("Unhandled op name: {s}\n", .{@tagName(op)});
                @panic("Unhandled op.");
            }
        }
    }
}

fn eq(self: *VM, a: Value, b: Value) bool {
    // Doing very simple thing of checking A extends B and B extends A for equality
    return self.extends(a, b) and self.extends(b, a);
}

/// Typescript's "extends" is a terrible name, but kept here for consistency's sake.
/// A better term is "subtype". First think of types as sets. For example, `string` is a set of every possible string type: "foo", "bar", "sldsad", and so on
/// When we say `A subtypes B`, we are actually saying `A is a subset of B`. For example: "foo" subtypes `string`.
fn extends(self: *VM, a: Value, b: Value) bool {
    if (b == .Any or a == .Any) return true;
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
    if (@as(ValueKind, b) == .String) return @as(ValueKind, a) == .String and a.String.ptr == b.String.ptr;

    if (@as(ValueKind, b) == .Array) return @as(ValueKind, a) == .Array and self.extends_array(a.Array, b.Array);
    if (@as(ValueKind, b) == .Object) return @as(ValueKind, a) == .Object and self.extends_object(a.Object, b.Object); 

    if (@as(ValueKind, b) == .Union) return self.extends_any_of(a, b.Union.variants_slice());
    if (@as(ValueKind, a) == .Union) return self.extends_many_all(a.Union.variants_slice(), b);

    return false;
}

fn extends_object(self: *VM, a: Object, b: Object) bool {
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

fn extends_array(self: *VM, a: Array, b: Array) bool {
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
    _ = self;
    switch (object) {
        .Object => |obj| {
            const field = obj.get_field(index.String) orelse return .Any;
            return field.value;
        },
        .Array => |arr| {
            if (@as(ValueKind, index) == ValueKind.Number) { 
                if (std.math.floor(index.Number) != index.Number) return .Undefined;
                return arr.item_at_index(@intFromFloat(index.Number));
            }
            // TODO: 
            unreachable;
        },
        .String => |str| {
            _ = str;
            if (@as(ValueKind, index) == ValueKind.Number) return .StringKeyword;
            // TODO: 
            unreachable;
        },
        else => return .Any,
    }
}

fn update_object(self: *VM, alloc: Allocator, base: Object, additional: Object) !Value {
    _ = self;
    var new_fields = try alloc.alloc(Object.Field, base.len + additional.len);
    if (base.len > 0) @memcpy(new_fields[0..base.len], base.fields_slice());
    if (additional.len > 0) @memcpy(new_fields[base.len..base.len + additional.len], additional.fields_slice());

    std.sort.block(Object.Field, new_fields, {}, Object.Field.less_than_key);

    var object: Object = .{
        .fields = new_fields.ptr,
        .len = base.len + additional.len
    };

    object.panic_on_duplicate_keys();

    return Value.object(object);
}

fn make_union(self: *VM, alloc: Allocator, variants: []const Value) !Union {
    var variants_list = try std.ArrayListUnmanaged(Value).initCapacity(alloc, variants.len);

    for (variants) |variant| {
        if (@as(ValueKind, variant) == .Union) {
            for (variant.Union.variants_slice()) |nested_variant| {
                try self.make_union_impl(alloc, nested_variant, &variants_list);
            }
            continue;
        }
        try self.make_union_impl(alloc, variant, &variants_list);
    }

    if (variants_list.items.len != variants.len) {
        variants_list.shrinkAndFree(alloc, variants_list.items.len);
    }

    return .{
        .variants = variants_list.items.ptr,
        .len = @intCast(variants_list.items.len),
    };
}

fn make_union_impl(self: *VM, alloc: Allocator, potential_variant: Value, existing_variants: *std.ArrayListUnmanaged(Value)) !void {
    for (existing_variants.items) |existing_variant| {
        if (self.extends(potential_variant, existing_variant)) return;
    }
    try existing_variants.append(alloc, potential_variant);
}

fn make_string_from_slice(self: *VM, slice: []const u8) String {
    return self.interned_strings.get(slice) orelse .{
        .len = @intCast(slice.len),
        .ptr = @ptrCast(slice.ptr),
    };
}

fn make_array_spread(self: *VM, count: u32, spread_bitfield: u256) !void {
    const spread_count = @popCount(spread_bitfield);
    std.debug.assert(spread_count > 0);

    var bitset = std.bit_set.IntegerBitSet(256).initEmpty();
    bitset.mask = spread_bitfield;

    const array_args_base = self.stack_top - count;

    const total_size = size: {
        var total: u32 = @as(u32, @intCast(count)) - @as(u32, @intCast(spread_count));

        var iter = bitset.iterator(.{});
        while (iter.next()) |idx| {
            const val: Value = array_args_base[idx];
            std.debug.assert(@as(ValueKind, val) == ValueKind.Array);
            total += val.Array.len;
        }
        
        break :size total;
    };

    if (total_size == 0) {
        self.pop_n(count);
        self.push(Value.array(Array.empty_tuple()));
        return;
    }

    var ptr = try std.heap.c_allocator.alloc(Value, total_size);

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
            @memcpy(ptr[i..i + len], array_args_base[start..end]);
            i += len;
        }

        const val: Value = array_args_base[idx];
        std.debug.assert(@as(ValueKind, val) == ValueKind.Array);
        @memcpy(ptr[i..i + val.Array.len], val.Array.items());

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
        @memcpy(ptr[i..i + len], array_args_base[prev..count]);
    }

    const arr = Array {
        .ptr = ptr.ptr,
        .len = total_size,
        .flags = Array.Flags {
            .is_tuple = true,
        },
    };

    self.pop_n(count);
    self.push(Value.array(arr));
}

fn make_array(self: *VM, comptime is_tuple: bool, count: u32) !void {
    if (!is_tuple) std.debug.assert(count == 1);
    const items_ptr = self.stack_top - count;
    const items = items_ptr[0..count];
    const array = try Array.new(std.heap.c_allocator, is_tuple, items, &[_]Value{});

    self.pop_n(count);
    self.push(Value.array(array));
}

fn call_main(self: *VM, main_name: ConstantTableIdx) !void {
    const count = 1;
    // TODO: read args from stdout
    try self.make_array(true, count);
    std.debug.assert(
        @as(ValueKind, (self.stack_top - 1)[0]) == ValueKind.Array
    );
    self.call(count, main_name, false);
}

/// Call a function. This assumes the top of the stack has the N values for the function's arguments.
fn call(self: *VM, arg_count: u8, fn_name: ConstantTableIdx, tail_call: bool) void {
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

        current_call_frame.* = .{
            .func = new_func,
            .ip = new_func.code.ptr,
            .slots = old_slots
        };
        return;
    }

    // 0 1 2 3 (4) 5 6 7 8 (9)
    const func: *const Function = self.functions.getPtr(fn_name).?;
    self.push_call_frame(.{
        .func = func,
        .ip = func.code.ptr,
        .slots = self.stack_top - arg_count
    });
}

inline fn cur_call_frame(self: *VM) *CallFrame {
    return &self.call_frames[self.call_frames_count - 1];
}

inline fn peek(self: *VM, distance: usize) Value {
    return (self.stack_top - 1 - distance)[0];
}

inline fn push(self: *VM, value: Value) void {
    self.stack_top[0] = value;
    self.stack_top += 1;
}

inline fn pop(self: *VM) Value {
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
        .Bool => .{.Bool = self.read_constant_boolean(constant_entry.idx)},
        .Number => .{.Number = self.read_constant_number(constant_entry.idx)},
        .String => .{.String = self.read_constant_string(constant_entry.idx)},
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
    const constant_string_ptr: [*]const u8 = @ptrCast(&self.constant_pool[constant_idx.v]);
    const len: u32 = @as(*const u32, @ptrCast(@alignCast(constant_string_ptr))).*;
    return .{
        .len = len,
        .ptr = constant_string_ptr + @sizeOf(u32),
    };
}

const Bytecode = struct {
    const Header = extern struct {
        magic: u32 align(8),
        constants_offset: u32,
        functions_offset: u32,
    };

    const ConstantsHeader = extern struct {
        table_len: u32 align(8),
        pool_size: u32,
        pool_padding: u32,
    };

    const FunctionsHeader = extern struct{
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

const ConstantString = extern struct {
    len: u32 align(8),
    ptr: [*]const u8
};

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

pub const ConstantKind = enum(u32) {
    Bool,
    Number,
    String
};

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
        const val = std.mem.readIntLittle(T, @ptrCast(self.ip[0..byte_amount]));
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

const Function = struct {
    name: ConstantTableIdx,
    code: []const u8,

    pub fn read_int(self: *const Function, comptime T: type, i: *usize) T {
        const byte_amount = comptime @divExact(@typeInfo(T).Int.bits, 8);
        const val = std.mem.readIntLittle(T, @ptrCast(self.code[i.*..i.* + byte_amount]));
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
                    std.debug.print("{} CallMain {s}\n", .{j, vm.read_constant(idx).String.as_str() });
                },
                .ToTypescriptSource => {
                    std.debug.print("{} ToTypescriptSource\n", .{j});
                },
                .WriteFile => {
                    std.debug.print("{} WriteFile\n", .{j});
                },
                .Lte => {
                    std.debug.print("{} LTE\n", .{j});
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
                .Intersect => {
                    const count = self.code[i];
                    i += 1;
                    std.debug.print("{} INTERSECT: {}\n", .{j, count});
                },
                .Union => {
                    const count = self.read_u8(&i);
                    std.debug.print("{} Union: {d}\n", .{j, count});
                },
                .Print => {
                    std.debug.print("{} Print\n", .{j});
                },
                .Constant => {
                    const idx = self.read_constant_table_idx(&i);
                    std.debug.print("{} CONST: {any}\n", .{j, vm.read_constant(idx)});
                },
                .Pop => {
                    std.debug.print("{} POP\n", .{j});
                },
                .TailCall, .Call => {
                    const count = self.code[i];
                    i += 1;
                    const name_idx = self.read_constant_table_idx(&i);
                    std.debug.print("{} {?} {?} {s}\n", .{j, op, count, vm.read_constant(name_idx).String.as_str()});
                },
                .SetLocal => {
                    const idx = self.code[i];
                    i += 1;
                    std.debug.print("{} Set local {?}\n", .{j, idx});
                },
                .GetLocal => {
                    const idx = self.code[i];
                    i += 1;
                    std.debug.print("{} Get local {?}\n", .{j, idx});
                },
                .SetGlobal => {
                    const idx = self.read_constant_table_idx(&i);
                    std.debug.print("{} SET GLOBAL {s}\n", .{j, vm.read_constant(idx).String.as_str()});
                },
                .GetGlobal => {
                    const idx = self.read_constant_table_idx(&i);
                    std.debug.print("{} GET GLOBAL {?}\n", .{j, vm.read_constant(idx)});
                },
                .PanicExtends => {
                    std.debug.print("{} PanicExtends\n", .{j});
                },
                .Extends, .ExtendsNoPopLeft => {
                    const skip_then = @as(u16, @intCast(self.code[i + 1])) << 8 | @as(u16, @intCast(self.code[i]));
                    i += 2;
                    std.debug.print("{} {?} (skip_then={})\n", .{j, op, skip_then});
                },
                .Jump => {
                    const offset = @as(u16, @intCast(self.code[i + 1])) << 8 | @as(u16, @intCast(self.code[i]));
                    i += 2;
                    std.debug.print("{} JUMP {}\n", .{j, offset});
                },
                .PopCallFrame => {
                    std.debug.print("{} POP CALL FRAME\n", .{j});
                },
                .MakeObj => {
                    const count = self.read_u8(&i);
                    std.debug.print("{} Make obj {?}\n", .{j, count});
                },
                .MakeArray => {
                    std.debug.print("{} MakeArray\n", .{j});
                },
                .EmptyTuple => {
                    std.debug.print("{} EmptyTuple\n", .{j});
                },
                .MakeTuple => {
                    const count = self.read_u8(&i);
                    std.debug.print("{} MakeTuple {d}\n", .{j, count});
                },
                .MakeTupleSpread => {
                    const count = self.read_u8(&i);
                    const spread_bitfield = self.read_u256(&i);
                    std.debug.print("{} MakeTupleSpread {d} {d}\n", .{j, count, spread_bitfield});
                },
                .Index => {
                    std.debug.print("{} Index\n", .{j});
                },
                .IndexLit => {
                    const constant = self.read_constant_table_idx(&i);
                    std.debug.print("{} IndexLit {?}\n", .{j, constant});
                },
                .FormatString => {
                    const count = self.code[i];
                    i += 1;
                    std.debug.print("{} FormatString {?}\n", .{j, count});
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
                .AssertEq => {
                    std.debug.print("{} AssertEq\n", .{j});
                },
                else => { 
                    print("UNHANDLED: {s}\n", .{ @tagName(op) });
                    @panic("Unimplemented: "); 
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

const Op = enum(u8) {
    // 2 stack operands
    Add = 0,
    Sub,
    Mul,
    Eq,
    Lte,
    Intersect,
    Union,

    // 1 instruction operand + N stack operands
    Print,

    // 1 instruction operand
    Constant,

    Pop,
    // N stack operands (args) + 1 stack operand (fn ident) + 1 instr for count
    Call,
    TailCall,
    CallMain,
    // 2 args, 2 jump instrs
    Extends,
    ExtendsNoPopLeft,
    PanicExtends,
    Jump,
    Number,
    Boolean,
    String,
    Object,
    PopCallFrame,
    // next instr is fields
    MakeObj,
    EmptyTuple,
    MakeArray,
    MakeTuple,
    MakeTupleSpread,

    Index,
    IndexLit,

    WriteFile,
    ToTypescriptSource,
    ParseInt,
    Panic,

    SetLocal,
    GetLocal,
    SetGlobal,
    GetGlobal,

    FormatString,
    Any,
    Length,

    Negate,
    Update,

    AssertEq,

    Exit,
};

const ValueKind = enum {
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
};

const Value = union(ValueKind){
    Any,
    Undefined,
    NumberKeyword,
    BoolKeyword,
    StringKeyword,
    ObjectKeyword,

    Number: f64,
    Bool: bool,
    String: String,
    Array: Array,
    Object: Object,
    Union: *Union,

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

    fn object(value: Object) Value {
        return .{
            .Object = value,
        };
    }

    fn array(value: Array) Value {
        return .{
            .Array = value
        };
    }

    fn number(value: f64) Value {
        return .{ .Number = value };
    }

    fn boolean(value: bool) Value {
        return .{ .Bool = value };
    }

    fn string(value: String) Value {
        return .{ .String = value };
    }

    fn debug(self: Value, alloc: Allocator, comptime str: []const u8) !void {
        var buf = std.ArrayListUnmanaged(u8){};
        try self.encode_as_string(alloc, &buf, true);
        print("{s}: {s}\n", .{str, buf.items.ptr[0..buf.items.len]});
    }

    fn encode_as_string(self: Value, alloc: Allocator, buf: *std.ArrayListUnmanaged(u8), format: bool) !void {
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
            .String => |v|{
                if (format) {
                    try write_str_expand(alloc, buf, "{s}", .{v.as_str()});
                } else {
                    try write_str_expand(alloc, buf, "\"{s}\"", .{v.as_str()});
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
                        try Value.string(field.name).encode_as_string(alloc, buf, format);
                        try write_str_expand(alloc, buf, ": ", .{});
                        try field.value.encode_as_string(alloc, buf, format);
                        if (i != last) try write_str_expand(alloc, buf, ",\n", .{});
                    }
                }
                try write_str_expand(alloc, buf, "\n}}\n", .{});
            },
            .Array => |v| {
                try write_str_expand(alloc, buf, "[", .{});
                if (v.ptr) |ptr| {
                    const last = v.len -| 1;
                    for (ptr[0..v.len], 0..) |val, i| {
                        try val.encode_as_string(alloc, buf, format);
                        if (i != last) try write_str_expand(alloc, buf, ", ", .{});
                    }
                }
                try write_str_expand(alloc, buf, "]\n", .{});
            },
            .Union => |v| {
                const last = v.len -| 1;
                for (v.variants[0..v.len], 0..) |variant, i| {
                    try variant.encode_as_string(alloc, buf, format);
                    if (i != last) {
                        try write_str_expand(alloc, buf, " | ", .{});
                    } 
                }
            }
        }
    }
};

fn write_str_expand(alloc: Allocator, buf: *std.ArrayListUnmanaged(u8), comptime fmt: []const u8, args: anytype) !void {
    const len: usize = @intCast(std.fmt.count(fmt, args));
    try buf.ensureUnusedCapacity(alloc, len);
    var insertion_slice = buf.items.ptr[buf.items.len..buf.items.len + len];
    _ = try std.fmt.bufPrint(insertion_slice, fmt, args);
    buf.items.len += len;
}

/// INVARIANTS:
/// - Strings are always interned, so two strings are equal if their pointer's are equal
const String = struct {
    ptr: [*]const u8,
    len: u32 align(8),

    pub fn from_constant(constant: ConstantString) String {
        return .{
            .ptr = constant.ptr,
            .len = constant.len,
        };
    }

    pub fn as_str(self: String) []const u8 {
        return self.ptr[0..self.len];
    }

    pub fn cmp(a: *const String, b: *const String) Order {
        if (@as(usize, @intFromPtr(a.ptr)) < @as(usize, @intFromPtr(b.ptr))) return .Less;
        if (@as(usize, @intFromPtr(a.ptr)) > @as(usize, @intFromPtr(b.ptr))) return .Greater;
        return .Equal;
    }
};

const Array = struct {
    ptr: ?[*]const Value align(8),
    len: u32,
    flags: Flags,

    const Flags = packed struct {
        is_tuple: bool = false,
        pad: u31 = 0,
    };

    pub fn new(alloc: Allocator, is_tuple: bool, values: []const Value, spread: []const Value) !Array {
        var ptr = try alloc.alloc(Value, values.len + spread.len);

        if (values.len > 0) @memcpy(ptr[0..values.len], values);
        if (spread.len > 0) @memcpy(ptr[values.len..values.len + spread.len], spread);

        return .{
            .ptr = @ptrCast(ptr),
            .len = @intCast(values.len + spread.len),
            .flags = Flags{
                .is_tuple = is_tuple,
            },
        };
    }

    pub fn is_tuple_array(self: *const Array) bool {
        return self.flags.is_tuple;
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

    pub fn empty_tuple() Array {
        return .{
            .ptr = null,
            .len = 0,
            .flags = Flags{
                .is_tuple = true,
            },
        };
    }
};

/// INVARIANTS:
/// - `fields` are ordered lexographically by key
const Object = struct { 
    fields: ?[*]const Field,
    len: u32,

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
            if (fields[i].name.ptr == fields[j].name.ptr) @panic("Duplicate keys!");
            i += 1;
            j += 1;
        }
    }

    pub fn new(alloc: Allocator, fields: []const Value) !Object {
        std.debug.assert(fields.len % 2 == 0);

        var object_fields = try alloc.alloc(Field, fields.len / 2);

        var i: usize = 0;
        for (object_fields) |*f| {
            f.name = fields[i].String;
            f.value = fields[i + 1];
            i += 2;
        }

        std.sort.block(Object.Field, object_fields, {}, Field.less_than_key);

        var object: Object = .{
            .fields = object_fields.ptr,
            .len = @intCast(fields.len / 2)
        };

        object.panic_on_duplicate_keys();

        return object;
    }

    pub inline fn fields_slice(self: *const Object) []const Field {
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
};

/// INVARIANTS:
/// - `len` must always be >= 2, otherwise its not a union
/// - `variants` is always **flat**, meaning no Value in `variants` will be a Union. 
///    This ensures that normalizing unions is cheap.
const Union = struct {
    variants: [*]Value,
    len: u32,

    pub fn variants_slice(self: Union) []const Value {
        return self.variants[0..self.len];
    }
};

pub const Order = enum {
    Less,
    Equal,
    Greater,
};

pub fn binary_search(comptime T: type, arr: []const T, search_elem: *const T, comptime cmp: fn(*const T, *const T) Order) ?usize {
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

test "count spread" {
    std.debug.print("HIHIIJKJLKJFD\n", .{});
    const args = &[_]u8{1, 1, 1, 1, 12, 1, 42, 1, 69};
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
    std.debug.print("SIZE: {d}\n", .{ total_size });
    const manual_size = manual: {
        var i: usize = 0;
        for (args) |a| {
            i += a;
        }
        break :manual i;
    };
    try std.testing.expectEqual(manual_size, total_size);
}