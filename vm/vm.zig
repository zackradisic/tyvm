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
interned_strings: std.StringArrayHashMap(ConstantTableIdx),

/// Constant data loaded from the bytecode
functions: HashMap(ConstantTableIdx, Function),
constant_table: []const ConstantTableEntry align(8),
constant_pool: []const u8 align(8),

pub fn init(alloc: Allocator, bytecode: []const u8) !VM {
    var vm: VM = .{
        .globals = HashMap(ConstantTableIdx, Value).init(alloc),
        .interned_strings = std.StringArrayHashMap(ConstantTableIdx).init(alloc),
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
        if (entry.kind == .String) {
            const str_slice = self.read_constant_string(entry.idx).as_str();
            try self.interned_strings.put(str_slice, ConstantTableIdx.new(@intCast(i)));
        }
    }
}

pub fn run(self: *VM) !void {
    self.stack_top = &self.stack;

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
            const stack_len = @as(usize, @intFromPtr(self.stack_top)) / @sizeOf(Value) - @as(usize, @intFromPtr(self.stack[0..])) / @sizeOf(Value);
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
                self.call_main(main_name);
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
            .String => {
                self.push(.StringKeyword);
            },
            .MakeArray => {
                const count = frame.read_u32();
                self.make_array(count);
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
                self.push(self.read_constant(constant_idx));
            },
            .GetGlobal => {
                const constant_idx = frame.read_constant_idx();
                self.push(self.read_constant(constant_idx));
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
                const args_count = frame.read_byte();
                const val = self.peek(0);
                if (@as(ValueKind, val) == ValueKind.String) {
                    print("{s}\n", .{val.String.as_str()});
                } else {
                    print("{?}\n", .{val});
                }
                self.pop_n(args_count);
            },
            .FormatString => {
                const args_count = frame.read_byte();
                const start = self.stack_top - args_count;
                var str_array = std.ArrayListUnmanaged(u8){};
                for (start[0..args_count]) |v_| {
                    try v_.encode_as_string(std.heap.c_allocator, &str_array);
                }
                str_array.shrinkAndFree(std.heap.c_allocator, str_array.items.len);
                self.pop_n(args_count);
                self.push(Value.string(String.from_slice(str_array.items)));
            },
            .Exit => return,
            else => { 
                @panic("Unhandled op.");
            }
        }
    }
}

fn extends(self: *VM, a: Value, b: Value) bool {
    _ = self;
    if (b == .Any) return true;
    if (b == .NumberKeyword) return @as(ValueKind, a) == ValueKind.Number or @as(ValueKind, a) == ValueKind.NumberKeyword;
    if (b == .BoolKeyword) return @as(ValueKind, a) == ValueKind.Bool or @as(ValueKind, a) == ValueKind.BoolKeyword;
    if (b == .StringKeyword) return @as(ValueKind, a) == ValueKind.String or @as(ValueKind, a) == ValueKind.StringKeyword;

    if (@as(ValueKind, b) == .Number) return @as(ValueKind, a) == .Number and a.Number == b.Number;
    if (@as(ValueKind, b) == .Bool) return @as(ValueKind, a) == .Bool and a.Bool == b.Bool;
    // `a.String.ptr == b.String.ptr` should be sufficient because all strings
    // are interned, meaning that string equality is synonymous to pointer
    // equality. However, if we allow strings to have the same pointer (e.g.
    // "foobar" and "foo" share the same base pointer), this will obviously
    // break without a length check.
    if (@as(ValueKind, b) == .String) return @as(ValueKind, a) == .String and a.String.ptr == b.String.ptr;

    return false;
}

fn make_array(self: *VM, count: u32) void {
    _ = count;
    // For now push dummy value 
    self.push(.Any);
}

fn call_main(self: *VM, main_name: ConstantTableIdx) void {
    const count = 0;
    // TODO: read args from stdout
    self.make_array(count);
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

    fn read_byte(self: *CallFrame) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    fn read_u16(self: *CallFrame) u16 {
        const short = @as(u16, @intCast(self.ip[1])) << 8 | @as(u16, @intCast(self.ip[0]));
        self.ip += 2;
        return short;
    }

    fn read_u32(self: *CallFrame) u32 {
        var val = @as(u32, @intCast(self.ip[3])) << 24;
        val |= @as(u32, @intCast(self.ip[2])) << 16;
        val |= @as(u32, @intCast(self.ip[1])) << 8;
        val |= @as(u32, @intCast(self.ip[0]));
        self.ip += 4;
        return val;
    }

    fn read_constant_idx(self: *CallFrame) ConstantTableIdx {
        return ConstantTableIdx.new(self.read_u32());
    }
};

const Function = struct {
    name: ConstantTableIdx,
    code: []const u8,

    pub fn read_u32(self: *const Function, i: *usize) u32 {
        var val = @as(u32, @intCast(self.code[i.* + 3])) << 24;
        val |= @as(u32, @intCast(self.code[i.* + 2])) << 16;
        val |= @as(u32, @intCast(self.code[i.* + 1])) << 8;
        val |= @as(u32, @intCast(self.code[i.* + 0]));
        i.* = i.* + 4;
        return val;
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
                .String => {
                    std.debug.print("{} String\n", .{j});
                },
                .Add => {
                    std.debug.print("{} ADD\n", .{j});
                },
                .Sub => {
                    std.debug.print("{} SUB\n", .{j});
                },
                .Intersect => {
                    const count = self.code[i];
                    i += 1;
                    std.debug.print("{} INTERSECT: {}\n", .{j, count});
                },
                .Union => unreachable,
                .Print => {
                    const count = self.code[i];
                    i += 1;
                    std.debug.print("{} Print: {}\n", .{j, count});
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
                .PanicExtends, .Extends, .ExtendsNoPopLeft => {
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
                    const count = self.code[i];
                    i += 1;
                    std.debug.print("{} Make obj {?}\n", .{j, count});
                },
                .MakeArray => {
                    const count = self.read_u32(&i);
                    std.debug.print("{} MakeArray {?}\n", .{j, count});
                },
                .Index => {
                    std.debug.print("{} Index\n", .{j});
                },
                .IndexNumLit => {
                    const count = self.code[i];
                    i += 1;
                    std.debug.print("{} IndexNumLit {?}\n", .{j, count});
                },
                .FormatString => {
                    const count = self.code[i];
                    i += 1;
                    std.debug.print("{} FormatString {?}\n", .{j, count});
                },
                .Exit => {
                    std.debug.print("{} Exit\n", .{j});
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
    String,
    PopCallFrame,
    // next instr is fields
    MakeObj,
    MakeArray,
    MakeUnion,

    Index,
    IndexNumLit,

    WriteFile,
    ToTypescriptSource,
    ParseInt,
    Panic,

    SetLocal,
    GetLocal,
    SetGlobal,
    GetGlobal,

    FormatString,

    Exit,
};

const ValueKind = enum {
    Any,
    Number,
    Bool,
    String,
    NumberKeyword,
    BoolKeyword,
    StringKeyword,
};

const Value = union(ValueKind){
    Any,
    Number: f64,
    Bool: bool,
    String: String,
    NumberKeyword,
    BoolKeyword,
    StringKeyword,

    fn number(value: f64) Value {
        return .{ .Number = value };
    }

    fn boolean(value: bool) Value {
        return .{ .Bool = value };
    }

    fn string(value: String) Value {
        return .{ .String = value };
    }

    fn encode_as_string(self: Value, alloc: Allocator, buf: *std.ArrayListUnmanaged(u8)) !void {
        switch (self) {
            .Any, .NumberKeyword, .BoolKeyword, .StringKeyword => @panic("Unhandled"),
            .String => |v|{
                const len = std.fmt.count("{s}", .{v.as_str()});
                try buf.ensureUnusedCapacity(alloc, len);
                var insertion_slice = buf.items.ptr[buf.items.len..buf.items.len + len];
                _ = try std.fmt.bufPrint(insertion_slice, "{s}", .{v.as_str()});
                buf.items.len += len;
            },
            .Bool => |v| {
                const len = if (v) "true".len else "false".len;
                try buf.ensureUnusedCapacity(alloc, len);
                try buf.appendSlice(std.heap.c_allocator, if (v) "true" else "false");
            },
            .Number => |v| {
                const len = std.fmt.count("{d}", .{v});
                try buf.ensureUnusedCapacity(alloc, len);
                var insertion_slice = buf.items.ptr[buf.items.len..buf.items.len + len];
                _ = try std.fmt.bufPrint(insertion_slice, "{d}", .{v});
                buf.items.len += len;
            }
        }
    }
};

const String = struct {
    len: u32,
    ptr: [*]const u8,

    pub fn as_str(self: String) []const u8 {
        return self.ptr[0..self.len];
    }

    pub fn from_slice(slice: []const u8) String {
        return .{
            .len = @intCast(slice.len),
            .ptr = @ptrCast(slice.ptr),
        };
    }
};
