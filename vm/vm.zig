const std = @import("std");

const print = std.debug.print;

const HashMap = std.AutoHashMap;
const StringHashMap = std.StringArrayHashMap;
const Allocator = std.mem.Allocator;

const VM = @This();

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
    self.call_frames_top = &self.call_frames;

    const global_fn: *const Function = self.functions.get(ConstantTableIdx.new(0)).?;
    self.push_call_frame(.{
        .func = global_fn,
        .ip = global_fn.code.ptr,
        .slots = self.stack_top,
    });

    var frame: *CallFrame = self.call_frames[self.call_frames_count - 1];
    
    while (true) {
        const op: Op = @enumFromInt(frame.read_byte());
        
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
        }
    }
}

inline fn push(self: *VM, value: Value) void {
    self.stack_top[0] = value;
    self.stack_top += 1;
}

inline fn pop(self: *VM) Value {
    self.stack_top -=1;
    return self.stack_top[0];
}

fn push_call_frame(self: *VM, call_frame: CallFrame) void {
    self.call_frames[self.call_frame_count] = call_frame;
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

    pub fn new(v: u32) ConstantTableIdx {
        return .{ .v = v };
    }
};
const ConstantIdx = extern struct {
    v: u32,

    pub fn new(v: u32) ConstantIdx {
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
    ip: [*]u8, 
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

    fn read_byte(self: *CallFrame) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    fn read_u32(self: *CallFrame) u32 {
        var val = @as(u32, @intCast(self.ip[3])) << 24;
        val |= @as(u32, @intCast(self.code[2])) << 16;
        val |= @as(u32, @intCast(self.code[1])) << 8;
        val |= @as(u32, @intCast(self.code[0]));
        self.ip += 4;
        return val;
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
            i += 1;
            switch (op) {
                .CallMain => {
                    const idx = self.read_constant_table_idx(&i);
                    std.debug.print("{} CallMain {s}\n", .{i, vm.read_constant(idx).String.as_str() });
                },
                .ToTypescriptSource => {
                    std.debug.print("{} ToTypescriptSource\n", .{i});
                },
                .WriteFile => {
                    std.debug.print("{} WriteFile\n", .{i});
                },
                .Lte => {
                    std.debug.print("{} LTE\n", .{i});
                },
                .Eq => {
                    std.debug.print("{} EQ\n", .{i});
                },
                .Number => {
                    std.debug.print("{} Number\n", .{i});
                },
                .String => {
                    std.debug.print("{} String\n", .{i});
                },
                .Add => {
                    std.debug.print("{} ADD\n", .{i});
                },
                .Sub => {
                    std.debug.print("{} SUB\n", .{i});
                },
                .Intersect => {
                    const count = self.code[i];
                    i += 1;
                    std.debug.print("{} INTERSECT: {}\n", .{i, count});
                },
                .Union => unreachable,
                .Print => {
                    const count = self.code[i];
                    i += 1;
                    std.debug.print("{} Print: {}\n", .{i, count});
                },
                .Constant => {
                    const idx = self.read_constant_table_idx(&i);
                    std.debug.print("{} CONST: {any}\n", .{i, vm.read_constant(idx)});
                },
                .Pop => {
                    std.debug.print("{} POP\n", .{i});
                },
                .TailCall, .Call => {
                    const count = self.code[i];
                    i += 1;
                    const name_idx = self.read_constant_table_idx(&i);
                    std.debug.print("{} {?} {?} {s}\n", .{i, op, count, vm.read_constant(name_idx).String.as_str()});
                },
                .SetLocal => {
                    const idx = self.code[i];
                    i += 1;
                    std.debug.print("{} Set local {?}\n", .{i, idx});
                },
                .GetLocal => {
                    const idx = self.code[i];
                    i += 1;
                    std.debug.print("{} Get local {?}\n", .{i, idx});
                },
                .SetGlobal => {
                    const idx = self.read_constant_table_idx(&i);
                    std.debug.print("{} SET GLOBAL {s}\n", .{i, vm.read_constant(idx).String.as_str()});
                },
                .GetGlobal => {
                    const idx = self.read_constant_table_idx(&i);
                    std.debug.print("{} GET GLOBAL {?}\n", .{i, vm.read_constant(idx)});
                },
                .PanicExtends, .Extends => {
                    std.debug.print("{} {s}\n", .{i, @tagName(op)});
                },
                .ExtendsNoPopLeft => {
                    const skip_then = @as(u16, @intCast(self.code[i])) << 8 | @as(u16, @intCast(self.code[i + 1]));
                    i += 2;
                    std.debug.print("{} {?} (skip_then={})\n", .{i, op, skip_then});
                },
                .Jump => {
                    const offset = @as(u16, @intCast(self.code[i])) << 8 | @as(u16, @intCast(self.code[i + 1]));
                    i += 2;
                    std.debug.print("{} JUMP {}\n", .{i, offset});
                },
                .PopCallFrame => {
                    std.debug.print("{} POP CALL FRAME\n", .{i});
                },
                .MakeObj => {
                    const count = self.code[i];
                    i += 1;
                    std.debug.print("{} Make obj {?}\n", .{i, count});
                },
                .MakeArray => {
                    const count = self.read_u32(&i);
                    std.debug.print("{} MakeArray {?}\n", .{i, count});
                },
                .Index => {
                    std.debug.print("{} Index\n", .{i});
                },
                .IndexNumLit => {
                    const count = self.code[i];
                    i += 1;
                    std.debug.print("{} IndexNumLit {?}\n", .{i, count});
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
};

const Value = union(enum){
    Number: f64,
    Bool: bool,
    String: String,

    fn number(value: f64) Value {
        return .{ .Number = value };
    }

    fn boolean(value: bool) Value {
        return .{ .Bool = value };
    }
};

const String = struct {
    len: u32,
    ptr: [*]const u8,

    pub fn as_str(self: String) []const u8 {
        print("LEN: {d}\n", .{self.len});
        return self.ptr[0..self.len];
    }
};
