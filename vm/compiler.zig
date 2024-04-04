pub const Source = extern struct {
    ptr: [*]const u8,
    len: usize,
};

pub const Bytecode = extern struct {
    ptr: [*]u8,
    len: usize,
    cap: usize,
};

extern fn tyvm_compile(source: Source) Bytecode;
pub extern fn tyvm_bytecode_free(bytecode: Bytecode) void;

pub fn compile(source: Source) Bytecode {
    return tyvm_compile(source);
}

pub fn bytecode_free(bytecode: Bytecode) void {
    return tyvm_bytecode_free(bytecode);
}
