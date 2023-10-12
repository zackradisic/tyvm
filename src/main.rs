use tyvm_compiler::compile;

pub fn main() {
    let arena = oxc_allocator::Allocator::default();
    let source = std::fs::read_to_string("./test/fib.ts").unwrap();
    let bytecode = compile(&arena, &source);
    std::fs::write("./fib.tsb", bytecode).unwrap();
}
