pub mod common;
pub mod compile;
pub mod ir;
pub mod op;
pub use common::*;
use compile::Compiler;
use oxc_span::SourceType;

pub fn main() {
    let arena = oxc_allocator::Allocator::default();
    let source = std::fs::read_to_string("./test/simple.ts").unwrap();
    let parser = oxc_parser::Parser::new(
        &arena,
        &source,
        SourceType::default().with_typescript_definition(true),
    );

    let result = parser.parse();
    if result.panicked {
        panic!("Shit")
    }

    if result.errors.len() > 0 {
        println!("ERRORS: {:?}", result.errors);
    }

    let transform = ir::Transform { arena: &arena };
    let ir = arena.alloc(transform.transform_oxc(arena.alloc(result.program)));
    let mut compiler = Compiler::new();
    compiler.compile(ir);

    let mut buf: Vec<u8> = vec![];
    println!("COMPILER: {:#?}", compiler.functions.len());
    compiler.serialize(&mut buf);

    std::fs::write("./fib.tyb", buf).unwrap();
}
