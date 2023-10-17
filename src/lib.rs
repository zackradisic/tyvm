pub mod common;
pub mod compile;
pub mod ir;
pub mod ir_transform;
pub mod ir_transform_oxc;
pub mod op;

pub use common::*;
use compile::Compiler;
use oxc_span::SourceType;

#[repr(C)]
pub struct Source {
    ptr: *const u8,
    len: usize,
}

#[repr(C)]
pub struct Bytecode {
    ptr: *mut u8,
    len: usize,
    cap: usize,
}

#[no_mangle]
pub extern "C" fn tyvm_compile(source: Source) -> Bytecode {
    unsafe {
        let source = std::slice::from_raw_parts(source.ptr, source.len);
        let source = std::str::from_utf8_unchecked(source);
        let ir_arena = oxc_allocator::Allocator::default();
        let bytecode_buf = compile(&ir_arena, &source);
        let cap = bytecode_buf.capacity();
        let bytecode = bytecode_buf.leak();
        let len = bytecode.len();
        let bytecode_ptr = bytecode.as_mut_ptr();

        Bytecode {
            ptr: bytecode_ptr,
            len,
            cap,
        }
    }
}

#[no_mangle]
pub extern "C" fn tyvm_bytecode_free(bytecode: Bytecode) {
    let _buf = unsafe { Vec::from_raw_parts(bytecode.ptr, bytecode.len, bytecode.cap) };
}

pub fn compile<'input, 'ir>(ir_arena: &'ir oxc_allocator::Allocator, source: &str) -> Vec<u8> {
    let ir = {
        let arena = oxc_allocator::Allocator::default();
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

        let transform = ir_transform_oxc::Transform { arena: &ir_arena };
        ir_arena.alloc(transform.transform_oxc(arena.alloc(result.program)))
    };

    let mut compiler = Compiler::new();
    compiler.compile(&ir);

    let mut buf: Vec<u8> = vec![];
    println!("COMPILER: {:#?}", compiler.functions.len());
    println!(
        "COMPILER FUNCTIONS: {:#?}",
        compiler
            .functions
            .iter()
            .map(|(k, _)| k)
            .collect::<Vec<_>>()
    );
    compiler.serialize(&mut buf);
    buf
}
