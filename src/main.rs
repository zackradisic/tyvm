pub mod common;
pub mod compile;
pub mod ir;
pub mod op;
pub use common::*;

use std::collections::{BTreeMap, HashMap};

use op::{Chunk, Op};
use oxc_span::SourceType;

pub use oxc_ast::ast;

use crate::compile::Compiler;

#[derive(Clone)]
pub struct Function {
    chunk: Chunk,
}

impl Function {
    pub fn from_compiled_function(comp_fn: compile::Function) -> Self {
        Self {
            chunk: comp_fn.chunk,
        }
    }
}

pub struct VM {
    stack: [Value; 1024],
    stack_len: usize,

    call_frame_stack: [CallFrame; 1024],
    call_frame_len: usize,

    /// TODO: globals are static, so can make this an integer index
    globals: HashMap<String, Value>,
    fns: BTreeMap<String, Function>,
}

impl VM {
    pub fn run(&mut self) {
        loop {
            let op: Op = {
                let call_frame = self.call_frame();
                /*
                println!(
                    "STACK: {:?}",
                    self.stack[call_frame.slot_offset..self.stack_len]
                        .iter()
                        .collect::<Vec<_>>()
                );
                */
                call_frame.chunk.code[call_frame.instr_offset].into()
            };

            println!("Running OP: {:?}", op);
            self.call_frame_mut().instr_offset += 1;

            match op {
                Op::Add => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Number(Some(
                        a.expect_num().unwrap() + b.expect_num().unwrap(),
                    )))
                }
                Op::Intersect => todo!(),
                Op::Union => todo!(),
                Op::Print => {
                    let count = self.read_byte();
                    let args = self.read_args(count, true);
                    self.push(Value::Never);
                    println!("{:?}", args);
                }
                Op::Constant => {
                    let val = self.read_constant();
                    self.push(val);
                }
                Op::Pop => todo!(),
                Op::Call => {
                    self.call();
                }
                Op::SetLocal => todo!(),
                Op::GetLocal => {
                    let slot = self.read_byte();
                    let val = self.call_frame_local(slot);
                    self.push(val);
                }
                Op::SetGlobal => {
                    let val = self.pop();
                    let name = self.read_constant();
                    self.globals.insert(name.expect_string().unwrap(), val);
                }
                Op::GetGlobal => {
                    let name = self.read_constant();
                    let val = self
                        .globals
                        .get(&name.expect_string().unwrap())
                        .unwrap()
                        .clone();
                    self.push(val);
                }
                Op::PanicExtends => {
                    let b = self.pop();
                    let a = self.pop();
                    if !self.extends(&a, &b) {
                        panic!("Extends failed: {:?} does not extend {:?}", a, b)
                    }
                }
                Op::Extends => {
                    let b = self.pop();
                    let a = self.pop();
                    let jump_if_not_extends = self.read_short();
                    if !self.extends(&a, &b) {
                        self.call_frame_mut().instr_offset = jump_if_not_extends as usize
                    }
                }
                Op::Jump => {
                    let offset = self.read_short();
                    self.call_frame_mut().instr_offset = offset as usize;
                }
                Op::Number => self.push(Value::Number(None)),
                Op::PopCallFrame => {
                    let return_val = self.peek(0);
                    let return_slot = self.call_frame().slot_offset;
                    self.stack[return_slot] = return_val;
                    self.stack_len = return_slot + 1;
                    self.pop_call_frame();
                }
            }

            if self.call_frame().instr_offset >= self.call_frame().chunk.code.len() {
                break;
            }
        }
    }

    pub fn extends(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Number(Some(_)), Value::Number(None)) => true,
            (Value::Number(None), Value::Number(Some(_))) => false,
            (Value::Number(Some(a)), Value::Number(Some(b))) => a == b,
            (_, Value::Number(_)) => false,
            (a, b) => todo!("Unimplemented: {:?} \n{:?}", a, b),
        }
    }

    fn push(&mut self, val: Value) {
        self.stack[self.stack_len] = val;
        self.stack_len += 1;
    }

    fn pop(&mut self) -> Value {
        let ret = self.stack[self.stack_len - 1].clone();
        self.stack_len -= 1;
        ret
    }

    fn peek(&self, n: usize) -> Value {
        self.stack[self.stack_len - 1 - n].clone()
    }

    /// TODO: no heap allocation
    fn read_args(&mut self, count: u8, with_pop: bool) -> Vec<Value> {
        let mut values: Vec<Value> =
            vec![unsafe { std::mem::transmute([0u8; 32]) }; count as usize];
        let mut i: i32 = count.saturating_sub(1) as i32;
        let values = (0..count).map(|i| self.peek(i as usize)).rev().collect();
        if with_pop {
            self.stack_len -= count as usize;
        }
        values
    }

    fn call(&mut self) {
        let count = self.read_byte();
        let name = self.read_constant().expect_string().unwrap();
        let new_slot_offset = self.stack_len - count as usize;
        // TODO: Clone here is terrible for perf
        let func = self.fns.get(&name).cloned().unwrap();
        self.push_call_frame(CallFrame {
            chunk: func.chunk,
            instr_offset: 0,
            slot_offset: new_slot_offset,
        });
    }

    fn read_constant(&mut self) -> Value {
        let idx = self.read_byte();
        self.call_frame_mut().chunk.constants[idx as usize].clone()
    }

    fn read_byte(&mut self) -> u8 {
        let ret = self.call_frame().chunk.code[self.call_frame().instr_offset];
        self.call_frame_mut().instr_offset += 1;
        ret
    }

    fn read_short(&mut self) -> u16 {
        let left = self.read_byte();
        let right = self.read_byte();
        (left as u16) << 8 | (right as u16)
    }

    fn call_frame_local(&self, idx: u8) -> Value {
        self.stack[self.call_frame().slot_offset + idx as usize].clone()
    }

    fn call_frame(&self) -> &CallFrame {
        &self.call_frame_stack[self.call_frame_len - 1]
    }

    fn call_frame_mut(&mut self) -> &mut CallFrame {
        &mut self.call_frame_stack[self.call_frame_len - 1]
    }

    fn push_call_frame(&mut self, call_frame: CallFrame) {
        self.call_frame_stack[self.call_frame_len] = call_frame;
        self.call_frame_len += 1;
    }

    fn pop_call_frame(&mut self) {
        self.call_frame_len -= 1;
    }
}

/// TODO: Compact representation
#[derive(Clone, Debug)]
pub enum Value {
    String(Option<String>),
    Bool(Option<bool>),
    Number(Option<f64>),
    Object(Option<BTreeMap<String, Value>>),
    Never,
    Undefined,
}

impl Value {
    pub fn expect_string(self) -> Option<String> {
        match self {
            Value::String(str) => str,
            _ => panic!(),
        }
    }

    pub fn expect_num(self) -> Option<f64> {
        match self {
            Value::Number(num) => num,
            _ => panic!(),
        }
    }
}

pub struct CallFrame {
    chunk: Chunk,
    instr_offset: usize,
    /// offset into VM stack that represents the first stack slot this function
    /// can use
    slot_offset: usize,
}

fn run<'alloc>(arena: &'alloc Arena, program: &'alloc ast::Program<'alloc>) {
    println!("{:#?}", program);
    let transform = ir::Transform { arena };
    let ir = arena.alloc(transform.transform_oxc(program));
    println!("IR: {:#?}", ir);
    let mut compiler = Compiler::new();
    compiler.compile(ir);

    let (main_fn, fns) = compiler.funcs();
    println!("main CODE: {}", main_fn.chunk.code.len());
    main_fn.chunk.debug_code();
    println!("\n\n");
    fns.get("IsFunnyNum").unwrap().chunk.debug_code();

    let mut vm = VM {
        fns: BTreeMap::from_iter(
            fns.into_iter()
                .map(|(k, v)| (k, Function::from_compiled_function(v))),
        ),
        stack: unsafe { std::mem::transmute([0u8; 32768]) },
        stack_len: 0,
        call_frame_stack: unsafe { std::mem::transmute([0u8; 65536]) },
        call_frame_len: 0,
        globals: HashMap::new(),
    };

    vm.push_call_frame(CallFrame {
        chunk: main_fn.chunk,
        instr_offset: 0,
        slot_offset: 0,
    });

    vm.run()
}

fn main() {
    let allocator = oxc_allocator::Allocator::default();
    let source = std::fs::read_to_string("./main.ts").unwrap();
    let parser = oxc_parser::Parser::new(
        &allocator,
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

    run(&allocator, allocator.alloc(result.program));
}
