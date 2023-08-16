pub mod common;
pub mod compile;
pub mod ir;
pub mod op;
pub use common::*;
use compile::GLOBAL_STR;

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
    /// TODO: We .clone() the values everywhere and its terrible for perf
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

            // println!("Running OP: {:?}", op);
            self.call_frame_mut().instr_offset += 1;

            match op {
                Op::Lte => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Bool(Some(
                        a.expect_num().unwrap() <= b.expect_num().unwrap(),
                    )))
                }
                Op::Eq => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Bool(Some(
                        b.expect_num().unwrap() == a.expect_num().unwrap(),
                    )))
                }
                Op::Sub => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Number(Some(
                        a.expect_num().unwrap() - b.expect_num().unwrap(),
                    )))
                }
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
                Op::TailCall => {
                    self.call(true);
                }
                Op::Call => {
                    self.call(false);
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
                Op::String => self.push(Value::String(None)),
                Op::PopCallFrame => {
                    let return_val = self.peek(0);
                    let return_slot = self.call_frame().slot_offset;
                    self.stack[return_slot] = return_val;
                    self.stack_len = return_slot + 1;
                    self.pop_call_frame();
                }
                Op::MakeObj => {
                    let count = self.read_byte();
                    let back = count * 2;
                    let obj_fields = BTreeMap::from_iter(
                        self.stack[self.stack_len - back as usize..self.stack_len]
                            .windows(2)
                            .map(|slice| {
                                (slice[0].clone().expect_string().unwrap(), slice[1].clone())
                            }),
                    );
                    self.push(Value::Object(Some(obj_fields)));
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

            (Value::Object(Some(a)), Value::Object(Some(b))) => self.extends_object(a, b),
            (Value::Object(_), Value::Object(None)) => true,

            (Value::String(Some(a)), Value::String(Some(b))) => a == b,
            (Value::String(_), Value::String(None)) => true,

            (Value::Bool(Some(a)), Value::Bool(Some(b))) => a == b,
            (Value::Bool(_), Value::Bool(None)) => true,
            (a, b) => todo!("Unimplemented: {:?} \n{:?}", a, b),
        }
    }

    fn extends_object(&self, a: &BTreeMap<String, Value>, b: &BTreeMap<String, Value>) -> bool {
        // If `a` has less keys than `b` then it cannot possibly subtype `b`
        if a.len() < b.len() {
            return false;
        }

        // `a` either has the same keys as `b` or more
        for (key, aval) in a.iter() {
            if let Some(bval) = b.get(key) {
                if !self.extends(aval, bval) {
                    return false;
                }
            }
        }

        true
    }

    fn push(&mut self, val: Value) {
        // unsafe {
        //     let ptr = self.stack.as_mut_ptr().offset(self.stack_len as isize);
        //     // *ptr = val;
        //     std::ptr::write(ptr, val);
        // }
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

    fn call(&mut self, tail_call: bool) {
        let count = self.read_byte();
        let name = self.read_global_constant().expect_string().unwrap();

        // Reuse the stack window
        if tail_call {
            if &name == &self.call_frame().name {
                self.call_frame_mut().instr_offset = 0;
                let start = self.call_frame().slot_offset;
                if count > 0 {
                    let base_ptr = self.stack.as_mut_ptr();
                    // let args_slice = &self.stack[self.stack_len - count as usize..self.stack_len];
                    // let dest_slice = &mut self.stack[start..start + count as usize];
                    unsafe {
                        let src_ptr = base_ptr.offset(self.stack_len as isize - count as isize);
                        let dest_ptr = base_ptr.offset(start as isize);
                        // This is safe because src_ptr and dest_ptr shouldn't overlap,
                        // because if args count > 0 then we have pushed the args onto the stack
                        std::ptr::copy_nonoverlapping(src_ptr, dest_ptr, count as usize);
                    }
                }
                self.stack_len = start + count as usize + 1;
                return;
            }

            let start = self.call_frame().slot_offset;
            if count > 0 {
                let base_ptr = self.stack.as_mut_ptr();
                // let args_slice = &self.stack[self.stack_len - count as usize..self.stack_len];
                // let dest_slice = &mut self.stack[start..start + count as usize];
                unsafe {
                    let src_ptr = base_ptr.offset(self.stack_len as isize - count as isize);
                    let dest_ptr = base_ptr.offset(start as isize);
                    // This is safe because src_ptr and dest_ptr shouldn't overlap,
                    // because if args count > 0 then we have pushed the args onto the stack
                    std::ptr::copy_nonoverlapping(src_ptr, dest_ptr, count as usize);
                }
            }

            self.stack_len = start + count as usize + 1;
            let func = self.fns.get(&name).cloned().unwrap();
            let cur_call_frame = self.call_frame_mut();
            cur_call_frame.chunk = func.chunk;
            cur_call_frame.instr_offset = 0;
            cur_call_frame.slot_offset = start;
            cur_call_frame.name = name.to_owned();
            return;
        }

        let new_slot_offset = self.stack_len - count as usize;
        let func = self.fns.get(&name).cloned().unwrap();
        self.push_call_frame(CallFrame {
            chunk: func.chunk,
            instr_offset: 0,
            slot_offset: new_slot_offset,
            name: name.to_owned(),
        });
    }

    fn read_global_constant(&mut self) -> Value {
        let idx = self.read_byte();
        self.fns.get(GLOBAL_STR).unwrap().chunk.constants[idx as usize].clone()
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
        // Clone bad
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
/// TODO: Clone is bad >:(
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
            other => panic!("Got {:?}", other),
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
    name: String,
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
    // fns.get("Fib").unwrap().chunk.debug_code();
    // fns.get("Recurse").unwrap().chunk.debug_code();
    let chunk = main_fn.chunk.clone();

    let mut vm = VM {
        fns: BTreeMap::from_iter(
            fns.into_iter()
                .map(|(k, v)| (k, Function::from_compiled_function(v)))
                .chain(std::iter::once((
                    GLOBAL_STR.to_owned(),
                    Function::from_compiled_function(main_fn),
                ))),
        ),
        stack: unsafe { std::mem::transmute([0u8; 32768]) },
        stack_len: 0,
        call_frame_stack: unsafe { std::mem::transmute([0u8; 90112]) },
        call_frame_len: 0,
        globals: HashMap::new(),
    };

    vm.push_call_frame(CallFrame {
        name: GLOBAL_STR.to_owned(),
        chunk,
        instr_offset: 0,
        slot_offset: 0,
    });

    vm.run()
}

fn main() {
    let allocator = oxc_allocator::Allocator::default();
    let source = std::fs::read_to_string("./shittyfib.ts").unwrap();
    // let source = std::fs::read_to_string("./fib.ts").unwrap();
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
