pub mod common;
pub mod compile;
pub mod ir;
pub mod op;
pub mod value;
pub use common::*;
use compile::GLOBAL_STR;
use value::{KeywordType, ObjRef, Object, ObjectField, Value};

use std::collections::{BTreeMap, HashMap};

use op::{Chunk, Op};
use oxc_span::SourceType;

pub use oxc_ast::ast;

use crate::{
    compile::Compiler,
    value::{StringRef, ValueFormatterTs},
};

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

    /// TODO: make this str ptr or value for the key
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

            println!(
                "{} Running OP: {:?}",
                self.call_frame().name.as_str_ref_owned().as_slice(),
                op,
            );
            self.call_frame_mut().instr_offset += 1;

            match op {
                Op::ToTypescriptSource => {
                    let value = self.pop();
                    let type_name_value = self.pop();
                    if !type_name_value.is_str() {
                        panic!("ToTypescriptSource first arg should be a string literal")
                    }
                    let type_name_str_ref = type_name_value.as_str_ref_owned();
                    let type_name = type_name_str_ref.as_slice();
                    let mut buf = String::new();
                    let mut ts_formatter = ValueFormatterTs {
                        depth: 0,
                        pretty: true,
                        buf: &mut buf,
                        name: type_name,
                    };
                    ts_formatter.to_json(value).unwrap();
                    buf.shrink_to_fit();
                    let len = buf.len();
                    let leaked_buf = buf.as_mut_ptr();
                    std::mem::forget(buf);
                    let new_str_ref = StringRef::new_heap(leaked_buf, len);
                    self.push(Value::from_obj_ref(ObjRef::alloc_new_str_ref(new_str_ref)));
                }
                Op::WriteFile => {
                    let value_to_write = self.pop();
                    let file_path_value = self.pop();
                    if !file_path_value.is_str() {
                        panic!("WriteFile path should be a string literal")
                    }
                    if !value_to_write.is_str() {
                        panic!("Can only write file string values")
                    }
                    let file_path_str_ref = file_path_value.as_str_ref_owned();
                    let value_str_ref = value_to_write.as_str_ref_owned();
                    std::fs::write(file_path_str_ref.as_slice(), value_str_ref.as_slice()).unwrap();
                    self.push(Value::NEVER);
                }
                Op::Lte => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::from_bool(a.as_num() <= b.as_num()));
                }
                Op::Eq => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::from_bool(b.as_num() == a.as_num()))
                }
                Op::Sub => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::from_num(a.as_num() - b.as_num()))
                }
                Op::Add => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::from_num(a.as_num() + b.as_num()))
                }
                Op::Intersect => {
                    self.intersect();
                }
                Op::Union => todo!(),
                Op::Print => {
                    let count = self.read_byte();
                    let args = self.read_args(count, true);
                    self.push(Value::NEVER);
                    println!(
                        "{}",
                        args.iter()
                            .map(|v| format!("{:#?}", v))
                            .collect::<Vec<_>>()
                            .join(" ")
                    );
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
                Op::CallMain => {
                    let main_name = self.read_constant();
                    self.call_main(main_name.as_obj_ref())
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
                    let raw_str_ref = unsafe { *name.as_obj_ref().as_str_ref() };
                    self.globals.insert(raw_str_ref.to_string(), val);
                }
                Op::GetGlobal => {
                    let name_val = self.read_global_constant();
                    let name = unsafe { *name_val.as_obj_ref().as_str_ref() };
                    let val = self.globals.get(name.as_slice()).unwrap().clone();
                    self.push(val);
                }
                Op::PanicExtends => {
                    let b = self.pop();
                    let a = self.pop();
                    if !self.extends(a, b) {
                        panic!("Extends failed: {:?} does not extend {:?}", a, b)
                    }
                }
                Op::Extends => {
                    let b = self.pop();
                    let a = self.pop();
                    let jump_if_not_extends = self.read_short();
                    if !self.extends(a, b) {
                        self.call_frame_mut().instr_offset = jump_if_not_extends as usize
                    }
                }
                Op::Jump => {
                    let offset = self.read_short();
                    self.call_frame_mut().instr_offset = offset as usize;
                }
                Op::Number => self.push(Value::from_keyword_type(KeywordType::Number)),
                Op::String => self.push(Value::from_keyword_type(KeywordType::String)),
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
                    let obj = Object::from_iter(
                        self.stack[self.stack_len - back as usize..self.stack_len]
                            .windows(2)
                            .map(|slice| (slice[0].as_str_ref(), slice[1])),
                    )
                    .alloc();
                    self.stack_len -= back as usize;
                    self.push(Value::from_obj_ref(obj.into()));
                }
            }

            if self.call_frame().instr_offset >= self.call_frame().chunk.code.len() {
                break;
            }
        }
    }

    pub fn intersect(&mut self) {
        let count = self.read_byte();
        let start = self.stack_len - count as usize;
        let end = self.stack_len;

        let first_obj = self.stack[start];
        let mut new_fields: Vec<ObjectField> = first_obj.as_obj_ref().as_obj_ref().fields.clone();

        for &val in &self.stack[start + 1..end] {
            let obj_ref = val.as_obj_ref();
            let obj = obj_ref.as_obj_ref();
            for &ObjectField(key, val) in obj.fields.iter() {
                // FUCK: Need string interning
                // match new_fields.binary_search_by(|&ObjectField(k, _)| k.cmp(&key)) {
                match new_fields.binary_search_by(|&ObjectField(k, _)| unsafe {
                    let k1 = (*k).as_slice();
                    let k2 = (*key).as_slice();
                    k1.cmp(k2)
                }) {
                    Ok(existing) => {
                        let existing_val = new_fields[existing].1;
                        let insertion_ty = if self.extends(existing_val, val) {
                            continue;
                        } else if self.extends(val, existing_val) {
                            val
                        } else {
                            Value::NEVER
                        };
                        new_fields[existing] = (key, insertion_ty).into();
                    }
                    Err(new_slot) => {
                        new_fields.insert(new_slot, (key, val).into());
                    }
                }
            }
        }

        let val = Value::from_obj_ref(ObjRef::from_obj(Object { fields: new_fields }.alloc()));
        self.stack_len -= count as usize;
        self.push(val);
    }

    /// Extends is a terrible name, think of this as `is a a subset of b`?
    pub fn extends(&self, a: Value, b: Value) -> bool {
        if b == Value::NUMBER_KW {
            return a.is_num() || a == Value::NUMBER_KW;
        }
        if b == Value::BOOL_KW {
            return a.is_bool() || a == Value::BOOL_KW;
        }
        if b == Value::STRING_KW {
            return a == Value::STRING_KW || a.is_str();
        }
        if b == Value::OBJECT_KW {
            return a == Value::OBJECT_KW || a.is_obj();
        }
        if b.is_num() {
            return a == b;
        }
        if b.is_bool() {
            return a == b;
        }
        if b.is_str() {
            return a == b;
        }
        if b.is_obj() {
            return a.is_obj()
                && self.extends_object(a.as_obj_ref().as_obj_ref(), b.as_obj_ref().as_obj_ref());
        }

        false
    }

    fn extends_object(&self, a: &Object, b: &Object) -> bool {
        // If `a` has less keys than `b` then it cannot possibly subtype `b`
        if a.fields.len() < b.fields.len() {
            return false;
        }

        // `a` either has the same keys as `b` or more
        for &ObjectField(key, aval) in a.fields.iter() {
            if let Ok(bval_idx) = b
                .fields
                .binary_search_by(|ObjectField(bkey, _)| bkey.cmp(&key))
            {
                let bval = b.fields[bval_idx].1;
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
        let mut values: Vec<Value> = vec![Value::NULL; count as usize];
        let mut i: i32 = count.saturating_sub(1) as i32;
        let values = (0..count).map(|i| self.peek(i as usize)).rev().collect();
        if with_pop {
            self.stack_len -= count as usize;
        }
        values
    }

    fn call_main(&mut self, name: ObjRef) {
        // let mut count: u8 = 0;
        // for arg in std::env::args() {
        //     let str_ref = StringRef::new(&arg);
        //     let obj_ref = ObjRef::alloc_new_str_ref(str_ref);
        //     let value = Value::from_obj_ref(obj_ref);
        //     self.push(value);
        //     count += 1;
        // }
        let count: u8 = 1;
        let str_ref = StringRef::new(&"FUCK");
        let obj_ref = ObjRef::alloc_new_str_ref(str_ref);
        let value = Value::from_obj_ref(obj_ref);
        self.push(value);
        self.call_impl(name, count, false);
    }

    fn call_impl(&mut self, name: ObjRef, count: u8, tail_call: bool) {
        // Reuse the stack window
        if tail_call {
            if name == self.call_frame().name {
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
            let func = self
                .fns
                .get(name.as_str_ref_owned().as_slice())
                .cloned()
                .unwrap();
            let cur_call_frame = self.call_frame_mut();
            cur_call_frame.chunk = func.chunk;
            cur_call_frame.instr_offset = 0;
            cur_call_frame.slot_offset = start;
            cur_call_frame.name = name.to_owned();
            return;
        }

        let new_slot_offset = self.stack_len - count as usize;
        let func = self
            .fns
            .get(name.as_str_ref_owned().as_slice())
            .cloned()
            .unwrap();
        self.push_call_frame(CallFrame {
            chunk: func.chunk,
            instr_offset: 0,
            slot_offset: new_slot_offset,
            name: name.to_owned(),
        });
    }

    fn call(&mut self, tail_call: bool) {
        let count = self.read_byte();
        let name = self.read_global_constant().as_obj_ref();

        self.call_impl(name, count, tail_call)
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

    fn top(&self) -> Value {
        self.stack[self.stack_len - 1]
    }
}

pub struct CallFrame {
    chunk: Chunk,
    instr_offset: usize,
    /// offset into VM stack that represents the first stack slot this function
    /// can use
    slot_offset: usize,
    name: ObjRef,
}

fn run<'alloc>(arena: &'alloc Arena, program: &'alloc ast::Program<'alloc>) -> Value {
    println!("{:#?}", program);
    let transform = ir::Transform { arena };
    let ir = arena.alloc(transform.transform_oxc(program));
    println!("IR: {:#?}", ir);
    let mut compiler = Compiler::new();
    compiler.compile(ir);

    let (global_fn, fns) = compiler.funcs();
    println!("main CODE: {}", global_fn.chunk.code.len());
    global_fn.chunk.debug_code(&global_fn);
    println!("\n\n");
    fns.get("Main").unwrap().chunk.debug_code(&global_fn);
    // fns.get("Recurse").unwrap().chunk.debug_code();
    let chunk = global_fn.chunk.clone();

    let mut vm = VM {
        fns: BTreeMap::from_iter(
            fns.into_iter()
                .map(|(k, v)| (k, Function::from_compiled_function(v)))
                .chain(std::iter::once((
                    GLOBAL_STR.to_owned(),
                    Function::from_compiled_function(global_fn),
                ))),
        ),
        stack: [Value::NULL; 1024],
        stack_len: 0,
        call_frame_stack: unsafe { std::mem::transmute([0u8; 73728]) },
        call_frame_len: 0,
        globals: HashMap::new(),
    };

    vm.push_call_frame(CallFrame {
        name: ObjRef::alloc_new_str_ref(StringRef::new_inlined(GLOBAL_STR)),
        chunk,
        instr_offset: 0,
        slot_offset: 0,
    });

    vm.run();
    // *vm.globals.get("Main").unwrap()
    Value::NULL
}

fn main() {
    let allocator = oxc_allocator::Allocator::default();
    // let source = std::fs::read_to_string("./test/main.ts").unwrap();
    // let source = std::fs::read_to_string("./test/shittyfib.ts").unwrap();
    let source = std::fs::read_to_string("./test/fib.ts").unwrap();
    // let source = std::fs::read_to_string("./test/cycle.ts").unwrap();
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

    let result = run(&allocator, allocator.alloc(result.program));
    println!("RESULT: {:?}", result);
    // assert!(!result.as_obj_ref().is_str_ref());
    // assert!(result.is_obj());
    // let obj = result.as_obj_ref();
    // let fields = &obj.as_obj_ref().fields;
    // println!(
    //     "FIELDS: {:?}",
    //     fields
    //         .iter()
    //         .map(|ObjectField(k, v)| {
    //             let str = unsafe { (*(*k)).to_string() };
    //             (str, v)
    //         })
    //         .collect::<Vec<_>>()
    // );
}

#[cfg(test)]
mod test {
    use oxc_allocator::Allocator;
    use oxc_span::SourceType;

    use crate::{
        run,
        value::{ObjRef, Object, Value},
    };

    fn run_code<'a>(allocator: &'a Allocator, source: &str) -> Value {
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

        run(&allocator, allocator.alloc(result.program))
    }

    #[test]
    fn intersect_subset() {
        let allocator = Allocator::default();
        let expected = Value::from_obj_ref(ObjRef::from_obj(Object { fields: vec![] }.alloc()));

        let value = run_code(
            &allocator,
            "
type FibHelper<
  X extends number,
  I extends number,
  Prev extends number,
  PrevPrev extends number
> = Eq<X, I> extends true
  ? Add<Prev, PrevPrev>
  : FibHelper<X, Add<I, 1>, Add<Prev, PrevPrev>, Prev>;

type Fib<X extends number> = Lte<X, 1> extends true ? X : FibHelper<X, 2, 1, 0>;

type Main = Fib<50>;
        ",
        );

        assert_eq!(value, expected);
    }

    #[test]
    fn fib() {
        let allocator = Allocator::default();
        let expected = Value::from_num(12586269025.0);
        let value = run_code(
            &allocator,
            "
type FibHelper<
  X extends number,
  I extends number,
  Prev extends number,
  PrevPrev extends number
> = Eq<X, I> extends true
  ? Add<Prev, PrevPrev>
  : FibHelper<X, Add<I, 1>, Add<Prev, PrevPrev>, Prev>;

type Fib<X extends number> = Lte<X, 1> extends true ? X : FibHelper<X, 2, 1, 0>;

type Main = Fib<50>;
        ",
        );

        assert_eq!(value, expected);
    }
}
