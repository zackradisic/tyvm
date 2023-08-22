use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::num::NonZeroUsize;

use crate::common::AllocBox as Box;
use crate::ir::{Expr, GlobalDecl};
use crate::value::{ObjRef, StringRef};
use crate::{common::*, ir, Value};

use oxc_ast::ast;
use oxc_ast::ast::*;

use crate::op::{Chunk, Op};

pub const GLOBAL_STR: &'static str = "__tyvm_global";
const UNINIT_STR: &'static str = "uninitialized memory string if you see this its bad";

pub trait Compile<'alloc> {
    fn compile(&self, compiler: &mut Compiler<'alloc>) {}
}

impl<'alloc> Compile<'alloc> for &ir::Expr<'alloc> {
    fn compile(&self, compiler: &mut Compiler<'alloc>) {
        compiler.compile_expr(self);
    }
}

impl<'alloc> Compile<'alloc> for () {
    fn compile(&self, compiler: &mut Compiler<'alloc>) {}
}

impl<'alloc, F: Fn(&mut Compiler<'alloc>) -> ()> Compile<'alloc> for F {
    fn compile(&self, compiler: &mut Compiler<'alloc>) {
        self(compiler);
    }
}

#[derive(Clone, Copy, Debug)]
pub struct LocalIdx(u8);

impl<'alloc> Compile<'alloc> for LocalIdx {
    fn compile(&self, compiler: &mut Compiler<'alloc>) {
        compiler.push_bytes(Op::GetLocal as u8, self.0);
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Local<'alloc> {
    name: &'alloc str,
}

#[derive(Debug)]
pub struct Locals<'alloc> {
    stack: [Local<'alloc>; u8::MAX as usize],
    count: u8,
}

impl<'alloc> Locals<'alloc> {
    pub fn push(&mut self, name: &'alloc str) -> u8 {
        if self.count == u8::MAX {
            panic!("TOO MANY LOCALS BRO WTF")
        }
        self.stack[self.count as usize] = Local { name };
        let ret = self.count;
        self.count += 1;
        ret
    }

    pub fn pop(&mut self) {
        if self.count == 0 {
            panic!("Popping empty locals")
        }
        self.count -= 1;
    }
}

impl<'alloc> Default for Locals<'alloc> {
    fn default() -> Self {
        Self {
            stack: [Local { name: UNINIT_STR }; u8::MAX as usize],
            count: Default::default(),
        }
    }
}

impl<'alloc> Locals<'alloc> {
    fn iter<'a>(&'a self) -> LocalsIter<'a, 'alloc> {
        LocalsIter { locals: self, i: 0 }
    }
}

#[derive(Debug)]
pub struct Function<'alloc> {
    locals: Locals<'alloc>,
    pub chunk: Chunk,
    global_constant_idx: Option<usize>,
}

pub struct Compiler<'alloc> {
    /// TODO Key can be Value or string ptr
    globals: HashMap<String, usize>,
    functions: BTreeMap<String, Function<'alloc>>,
    current_function_name: String,
}

impl<'alloc> Compiler<'alloc> {
    pub fn new() -> Self {
        Self {
            globals: Default::default(),
            functions: BTreeMap::from_iter([(
                GLOBAL_STR.to_owned(),
                Function {
                    locals: Locals {
                        stack: [Local { name: UNINIT_STR }; u8::MAX as usize],
                        count: 0,
                    },
                    chunk: Chunk::default(),
                    global_constant_idx: None,
                },
            )]),
            current_function_name: GLOBAL_STR.to_owned(),
        }
    }
    pub fn compile(&mut self, program: &'alloc ir::Program<'alloc>) {
        // Collect globals first
        {
            for stmt in &program.stmts {
                match stmt {
                    ir::Statement::LetDecl(GlobalDecl::Fn(fn_decl)) => {
                        let name = fn_decl.ident.name();
                        let str_ref = StringRef::new(name);
                        let name_constant_idx = self.push_constant_no_op(Value::from_obj_ref(
                            ObjRef::alloc_new_str_ref(str_ref),
                        ));
                        self.functions.insert(
                            name.to_string(),
                            Function {
                                locals: Locals::default(),
                                chunk: Chunk::default(),
                                global_constant_idx: Some(name_constant_idx),
                            },
                        );
                    }
                    ir::Statement::LetDecl(GlobalDecl::Var(var_decl)) => {
                        let name = var_decl.ident.name();
                        let str_ref = StringRef::new(name);
                        let name_constant_idx = self.push_constant_no_op(Value::from_obj_ref(
                            ObjRef::alloc_new_str_ref(str_ref),
                        ));
                        self.globals.insert(name.to_string(), name_constant_idx);
                    }
                    _ => (),
                }
            }
        }

        for stmt in &program.stmts {
            self.compile_statement(stmt)
        }

        if let Some(main_fn) = self.functions.get("Main") {
            self.push_bytes(
                Op::CallMain as u8,
                main_fn.global_constant_idx.unwrap() as u8,
            );
        }
    }

    fn compile_statement(&mut self, stmt: &'alloc ir::Statement<'alloc>) {
        match stmt {
            ir::Statement::LetDecl(let_decl) => self.compile_let_decl(let_decl),
            ir::Statement::Expr(_) => todo!(),
        }
    }

    fn compile_let_decl(&mut self, let_decl: &'alloc ir::GlobalDecl<'alloc>) {
        match let_decl {
            ir::GlobalDecl::Fn(fn_decl) => self.compile_fn_decl(fn_decl),
            ir::GlobalDecl::Var(var_decl) => self.compile_var_decl(var_decl),
        }
    }

    fn compile_fn_param_check(&mut self, param: &ir::FnParam<'alloc>, local_idx: u8) {
        if let Some(extends_ty) = &param.extends_type {
            self.push_bytes(Op::GetLocal as u8, local_idx);
            self.compile_expr(extends_ty);
            self.push_op(Op::PanicExtends);
        }
        if param.default.is_some() {
            todo!("Default parameters");
        }
    }

    fn compile_fn_decl(&mut self, fn_decl: &'alloc ir::FnDecl<'alloc>) {
        let name = fn_decl.ident.name();
        let name_string = name.to_string();
        let prev_name = std::mem::replace(&mut self.current_function_name, name_string.clone());
        let func = self.functions.get_mut(name).unwrap();
        for arg in &fn_decl.params {
            func.locals.push(arg.ident.name());
        }
        fn_decl
            .params
            .iter()
            .enumerate()
            .for_each(|(idx, param)| self.compile_fn_param_check(param, idx as u8));
        self.compile_expr(&fn_decl.body);
        self.push_op(Op::PopCallFrame);
        self.current_function_name = prev_name;
    }

    fn compile_var_decl(&mut self, var_decl: &ir::VarDecl<'alloc>) {
        self.compile_expr(&var_decl.expr);
        self.main_chunk_mut().chunk.push_op(Op::SetGlobal);
        let name = var_decl.ident.name();
        let constant_idx = *self.globals.get(name).unwrap();
        self.main_chunk_mut().chunk.push_u8(constant_idx as u8);
    }

    fn compile_ident(&mut self, ident: &ir::Ident<'alloc>) {
        let name_str: &str = ident.name();
        if let Some((op, idx)) = self.find_ident(name_str) {
            self.push_op(op);
            self.push_u8(idx);
            return;
        }
        if let Some(constant_idx) = self.globals.get(name_str) {
            self.push_bytes(Op::GetGlobal as u8, *constant_idx as u8);
            return;
        }
        panic!("Unknown ident: {:?}", name_str)
    }

    fn patch_jump(&mut self, instr_idx: u16, patch_idx: usize) {
        let part1 = ((instr_idx & 0b1111111100000000) >> 8) as u8;
        let part2 = (instr_idx & 0b11111111) as u8;

        let wtf: u16 = ((part1 as u16) << 8) | (part2 as u16);
        println!("WTF: {:?}", wtf);

        let code = &mut self.cur_fn_mut().chunk.code;
        code[patch_idx] = part1;
        code[patch_idx + 1] = part2;
    }

    fn compile_cond(
        &mut self,
        check_ty: impl Compile<'alloc>,
        extends_ty: &ir::Expr<'alloc>,
        then: impl Compile<'alloc>,
        r#else: &ir::Expr<'alloc>,
        extends_op: Op,
    ) {
        // self.compile_expr(check_ty);
        check_ty.compile(self);
        self.compile_expr(extends_ty);

        // self.push_op(Op::Extends);
        self.push_op(extends_op);

        let jump_to_else_branch_if_false_instr_idx = self.cur_fn_mut().chunk.code.len();
        self.push_bytes(0, 0);

        // self.compile_expr(then);
        then.compile(self);

        self.push_op(Op::Jump);
        let skip_else_branch_idx = self.cur_fn_mut().chunk.code.len();
        self.push_bytes(0, 0);

        let else_starting_idx = self.cur_fn_mut().chunk.code.len();
        self.patch_jump(
            else_starting_idx as u16,
            jump_to_else_branch_if_false_instr_idx,
        );
        self.compile_expr(r#else);
        let end_of_else_idx = self.cur_fn_mut().chunk.code.len();
        self.patch_jump(end_of_else_idx as u16, skip_else_branch_idx);
    }

    fn compile_make_obj<'a, I: Iterator<Item = (&'a str, &'alloc Expr<'alloc>)>>(
        &mut self,
        len: u8,
        iter: I,
    ) {
        for (k, v) in iter {
            self.push_constant(Value::from_obj_ref(ObjRef::alloc_new_str_ref(
                StringRef::new(k),
            )));
            self.compile_expr(v);
        }
        let count = len;
        self.push_bytes(Op::MakeObj as u8, count as u8);
    }

    fn compile_array(&mut self, types: &[&ir::Expr<'alloc>]) {
        let count: u8 = types.len().try_into().unwrap();
        types.iter().for_each(|t| self.compile_expr(t));
        self.push_bytes(Op::MakeArray as u8, count);
    }

    fn compile_num_lit(&mut self, num_lit: &NumberLiteral<'alloc>) -> usize {
        self.push_constant(Value::from_num(num_lit.value))
    }

    fn compile_expr(&mut self, expr: &ir::Expr<'alloc>) {
        match expr {
            Expr::Let(let_expr) => {
                match let_expr.cond {
                    Some(cond_ty) => {
                        self.compile_cond(
                            let_expr.check,
                            cond_ty,
                            // If the check branch succeeds, the bound variable must
                            // be added. We do this by using the
                            // ExtendsNoPopLeft instruction which will keep the
                            // lhs of the extends (the bound variable value) on
                            // the stack, and pushing the name to the locals of
                            // the current function.
                            |compiler: &mut Compiler<'alloc>| {
                                compiler.cur_fn_mut().locals.push(let_expr.name);
                                compiler.compile_expr(let_expr.then);
                            },
                            let_expr.r#else,
                            Op::ExtendsNoPopLeft,
                        );
                    }
                    None => {
                        // A conditional with no condition always succeeds:
                        // `type Check<T> = T extends infer P ? "always here" : "never here"
                        self.compile_expr(let_expr.check);
                        self.cur_fn_mut().locals.push(let_expr.name);
                        self.compile_expr(let_expr.then);
                    }
                }
            }
            Expr::Index(index) => {
                self.compile_expr(index.object_ty);
                match index.object_ty.as_num_lit() {
                    Some(num_lit) => {
                        let constant_idx = self.compile_num_lit(num_lit);
                        self.push_bytes(Op::IndexNumLit as u8, constant_idx as u8);
                    }
                    None => {
                        self.compile_expr(index.index_ty);
                        self.push_op(Op::Index);
                    }
                }
            }
            Expr::Array(array) => self.compile_array(&[array.the_type]),
            Expr::Tuple(tup) => self.compile_array(tup.types.as_slice()),
            ir::Expr::Intersect(intersect) => {
                // If all arguments are object literals we can compile this to one big MakeObj op
                // if intersect
                //     .types
                //     .iter()
                //     .all(|t| matches!(t, Expr::ObjectLit(_)))
                // {
                //     let mut final_values = BTreeMap::<&'alloc str, &'alloc Expr<'alloc>>::new();
                //     for expr in intersect.types.iter() {
                //         match expr {
                //             Expr::ObjectLit(object_lit) => {
                //                 for (key, val) in object_lit.fields.iter() {
                //                     match final_values.entry(key.name()) {
                //                         Entry::Vacant(_) => todo!(),
                //                         Entry::Occupied(mut entry) => todo!(),
                //                     }
                //                 }
                //             }
                //             _ => unreachable!(),
                //         }
                //     }

                //     self.compile_make_obj(
                //         final_values.len() as u8,
                //         final_values.iter().map(|(&k, &v)| (k, v)),
                //     );
                // }
                for ty in intersect.types.iter() {
                    self.compile_expr(ty);
                }
                self.push_bytes(Op::Intersect as u8, intersect.types.len() as u8);
            }
            // TODO: fast path for object lit
            ir::Expr::ObjectLit(obj_lit) => {
                self.compile_make_obj(
                    obj_lit.fields.len() as u8,
                    obj_lit.fields.iter().map(|(k, &v)| (k.name(), v)),
                );
            }
            ir::Expr::Object(obj) => {
                for (k, v) in obj.fields.iter() {
                    self.push_constant(Value::from_obj_ref(ObjRef::alloc_new_str_ref(
                        StringRef::new(k.name()),
                    )));
                    self.compile_expr(v);
                }
                let count = obj.fields.len();
                self.push_bytes(Op::MakeObj as u8, count as u8);
            }
            ir::Expr::Number => {
                self.push_op(Op::Number);
            }
            ir::Expr::String => {
                self.push_op(Op::String);
            }
            ir::Expr::If(if_expr) => self.compile_cond(
                &if_expr.check_type,
                &if_expr.extends_type,
                &if_expr.then,
                &if_expr.r#else,
                Op::Extends,
            ),
            ir::Expr::Identifier(ident) => self.compile_ident(ident),
            ir::Expr::StringLiteral(str_lit) => {
                self.push_constant(Value::from_obj_ref(ObjRef::alloc_new_str_ref(
                    StringRef::new(&str_lit.value),
                )));
            }
            ir::Expr::BooleanLiteral(bool_lit) => {
                self.push_constant(Value::from_bool(bool_lit.value));
            }
            ir::Expr::NumberLiteral(num_lit) => {
                self.compile_num_lit(num_lit);
            }
            ir::Expr::Call(call) => {
                match call.name() {
                    "Add" => {
                        assert_eq!(2, call.args.len());
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        self.push_op(Op::Add);
                        return;
                    }
                    "Sub" => {
                        assert_eq!(2, call.args.len());
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        self.push_op(Op::Sub);
                        return;
                    }
                    "Eq" => {
                        assert_eq!(2, call.args.len());
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        self.push_op(Op::Eq);
                        return;
                    }
                    "Lte" => {
                        assert_eq!(2, call.args.len());
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        self.push_op(Op::Lte);
                        return;
                    }
                    _ => {}
                }

                let count = call.args.len() as u8;
                match call.name() {
                    // TODO: Need to actually properly resolve this to
                    // see if it is imported from the stdlib
                    "Print" => {
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        self.push_bytes(Op::Print as u8, count);
                    }
                    "WriteFile" => {
                        if count != 2 {
                            panic!("BAD COUNT")
                        }
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        self.push_op(Op::WriteFile);
                    }
                    "ToTypescriptSource" => {
                        if count != 2 {
                            panic!("BAD COUNT")
                        }
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        self.push_op(Op::ToTypescriptSource);
                    }
                    _ => {
                        println!("NAME: {:?}", call.name());
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        let name_str: &str = &call.name();
                        if let Some(func_global_constant_idx) = self
                            .functions
                            .get(name_str)
                            .and_then(|func| func.global_constant_idx)
                        {
                            self.push_bytes(
                                if call.tail_call {
                                    Op::TailCall
                                } else {
                                    Op::Call
                                } as u8,
                                count,
                            );
                            self.push_u8(func_global_constant_idx as u8);
                        } else {
                            panic!("Unknown function name! {:?}", name_str)
                        }
                    }
                }
            }
        }
    }

    fn push_op(&mut self, op: Op) {
        self.cur_fn_mut().chunk.push_op(op)
    }

    fn push_u8(&mut self, a: u8) {
        self.cur_fn_mut().chunk.push_u8(a);
    }

    fn push_bytes(&mut self, a: u8, b: u8) {
        self.cur_fn_mut().chunk.push_bytes(a, b);
    }

    fn push_constant_no_op(&mut self, val: Value) -> usize {
        self.cur_fn_mut().chunk.push_constant(val)
    }

    fn push_constant(&mut self, val: Value) -> usize {
        let constant_idx = self.push_constant_no_op(val);
        self.push_bytes(Op::Constant as u8, constant_idx as u8);
        constant_idx
    }

    fn find_ident(&self, name: &str) -> Option<(Op, u8)> {
        if name != GLOBAL_STR {
            if let Some((idx, _)) = self
                .functions
                .get(&self.current_function_name)
                .and_then(|f| f.locals.iter().enumerate().rev().find(|(i, l)| l == &name))
            {
                return Some((Op::GetLocal, idx as u8));
            }
        }

        self.functions
            .get(GLOBAL_STR)
            .unwrap()
            .locals
            .iter()
            .enumerate()
            .rev()
            .find(|(_, l)| l == &name)
            .and_then(|(idx, _)| Some((Op::GetGlobal, idx as u8)))
    }

    pub fn funcs(mut self) -> (Function<'alloc>, BTreeMap<String, Function<'alloc>>) {
        let global = self.functions.remove(GLOBAL_STR).unwrap();
        (global, self.functions)
    }

    fn main_chunk_mut(&mut self) -> &mut Function<'alloc> {
        self.functions.get_mut(GLOBAL_STR).unwrap()
    }

    fn cur_fn_mut(&mut self) -> &mut Function<'alloc> {
        self.functions.get_mut(&self.current_function_name).unwrap()
    }
}

pub struct LocalsIter<'a, 'alloc> {
    locals: &'a Locals<'alloc>,
    i: u16,
}

impl<'a, 'alloc> Iterator for LocalsIter<'a, 'alloc> {
    type Item = &'alloc str;

    fn next(&mut self) -> Option<Self::Item> {
        if self.i >= self.locals.count as u16 {
            return None;
        }
        let ret = Some(self.locals.stack[self.i as usize].name);
        self.i += 1;
        ret
    }
}

impl<'a, 'alloc> DoubleEndedIterator for LocalsIter<'a, 'alloc> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let end_idx_i32 = self.locals.count as i32 - 1 - self.i as i32;
        if end_idx_i32 < 0 {
            return None;
        }
        let ret = Some(self.locals.stack[end_idx_i32 as usize].name);
        self.i += 1;
        ret
    }
}
impl<'a, 'alloc> ExactSizeIterator for LocalsIter<'a, 'alloc> {
    fn len(&self) -> usize {
        self.locals.count as usize
    }
}
