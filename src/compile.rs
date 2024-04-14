use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::mem::size_of;
use std::num::NonZeroUsize;
use std::ops::Range;
use std::thread::panicking;

use crate::common::AllocBox as Box;
use crate::ir::{
    BooleanLiteral, Expr, GlobalDecl, LiteralExpr, NumberLiteral, ObjectLit, TupleItem, Unary,
    UnaryOperator,
};
use crate::op::Op;
use crate::{common::*, ir};

pub const GLOBAL_STR: &'static str = "__tyvm_global";
pub const MAIN_STR: &'static str = "Main";
pub const GLOBAL_STR_TABLE_IDX: ConstantTableIdx = ConstantTableIdx(0);
pub const MAIN_STR_TABLE_IDX: ConstantTableIdx = ConstantTableIdx(2);
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

#[derive(Default, Debug)]
pub struct Compiler<'alloc> {
    /// Indexed by ConstantTableIdx, values themselves represent byte-wise indices into the
    /// constants buffer
    constant_table: Vec<ConstantEntry>,
    /// The constant pool section of the bytecode.
    /// INVARIANTS:
    /// 1. All values in the pool are 8 byte aligned.
    /// 2. All values in the pool are encoded in little endian order
    constants: Vec<u8>,

    globals: BTreeSet<ConstantTableIdx>,
    pub functions: BTreeMap<ConstantTableIdx, Function<'alloc>>,

    current_function_name: ConstantTableIdx,
    interned_strings: BTreeMap<&'alloc str, ConstantTableIdx>,

    is_game: bool,
    initial_state_index: Option<ConstantTableIdx>,
}

#[derive(Debug)]
pub struct Function<'alloc> {
    locals: Locals<'alloc>,
    code: Code,
    name: Option<ConstantTableIdx>,
}

#[repr(C, align(8))]
#[derive(
    Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, bytemuck::Pod, bytemuck::Zeroable,
)]
pub struct ConstantEntry {
    idx: ConstantIdx,
    kind: ConstantKind,
}

#[repr(u32)]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ConstantKind {
    #[default]
    Boolean,
    Number,
    String,
    Bytes,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BytesEncoding {
    Utf8,
}

unsafe impl bytemuck::Pod for ConstantKind {}
unsafe impl bytemuck::Zeroable for ConstantKind {
    fn zeroed() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

#[repr(transparent)]
#[derive(
    Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, bytemuck::Pod, bytemuck::Zeroable,
)]
pub struct ConstantIdx(u32);
#[repr(transparent)]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ConstantTableIdx(u32);

#[derive(Clone, Copy, Debug)]
pub struct Local<'alloc> {
    name: &'alloc str,
}

#[derive(Clone, Debug, Default)]
pub struct Code {
    buf: Vec<u8>,
}

/// Represents the local variables of a function. Local variables in this VM
/// take place as the first values on a function's "stack window".
#[derive(Debug)]
pub struct Locals<'alloc> {
    stack: [Local<'alloc>; u8::MAX as usize],
    count: u8,
}

// #[derive(Clone, Debug, Default)]
// pub enum ConstantKind {}

// Utility
impl<'alloc> Compiler<'alloc> {
    pub fn new() -> Self {
        let mut this = Self {
            ..Default::default()
        };
        this.init();
        this
    }

    fn init(&mut self) {
        let global_str = self.alloc_constant_string(GLOBAL_STR);
        let main_str = self.alloc_constant_string(MAIN_STR);
        assert_eq!(global_str, GLOBAL_STR_TABLE_IDX);
        assert_eq!(main_str, MAIN_STR_TABLE_IDX);
        self.current_function_name = global_str;
        self.functions.insert(
            global_str,
            Function {
                locals: Locals::default(),
                code: Code::default(),
                name: None,
            },
        );
    }

    pub fn serialize(&self, buf: &mut Vec<u8>) {
        serialize::serialize(self, buf);
    }

    fn get_name_constant(&self, string: &str) -> Option<ConstantTableIdx> {
        self.interned_strings.get(string).cloned()
    }

    fn alloc_constant_object(&mut self, string: &ObjectLit<'alloc>) -> ConstantTableIdx {
        todo!()
    }

    fn alloc_constant_bytes(&mut self, bytes: &[u8]) -> ConstantTableIdx {
        let size = bytes.len();

        let len_size = size_of::<u32>();
        let pad = 8 - len_size;

        let (idx, buf, range) = self.alloc_constant_with_len(
            ConstantKind::Bytes,
            (len_size + pad + size).try_into().unwrap(),
        );

        println!("Alloc bytes: {} range={:?}", idx.0, range);
        buf[0..len_size].copy_from_slice(&(size as u32).to_le_bytes());
        buf[(len_size + pad)..].copy_from_slice(bytes);

        return idx;
    }

    /// Allocates the given string in the constant pool. If the string already exists,
    /// it instead returns an index to the existing allocation.
    fn alloc_constant_string(&mut self, string: &'alloc str) -> ConstantTableIdx {
        if let Some(idx) = self.interned_strings.get(string) {
            return *idx;
        }

        let u32_size = size_of::<u32>();

        let (idx, _, range) = self.alloc_constant_with_len(
            ConstantKind::String,
            (size_of::<u32>() + size_of::<u32>()).try_into().unwrap(),
        );

        let bytes_idx = self.alloc_constant_bytes(string.as_bytes());

        let buf = &mut self.constants[range.clone()];

        let len: u32 = string.as_bytes().len().try_into().unwrap();

        buf[0..u32_size].copy_from_slice(&bytes_idx.0.to_le_bytes());
        buf[u32_size..].copy_from_slice(&len.to_le_bytes());

        self.interned_strings.insert(string, idx);

        println!(
            "Alloc string: {}, idx={}, len={}, range={:?}, byte_idx={}",
            string, idx.0, len, range, bytes_idx.0
        );

        idx
    }

    fn alloc_constant_num(&mut self, num: f64) -> ConstantTableIdx {
        let size = size_of::<f64>();

        let (idx, buf, _) =
            self.alloc_constant_with_len(ConstantKind::Number, size.try_into().unwrap());

        buf[0..size].copy_from_slice(&num.to_le_bytes());

        idx
    }

    fn alloc_constant_bool(&mut self, b: bool) -> ConstantTableIdx {
        let size = 1;
        let (idx, buf, _) = self.alloc_constant_with_len(ConstantKind::Boolean, 1);
        buf[0] = b as u8;
        idx
    }

    /// Reserves data in the constant pool with length of `data_len` and returns
    /// the index and the reserved buffer of data. This function makes sure the
    /// index is aligned to 8 bytes.
    pub fn alloc_constant_with_len(
        &mut self,
        kind: ConstantKind,
        data_len: u32,
    ) -> (ConstantTableIdx, &mut [u8], Range<usize>) {
        // Calculate the required padding to achieve 8-byte alignment
        let padding = (8 - (self.constants.len() % 8)) % 8;

        // Extend the constants with padding bytes
        self.constants.extend(std::iter::repeat(0).take(padding));

        // Store the starting index of the data in the constants vector
        let idx = self.new_constant_idx();
        let table_idx = self.add_constant_table_entry(idx, kind);

        // Extend the constants with data_len
        self.constants
            .extend(std::iter::repeat(0).take(data_len as usize));

        let start = idx.0 as usize;
        let end = self.constants.len();

        // Return the starting index of the allocated data
        (table_idx, &mut self.constants[start..end], start..end)
    }

    fn add_constant_table_entry(
        &mut self,
        idx: ConstantIdx,
        kind: ConstantKind,
    ) -> ConstantTableIdx {
        let table_idx = self.constant_table.len();
        self.constant_table.push(ConstantEntry { idx, kind });
        ConstantTableIdx(table_idx.try_into().unwrap())
    }

    fn new_constant_idx(&self) -> ConstantIdx {
        let idx = self.constants.len();
        ConstantIdx(idx.try_into().expect("Constant pool has exceeded ~4gb"))
    }

    fn push_call_native(&mut self, fn_name: ConstantTableIdx, arg_count: u8) {
        self.cur_fn_mut()
            .code
            .push_bytes(Op::CallNative as u8, arg_count);
        self.cur_fn_mut().code.push_constant(fn_name);
    }

    fn push_op(&mut self, op: Op) {
        self.cur_fn_mut().code.push_op(op)
    }

    fn push_constant(&mut self, constant: ConstantTableIdx) {
        self.cur_fn_mut().code.push_constant(constant);
    }

    fn push_u32(&mut self, val: u32) {
        self.cur_fn_mut().code.push_u32(val)
    }

    fn push_op_with_constant(&mut self, op: Op, constant: ConstantTableIdx) {
        self.cur_fn_mut().code.push_op_with_constant(op, constant)
    }

    fn push_u8(&mut self, a: u8) {
        self.cur_fn_mut().code.push_u8(a);
    }

    fn push_u128(&mut self, a: u128) {
        self.cur_fn_mut().code.push_u128(a);
    }

    fn push_bytes(&mut self, a: u8, b: u8) {
        self.cur_fn_mut().code.push_bytes(a, b);
    }

    fn main_chunk_mut(&mut self) -> &mut Function<'alloc> {
        self.functions.get_mut(&GLOBAL_STR_TABLE_IDX).unwrap()
    }

    fn cur_fn_mut(&mut self) -> &mut Function<'alloc> {
        self.functions.get_mut(&self.current_function_name).unwrap()
    }
}

// Compilation logic
impl<'alloc> Compiler<'alloc> {
    pub fn compile(&mut self, program: &'alloc ir::Program<'alloc>) {
        // Collect globals first
        {
            for stmt in &program.stmts {
                match stmt {
                    ir::Statement::LetDecl(GlobalDecl::Fn(fn_decl)) => {
                        let name_str = fn_decl.ident.name();
                        let name = self.alloc_constant_string(name_str);
                        println!("Compiled function: {:?} {:?}", name_str, name);
                        self.functions.insert(
                            name,
                            Function {
                                locals: Locals::default(),
                                code: Code::default(),
                                name: Some(name),
                            },
                        );
                    }
                    ir::Statement::LetDecl(GlobalDecl::Var(var_decl)) => {
                        let name = var_decl.ident.name();
                        let name = self.alloc_constant_string(name);
                        self.globals.insert(name);
                    }
                    _ => (),
                }
            }
        }

        for stmt in &program.stmts {
            self.compile_statement(stmt)
        }

        if let Some(main_fn_name) = self.get_name_constant("Main") {
            self.push_op_with_constant(Op::CallMain, main_fn_name);

            if self.is_game && self.initial_state_index.is_none() {
                panic!(
                    "`InitialStateIndex` must be exported if RequestAnimationFrame<...> is called"
                )
            }
        }

        self.push_op(Op::Exit);
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

    fn compile_var_decl(&mut self, var_decl: &'alloc ir::VarDecl<'alloc>) {
        self.compile_expr(&var_decl.expr);
        let name = var_decl.ident.name();
        let name = self.alloc_constant_string(name);

        if var_decl.ident.name() == "InitialState" {
            self.initial_state_index = Some(name);
            self.main_chunk_mut()
                .code
                .push_op_with_constant(Op::SetInitialState, name);
            return;
        }

        self.main_chunk_mut()
            .code
            .push_op_with_constant(Op::SetGlobal, name);
    }

    fn compile_fn_decl(&mut self, fn_decl: &'alloc ir::FnDecl<'alloc>) {
        let name = fn_decl.ident.name();
        let name_constant = self.alloc_constant_string(name);
        let prev_name = std::mem::replace(&mut self.current_function_name, name_constant);
        let func = self.functions.get_mut(&name_constant).unwrap();
        for arg in &fn_decl.params {
            func.locals.push(arg.ident.name());
        }
        fn_decl
            .params
            .iter()
            .enumerate()
            .for_each(|(idx, param)| self.compile_fn_param_check(param, idx as u8));
        self.compile_expr(&fn_decl.body);

        // if name_constant == MAIN_STR_TABLE_IDX && self.is_game {
        //     let initial_state_constant = self
        //         .initial_state_index
        //         .expect("Game requires InitialStateIndex to be exported");
        //     self.push_op_with_constant(Op::SetGlobal, initial_state_constant);
        // }

        self.push_op(Op::PopCallFrame);

        self.current_function_name = prev_name;
    }

    fn compile_fn_param_check(&mut self, param: &ir::FnParam<'alloc>, local_idx: u8) {
        if let Some(extends_ty) = &param.extends_type {
            self.push_bytes(Op::GetLocal as u8, local_idx);
            println!("EXTENDS TY: {:#?}", extends_ty);
            self.compile_expr(extends_ty);
            self.push_op(Op::PanicExtends);
        }
        if param.default.is_some() {
            todo!("Default parameters");
        }
    }

    fn compile_expr(&mut self, expr: &ir::Expr<'alloc>) {
        match expr {
            Expr::ObjectKeyword => self.push_op(Op::Object),
            Expr::Boolean => self.push_op(Op::Boolean),
            Expr::Unary(Unary {
                op: UnaryOperator::UnaryNegation,
                expr,
            }) => {
                self.compile_expr(expr);
                self.push_op(Op::Negate)
            }
            Expr::Unary(_) => todo!(),
            Expr::Any => {
                self.push_op(Op::Any);
            }
            Expr::FormattedString(formatted_string) => {
                let count: u8 = formatted_string.components.len().try_into().unwrap();
                formatted_string
                    .components
                    .iter()
                    .for_each(|v| self.compile_expr(v));
                self.push_bytes(Op::FormatString as u8, count)
            }
            Expr::Union(union) => {
                let len: u8 = union.variants.len().try_into().unwrap();
                assert!(len >= 2);
                union.variants.iter().for_each(|v| self.compile_expr(v));
                self.push_bytes(Op::Union as u8, len);
            }
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
                            //
                            // This works because an invariant of this stack VM
                            // is all expressions will end with the stack
                            // unchanged (expressions will temporarily push
                            // values onto the stack, but at the end of an
                            // expression, the stack will be the same as it was
                            // before the expression was executed). This means,
                            // we can rely on this newly bound variable's index
                            // to be stable for the duration of this function.
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
                match index.index_ty.as_literal() {
                    Some(lit) => {
                        let constant_idx = self.alloc_lit(&lit);
                        self.push_op_with_constant(Op::IndexLit, constant_idx);
                    }
                    None => {
                        self.compile_expr(index.index_ty);
                        self.push_op(Op::Index);
                    }
                }
            }
            Expr::Array(array) => self.compile_array(array.the_type, expr.is_comptime_known()),
            Expr::Tuple(tup) => self.compile_tuple(tup.types.as_slice(), expr.is_comptime_known()),
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
            // TODO: constants for objectlit
            ir::Expr::ObjectLit(obj_lit) => {
                for (k, v) in obj_lit.fields.iter() {
                    let field_constant_idx = self.alloc_constant_string(k.name());
                    self.push_op_with_constant(Op::Constant, field_constant_idx);
                    self.compile_expr(v);
                }
                let count: u8 = obj_lit.fields.len().try_into().unwrap();
                self.push_bytes(Op::MakeObj as u8, count as u8);
            }
            ir::Expr::Object(obj) => {
                for (k, v) in obj.fields.iter() {
                    let field_constant_idx = self.alloc_constant_string(k.name());
                    self.push_op_with_constant(Op::Constant, field_constant_idx);
                    self.compile_expr(v);
                }
                let count: u8 = obj.fields.len().try_into().unwrap();
                self.push_bytes(Op::MakeObj as u8, count as u8);
            }
            ir::Expr::Number => {
                self.push_op(Op::Number);
            }
            ir::Expr::String => {
                self.push_op(Op::String);
            }
            ir::Expr::If(if_expr) => {
                // Specialization: T extends true
                if let Some(LiteralExpr::Boolean(BooleanLiteral { value: true })) =
                    if_expr.extends_type.as_literal()
                {
                    self.compile_cond(
                        &if_expr.check_type,
                        (),
                        &if_expr.then,
                        &if_expr.r#else,
                        Op::ExtendsTrue,
                    );
                    return;
                }

                self.compile_cond(
                    &if_expr.check_type,
                    &if_expr.extends_type,
                    &if_expr.then,
                    &if_expr.r#else,
                    Op::Extends,
                );
            }
            ir::Expr::Identifier(ident) => self.compile_ident(ident),
            ir::Expr::StringLiteral(str_lit) => {
                let constant_idx = self.alloc_constant_string(&str_lit.value);
                self.push_op_with_constant(Op::Constant, constant_idx);
            }
            ir::Expr::BooleanLiteral(bool_lit) => {
                let constant_idx = self.alloc_constant_bool(bool_lit.value);
                self.push_op_with_constant(Op::Constant, constant_idx);
            }
            ir::Expr::NumberLiteral(num_lit) => {
                self.compile_num_lit(num_lit);
            }
            ir::Expr::Call(call) => {
                let count: u8 = call.args.len().try_into().unwrap();
                match call.name {
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
                    "Mul" => {
                        assert_eq!(2, call.args.len());
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        self.push_op(Op::Mul);
                        return;
                    }
                    "Div" => {
                        assert_eq!(2, call.args.len());
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        self.push_op(Op::Div);
                        return;
                    }
                    "Floor" => {
                        assert_eq!(1, call.args.len());
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        self.push_op(Op::Floor);
                        return;
                    }
                    "Mod" => {
                        assert_eq!(2, call.args.len());
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        self.push_op(Op::Mod);
                        return;
                    }
                    "Eq" => {
                        assert_eq!(2, call.args.len());
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        self.push_op(Op::Eq);
                        return;
                    }
                    "Lt" => {
                        assert_eq!(2, call.args.len());
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        self.push_op(Op::Lt);
                        return;
                    }
                    "Lte" => {
                        assert_eq!(2, call.args.len());
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        self.push_op(Op::Lte);
                        return;
                    }
                    "Gte" => {
                        assert_eq!(2, call.args.len());
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        self.push_op(Op::Gte);
                        return;
                    }
                    "And" => {
                        assert_eq!(2, call.args.len());
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        self.push_op(Op::And);
                        return;
                    }
                    "Or" => {
                        assert_eq!(2, call.args.len());
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        self.push_op(Op::Or);
                        return;
                    }
                    "Update" => {
                        assert_eq!(2, call.args.len());
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        self.push_op(Op::Update);
                        return;
                    }
                    _ => {}
                }

                match call.name {
                    // TODO: Need to actually properly resolve these to
                    // see if it is imported from the stdlib
                    "AssertEq" => {
                        assert_eq!(2, count);
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        let name = self.alloc_constant_string("AssertEq");
                        self.push_call_native(name, count);
                    }
                    "Print" => {
                        assert_eq!(count, 1);
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        let name = self.alloc_constant_string("Print");
                        self.push_call_native(name, count);
                    }
                    "WriteFile" => {
                        assert_eq!(count, 2);
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        let name = self.alloc_constant_string("WriteFile");
                        self.push_call_native(name, count);
                    }
                    "ToTypescriptSource" => {
                        assert_eq!(count, 2);
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        let name = self.alloc_constant_string("ToTypescriptSource");
                        self.push_call_native(name, count);
                    }
                    "ParseInt" => {
                        assert_eq!(count, 1);
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        let name = self.alloc_constant_string("ParseInt");
                        self.push_call_native(name, count);
                    }
                    "Panic" => {
                        assert_eq!(count, 1);
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        let name = self.alloc_constant_string("Panic");
                        self.push_call_native(name, count);
                    }
                    "RequestAnimFrame" => {
                        assert_eq!(count, 1);
                        if self.current_function_name != MAIN_STR_TABLE_IDX {
                            panic!("RequestAnimFrame can only be called in Main<...>");
                        }

                        if self.is_game == true {
                            panic!("RequestAnimFrame already called.");
                        }

                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        let name = self.alloc_constant_string("RequestAnimFrame");
                        self.push_call_native(name, count);
                        self.is_game = true;
                    }
                    "Rand" => {
                        assert_eq!(count, 2);
                        call.args.iter().for_each(|arg| self.compile_expr(arg));
                        let name = self.alloc_constant_string("Rand");
                        self.push_call_native(name, count);
                    }
                    _ => {
                        println!("NAME: {:?}", call.name);
                        let name_constant = self.alloc_constant_string(call.name);
                        // call.args.iter().for_each(|arg| self.compile_expr(arg));
                        for arg in call.args.iter() {
                            self.compile_expr(arg);
                        }
                        let name_str: &str = &call.name;
                        if !self.functions.contains_key(&name_constant) {
                            panic!("Unknown function name! {:?}", name_str)
                        }

                        self.push_bytes(
                            if call.tail_call {
                                Op::TailCall as u8
                            } else {
                                Op::Call as u8
                            },
                            count,
                        );
                        self.push_constant(name_constant);
                    }
                }
            }
        }
    }

    fn alloc_lit(&mut self, lit: &ir::LiteralExpr<'alloc>) -> ConstantTableIdx {
        match lit {
            ir::LiteralExpr::String(str) => self.alloc_constant_string(str.value),
            ir::LiteralExpr::Boolean(boolean) => self.alloc_constant_bool(boolean.value),
            ir::LiteralExpr::Number(num) => self.alloc_constant_num(num.value),
            ir::LiteralExpr::Object(obj) => self.alloc_constant_object(obj),
        }
    }

    fn compile_num_lit(&mut self, num_lit: &NumberLiteral) -> ConstantTableIdx {
        let idx = self.alloc_constant_num(num_lit.value);
        self.push_op_with_constant(Op::Constant, idx);
        idx
    }

    fn compile_array(&mut self, item: &Expr<'alloc>, is_comptime_known: bool) {
        // TODO: constant tuples
        if is_comptime_known {}

        self.compile_expr(item);
        self.push_op(Op::MakeArray);
    }

    fn compile_tuple(&mut self, types: &[TupleItem<'alloc>], is_comptime_known: bool) {
        if types.is_empty() {
            self.push_op(Op::EmptyTuple);
            return;
        }

        // TODO: constant tuples
        if is_comptime_known {}

        let spread_count: u32 = types
            .iter()
            .filter(|t| t.spread)
            .count()
            .try_into()
            .unwrap();

        let count: u8 = types.len().try_into().unwrap();

        types.iter().for_each(|t| self.compile_expr(t.expr));

        if spread_count == 0 {
            self.push_op(Op::MakeTuple);
            self.push_u8(count);
            return;
        }

        let mut bitfield1: u128 = 0;
        let mut bitfield2: u128 = 0;

        self.push_op(Op::MakeTupleSpread);
        self.push_u8(count);
        for (i, item) in types.iter().enumerate() {
            if !item.spread {
                continue;
            }

            if i >= 128 {
                bitfield2 |= 1 << (i - 128);
                continue;
            }

            bitfield1 |= 1 << i;
        }
        self.push_u128(bitfield1);
        self.push_u128(bitfield2);
    }

    fn compile_ident(&mut self, ident: &ir::Ident<'alloc>) {
        let name_str: &str = ident.name();
        println!("FINDING IDENT: {}", ident.name());
        if let Some((op, idx)) = self.find_ident(name_str) {
            println!("It's a local: {:?} {:?}", op, idx);
            self.push_op(op);
            self.push_u8(idx);
            return;
        }
        if let Some(constant_idx) = self.interned_strings.get(name_str) {
            println!("It's a global");
            if self.globals.contains(constant_idx) {
                self.push_op_with_constant(Op::GetGlobal, *constant_idx);
                return;
            }
        }
        panic!("Unknown ident: {:?}", name_str)
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
            .get(&GLOBAL_STR_TABLE_IDX)
            .unwrap()
            .locals
            .iter()
            .enumerate()
            .rev()
            .find(|(_, l)| l == &name)
            .and_then(|(idx, _)| Some((Op::GetGlobal, idx as u8)))
    }

    fn compile_cond(
        &mut self,
        check_ty: impl Compile<'alloc>,
        extends_ty: impl Compile<'alloc>,
        then: impl Compile<'alloc>,
        r#else: &ir::Expr<'alloc>,
        extends_op: Op,
    ) {
        // self.compile_expr(check_ty);
        check_ty.compile(self);
        extends_ty.compile(self);
        self.push_op(extends_op);

        let jump_to_else_branch_if_false_instr_idx = self.cur_fn_mut().code.buf.len();
        self.push_bytes(0, 0);

        // self.compile_expr(then);
        then.compile(self);

        self.push_op(Op::Jump);
        let skip_else_branch_idx = self.cur_fn_mut().code.buf.len();
        self.push_bytes(0, 0);

        let else_starting_idx = self.cur_fn_mut().code.buf.len();
        self.patch_jump(
            else_starting_idx as u16,
            jump_to_else_branch_if_false_instr_idx,
        );
        self.compile_expr(r#else);
        let end_of_else_idx = self.cur_fn_mut().code.buf.len();
        self.patch_jump(end_of_else_idx as u16, skip_else_branch_idx);
    }

    fn patch_jump(&mut self, instr_idx: u16, patch_idx: usize) {
        let part1 = (instr_idx & 0b11111111) as u8;
        let part2 = ((instr_idx & 0b1111111100000000) >> 8) as u8;

        let code = &mut self.cur_fn_mut().code.buf;
        println!("PATCH JUMP: {:?}", patch_idx);
        code[patch_idx] = part1;
        code[patch_idx + 1] = part2;
    }
}

impl Code {
    pub fn push_constant(&mut self, constant: ConstantTableIdx) {
        self.push_u32(constant.0);
    }

    pub fn push_op_with_constant(&mut self, op: Op, constant: ConstantTableIdx) {
        self.push_op(op);
        self.push_constant(constant);
    }

    pub fn push_op(&mut self, op: Op) {
        self.push_u8(op as u8)
    }

    pub fn push_u8(&mut self, val: u8) {
        self.buf.push(val)
    }

    pub fn push_u32(&mut self, val: u32) {
        self.buf.extend(val.to_le_bytes())
    }

    pub fn push_u128(&mut self, val: u128) {
        self.buf.extend(val.to_le_bytes())
    }

    pub fn push_bytes(&mut self, a: u8, b: u8) {
        self.push_u8(a);
        self.push_u8(b);
    }

    // pub fn debug_code(&self, globals: &Function) {
    //     let mut i: usize = 0;
    //     if self.code.is_empty() {
    //         return;
    //     }

    //     while i < self.code.len() {
    //         let op: Op = self.code[i].into();
    //         i += 1;
    //         match op {
    //             Op::CallMain => {
    //                 let idx = self.code[i];
    //                 i += 1;
    //                 println!("{} CallMain {:?}", i, self.constants[idx as usize])
    //             }
    //             Op::ToTypescriptSource => {
    //                 println!("{} ToTypescriptSource", i)
    //             }
    //             Op::WriteFile => {
    //                 println!("{} WriteFile", i)
    //             }
    //             Op::Lte => {
    //                 println!("{} LTE", i);
    //             }
    //             Op::Eq => {
    //                 println!("{} EQ", i);
    //             }
    //             Op::Number => {
    //                 println!("{} Number", i);
    //             }
    //             Op::String => {
    //                 println!("{} String", i);
    //             }
    //             Op::Add => {
    //                 println!("{} ADD", i);
    //             }
    //             Op::Sub => {
    //                 println!("{} SUB", i);
    //             }
    //             Op::Intersect => {
    //                 let count = self.code[i];
    //                 i += 1;
    //                 println!("{} INTERSECT: {}", i, count)
    //             }
    //             Op::Union => todo!(),
    //             Op::Print => {
    //                 let count = self.code[i];
    //                 i += 1;
    //                 println!("{} Print: {}", i, count)
    //             }
    //             Op::Constant => {
    //                 let idx = self.code[i];
    //                 i += 1;
    //                 println!("{} CONST: {:?}", i, self.constants[idx as usize]);
    //             }
    //             Op::Pop => {
    //                 println!("{} POP", i);
    //             }
    //             Op::TailCall | Op::Call => {
    //                 let count = self.code[i];
    //                 i += 1;
    //                 let name_idx = self.code[i];
    //                 i += 1;
    //                 println!(
    //                     "{} {:?} {:?} {:?}",
    //                     i, op, count, globals.chunk.constants[name_idx as usize]
    //                 )
    //             }
    //             Op::SetLocal => {
    //                 let idx = self.code[i];
    //                 i += 1;
    //                 println!("{} Set local {:?}", i, idx);
    //             }
    //             Op::GetLocal => {
    //                 let idx = self.code[i];
    //                 i += 1;
    //                 println!("{} Get local {:?}", i, idx);
    //             }
    //             Op::SetGlobal => {
    //                 let idx = self.code[i];
    //                 i += 1;
    //                 println!("{} SET GLOBAL {:?}", i, self.constants[idx as usize]);
    //             }
    //             Op::GetGlobal => {
    //                 let idx = self.code[i];
    //                 i += 1;
    //                 println!(
    //                     "{} GET GLOBAL {:?}",
    //                     i, globals.chunk.constants[idx as usize]
    //                 );
    //             }
    //             Op::ExtendsNoPopLeft | Op::PanicExtends | Op::Extends => {
    //                 let skip_then = ((self.code[i] as u16) << 8) | (self.code[i + 1] as u16);
    //                 i += 2;
    //                 println!("{} {:?} (skip_then={})", i, op, skip_then)
    //             }
    //             Op::Jump => {
    //                 let offset = ((self.code[i] as u16) << 8) | (self.code[i + 1] as u16);
    //                 i += 2;
    //                 println!("{} JUMP {}", i, offset)
    //             }
    //             Op::PopCallFrame => {
    //                 println!("{} POP CALL FRAME", i);
    //             }
    //             Op::MakeObj => {
    //                 let count = self.code[i];
    //                 i += 1;
    //                 println!("{} Make obj {:?}", i, count);
    //             }
    //             Op::MakeArray => {
    //                 let count = self.code[i];
    //                 i += 1;
    //                 println!("{} MakeArray {:?}", i, count);
    //             }
    //             Op::Index => {
    //                 println!("{} Index", i);
    //             }
    //             Op::IndexNumLit => {
    //                 let count = self.code[i];
    //                 i += 1;
    //                 println!("{} IndexNumLit {:?}", i, count);
    //             }
    //         }
    //     }
    // }
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

pub mod serialize {
    use bytemuck::Zeroable;

    use super::*;

    const MAGIC_VALUE: u32 = 69420;

    #[repr(C, align(8))]
    #[derive(bytemuck::Pod, bytemuck::Zeroable, Clone, Copy)]
    struct Header {
        magic: u32,
        constants_offset: u32,
        functions_offset: u32,
        is_game: u32,
    }

    #[repr(C, align(8))]
    #[derive(bytemuck::Pod, bytemuck::Zeroable, Clone, Copy)]
    struct ConstantsHeader {
        table_len: u32,
        pool_size: u32,
        pool_padding: u32,
        _pad: u32,
    }

    #[repr(C, align(8))]
    #[derive(bytemuck::Pod, bytemuck::Zeroable, Clone, Copy)]
    struct FunctionsHeader {
        table_len: u32,
        table_offset: u32,
        function_code_size: u32,
        function_code_padding: u32,
    }

    #[repr(C, align(8))]
    #[derive(bytemuck::Pod, bytemuck::Zeroable, Clone, Copy)]
    struct FunctionTableEntry {
        name_constant_idx: u32,
        offset: u32,
        size: u32,
        _pad: u32,
    }

    fn pad_8(length: usize) -> (usize, usize) {
        let new_length = (length + 7) & !7;
        (new_length, new_length - length)
    }

    pub fn serialize<'alloc>(compiler: &Compiler<'alloc>, buf: &mut Vec<u8>) {
        // Make sure all serializable structs have size that is aligned to 8 bytes
        assert_eq!(size_of::<Header>() % 8, 0);
        assert_eq!(size_of::<ConstantsHeader>() % 8, 0);
        assert_eq!(size_of::<FunctionsHeader>() % 8, 0);
        assert_eq!(size_of::<FunctionTableEntry>() % 8, 0);
        assert_eq!(size_of::<ConstantEntry>() % 8, 0);

        let mut header = Header {
            magic: MAGIC_VALUE,
            constants_offset: size_of::<Header>() as u32,
            // patched later
            functions_offset: 0,
            is_game: compiler.is_game as u32,
        };
        buf.extend_from_slice(bytemuck::cast_slice(&[header]));

        // let (constants_table_len, constants_table_pad) =
        //     pad_8(compiler.constant_table.len() * size_of::<ConstantEntry>());

        let (pool_size, pool_padding) = pad_8(compiler.constants.len());

        let constants_header = ConstantsHeader {
            table_len: (compiler.constant_table.len() * size_of::<ConstantEntry>())
                .try_into()
                .unwrap(),
            pool_size: pool_size.try_into().unwrap(),
            pool_padding: pool_padding.try_into().unwrap(),
            _pad: 0,
        };
        buf.extend_from_slice(bytemuck::cast_slice(&[constants_header]));
        // Add constants + padding
        buf.extend_from_slice(bytemuck::cast_slice(compiler.constant_table.as_slice()));

        buf.extend_from_slice(compiler.constants.as_slice());
        buf.extend(std::iter::repeat(0).take(pool_padding as usize));

        let functions_offset: u32 = buf.len().try_into().unwrap();
        header.functions_offset = functions_offset;

        let (functions_code_size, functions_code_padding) =
            pad_8(size_of::<FunctionTableEntry>() * compiler.functions.len());

        let function_header = FunctionsHeader {
            table_len: compiler.functions.len().try_into().unwrap(),
            table_offset: ((functions_offset as usize) + size_of::<FunctionsHeader>())
                .try_into()
                .unwrap(),
            function_code_size: functions_code_size.try_into().unwrap(),
            function_code_padding: functions_code_padding.try_into().unwrap(),
        };
        buf.extend_from_slice(bytemuck::cast_slice(&[function_header]));
        let mut offset = 0;
        for (&name, func) in compiler.functions.iter() {
            let entry = FunctionTableEntry {
                name_constant_idx: name.0,
                offset,
                size: func.code.buf.len().try_into().unwrap(),
                _pad: 0,
            };
            buf.extend_from_slice(bytemuck::cast_slice(&[entry]));
            let (padded_size, _) = pad_8(entry.size as usize);
            let padded_size: u32 = padded_size.try_into().unwrap();
            offset += padded_size;
        }
        for (_, func) in compiler.functions.iter() {
            let (_, pad) = pad_8(func.code.buf.len());
            buf.extend_from_slice(&func.code.buf);
            buf.extend(std::iter::repeat(0).take(pad));
        }

        let header_region = &mut buf.as_mut_slice()[0..size_of::<Header>()];
        header_region.copy_from_slice(bytemuck::cast_slice(&[header]));
    }
}
