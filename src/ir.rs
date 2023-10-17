use std::collections::BTreeMap;
use std::ops::Deref;

use crate::common::AllocBox as Box;
use crate::common::*;

#[derive(Debug)]
pub struct Program<'ir> {
    pub stmts: AllocVec<'ir, Statement<'ir>>,
}

#[derive(Debug)]
pub enum Statement<'ir> {
    LetDecl(GlobalDecl<'ir>),
    Expr(Expr<'ir>),
}

pub enum LiteralExpr<'ir> {
    String(&'ir StringLiteral<'ir>),
    Boolean(&'ir BooleanLiteral),
    Number(&'ir NumberLiteral),
    Object(&'ir ObjectLit<'ir>),
}

/// TODO: compact repr (box them bad boys son)
#[derive(Debug)]
pub enum Expr<'ir> {
    Any,
    Identifier(Ident<'ir>),
    StringLiteral(&'ir StringLiteral<'ir>),
    BooleanLiteral(&'ir BooleanLiteral),
    NumberLiteral(&'ir NumberLiteral),
    Number,
    Boolean,
    String,
    Object(&'ir Object<'ir>),
    ObjectLit(&'ir ObjectLit<'ir>),
    ObjectKeyword,
    Call(&'ir Call<'ir>),
    If(&'ir If<'ir>),
    Intersect(&'ir Intersect<'ir>),
    Array(&'ir Array<'ir>),
    Tuple(&'ir Tuple<'ir>),
    Index(&'ir Index<'ir>),
    Let(&'ir Let<'ir>),
    Union(&'ir Union<'ir>),
    FormattedString(&'ir FormattedString<'ir>),
    Unary(&'ir Unary<'ir>),
}

impl<'ir> Expr<'ir> {
    pub fn as_num_lit(&self) -> Option<&'ir NumberLiteral> {
        match self {
            Expr::NumberLiteral(n) => Some(n),
            _ => None,
        }
    }

    pub fn as_literal(&self) -> Option<LiteralExpr<'ir>> {
        match self {
            Expr::StringLiteral(a) => Some(LiteralExpr::String(a)),
            Expr::BooleanLiteral(a) => Some(LiteralExpr::Boolean(a)),
            Expr::NumberLiteral(a) => Some(LiteralExpr::Number(a)),
            Expr::ObjectLit(a) => Some(LiteralExpr::Object(a)),
            _ => None,
        }
    }

    /// This function returns true if the expression can be computed at compile time
    pub fn is_comptime_known(&self) -> bool {
        match self {
            Expr::Any => true,
            Expr::Identifier(_) => false,
            Expr::StringLiteral(_) => true,
            Expr::BooleanLiteral(_) => true,
            Expr::NumberLiteral(_) => true,
            Expr::Boolean => true,
            Expr::Number => true,
            Expr::String => true,
            Expr::Object(obj) => obj.can_be_object_lit(),
            Expr::ObjectLit(_) => true,
            Expr::ObjectKeyword => true,
            Expr::Intersect(intersect) => intersect.can_be_object_lit(),
            Expr::Call(_) => false,
            Expr::If(_) => false,
            Expr::Array(arr) => arr.the_type.is_comptime_known(),
            Expr::Tuple(tup) => tup.types.iter().all(|e| e.expr.is_comptime_known()),
            Expr::Index(_) => false,
            Expr::Let(_) => false,
            Expr::Union(_) => false,
            Expr::FormattedString(_) => false,
            Expr::Unary(unary) => unary.expr.is_comptime_known(),
        }
    }
}

#[derive(Debug)]
pub struct StringLiteral<'ir> {
    pub value: &'ir str,
}

#[derive(Debug)]
pub struct BooleanLiteral {
    pub value: bool,
}

#[derive(Debug)]
pub struct NumberLiteral {
    pub value: f64,
}

#[derive(Debug)]
pub struct ObjectLit<'ir> {
    pub fields: BTreeMap<Ident<'ir>, &'ir Expr<'ir>>,
}

#[derive(Debug)]
pub struct Object<'ir> {
    pub fields: BTreeMap<Ident<'ir>, &'ir Expr<'ir>>,
}

impl<'ir> Object<'ir> {
    pub fn can_be_object_lit(&self) -> bool {
        self.fields.values().all(|e| e.is_comptime_known())
    }
    pub fn try_into_object_lit(&self, arena: &'ir Arena) -> Option<ObjectLit<'ir>> {
        if self.can_be_object_lit() {
            Some(ObjectLit {
                fields: BTreeMap::from_iter(self.fields.iter().map(|(k, v)| (*k, *v))),
            })
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct If<'ir> {
    pub check_type: Expr<'ir>,
    pub extends_type: Expr<'ir>,
    pub then: Expr<'ir>,
    pub r#else: Expr<'ir>,
}

#[derive(Debug)]
pub struct Intersect<'ir> {
    pub types: AllocVec<'ir, Expr<'ir>>,
}

impl<'ir> Intersect<'ir> {
    /// TODO: implement this, find good heuristic (arg length, resulting field length)
    pub fn can_be_object_lit(&self) -> bool {
        false
    }
}

#[derive(Debug)]
pub struct Array<'ir> {
    pub the_type: &'ir Expr<'ir>,
}

#[derive(Debug)]
pub struct Tuple<'ir> {
    pub types: AllocVec<'ir, TupleItem<'ir>>,
}

#[derive(Debug)]
pub struct TupleItem<'ir> {
    pub spread: bool,
    pub expr: &'ir Expr<'ir>,
}

#[derive(Debug)]
pub struct Index<'ir> {
    pub object_ty: &'ir Expr<'ir>,
    pub index_ty: &'ir Expr<'ir>,
}

#[derive(Debug)]
pub struct Let<'ir> {
    pub name: &'ir str,
    pub cond: Option<&'ir Expr<'ir>>,
    pub check: &'ir Expr<'ir>,
    pub then: &'ir Expr<'ir>,
    pub r#else: &'ir Expr<'ir>,
}

#[derive(Debug)]
pub struct Union<'ir> {
    /// TODO: Discriminated union
    pub variants: AllocVec<'ir, &'ir Expr<'ir>>,
}

#[derive(Debug)]
pub struct FormattedString<'ir> {
    pub components: AllocVec<'ir, Expr<'ir>>,
}

#[derive(Debug)]
pub enum UnaryOperator {
    UnaryNegation,
}

#[derive(Debug)]
pub struct Unary<'ir> {
    pub op: UnaryOperator,
    pub expr: Expr<'ir>,
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone, Copy)]
#[repr(transparent)]
pub struct Ident<'ir>(pub &'ir str);

impl<'ir> Ident<'ir> {
    // pub fn name(&self) -> &str {
    //     match self.0 {
    //         TSTypeName::IdentifierName(name) => name.name.as_str(),
    //         TSTypeName::QualifiedName(_) => todo!(),
    //     }
    // }

    pub fn name(&self) -> &str {
        self.0
    }

    pub fn from_str(val: &'ir str) -> Self {
        Self(val)
    }
}

#[derive(Debug)]
pub enum GlobalDecl<'ir> {
    Fn(Box<'ir, FnDecl<'ir>>),
    Var(Box<'ir, VarDecl<'ir>>),
}

#[derive(Debug)]
pub struct VarDecl<'ir> {
    pub ident: Ident<'ir>,
    pub expr: Expr<'ir>,
}

#[derive(Debug)]
pub struct FnDecl<'ir> {
    pub ident: Ident<'ir>,
    pub body: Expr<'ir>,
    pub params: AllocVec<'ir, FnParam<'ir>>,
}

#[derive(Debug)]
pub struct FnParam<'ir> {
    pub ident: Ident<'ir>,
    pub default: Option<Expr<'ir>>,
    pub extends_type: Option<Expr<'ir>>,
}

#[derive(Debug)]
pub struct Call<'ir> {
    pub name: &'ir str,
    pub args: AllocVec<'ir, Expr<'ir>>,
    pub tail_call: bool,
}
