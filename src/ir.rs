use std::collections::BTreeMap;

use crate::common::AllocBox as Box;
use crate::common::*;

use oxc_ast::ast;
use oxc_ast::ast::*;

pub struct Transform<'ir> {
    pub arena: &'ir Arena,
}

#[derive(Debug)]
pub struct Program<'ir> {
    pub stmts: AllocVec<'ir, Statement<'ir>>,
}

#[derive(Debug)]
pub enum Statement<'ir> {
    LetDecl(LetDecl<'ir>),
    Expr(Expr<'ir>),
}

/// TODO: compact repr (box them bad boys son)
#[derive(Debug)]
pub enum Expr<'ir> {
    Identifier(Ident<'ir>),
    StringLiteral(&'ir StringLiteral),
    BooleanLiteral(&'ir BooleanLiteral),
    NumberLiteral(&'ir NumberLiteral<'ir>),
    Number,
    String,
    Object(&'ir Object<'ir>),
    ObjectLit(&'ir ObjectLit<'ir>),
    Call(&'ir Call<'ir>),
    If(&'ir If<'ir>),
}

impl<'ir> Expr<'ir> {
    pub fn is_lit(&self) -> bool {
        match self {
            Expr::Identifier(_) => false,
            Expr::StringLiteral(_) => true,
            Expr::BooleanLiteral(_) => true,
            Expr::NumberLiteral(_) => true,
            Expr::Number => true,
            Expr::String => true,
            Expr::Object(obj) => obj.can_be_object_lit(),
            Expr::ObjectLit(lit) => true,
            Expr::Call(_) => false,
            Expr::If(_) => false,
        }
    }
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
        self.fields.values().all(|e| e.is_lit())
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

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone, Copy)]
pub struct Ident<'ir>(pub &'ir str);

impl<'ir> Ident<'ir> {
    // pub fn name(&self) -> &str {
    //     match self.0 {
    //         TSTypeName::IdentifierName(name) => name.name.as_str(),
    //         TSTypeName::QualifiedName(_) => todo!(),
    //     }
    // }

    pub fn from_ts_type_name(tname: &'ir TSTypeName<'ir>) -> Self {
        match tname {
            TSTypeName::IdentifierName(n) => Self(&n.name),
            TSTypeName::QualifiedName(_) => todo!(),
        }
    }
    pub fn name(&self) -> &str {
        self.0
    }

    pub fn from_str(val: &'ir str) -> Self {
        Self(val)
    }

    pub fn from_binding_ident(arena: &'ir Arena, b: &'ir BindingIdentifier) -> Self {
        Self(&b.name)
    }
}

#[derive(Debug)]
pub enum LetDecl<'ir> {
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
    pub name: &'ir TSTypeName<'ir>,
    pub args: AllocVec<'ir, Expr<'ir>>,
    pub tail_call: bool,
}

impl<'ir> Call<'ir> {
    pub fn name(&self) -> &'ir str {
        match self.name {
            TSTypeName::IdentifierName(name) => name.name.as_str(),
            TSTypeName::QualifiedName(_) => todo!(),
        }
    }
}

impl<'ir> Transform<'ir> {
    pub fn transform_oxc(&self, program: &'ir ast::Program<'ir>) -> Program<'ir> {
        let mut stmts = AllocVec::new_in(self.arena);

        for stmt in &program.body {
            if let Some(ir_stmt) = self.transform_stmt(&stmt) {
                stmts.push(ir_stmt)
            }
        }

        Program { stmts }
    }

    fn transform_stmt(&self, stmt: &'ir ast::Statement<'ir>) -> Option<Statement<'ir>> {
        match stmt {
            ast::Statement::Declaration(Declaration::TSTypeAliasDeclaration(let_decl)) => Some(
                Statement::LetDecl(self.transform_type_alias_decl(&let_decl)),
            ),
            ast::Statement::ModuleDeclaration(_) => {
                // todo!()
                println!("FUCKING FIX THIS ZACK!");
                None
            }
            _ => todo!(),
        }
    }

    fn transform_ts_type_param(&self, param: &'ir ast::TSTypeParameter<'ir>) -> FnParam<'ir> {
        let default: Option<Expr<'ir>> = match &param.default {
            Some(val) => Some(self.transform_type(&val, false)),
            None => None,
        };
        let extends_type: Option<Expr<'ir>> = match &param.constraint {
            Some(val) => Some(self.transform_type(&val, false)),
            None => None,
        };
        FnParam {
            ident: Ident::from_binding_ident(self.arena, &param.name),
            default,
            extends_type,
        }
    }

    fn transform_type_alias_decl(
        &self,
        alias_decl: &'ir ast::TSTypeAliasDeclaration<'ir>,
    ) -> LetDecl<'ir> {
        let ident = Ident::from_binding_ident(self.arena, &alias_decl.id);
        if let Some(params) = &alias_decl.type_parameters {
            return LetDecl::Fn(Box(self.arena.alloc(FnDecl {
                ident,
                body: self.transform_type(&alias_decl.type_annotation, false),
                params: AllocVec::from_iter_in(
                    params
                        .params
                        .iter()
                        .map(|p| self.transform_ts_type_param(&p)),
                    self.arena,
                ),
            })));
        }

        LetDecl::Var(Box(self.arena.alloc(VarDecl {
            ident,
            expr: self.transform_type(&alias_decl.type_annotation, false),
        })))
    }

    fn transform_type(&self, ty: &'ir ast::TSType<'ir>, tail_call: bool) -> Expr<'ir> {
        match ty {
            TSType::TSTypeLiteral(lit_type) => {
                let fields = BTreeMap::from_iter(lit_type.members.iter().map(|memb| match memb {
                    TSSignature::TSPropertySignature(prop) => {
                        let static_name: &str = self.arena.alloc(prop.key.static_name().unwrap());
                        let expr: &Expr<'ir> = self.arena.alloc(self.transform_type(
                            &prop.type_annotation.as_ref().unwrap().type_annotation,
                            false,
                        ));
                        (Ident(static_name), expr)
                    }
                    TSSignature::TSIndexSignature(_) => todo!(),
                    TSSignature::TSCallSignatureDeclaration(_) => todo!(),
                    TSSignature::TSConstructSignatureDeclaration(_) => todo!(),
                    TSSignature::TSMethodSignature(_) => todo!(),
                }));
                let object = Object { fields };
                // TODO: Can calcuate if can be object lit while constructing?
                if object.can_be_object_lit() {
                    Expr::ObjectLit(
                        self.arena
                            .alloc(object.try_into_object_lit(self.arena).unwrap()),
                    )
                } else {
                    Expr::Object(self.arena.alloc(object))
                }
            }
            TSType::TSConditionalType(cond_ty) => {
                let if_expr = If {
                    check_type: self.transform_type(&cond_ty.check_type, false),
                    extends_type: self.transform_type(&cond_ty.extends_type, false),
                    then: self.transform_type(&cond_ty.true_type, true),
                    r#else: self.transform_type(&cond_ty.false_type, true),
                };
                Expr::If(self.arena.alloc(if_expr))
            }
            TSType::TSTypeReference(ty_ref) => {
                if let Some(params) = &ty_ref.type_parameters {
                    let args = AllocVec::from_iter_in(
                        params
                            .params
                            .iter()
                            .map(|ty| self.transform_type(ty, false)),
                        self.arena,
                    );
                    return Expr::Call(self.arena.alloc(Call {
                        name: &ty_ref.type_name,
                        args,
                        tail_call,
                    }));
                }
                Expr::Identifier(Ident::from_ts_type_name(&ty_ref.type_name))
            }
            TSType::TSLiteralType(lit_type) => match &lit_type.literal {
                TSLiteral::BooleanLiteral(b) => Expr::BooleanLiteral(b),
                TSLiteral::StringLiteral(s) => Expr::StringLiteral(s),
                TSLiteral::NumberLiteral(n) => Expr::NumberLiteral(n),
                TSLiteral::NullLiteral(_) => todo!(),
                TSLiteral::BigintLiteral(_) => todo!(),
                TSLiteral::RegExpLiteral(_) => todo!(),
                TSLiteral::TemplateLiteral(_) => todo!(),
                TSLiteral::UnaryExpression(_) => todo!(),
            },
            TSType::TSNumberKeyword(_) => Expr::Number,
            TSType::TSStringKeyword(_) => Expr::String,
            TSType::TSAnyKeyword(_) => todo!(),
            TSType::TSBigIntKeyword(_) => todo!(),
            TSType::TSBooleanKeyword(_) => todo!(),
            TSType::TSNeverKeyword(_) => todo!(),
            TSType::TSNullKeyword(_) => todo!(),
            TSType::TSObjectKeyword(_) => todo!(),
            TSType::TSSymbolKeyword(_) => todo!(),
            TSType::TSThisKeyword(_) => todo!(),
            TSType::TSUndefinedKeyword(_) => todo!(),
            TSType::TSUnknownKeyword(_) => todo!(),
            TSType::TSVoidKeyword(_) => todo!(),
            TSType::TSArrayType(_) => todo!(),
            TSType::TSConstructorType(_) => todo!(),
            TSType::TSFunctionType(_) => todo!(),
            TSType::TSImportType(_) => todo!(),
            TSType::TSIndexedAccessType(_) => todo!(),
            TSType::TSInferType(_) => todo!(),
            TSType::TSIntersectionType(_) => todo!(),
            TSType::TSMappedType(_) => todo!(),
            TSType::TSQualifiedName(_) => todo!(),
            TSType::TSTemplateLiteralType(_) => todo!(),
            TSType::TSTupleType(_) => todo!(),
            TSType::TSTypeOperatorType(_) => todo!(),
            TSType::TSTypePredicate(_) => todo!(),
            TSType::TSTypeQuery(_) => todo!(),
            TSType::TSUnionType(_) => todo!(),
            TSType::JSDocNullableType(_) => todo!(),
            TSType::JSDocUnknownType(_) => todo!(),
        }
    }
}