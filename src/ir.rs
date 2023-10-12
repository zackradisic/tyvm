use std::collections::BTreeMap;
use std::ops::Deref;

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
    LetDecl(GlobalDecl<'ir>),
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
    Intersect(&'ir Intersect<'ir>),
    Array(&'ir Array<'ir>),
    Tuple(&'ir Tuple<'ir>),
    Index(&'ir Index<'ir>),
    Let(&'ir Let<'ir>),
    Union(&'ir Union<'ir>),
    FormattedString(&'ir FormattedString<'ir>),
}

impl<'ir> Expr<'ir> {
    pub fn as_num_lit(&self) -> Option<&'ir NumberLiteral<'ir>> {
        match self {
            Expr::NumberLiteral(n) => Some(n),
            _ => None,
        }
    }
    pub fn is_lit(&self) -> bool {
        match self {
            Expr::Identifier(_) => false,
            Expr::StringLiteral(_) => true,
            Expr::BooleanLiteral(_) => true,
            Expr::NumberLiteral(_) => true,
            Expr::Number => true,
            Expr::String => true,
            Expr::Object(obj) => obj.can_be_object_lit(),
            Expr::Intersect(intersect) => intersect.can_be_object_lit(),
            Expr::ObjectLit(lit) => true,
            Expr::Call(_) => false,
            Expr::If(_) => false,
            Expr::Array(arr) => arr.the_type.is_lit(),
            Expr::Tuple(tup) => tup.types.iter().all(|t| t.is_lit()),
            Expr::Index(_) => false,
            Expr::Let(_) => false,
            Expr::Union(_) => false,
            Expr::FormattedString(_) => false,
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
    pub types: AllocVec<'ir, &'ir Expr<'ir>>,
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
    pub variants: AllocVec<'ir, &'ir Expr<'ir>>,
}

#[derive(Debug)]
pub struct FormattedString<'ir> {
    pub components: AllocVec<'ir, Expr<'ir>>,
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

    pub fn from_ts_type_name(tname: &'ir TSTypeName<'ir>) -> Self {
        match tname {
            TSTypeName::IdentifierReference(refer) => Self(&refer.name),
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
    pub name: &'ir TSTypeName<'ir>,
    pub args: AllocVec<'ir, Expr<'ir>>,
    pub tail_call: bool,
}

impl<'ir> Call<'ir> {
    pub fn name(&self) -> &'ir str {
        match self.name {
            TSTypeName::IdentifierReference(refer) => refer.name.as_str(),
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
            ast::Statement::Declaration(decl) => self.transform_decl(decl),
            ast::Statement::ModuleDeclaration(module_decl) => match module_decl.deref() {
                ModuleDeclaration::ExportNamedDeclaration(export_decl) => export_decl
                    .declaration
                    .as_ref()
                    .and_then(|decl| self.transform_decl(decl)),
                _ => {
                    // todo!()
                    println!("FUCKING FIX THIS ZACK!: {:#?}", module_decl);
                    None
                }
            },
            ty => todo!("Unimplemented: {:?}", ty),
        }
    }

    fn transform_decl(&self, decl: &'ir ast::Declaration<'ir>) -> Option<Statement<'ir>> {
        match decl {
            Declaration::TSTypeAliasDeclaration(let_decl) => Some(Statement::LetDecl(
                self.transform_type_alias_decl(&let_decl),
            )),
            _ => {
                // todo!()
                println!("FUCKING FIX THIS ZACK!: {:#?}", decl);
                None
            }
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
    ) -> GlobalDecl<'ir> {
        let ident = Ident::from_binding_ident(self.arena, &alias_decl.id);
        if let Some(params) = &alias_decl.type_parameters {
            return GlobalDecl::Fn(Box(self.arena.alloc(FnDecl {
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

        GlobalDecl::Var(Box(self.arena.alloc(VarDecl {
            ident,
            expr: self.transform_type(&alias_decl.type_annotation, false),
        })))
    }

    fn transform_type(&self, ty: &'ir ast::TSType<'ir>, tail_call: bool) -> Expr<'ir> {
        match ty {
            TSType::TSTemplateLiteralType(template) => {
                let mut components = AllocVec::<Expr<'ir>>::new_in(self.arena);

                let total_count = template.types.len() + template.quasis.len();
                let mut i = 0;
                let mut j = 0;
                for k in 0..total_count {
                    let ty = if k % 2 == 0 {
                        let value = template.quasis[i]
                            .value
                            .cooked
                            .as_ref()
                            .unwrap_or(&template.quasis[i].value.raw);

                        if value.as_str() == "" {
                            i += 1;
                            continue;
                        }

                        let val = Expr::StringLiteral(self.arena.alloc(StringLiteral {
                            span: template.quasis[i].span,
                            value: value.clone(),
                        }));

                        i += 1;

                        val
                    } else {
                        let val = self.transform_type(&template.types[j], false);
                        j += 1;
                        val
                    };
                    components.push(ty);
                }

                Expr::FormattedString(self.arena.alloc(FormattedString { components }))
            }
            TSType::TSUnionType(union) => {
                todo!()
            }
            TSType::TSIndexedAccessType(index) => Expr::Index(
                self.arena.alloc(Index {
                    object_ty: self
                        .arena
                        .alloc(self.transform_type(&index.object_type, false)),
                    index_ty: self
                        .arena
                        .alloc(self.transform_type(&index.index_type, false)),
                }),
            ),
            TSType::TSTupleType(tuple) => {
                let elements: AllocVec<'ir, &'ir Expr<'ir>> = AllocVec::from_iter_in(
                    tuple
                        .element_types
                        .iter()
                        .map(|ty| match ty {
                            TSTupleElement::TSType(ty) => {
                                self.arena.alloc(self.transform_type(ty, false))
                            }
                            TSTupleElement::TSOptionalType(_) => todo!(),
                            TSTupleElement::TSRestType(_) => todo!(),
                            TSTupleElement::TSNamedTupleMember(_) => todo!(),
                        })
                        .map(|t| &*t),
                    self.arena,
                );

                Expr::Tuple(self.arena.alloc(Tuple { types: elements }))
            }
            TSType::TSArrayType(array_ty) => {
                let types = self
                    .arena
                    .alloc(self.transform_type(&array_ty.element_type, false));
                Expr::Array(self.arena.alloc(Array { the_type: types }))
            }
            TSType::TSIntersectionType(intersect_type) => {
                let types = AllocVec::from_iter_in(
                    intersect_type
                        .types
                        .iter()
                        .map(|t| self.transform_type(t, false)),
                    self.arena,
                );
                Expr::Intersect(self.arena.alloc(Intersect { types }))
            }
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
                match &cond_ty.extends_type {
                    TSType::TSInferType(infer) => {
                        let let_expr = Let {
                            name: &infer.type_parameter.name.name,
                            check: self
                                .arena
                                .alloc(self.transform_type(&cond_ty.check_type, false)),
                            cond: infer
                                .type_parameter
                                .constraint
                                .as_ref()
                                .map(|t| &*self.arena.alloc(self.transform_type(&t, false))),
                            then: self
                                .arena
                                .alloc(self.transform_type(&cond_ty.true_type, false)),
                            r#else: self
                                .arena
                                .alloc(self.transform_type(&cond_ty.false_type, true)),
                        };
                        return Expr::Let(self.arena.alloc(let_expr));
                    }
                    _ => {}
                }
                let if_expr = If {
                    check_type: self.transform_type(&cond_ty.check_type, false),
                    extends_type: self.transform_type(&cond_ty.extends_type, false),
                    then: self.transform_type(&cond_ty.true_type, true),
                    r#else: self.transform_type(&cond_ty.false_type, true),
                };
                Expr::If(self.arena.alloc(if_expr))
            }
            TSType::TSTypeReference(ty_ref) => {
                // Handle builtins: Array<T>, Record<K, V>
                match &ty_ref.type_name {
                    TSTypeName::IdentifierReference(refer) => {
                        match refer.name.as_str() {
                            "Array" => match &ty_ref.type_parameters {
                                Some(params) => {
                                    assert_eq!(params.params.len(), 1);
                                    return Expr::Array(self.arena.alloc(Array {
                                        the_type:
                                            self.arena.alloc(
                                                self.transform_type(&params.params[0], false),
                                            ),
                                    }));
                                }
                                None => panic!("Array<...> requires one type parameter"),
                            },
                            _ => (),
                        }
                    }
                    TSTypeName::QualifiedName(_) => todo!(),
                }

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
            TSType::TSConstructorType(_) => todo!(),
            TSType::TSFunctionType(_) => todo!(),
            TSType::TSImportType(_) => todo!(),
            TSType::TSMappedType(_) => todo!(),
            TSType::TSQualifiedName(_) => todo!(),
            TSType::TSTypeOperatorType(_) => todo!(),
            TSType::TSTypePredicate(_) => todo!(),
            TSType::TSTypeQuery(_) => todo!(),
            TSType::JSDocNullableType(_) => todo!(),
            TSType::JSDocUnknownType(_) => todo!(),
            TSType::TSInferType(_) => todo!(),
        }
    }
}
