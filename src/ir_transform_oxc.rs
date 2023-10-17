use std::collections::BTreeMap;
use std::ops::Deref;

use crate::ir::*;
use crate::Arena;
use oxc_allocator::Box;
use oxc_allocator::Vec as AllocVec;
use oxc_ast::ast;
use oxc_ast::ast::BindingIdentifier;
use oxc_ast::ast::Declaration;
use oxc_ast::ast::Expression;
use oxc_ast::ast::ModuleDeclaration;
use oxc_ast::ast::TSLiteral;
use oxc_ast::ast::TSSignature;
use oxc_ast::ast::TSTupleElement;
use oxc_ast::ast::TSType;
use oxc_ast::ast::TSTypeName;

pub struct Transform<'ir> {
    pub arena: &'ir Arena,
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
            ident: binding_ident_to_identifier(self.arena, &param.name),
            default,
            extends_type,
        }
    }

    fn transform_type_alias_decl(
        &self,
        alias_decl: &'ir ast::TSTypeAliasDeclaration<'ir>,
    ) -> GlobalDecl<'ir> {
        let ident = binding_ident_to_identifier(self.arena, &alias_decl.id);
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
                            value: value.as_str(),
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
                let mut variants = AllocVec::with_capacity_in(union.types.len(), self.arena);

                variants.extend(
                    union
                        .types
                        .iter()
                        .map(|t| self.arena.alloc(self.transform_type(t, false)) as &_),
                );

                Expr::Union(self.arena.alloc(Union { variants }))
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
                let types: AllocVec<'ir, TupleItem<'ir>> = AllocVec::from_iter_in(
                    tuple.element_types.iter().map(|ty| match ty {
                        TSTupleElement::TSType(ty) => TupleItem {
                            expr: self.arena.alloc(self.transform_type(ty, false)) as &_,
                            spread: false,
                        },
                        TSTupleElement::TSRestType(rest) => TupleItem {
                            spread: true,
                            expr: self
                                .arena
                                .alloc(self.transform_type(&rest.type_annotation, false))
                                as &_,
                        },
                        TSTupleElement::TSOptionalType(_) => todo!(),
                        TSTupleElement::TSNamedTupleMember(_) => todo!(),
                    }),
                    self.arena,
                );

                Expr::Tuple(self.arena.alloc(Tuple { types }))
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
                                None => {
                                    // return Expr::Tuple(self.arena.alloc(Tuple {
                                    //     types: AllocVec::new_in(&self.arena),
                                    //     spread: None,
                                    // }))
                                }
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

                    let name = match &ty_ref.type_name {
                        TSTypeName::IdentifierReference(n) => n.name.as_str(),
                        TSTypeName::QualifiedName(_) => todo!(),
                    };

                    return Expr::Call(self.arena.alloc(Call {
                        name,
                        args,
                        tail_call,
                    }));
                }
                Expr::Identifier(ts_type_name_to_identifier(&ty_ref.type_name))
            }
            TSType::TSLiteralType(lit_type) => match &lit_type.literal {
                TSLiteral::BooleanLiteral(b) => {
                    Expr::BooleanLiteral(self.arena.alloc(BooleanLiteral { value: b.value }))
                }
                TSLiteral::StringLiteral(s) => {
                    Expr::StringLiteral(self.arena.alloc(StringLiteral {
                        value: s.value.as_str(),
                    }))
                }
                TSLiteral::NumberLiteral(n) => {
                    Expr::NumberLiteral(self.arena.alloc(NumberLiteral { value: n.value }))
                }
                TSLiteral::NullLiteral(_) => todo!(),
                TSLiteral::BigintLiteral(_) => todo!(),
                TSLiteral::RegExpLiteral(_) => todo!(),
                TSLiteral::TemplateLiteral(_) => todo!(),
                TSLiteral::UnaryExpression(unary) => match unary.operator {
                    oxc_syntax::operator::UnaryOperator::UnaryNegation => {
                        Expr::Unary(self.arena.alloc(Unary {
                            op: UnaryOperator::UnaryNegation,
                            expr: self.transform_expr(&unary.argument, tail_call),
                        }))
                    }
                    _ => todo!(),
                },
            },
            TSType::TSNumberKeyword(_) => Expr::Number,
            TSType::TSStringKeyword(_) => Expr::String,
            TSType::TSAnyKeyword(_) => Expr::Any,
            TSType::TSBooleanKeyword(_) => Expr::Boolean,
            TSType::TSObjectKeyword(_) => Expr::ObjectKeyword,
            TSType::TSBigIntKeyword(_) => todo!(),
            TSType::TSNeverKeyword(_) => todo!(),
            TSType::TSNullKeyword(_) => todo!(),
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

    fn transform_expr(&self, expr: &'ir ast::Expression<'ir>, tail_call: bool) -> Expr<'ir> {
        match expr {
            Expression::NumberLiteral(num) => {
                Expr::NumberLiteral(self.arena.alloc(NumberLiteral { value: num.value }))
            }
            Expression::BooleanLiteral(_) => todo!(),
            Expression::NullLiteral(_) => todo!(),
            Expression::BigintLiteral(_) => todo!(),
            Expression::RegExpLiteral(_) => todo!(),
            Expression::StringLiteral(_) => todo!(),
            Expression::TemplateLiteral(_) => todo!(),
            Expression::Identifier(_) => todo!(),
            Expression::MetaProperty(_) => todo!(),
            Expression::Super(_) => todo!(),
            Expression::ArrayExpression(_) => todo!(),
            Expression::ArrowExpression(_) => todo!(),
            Expression::AssignmentExpression(_) => todo!(),
            Expression::AwaitExpression(_) => todo!(),
            Expression::BinaryExpression(_) => todo!(),
            Expression::CallExpression(_) => todo!(),
            Expression::ChainExpression(_) => todo!(),
            Expression::ClassExpression(_) => todo!(),
            Expression::ConditionalExpression(_) => todo!(),
            Expression::FunctionExpression(_) => todo!(),
            Expression::ImportExpression(_) => todo!(),
            Expression::LogicalExpression(_) => todo!(),
            Expression::MemberExpression(_) => todo!(),
            Expression::NewExpression(_) => todo!(),
            Expression::ObjectExpression(_) => todo!(),
            Expression::ParenthesizedExpression(_) => todo!(),
            Expression::SequenceExpression(_) => todo!(),
            Expression::TaggedTemplateExpression(_) => todo!(),
            Expression::ThisExpression(_) => todo!(),
            Expression::UnaryExpression(_) => todo!(),
            Expression::UpdateExpression(_) => todo!(),
            Expression::YieldExpression(_) => todo!(),
            Expression::PrivateInExpression(_) => todo!(),
            Expression::JSXElement(_) => todo!(),
            Expression::JSXFragment(_) => todo!(),
            Expression::TSAsExpression(_) => todo!(),
            Expression::TSSatisfiesExpression(_) => todo!(),
            Expression::TSTypeAssertion(_) => todo!(),
            Expression::TSNonNullExpression(_) => todo!(),
            Expression::TSInstantiationExpression(_) => todo!(),
        }
    }
}

fn ts_type_name_to_identifier<'ir>(tname: &'ir TSTypeName<'ir>) -> Ident<'ir> {
    match tname {
        TSTypeName::IdentifierReference(refer) => Ident(&refer.name),
        TSTypeName::QualifiedName(_) => todo!(),
    }
}

pub fn binding_ident_to_identifier<'ir>(
    arena: &'ir Arena,
    b: &'ir BindingIdentifier,
) -> Ident<'ir> {
    Ident(&b.name)
}
