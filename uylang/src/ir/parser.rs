use std::collections::HashSet;

use anyhow::Result;

use crate::parser::ast;

use super::tree::{
    BinaryExpr, Block, Expr, FnCall, FnDef, FnMeta, IfBlock, Import, JsxDef, Literal, Module,
    ObjectEntry, Stmt, UnaryExpr, VarDef,
};

/// Validate the AST and convert into IR.
pub fn convert<'src>(raw: ast::Module<'src>) -> Result<Module> {
    let mut converter = Converter::default();
    Ok(converter.convert(raw))
}

#[derive(Default)]
struct Converter {}

impl Converter {
    fn convert<'src>(&mut self, raw: ast::Module<'src>) -> Module {
        let mut ctx = Ctx::default();
        let ast::Module { imports, body } = raw;
        let Block {
            fns, statements, ..
        } = self.convert_block(
            &mut ctx,
            ast::Block {
                statements: body,
                out: None,
            },
        );

        let imports = imports
            .into_iter()
            .map(|i| Import {
                refs: i.refs.into_iter().map(str::to_string).collect(),
                from: i.from.to_string(),
            })
            .collect();

        Module {
            imports,
            fns,
            statements,
        }
    }

    fn convert_expr<'src>(&mut self, ctx: &mut Ctx, raw: ast::Expr<'src>) -> Expr {
        match raw {
            ast::Expr::Error => todo!(),
            ast::Expr::Literal(l) => match l {
                ast::Literal::Null => Expr::Literal(Literal::Null),
                ast::Literal::Bool(b) => Expr::Literal(Literal::Bool(b)),
                ast::Literal::Num(n) => Expr::Literal(Literal::Num(n)),
                ast::Literal::Str(s) => Expr::Literal(Literal::Str(s.into())),
            },
            ast::Expr::VarRef(v) => {
                if !ctx.defined_vars.contains(v) {
                    ctx.global_vars.insert(v.into());
                }

                Expr::VarRef(v.into())
            }
            ast::Expr::Grouped(e) => Expr::Grouped(Box::new(self.convert_expr(ctx, *e))),
            ast::Expr::Field(e, field) => {
                Expr::Field(Box::new(self.convert_expr(ctx, *e)), field.into())
            }
            ast::Expr::Index(left, right) => Expr::Index(
                Box::new(self.convert_expr(ctx, *left)),
                Box::new(self.convert_expr(ctx, *right)),
            ),
            ast::Expr::List(list) => Expr::List(
                list.into_iter()
                    .map(|(e, _span)| self.convert_expr(ctx, e))
                    .collect(),
            ),
            ast::Expr::Object(entries) => Expr::Object(
                entries
                    .into_iter()
                    .map(|e| match e {
                        ast::ObjectEntry::KeyValue(k, v) => {
                            ObjectEntry::KeyValue(k.to_string(), self.convert_expr(ctx, v.0))
                        }
                        ast::ObjectEntry::Rest(name) => ObjectEntry::Rest(name.into()),
                    })
                    .collect(),
            ),
            ast::Expr::Binary(b) => Expr::Binary(BinaryExpr {
                left: Box::new(self.convert_expr(ctx, b.left.0)),
                op: b.op,
                right: Box::new(self.convert_expr(ctx, b.right.0)),
            }),
            ast::Expr::Unary(u) => Expr::Unary(UnaryExpr {
                op: u.op,
                expr: Box::new(self.convert_expr(ctx, u.expr.0)),
            }),
            ast::Expr::FnCall(f) => Expr::FnCall(FnCall {
                fn_name: Box::new(self.convert_expr(ctx, f.fn_name.0)),
                args: f
                    .args
                    .into_iter()
                    .map(|(e, _)| self.convert_expr(ctx, e))
                    .collect(),
            }),
            ast::Expr::Block(b) => Expr::Block(self.convert_block(ctx, b)),
            ast::Expr::If(if_) => Expr::If(IfBlock {
                cond: Box::new(self.convert_expr(ctx, if_.cond.0)),
                block: self.convert_block(ctx, if_.block),
                r#else: if_.r#else.map(|e| Box::new(self.convert_expr(ctx, *e))),
            }),
            ast::Expr::Jsx(jsx) => {
                let props = jsx
                    .props
                    .into_iter()
                    .map(|(k, v)| {
                        if k.starts_with("on") && k.len() > 2 {
                            ctx.needs_hydration = true;
                        }

                        (k.into(), self.convert_expr(ctx, v))
                    })
                    .collect();

                // TODO: if jsx.name is a custom function then add to ctx.global_refs

                Expr::Jsx(JsxDef {
                    name: jsx.name.into(),
                    props,
                    children: jsx
                        .children
                        .into_iter()
                        .map(|e| self.convert_expr(ctx, e))
                        .collect(),
                })
            }
            ast::Expr::Lambda(def) => {
                let args: Vec<_> = def.args.into_iter().map(str::to_string).collect();
                let mut child_ctx = Ctx::default();
                child_ctx.defined_vars.extend(args.clone());

                Expr::Lambda(FnDef {
                    exported: def.exported,
                    args,
                    body: self.convert_block(&mut child_ctx, def.body),
                    meta: FnMeta {
                        defined_vars: child_ctx.defined_vars,
                        global_vars: child_ctx.global_vars,
                        needs_hydration: child_ctx.needs_hydration,
                    },
                })
            }
        }
    }

    fn convert_block<'src>(&mut self, ctx: &mut Ctx, raw: ast::Block<'src>) -> Block {
        let mut fns = vec![];
        let mut statements = vec![];

        for stmt in raw.statements.into_iter() {
            match stmt {
                ast::Stmt::LineComment(c) => {
                    statements.push(Stmt::LineComment(c.into()));
                }
                ast::Stmt::VarDef((v, _)) => {
                    statements.push(Stmt::VarDef(VarDef {
                        exported: v.exported,
                        pattern: v.pattern.into(),
                        def: self.convert_expr(ctx, v.def),
                    }));
                }
                ast::Stmt::FnDef((f, _)) => {
                    let args: Vec<_> = f.args.into_iter().map(str::to_string).collect();
                    let mut child_ctx = Ctx::default();
                    child_ctx.defined_vars.extend(args.clone());

                    fns.push((
                        f.name.into(),
                        FnDef {
                            exported: f.exported,
                            args,
                            body: self.convert_block(&mut child_ctx, f.body),
                            meta: FnMeta {
                                defined_vars: child_ctx.defined_vars,
                                global_vars: child_ctx.global_vars,
                                needs_hydration: child_ctx.needs_hydration,
                            },
                        },
                    ));
                }
                ast::Stmt::Expr((e, _)) => {
                    statements.push(Stmt::Expr(self.convert_expr(ctx, e)));
                }
            }
        }

        let out = raw.out.map(|(e, _)| Box::new(self.convert_expr(ctx, *e)));

        Block {
            fns,
            statements,
            out,
        }
    }
}

#[derive(Default)]
struct Ctx {
    /// List of variables defined in this scope.
    defined_vars: HashSet<String>,
    /// List of referenced variables defined outside of current function scope.
    global_vars: HashSet<String>,
    /// Should current function be hydrated in the browser.
    needs_hydration: bool,
}

#[cfg(test)]
mod tests {
    use chumsky::span::SimpleSpan;

    use crate::{
        ir::tree::{Block, Expr, FnDef, FnMeta, Literal, MatchPattern, Module, Stmt, VarDef},
        parser::ast,
    };

    use super::convert;

    #[test]
    fn test_parse() {
        let span = SimpleSpan::new(0, 0);

        let raw = ast::Module {
            imports: vec![],
            body: vec![
                ast::Stmt::VarDef((
                    ast::VarDef {
                        exported: false,
                        pattern: ast::MatchPattern::Ident("a"),
                        def: ast::Expr::Literal(ast::Literal::Num(2.0)),
                    },
                    span,
                )),
                ast::Stmt::Expr((ast::Expr::VarRef("a"), span)),
            ],
        };

        let res = convert(raw).unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                fns: vec![],
                statements: vec![
                    Stmt::VarDef(VarDef {
                        exported: false,
                        pattern: MatchPattern::Ident("a".into()),
                        def: Expr::Literal(Literal::Num(2.0))
                    }),
                    Stmt::Expr(Expr::VarRef("a".into())),
                ]
            }
        );

        let raw = ast::Module {
            imports: vec![],
            body: vec![
                ast::Stmt::VarDef((
                    ast::VarDef {
                        exported: false,
                        pattern: ast::MatchPattern::Ident("a"),
                        def: ast::Expr::Literal(ast::Literal::Num(2.0)),
                    },
                    span,
                )),
                ast::Stmt::FnDef((
                    ast::FnDef {
                        exported: false,
                        name: "one",
                        args: vec![],
                        body: ast::Block {
                            statements: vec![],
                            out: None,
                        },
                    },
                    span,
                )),
            ],
        };
        let res = convert(raw).unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                fns: vec![(
                    "one".into(),
                    FnDef {
                        exported: false,
                        args: vec![],
                        body: Block {
                            statements: vec![],
                            out: None,
                            fns: vec![],
                        },
                        meta: FnMeta::default(),
                    },
                )],
                statements: vec![Stmt::VarDef(VarDef {
                    exported: false,
                    pattern: MatchPattern::Ident("a".into()),
                    def: Expr::Literal(Literal::Num(2.0))
                }),]
            }
        );
    }
}
