use chumsky::{prelude::*, text::ascii::keyword};

use self::ast::{
    BinaryExpr, BinaryOp, Block, Expr, FnCall, FnDef, IfBlock, Import, JsxDef, Literal,
    MatchPattern, Module, ObjectEntry, Span, Spanned, Stmt, UnaryExpr, UnaryOp, VarDef,
};

pub mod ast;

type ParserInput<'src> = &'src str;
type ParserError<'src> = extra::Err<Rich<'src, char, Span>>;

// Future (Alias type is not in stable yet):
// type ExprParser<'src> = impl Parser<'src, ParserInput<'src>, Expr<'src>, ParserError<'src>> + Clone;
//
// Now:
trait ExprParser<'src>: Parser<'src, ParserInput<'src>, Expr<'src>, ParserError<'src>> + Clone {}
impl<'src, T> ExprParser<'src> for T where
    T: Parser<'src, ParserInput<'src>, Expr<'src>, ParserError<'src>> + Clone
{
}
//
// Thanks to https://github.com/rust-lang/rust/issues/41517#issuecomment-1100644808

pub fn module_parser<'src>(
) -> impl Parser<'src, ParserInput<'src>, Module<'src>, ParserError<'src>> + Clone {
    let (stmt, expr) = parsers();

    // Module = Import* Stmt* Expr?
    import_parser()
        .repeated()
        .collect::<Vec<_>>()
        .then(stmt.repeated().collect::<Vec<_>>())
        .then(expr.map_with_span(|e, span| (e, span)).or_not())
        .then_ignore(end())
        .map(|((imports, mut statements), e)| {
            if let Some(e) = e {
                statements.push(Stmt::Expr(e));
            }

            Module {
                imports,
                body: statements,
            }
        })
        .boxed()
}

/// Import = import { Ident ( , Ident )* ,?  } from Str ;
fn import_parser<'src>(
) -> impl Parser<'src, ParserInput<'src>, Import<'src>, ParserError<'src>> + Clone {
    let ident_ = text::ascii::ident().padded();

    keyword("import")
        .ignore_then(
            ident_
                .separated_by(just(',').padded())
                .at_least(1)
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .then_ignore(keyword("from").padded())
        .then(str_parser())
        .then_ignore(just(';').padded())
        .map(|a| Import {
            refs: a.0,
            from: a.1,
        })
        .labelled("import")
        .padded()
        .boxed()
}

fn str_parser<'src>() -> impl Parser<'src, ParserInput<'src>, &'src str, ParserError<'src>> + Clone
{
    none_of('"').repeated().slice().padded_by(just('"'))
}

/// RestPattern = ...Ident
fn rest_parser<'src>() -> impl Parser<'src, ParserInput<'src>, &'src str, ParserError<'src>> + Clone
{
    let ident_ = text::ascii::ident();
    just("...")
        .ignore_then(ident_)
        .labelled("rest pattern")
        .padded()
}

/// Literal = Num | Str | Bool | Null
fn literal_parser<'src>(
) -> impl Parser<'src, ParserInput<'src>, Literal<'src>, ParserError<'src>> + Clone {
    let num = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .slice()
        .from_str()
        .unwrapped()
        .map(|n| Literal::Num(n))
        .labelled("number");

    let str_ = str_parser().map(Literal::Str).labelled("string");

    let bool_ = keyword("true")
        .to(Literal::Bool(true))
        .or(keyword("false").to(Literal::Bool(false)))
        .labelled("boolean");

    let null_ = keyword("null").to(Literal::Null).labelled("null");

    choice((
        num,   //
        str_,  //
        bool_, //
        null_, //
    ))
    .padded()
    .boxed()
}

/// ListItems = Expr ( , Expr )* ,?
fn list_items_parser<'src>(
    expr: impl ExprParser<'src>,
) -> impl Parser<'src, ParserInput<'src>, Vec<Spanned<Expr<'src>>>, ParserError<'src>> + Clone {
    expr.map_with_span(|e, span| (e, span))
        .separated_by(just(',').padded())
        .allow_trailing()
        .collect::<Vec<_>>()
}

/// List = [ ListItems ]
fn list_parser<'src>(
    expr: impl ExprParser<'src>,
) -> impl Parser<'src, ParserInput<'src>, Vec<Spanned<Expr<'src>>>, ParserError<'src>> + Clone {
    list_items_parser(expr)
        .delimited_by(just('['), just(']'))
        .labelled("list")
        .padded()
}

/// Object = { ObjectEntry ( , ObjectEntry )* ,? }
/// ObjectEntry = KeyValueEntry | RestPattern
/// KeyValueEntry = Str : Expression
fn obj_parser<'src>(
    expr: impl ExprParser<'src>,
) -> impl Parser<'src, ParserInput<'src>, Vec<ObjectEntry<'src>>, ParserError<'src>> + Clone {
    let key_value = str_parser()
        .padded()
        .then_ignore(just(':'))
        .then(expr.map_with_span(|e, span| (e, span)));
    let obj_entries = choice((
        key_value.map(|(key, value)| ObjectEntry::KeyValue(key, value)), //
        rest_parser().map(ObjectEntry::Rest),                            //
    ))
    .separated_by(just(',').padded())
    .allow_trailing()
    .collect::<Vec<_>>();
    obj_entries
        .delimited_by(just('{'), just('}'))
        .labelled("object")
        .padded()
}

/// FnCall = AtomExpr \( ListItems \)
fn fn_call_parser<'src>(
    atom_expr: impl ExprParser<'src>,
    expr: impl ExprParser<'src>,
) -> impl Parser<'src, ParserInput<'src>, Expr<'src>, ParserError<'src>> + Clone {
    atom_expr
        .clone()
        .map_with_span(|v, span: Span| (v, span))
        .foldl(
            list_items_parser(expr)
                .delimited_by(just('(').padded(), just(')').padded())
                .map_with_span(|args, span: Span| (args, span))
                .repeated(),
            |(name, name_span), (args, args_span)| {
                let span = (name_span.start..args_span.end).into();
                (
                    Expr::FnCall(FnCall {
                        fn_name: Box::new((name, span)),
                        args,
                    }),
                    span,
                )
            },
        )
        .map(|(c, _span)| c)
}

/// Field = AtomExpr . Ident
fn field_parser<'src>(
    atom_expr: impl ExprParser<'src>,
) -> impl Parser<'src, ParserInput<'src>, Expr<'src>, ParserError<'src>> + Clone {
    let ident_ = text::ascii::ident().padded();

    atom_expr.clone().foldl(
        just('.').ignore_then(ident_.clone()).repeated(),
        |e, field| Expr::Field(Box::new(e), field),
    )
}

/// Index = AtomExpr [ Expr ]
fn index_parser<'src>(
    atom_expr: impl ExprParser<'src>,
    expr: impl ExprParser<'src>,
) -> impl Parser<'src, ParserInput<'src>, Expr<'src>, ParserError<'src>> + Clone {
    atom_expr.clone().foldl(
        expr.delimited_by(just('[').padded(), just(']').padded())
            .repeated(),
        |left, right| Expr::Index(Box::new(left), Box::new(right)),
    )
}

/// Unary = - Expression | ! Expression
fn unary_parser<'src>(
    expr: impl ExprParser<'src>,
) -> impl Parser<'src, ParserInput<'src>, Expr<'src>, ParserError<'src>> + Clone {
    let e = expr.map_with_span(|e, span| (e, span));

    // Unary ops
    //   - https://github.com/zesterer/chumsky/blob/main/tutorial.md#parsing-unary-operators
    //   - https://docs.rs/chumsky/1.0.0-alpha.4/chumsky/trait.IterParser.html#method.foldr
    //   - https://github.com/PRQL/prql/blob/main/crates/prql-parser/src/expr.rs#L134
    let unary = choice((
        just('-').to(UnaryOp::Neg), //
        just('!').to(UnaryOp::Not), //
    ))
    .map_with_span(|op, span: Span| (op, span))
    .repeated()
    .at_least(1)
    .foldr(e, |(op, op_span), e| {
        let span = (op_span.start..e.1.end).into();
        (
            Expr::Unary(UnaryExpr {
                op,
                expr: Box::new(e),
            }),
            span,
        )
    });

    unary.map(|(e, _span)| e)
}

/// Binary operations
/// Ref: https://github.com/zesterer/chumsky/blob/main/examples/nano_rust.rs#L286
/// Ref: https://github.com/zesterer/chumsky/blob/main/tutorial.md#parsing-binary-operators
fn binary_parser<'src>(
    atom_expr: impl ExprParser<'src>,
) -> impl Parser<'src, ParserInput<'src>, Expr<'src>, ParserError<'src>> + Clone {
    fn build_binary_expr<'src>(
        a: (Expr<'src>, SimpleSpan),
        (op, b): (BinaryOp, (Expr<'src>, SimpleSpan)),
    ) -> (Expr<'src>, SimpleSpan) {
        let span = a.1.start..b.1.end;
        (
            Expr::Binary(BinaryExpr {
                left: Box::new(a),
                op,
                right: Box::new(b),
            }),
            span.into(),
        )
    }

    let a = atom_expr.map_with_span(|e, span| (e, span));

    // Binary = Product | Sum | Comparison

    // Let's say: Product = Expression * Expression | Expression / Expression
    // <-- This is left-recursive and chumsky will be stuck in an infinite loop. We need to use right recursion.
    //
    // Theory:
    // Product = AtomExpr Product'
    // Product' = * Expression | / Expression

    // Product ops have equal precedence
    let op = choice((
        just('*').to(BinaryOp::Mul), //
        just('/').to(BinaryOp::Div), //
    ));
    let product = a.clone().foldl(op.then(a).repeated(), build_binary_expr);

    // Sum ops have equal precedence
    let op = choice((
        just('+').to(BinaryOp::Add), //
        just('-').to(BinaryOp::Sub), //
    ));
    let sum = product
        .clone()
        .foldl(op.then(product).repeated(), build_binary_expr);

    // Comparison ops
    let op = choice((
        just("==").to(BinaryOp::Eq),    //
        just("!=").to(BinaryOp::NotEq), //
        just(">").to(BinaryOp::Gt),     //
        just(">=").to(BinaryOp::Gte),   //
        just("<").to(BinaryOp::Lt),     //
        just("<=").to(BinaryOp::Lte),   //
    ));
    let compare = sum
        .clone()
        .foldl(op.then(sum).repeated(), build_binary_expr);

    // Assignment
    let op = choice((
        just("+=").to(BinaryOp::AssignAdd), //
        just("-=").to(BinaryOp::AssignSub), //
        just("*=").to(BinaryOp::AssignMul), //
        just("/=").to(BinaryOp::AssignDiv), //
        just("%=").to(BinaryOp::AssignMod), //
        just("=").to(BinaryOp::Assign),     //
    ));
    let assign = compare
        .clone()
        .foldl(op.then(compare).repeated(), build_binary_expr);

    assign.map(|(e, _span)| e)
}

fn parsers<'src>() -> (
    impl Parser<'src, ParserInput<'src>, Stmt<'src>, ParserError<'src>> + Clone,
    impl Parser<'src, ParserInput<'src>, Expr<'src>, ParserError<'src>> + Clone,
) {
    // TODO: disallow reserved keywords (use `and_is(none_of())` combinators)
    let ident_ = text::ascii::ident().padded();
    let op = |c| just(c).padded();

    let mut stmt = Recursive::declare();
    let mut expr = Recursive::declare();

    // Grouped = ( Expr )
    let grouped = expr
        .clone()
        .delimited_by(op('('), op(')'))
        .padded()
        .map(Box::new);

    // InlineExpr = Literal | VarRef | Unary | Grouped | List | Object | JSX
    let inline_expr = choice((
        literal_parser().map(Expr::Literal),
        ident_.padded().map(Expr::VarRef),
        unary_parser(expr.clone()),
        grouped.map(Expr::Grouped),
        list_parser(expr.clone()).map(Expr::List),
        obj_parser(expr.clone()).map(Expr::Object),
        jsx_parser(expr.clone()).map(Expr::Jsx),
    ))
    .boxed();

    // Block body are statements with an optional return expression in the end
    // BlockBody = Stmt* Expr?
    let block_body = stmt
        .clone()
        .repeated()
        .collect::<Vec<_>>()
        .then(expr.clone().map_with_span(|e, span| (e, span)).or_not());

    // Blocks are delimited by braces
    let block = block_body
        .delimited_by(just('{').padded(), just('}').padded())
        .map(|(statements, e)| Block {
            statements,
            out: e.map(|(e, span)| (Box::new(e), span)),
        })
        .labelled("block");

    // If-else block
    let if_else = recursive(|if_else| {
        just("if")
            .padded()
            .ignore_then(expr.clone().map_with_span(|e, span| (e, span)))
            .then(block.clone())
            // Else block is optional
            .then(
                just("else")
                    .padded()
                    .ignore_then(choice((
                        if_else,                        //
                        block.clone().map(Expr::Block), //
                    )))
                    .or_not()
                    .labelled("else branch"),
            )
            .map(|((cond, block), else_block)| {
                Expr::If(IfBlock {
                    cond: Box::new(cond),
                    block,
                    r#else: else_block.map(Box::new),
                })
            })
            .boxed()
    });

    // Lambda def
    let args = ident_
        .separated_by(just(','))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just('|'), just('|'));

    let lambda = args
        .then(block.clone().map(|b| Expr::Block(b)).or(expr.clone()))
        .map(|(args, body)| -> Expr<'_> {
            Expr::Lambda(FnDef {
                exported: false,
                name: "",
                args,
                body: match body {
                    Expr::Block(b) => b,
                    _ => Block {
                        statements: vec![],
                        out: Some((Box::new(body), SimpleSpan::new(0, 0))),
                    },
                },
            })
        })
        .labelled("lambda function");

    // BlockExpr = Lambda | IfBlock | Block
    let block_expr = choice((
        lambda,
        if_else,
        block.clone().map(|b| Expr::Block(b)),
        // TODO: loops
        // TODO: match
        // TODO: try/catch
    ))
    .boxed();

    // AtomExpr = InlineExpr | BlockExpr
    // AtomExpr is expression that contains no ambiguity
    let atom = choice((
        block_expr,  //
        inline_expr, //
    ))
    .padded();

    // Now we parse ambiguous expressions (binary ops, fn calls and fields).
    // The reason we do it separately is because these ambiguous parsers are left-recursive.
    // Chumsky will be stuck in an infinite loop. We need to convert these parsers to right-recursive.
    // Ref: https://en.wikipedia.org/wiki/Left_recursion#Removing_left_recursion

    // Ambiguous parsers start to parse unambiguous expression and then continue with other expressions.
    let field = field_parser(atom);
    let index = index_parser(field, expr.clone()).boxed();
    let binary = binary_parser(index).boxed();
    let fn_call = fn_call_parser(binary, expr.clone());
    // We try to parse field access and binary again here to support chaining of field, index and fn calls.
    let complex_field = binary_parser(field_parser(fn_call));

    expr.define(complex_field.labelled("expression").boxed());

    let match_pattern = recursive(|match_pattern| {
        // SlicePattern = [ SlicePatternItems? ( , RestPattern )? ]
        // SlicePatternItems = MatchPattern ( , MatchPattern )*
        let slice_pattern = match_pattern
            .separated_by(just(','))
            .collect::<Vec<_>>()
            .then(just(",").padded().ignore_then(rest_parser()).or_not())
            .delimited_by(just('['), just(']'))
            .map(|(items, rest)| MatchPattern::Slice { items, rest });

        choice((
            slice_pattern,                   //
            ident_.map(MatchPattern::Ident), //
        ))
        .labelled("match pattern")
        .padded()
    });

    // Variable definition
    let let_ = (keyword("export").or_not())
        .then_ignore(keyword("let").padded())
        .then(match_pattern)
        .then_ignore(just('='))
        .then(expr.clone())
        .then_ignore(just(";"))
        .map_with_span(|((exported, pattern), e), span| {
            Stmt::VarDef((
                VarDef {
                    exported: exported.is_some(),
                    pattern,
                    def: e,
                },
                span,
            ))
        })
        .labelled("variable definition")
        .boxed();

    // Function definition
    let args = ident_
        .separated_by(just(','))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just('('), just(')'))
        .labelled("function arguments");

    let fn_ = (keyword("export").or_not())
        .then_ignore(keyword("fn").padded())
        .then(ident_)
        .then(args)
        .then(block.padded())
        .map_with_span(|(((exported, name), args), body), span| {
            Stmt::FnDef((
                FnDef {
                    exported: exported.is_some(),
                    name,
                    args,
                    body,
                },
                span,
            ))
        })
        .labelled("function definition")
        .boxed();

    // Comments that start with //
    let line_comment = just("//")
        .padded()
        .ignore_then(none_of('\n').repeated().map_slice(Stmt::LineComment))
        .labelled("line comment");

    stmt.define(
        choice((
            line_comment,
            let_,
            fn_,
            expr.clone()
                .map_with_span(|e, span| Stmt::Expr((e, span)))
                .then_ignore(just(';').padded()),
        ))
        .padded()
        .labelled("statement")
        .boxed(),
    );

    (stmt, expr)
}

fn jsx_parser<'src>(
    expr: impl ExprParser<'src> + 'src,
) -> impl Parser<'src, ParserInput<'src>, JsxDef<'src>, ParserError<'src>> + Clone {
    recursive(|jsx| {
        // TODO: JSX tag name has different requirements
        let ident_ = text::ascii::ident();

        let str_ = none_of('"').repeated().slice().padded_by(just('"'));

        // Attributes could have an optional value. I.E <div class="one" disabled> both are supported.
        let attrs = ident_
            .clone()
            .then(
                just('=')
                    // Either an expression in {} or a string
                    .ignore_then(
                        str_.map(|s| Expr::Literal(Literal::Str(s))) //
                            .or(expr.clone().delimited_by(just('{'), just('}')))
                    )
                    .or_not()
                    .map(|attr| attr.unwrap_or(Expr::Literal(Literal::Bool(true)))),
            )
            .padded()
            .repeated()
            .collect::<Vec<_>>();

        // Self-closing tag (e.g <img src="" />)
        let self_closing_tag = just('<')
            .padded()
            .ignore_then(ident_.clone())
            .then(attrs.clone())
            .then_ignore(just("/>").padded())
            .map(|(name, attrs)| JsxDef {
                name,
                props: attrs,
                children: vec![],
            });

        // Wrapping tags (e.g <span>Hello</span> OR <></>)
        let open_tag = just('<')
            .padded()
            .ignore_then(ident_.clone().then(attrs.clone().padded()).or_not())
            .then_ignore(just('>').padded());

        let close_tag = just("</")
            .padded()
            .ignore_then(ident_.or_not())
            .then_ignore(just('>').padded());

        // Expressions are wrapped in {}
        let expr_in_jsx = expr.delimited_by(just('{'), just('}'));

        let text_in_jsx = any()
            .filter(|c| *c != '<' && *c != '{' && *c != '\n')
            .repeated()
            .at_least(1)
            .map_slice(|s| Expr::Literal(Literal::Str(s)));

        let children = choice((
            jsx.map(Expr::Jsx), //
            expr_in_jsx,        //
            text_in_jsx,        //
        ))
        .padded()
        .repeated()
        .collect::<Vec<_>>();

        let wrapping_tag =
            open_tag
                .then(children)
                .then(close_tag)
                .map(|((start, children), end_name)| {
                    let (name, props) = match start {
                        Some((name, props)) => (name, props),
                        None => ("", vec![]),
                    };
                    // TODO: fail if end_name doesn't match name: use `try_map` for that
                    JsxDef {
                        name,
                        props,
                        children,
                    }
                });

        self_closing_tag.or(wrapping_tag)
    })
}

#[cfg(test)]
mod tests {
    use ariadne::{Color, Label, Report, ReportKind, Source};
    use chumsky::{prelude::Rich, span::SimpleSpan, Parser};
    use pretty_assertions::assert_eq;

    use crate::parser::{
        ast::{
            BinaryExpr, BinaryOp, Block, Expr, FnCall, IfBlock, Import, Literal, MatchPattern,
            Module, ObjectEntry, UnaryExpr, UnaryOp, VarDef,
        },
        module_parser,
    };

    use super::ast::{FnDef, JsxDef, Stmt};

    #[test]
    fn test_module_literals() {
        let res = parse_module(r#"1"#).unwrap();
        assert_eq!(
            format!("{:#?}", res),
            r#"Module {
    imports: [],
    body: [
        Expr(
            (
                Literal(
                    Num(
                        1.0,
                    ),
                ),
                0..1,
            ),
        ),
    ],
}"#
        );

        let res = parse_module(r#"true; "one";"#).unwrap();
        assert_eq!(
            format!("{:#?}", res),
            r#"Module {
    imports: [],
    body: [
        Expr(
            (
                Literal(
                    Bool(
                        true,
                    ),
                ),
                0..4,
            ),
        ),
        Expr(
            (
                Literal(
                    Str(
                        "one",
                    ),
                ),
                6..11,
            ),
        ),
    ],
}"#
        );
    }

    #[test]
    fn test_trial_1() {
        let res = parse_module(r#"[1, "two", true, ]"#).unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::List(vec![
                        (Expr::Literal(Literal::Num(1.)), SimpleSpan::new(1, 2)),
                        (Expr::Literal(Literal::Str("two")), SimpleSpan::new(4, 9)),
                        (Expr::Literal(Literal::Bool(true)), SimpleSpan::new(11, 15)),
                    ]),
                    SimpleSpan::new(0, 18)
                ))]
            }
        );

        let res = parse_module(r#"{ "one": 1, "two": "2", }"#).unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::Object(vec![
                        ObjectEntry::KeyValue(
                            "one",
                            (Expr::Literal(Literal::Num(1.)), SimpleSpan::new(8, 10))
                        ),
                        ObjectEntry::KeyValue(
                            "two",
                            (Expr::Literal(Literal::Str("2")), SimpleSpan::new(18, 22))
                        ),
                    ]),
                    SimpleSpan::new(0, 25)
                ))]
            }
        );

        let res = parse_module(r#"one(1, two)("two",)"#).unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::FnCall(FnCall {
                        fn_name: Box::new((
                            Expr::FnCall(FnCall {
                                fn_name: Box::new((Expr::VarRef("one"), SimpleSpan::new(0, 11))),
                                args: vec![
                                    (Expr::Literal(Literal::Num(1.)), SimpleSpan::new(4, 5)),
                                    (Expr::VarRef("two"), SimpleSpan::new(7, 10)),
                                ],
                            }),
                            SimpleSpan::new(0, 19)
                        )),
                        args: vec![(Expr::Literal(Literal::Str("two")), SimpleSpan::new(12, 17)),],
                    }),
                    SimpleSpan::new(0, 19)
                ))]
            }
        );

        let res = parse_module(r#"-- 2"#).unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::Unary(UnaryExpr {
                        op: UnaryOp::Neg,
                        expr: Box::new((
                            Expr::Unary(UnaryExpr {
                                op: UnaryOp::Neg,
                                expr: Box::new((
                                    Expr::Literal(Literal::Num(2.)),
                                    SimpleSpan::new(2, 4)
                                )),
                            }),
                            SimpleSpan::new(1, 4)
                        ))
                    }),
                    SimpleSpan::new(0, 4)
                ))]
            }
        );

        let res = parse_module(r#"1 - 2 * 3"#).unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::Binary(BinaryExpr {
                        left: Box::new((Expr::Literal(Literal::Num(1.)), SimpleSpan::new(0, 2)),),
                        op: BinaryOp::Sub,
                        right: Box::new((
                            Expr::Binary(BinaryExpr {
                                left: Box::new((
                                    Expr::Literal(Literal::Num(2.)),
                                    SimpleSpan::new(3, 6)
                                )),
                                op: BinaryOp::Mul,
                                right: Box::new((
                                    Expr::Literal(Literal::Num(3.)),
                                    SimpleSpan::new(7, 9)
                                ))
                            }),
                            SimpleSpan::new(3, 9)
                        ))
                    }),
                    SimpleSpan::new(0, 9)
                ))]
            }
        );

        let res = parse_module(r#"4 * 3 - 2"#).unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::Binary(BinaryExpr {
                        left: Box::new((
                            Expr::Binary(BinaryExpr {
                                left: Box::new((
                                    Expr::Literal(Literal::Num(4.)),
                                    SimpleSpan::new(0, 2)
                                )),
                                op: BinaryOp::Mul,
                                right: Box::new((
                                    Expr::Literal(Literal::Num(3.)),
                                    SimpleSpan::new(3, 6)
                                )),
                            }),
                            SimpleSpan::new(0, 6)
                        )),
                        op: BinaryOp::Sub,
                        right: Box::new((Expr::Literal(Literal::Num(2.)), SimpleSpan::new(7, 9))),
                    }),
                    SimpleSpan::new(0, 9)
                ))]
            }
        );

        let res = parse_module(r#"b.c - a.d * f"#).unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::Binary(BinaryExpr {
                        left: Box::new((
                            Expr::Field(Box::new(Expr::VarRef("b")), "c"),
                            SimpleSpan::new(0, 4)
                        ),),
                        op: BinaryOp::Sub,
                        right: Box::new((
                            Expr::Binary(BinaryExpr {
                                left: Box::new((
                                    Expr::Field(Box::new(Expr::VarRef("a")), "d"),
                                    SimpleSpan::new(5, 10)
                                )),
                                op: BinaryOp::Mul,
                                right: Box::new((Expr::VarRef("f"), SimpleSpan::new(11, 13)))
                            }),
                            SimpleSpan::new(5, 13)
                        ))
                    }),
                    SimpleSpan::new(0, 13)
                ))]
            }
        );

        let res = parse_module(r#"a.c[0][f()].d "#).unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::Field(
                        Box::new(Expr::Index(
                            Box::new(Expr::Index(
                                Box::new(Expr::Field(Box::new(Expr::VarRef("a")), "c")),
                                Box::new(Expr::Literal(Literal::Num(0.)))
                            )),
                            Box::new(Expr::FnCall(FnCall {
                                fn_name: Box::new((Expr::VarRef("f"), SimpleSpan::new(7, 10))),
                                args: vec![],
                            }))
                        )),
                        "d"
                    ),
                    SimpleSpan::new(0, 14)
                ))]
            }
        );

        let res = parse_module(
            r#"
{
  "one"
}
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::Block(Block {
                        statements: vec![],
                        out: Some((
                            Box::new(Expr::Literal(Literal::Str("one"))),
                            SimpleSpan::new(5, 11)
                        ))
                    }),
                    SimpleSpan::new(0, 13)
                ))]
            }
        );

        let res = parse_module(
            r#"
{
  let b = 2;
  "one"
}
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::Block(Block {
                        statements: vec![Stmt::VarDef((
                            VarDef {
                                exported: false,
                                pattern: MatchPattern::Ident("b"),
                                def: Expr::Literal(Literal::Num(2.0))
                            },
                            SimpleSpan::new(5, 15)
                        )),],
                        out: Some((
                            Box::new(Expr::Literal(Literal::Str("one"))),
                            SimpleSpan::new(18, 24)
                        ))
                    }),
                    SimpleSpan::new(0, 26)
                ))]
            }
        );

        let res = parse_module(
            r#"
let todos = [
  { "id": 1 },
  { "id": 2 }
];
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::VarDef((
                    VarDef {
                        exported: false,
                        pattern: MatchPattern::Ident("todos"),
                        def: Expr::List(vec![
                            (
                                Expr::Object(vec![ObjectEntry::KeyValue(
                                    "id",
                                    (Expr::Literal(Literal::Num(1.)), SimpleSpan::new(24, 27))
                                )]),
                                SimpleSpan::new(14, 28)
                            ),
                            (
                                Expr::Object(vec![ObjectEntry::KeyValue(
                                    "id",
                                    (Expr::Literal(Literal::Num(2.)), SimpleSpan::new(39, 42))
                                )]),
                                SimpleSpan::new(32, 44)
                            ),
                        ])
                    },
                    SimpleSpan::new(1, 46)
                ))]
            }
        );

        let res = parse_module(
            r#"
if 1 < 2 {
  let b = 2;
  true
} else {
  false
}
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::If(IfBlock {
                        cond: Box::new((
                            Expr::Binary(BinaryExpr {
                                left: Box::new((
                                    Expr::Literal(Literal::Num(1.)),
                                    SimpleSpan::new(4, 6)
                                )),
                                op: BinaryOp::Lt,
                                right: Box::new((
                                    Expr::Literal(Literal::Num(2.)),
                                    SimpleSpan::new(7, 10),
                                ))
                            },),
                            SimpleSpan::new(4, 10)
                        )),
                        block: Block {
                            statements: vec![Stmt::VarDef((
                                VarDef {
                                    exported: false,
                                    pattern: MatchPattern::Ident("b"),
                                    def: Expr::Literal(Literal::Num(2.))
                                },
                                SimpleSpan::new(14, 24)
                            )),],
                            out: Some((
                                Box::new(Expr::Literal(Literal::Bool(true))),
                                SimpleSpan::new(27, 32)
                            ))
                        },
                        r#else: Some(Box::new(Expr::Block(Block {
                            statements: vec![],
                            out: Some((
                                Box::new(Expr::Literal(Literal::Bool(false))),
                                SimpleSpan::new(43, 49)
                            ))
                        }))),
                    }),
                    SimpleSpan::new(0, 51),
                )),]
            }
        );

        let res = parse_module(
            r#"
if true {
  one();
}
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::If(IfBlock {
                        cond: Box::new((Expr::Literal(Literal::Bool(true)), SimpleSpan::new(4, 9))),
                        block: Block {
                            statements: vec![Stmt::Expr((
                                Expr::FnCall(FnCall {
                                    fn_name: Box::new((
                                        Expr::VarRef("one"),
                                        SimpleSpan::new(13, 18)
                                    )),
                                    args: vec![]
                                }),
                                SimpleSpan::new(13, 18)
                            ))],
                            out: None,
                        },
                        r#else: None,
                    }),
                    SimpleSpan::new(0, 22),
                )),]
            }
        );

        let res = parse_module(
            r#"
let a = 2;
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::VarDef((
                    VarDef {
                        exported: false,
                        pattern: MatchPattern::Ident("a"),
                        def: Expr::Literal(Literal::Num(2.))
                    },
                    SimpleSpan::new(1, 11)
                ))]
            }
        );

        let res = parse_module(
            r#"
export let b = 2;
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::VarDef((
                    VarDef {
                        exported: true,
                        pattern: MatchPattern::Ident("b"),
                        def: Expr::Literal(Literal::Num(2.))
                    },
                    SimpleSpan::new(1, 18)
                ))]
            }
        );

        let res = parse_module(
            r#"
let a = 2;
a += 1;
b.c = 3;
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![
                    Stmt::VarDef((
                        VarDef {
                            exported: false,
                            pattern: MatchPattern::Ident("a"),
                            def: Expr::Literal(Literal::Num(2.))
                        },
                        SimpleSpan::new(1, 11)
                    )),
                    Stmt::Expr((
                        Expr::Binary(BinaryExpr {
                            left: Box::new((Expr::VarRef("a"), SimpleSpan::new(12, 14))),
                            op: BinaryOp::AssignAdd,
                            right: Box::new((
                                Expr::Literal(Literal::Num(1.)),
                                SimpleSpan::new(16, 18)
                            ))
                        }),
                        SimpleSpan::new(12, 18)
                    )),
                    Stmt::Expr((
                        Expr::Binary(BinaryExpr {
                            left: Box::new((
                                Expr::Field(Box::new(Expr::VarRef("b")), "c"),
                                SimpleSpan::new(20, 24)
                            )),
                            op: BinaryOp::Assign,
                            right: Box::new((
                                Expr::Literal(Literal::Num(3.)),
                                SimpleSpan::new(25, 27)
                            ))
                        }),
                        SimpleSpan::new(20, 27)
                    )),
                ]
            }
        );

        let res = parse_module(
            r#"
fn one() {
  a
}
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::FnDef((
                    FnDef {
                        exported: false,
                        name: "one",
                        args: vec![],
                        body: Block {
                            statements: vec![],
                            out: Some((Box::new(Expr::VarRef("a")), SimpleSpan::new(14, 16)))
                        }
                    },
                    SimpleSpan::new(1, 18),
                )),]
            }
        );

        let res = parse_module(
            r#"
export fn one() {}
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::FnDef((
                    FnDef {
                        exported: true,
                        name: "one",
                        args: vec![],
                        body: Block {
                            statements: vec![],
                            out: None
                        }
                    },
                    SimpleSpan::new(1, 20),
                )),]
            }
        );

        let res = parse_module(
            r#"
let a = |one| 2;
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::VarDef((
                    VarDef {
                        exported: false,
                        pattern: MatchPattern::Ident("a"),
                        def: Expr::Lambda(FnDef {
                            exported: false,
                            name: "",
                            args: vec!["one"],
                            body: Block {
                                statements: vec![],
                                out: Some((
                                    Box::new(Expr::Literal(Literal::Num(2.))),
                                    SimpleSpan::new(0, 0)
                                ))
                            }
                        })
                    },
                    SimpleSpan::new(1, 17),
                )),]
            }
        );

        let res = parse_module(
            r#"
call(|one| {
  let a = 1;
  a;
})
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::FnCall(FnCall {
                        fn_name: Box::new((Expr::VarRef("call"), SimpleSpan::new(0, 35))),
                        args: vec![(
                            Expr::Lambda(FnDef {
                                exported: false,
                                name: "",
                                args: vec!["one"],
                                body: Block {
                                    statements: vec![
                                        Stmt::VarDef((
                                            VarDef {
                                                exported: false,
                                                pattern: MatchPattern::Ident("a"),
                                                def: Expr::Literal(Literal::Num(1.)),
                                            },
                                            SimpleSpan::new(16, 26)
                                        )),
                                        Stmt::Expr((Expr::VarRef("a"), SimpleSpan::new(29, 30))),
                                    ],
                                    out: None,
                                }
                            }),
                            SimpleSpan::new(6, 33)
                        )],
                    }),
                    SimpleSpan::new(0, 35)
                )),]
            }
        );

        let res = parse_module(
            r#"
fn doc() {
  let a = 1;
  a
}
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::FnDef((
                    FnDef {
                        exported: false,
                        name: "doc",
                        args: vec![],
                        body: Block {
                            statements: vec![Stmt::VarDef((
                                VarDef {
                                    exported: false,
                                    pattern: MatchPattern::Ident("a"),
                                    def: Expr::Literal(Literal::Num(1.0)),
                                },
                                SimpleSpan::new(14, 24)
                            )),],
                            out: Some((Box::new(Expr::VarRef("a")), SimpleSpan::new(27, 29)))
                        }
                    },
                    SimpleSpan::new(1, 31),
                )),]
            }
        );

        let res = parse_module(
            r#"
a.b.c()
        "#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::FnCall(FnCall {
                        fn_name: Box::new((
                            Expr::Field(
                                Box::new(Expr::Field(Box::new(Expr::VarRef("a")), "b")),
                                "c"
                            ),
                            SimpleSpan::new(0, 17)
                        )),
                        args: vec![]
                    }),
                    SimpleSpan::new(0, 17)
                )),]
            }
        );

        let res = parse_module(
            r#"
<span>Hello</span>
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::Jsx(JsxDef {
                        name: "span",
                        props: vec![],
                        children: vec![Expr::Literal(Literal::Str("Hello"))]
                    }),
                    SimpleSpan::new(0, 20)
                )),]
            }
        );

        let res = parse_module(
            r#"
<div class="one">
  Hello
</div>
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::Jsx(JsxDef {
                        name: "div",
                        props: vec![("class", Expr::Literal(Literal::Str("one"))),],
                        children: vec![Expr::Literal(Literal::Str("Hello")),]
                    }),
                    SimpleSpan::new(0, 34)
                )),]
            }
        );

        let res = parse_module(
            r#"
<div class="one">
  Hello {world}
  <hr />
</div>
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::Jsx(JsxDef {
                        name: "div",
                        props: vec![("class", Expr::Literal(Literal::Str("one"))),],
                        children: vec![
                            Expr::Literal(Literal::Str("Hello ")),
                            Expr::VarRef("world"),
                            Expr::Jsx(JsxDef {
                                name: "hr",
                                props: vec![],
                                children: vec![]
                            })
                        ]
                    }),
                    SimpleSpan::new(0, 51)
                )),]
            }
        );

        let res = parse_module(
            r#"
<>
  <main></main>
  <footer></footer>
</>
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::Jsx(JsxDef {
                        name: "",
                        props: vec![],
                        children: vec![
                            Expr::Jsx(JsxDef {
                                name: "main",
                                props: vec![],
                                children: vec![]
                            }),
                            Expr::Jsx(JsxDef {
                                name: "footer",
                                props: vec![],
                                children: vec![]
                            }),
                        ]
                    }),
                    SimpleSpan::new(0, 44)
                )),]
            }
        );

        let res = parse_module(
            r#"
<button disabled test={false}>Hello</button>
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::Jsx(JsxDef {
                        name: "button",
                        props: vec![
                            ("disabled", Expr::Literal(Literal::Bool(true))),
                            ("test", Expr::Literal(Literal::Bool(false)))
                        ],
                        children: vec![Expr::Literal(Literal::Str("Hello"))]
                    }),
                    SimpleSpan::new(0, 46)
                )),]
            }
        );

        let res = parse_module(
            r#"
<span>Result: {1 + 2}</span>
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::Jsx(JsxDef {
                        name: "span",
                        props: vec![],
                        children: vec![
                            Expr::Literal(Literal::Str("Result: ")),
                            Expr::Binary(BinaryExpr {
                                left: Box::new((
                                    Expr::Literal(Literal::Num(1.)),
                                    SimpleSpan::new(16, 18)
                                )),
                                op: BinaryOp::Add,
                                right: Box::new((
                                    Expr::Literal(Literal::Num(2.)),
                                    SimpleSpan::new(19, 21)
                                ))
                            })
                        ]
                    }),
                    SimpleSpan::new(0, 30)
                )),]
            }
        );

        let res = parse_module(
            r#"
<ul>
  {list.map(|el| <li>{el}</li>)}
</ul>
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::Jsx(JsxDef {
                        name: "ul",
                        props: vec![],
                        children: vec![Expr::FnCall(FnCall {
                            fn_name: Box::new((
                                Expr::Field(Box::new(Expr::VarRef("list")), "map"),
                                SimpleSpan::new(9, 37)
                            )),
                            args: vec![(
                                Expr::Lambda(FnDef {
                                    exported: false,
                                    name: "",
                                    args: vec!["el"],
                                    body: Block {
                                        statements: vec![],
                                        out: Some((
                                            Box::new(Expr::Jsx(JsxDef {
                                                name: "li",
                                                props: vec![],
                                                children: vec![Expr::VarRef("el")]
                                            })),
                                            SimpleSpan::new(0, 0)
                                        ))
                                    }
                                }),
                                SimpleSpan::new(18, 36)
                            )]
                        })]
                    }),
                    SimpleSpan::new(0, 45)
                )),]
            }
        );

        let res = parse_module(
            r#"
// One
// Two

// Three
let a = 1;
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![
                    Stmt::LineComment("One"),
                    Stmt::LineComment("Two"),
                    Stmt::LineComment("Three"),
                    Stmt::VarDef((
                        VarDef {
                            exported: false,
                            pattern: MatchPattern::Ident("a"),
                            def: Expr::Literal(Literal::Num(1.))
                        },
                        SimpleSpan::new(25, 35)
                    ))
                ]
            }
        );

        let res = parse_module(
            r#"
import { useState } from "preact/hooks";
import {one, two,three,} from "./hello.js";
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![
                    Import {
                        refs: vec!["useState"],
                        from: "preact/hooks"
                    },
                    Import {
                        refs: vec!["one", "two", "three"],
                        from: "./hello.js"
                    },
                ],
                body: vec![]
            }
        );

        let res = parse_module(
            r#"
let [a, b] = [1, 2, 3];
let [[a, b], c] = [[1, 2], 3,];
let [a, b, ...rest] = [1, 2, 3];
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![
                    Stmt::VarDef((
                        VarDef {
                            exported: false,
                            pattern: MatchPattern::Slice {
                                items: vec![MatchPattern::Ident("a"), MatchPattern::Ident("b"),],
                                rest: None
                            },
                            def: Expr::List(vec![
                                (Expr::Literal(Literal::Num(1.)), SimpleSpan::new(15, 16)),
                                (Expr::Literal(Literal::Num(2.)), SimpleSpan::new(18, 19)),
                                (Expr::Literal(Literal::Num(3.)), SimpleSpan::new(21, 22)),
                            ])
                        },
                        SimpleSpan::new(1, 24)
                    )),
                    Stmt::VarDef((
                        VarDef {
                            exported: false,
                            pattern: MatchPattern::Slice {
                                items: vec![
                                    MatchPattern::Slice {
                                        items: vec![
                                            MatchPattern::Ident("a"),
                                            MatchPattern::Ident("b"),
                                        ],
                                        rest: None
                                    },
                                    MatchPattern::Ident("c"),
                                ],
                                rest: None
                            },
                            def: Expr::List(vec![
                                (
                                    Expr::List(vec![
                                        (Expr::Literal(Literal::Num(1.)), SimpleSpan::new(45, 46)),
                                        (Expr::Literal(Literal::Num(2.)), SimpleSpan::new(48, 49)),
                                    ]),
                                    SimpleSpan::new(44, 50)
                                ),
                                (Expr::Literal(Literal::Num(3.)), SimpleSpan::new(52, 53)),
                            ])
                        },
                        SimpleSpan::new(25, 56)
                    )),
                    Stmt::VarDef((
                        VarDef {
                            exported: false,
                            pattern: MatchPattern::Slice {
                                items: vec![MatchPattern::Ident("a"), MatchPattern::Ident("b"),],
                                rest: Some("rest")
                            },
                            def: Expr::List(vec![
                                (Expr::Literal(Literal::Num(1.)), SimpleSpan::new(80, 81)),
                                (Expr::Literal(Literal::Num(2.)), SimpleSpan::new(83, 84)),
                                (Expr::Literal(Literal::Num(3.)), SimpleSpan::new(86, 87)),
                            ])
                        },
                        SimpleSpan::new(57, 89)
                    ))
                ]
            }
        );

        let res = parse_module(
            r#"
if false {
  1
} else if true {
  2
} else {
  3
};
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![Stmt::Expr((
                    Expr::If(IfBlock {
                        cond: Box::new((
                            Expr::Literal(Literal::Bool(false)),
                            SimpleSpan::new(4, 10)
                        )),
                        block: Block {
                            statements: vec![],
                            out: Some((
                                Box::new(Expr::Literal(Literal::Num(1.))),
                                SimpleSpan::new(14, 16)
                            ))
                        },
                        r#else: Some(Box::new(Expr::If(IfBlock {
                            cond: Box::new((
                                Expr::Literal(Literal::Bool(true)),
                                SimpleSpan::new(26, 31)
                            )),
                            block: Block {
                                statements: vec![],
                                out: Some((
                                    Box::new(Expr::Literal(Literal::Num(2.))),
                                    SimpleSpan::new(35, 37)
                                ))
                            },
                            r#else: Some(Box::new(Expr::Block(Block {
                                statements: vec![],
                                out: Some((
                                    Box::new(Expr::Literal(Literal::Num(3.))),
                                    SimpleSpan::new(48, 50)
                                ))
                            })))
                        }))),
                    }),
                    SimpleSpan::new(1, 51)
                ))]
            }
        );

        let res = parse_module(
            r#"
{ "a": 1, ...b };
{ ...b, "a": 1 };
"#,
        )
        .unwrap();
        assert_eq!(
            res,
            Module {
                imports: vec![],
                body: vec![
                    Stmt::Expr((
                        Expr::Object(vec![
                            ObjectEntry::KeyValue(
                                "a",
                                (Expr::Literal(Literal::Num(1.)), SimpleSpan::new(7, 9))
                            ),
                            ObjectEntry::Rest("b"),
                        ]),
                        SimpleSpan::new(1, 17)
                    )),
                    Stmt::Expr((
                        Expr::Object(vec![
                            ObjectEntry::Rest("b"),
                            ObjectEntry::KeyValue(
                                "a",
                                (Expr::Literal(Literal::Num(1.)), SimpleSpan::new(31, 34))
                            ),
                        ]),
                        SimpleSpan::new(19, 35)
                    ))
                ]
            }
        );
    }

    fn parse_module<'src>(src: &'src str) -> Result<Module, Vec<Rich<'src, char>>> {
        let (parsed, errs) = module_parser().parse(src).into_output_errors();

        errs.iter().for_each(|e| {
            Report::build(ReportKind::Error, /* filename */ (), e.span().start)
                .with_message(e.to_string())
                .with_label(
                    Label::new((/* filename */ (), e.span().into_range()))
                        .with_message(e.reason().to_string())
                        .with_color(Color::Red),
                )
                .with_labels(e.contexts().map(|(label, span)| {
                    Label::new((/* filename */ (), span.into_range()))
                        .with_message(format!("while parsing this {}", label))
                        .with_color(Color::Yellow)
                }))
                .finish()
                // .print(sources([(filename.clone(), src.clone())]))
                .eprint(Source::from(&src))
                .unwrap();
        });

        if errs.is_empty() {
            if let Some(p) = parsed {
                Ok(p)
            } else {
                Err(vec![])
            }
        } else {
            Err(errs)
        }
    }
}
