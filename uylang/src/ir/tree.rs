use std::collections::HashSet;

use crate::parser::ast::{BinaryOp, UnaryOp};

#[derive(PartialEq, Debug)]
pub struct Module {
    pub imports: Vec<Import>,
    pub fns: Vec<(String, FnDef)>,
    pub statements: Vec<Stmt>,
}

#[derive(Debug, PartialEq)]
pub struct Import {
    /// Variable or function references
    pub refs: Vec<String>,
    pub from: String,
}

#[derive(PartialEq, Debug, Clone)]
pub struct FnDef {
    pub exported: bool,
    pub args: Vec<String>,
    pub body: Block,
    pub meta: FnMeta,
}

#[derive(Default, PartialEq, Debug, Clone)]
pub struct FnMeta {
    /// List of variables defined in this scope.
    pub defined_vars: HashSet<String>,
    /// List of referenced variables defined outside of current function scope.
    pub global_vars: HashSet<String>,
    /// Should current function be hydrated in the browser.
    pub needs_hydration: bool,
}

#[derive(PartialEq, Debug, Clone)]
pub struct VarDef {
    pub exported: bool,
    pub pattern: MatchPattern,
    pub def: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub enum MatchPattern {
    /// Skip value from this pattern
    Empty,
    /// Variable reference
    Ident(String),
    /// Slice pattern with an optional rest item
    ///
    /// const [a, b, ...others] = [1, 2, 3, 4];
    /// console.log(a); // 1
    /// console.log(others); // [3, 4]
    Slice {
        items: Vec<Self>,
        rest: Option<String>,
    },
}

impl<'src> From<crate::parser::ast::MatchPattern<'src>> for MatchPattern {
    fn from(value: crate::parser::ast::MatchPattern) -> Self {
        match value {
            crate::parser::ast::MatchPattern::Empty => Self::Empty,
            crate::parser::ast::MatchPattern::Ident(ident) => Self::Ident(ident.into()),
            crate::parser::ast::MatchPattern::Slice { items, rest } => Self::Slice {
                items: items.into_iter().map(MatchPattern::from).collect(),
                rest: rest.map(String::from),
            },
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Stmt {
    LineComment(String),
    VarDef(VarDef),
    Expr(Expr),
}

#[derive(PartialEq, Debug, Clone)]
pub struct Block {
    pub fns: Vec<(String, FnDef)>,
    pub statements: Vec<Stmt>,
    pub out: Option<Box<Expr>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct IfBlock {
    pub cond: Box<Expr>,
    pub block: Block,
    pub r#else: Option<Box<Expr>>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    VarRef(String),
    Grouped(Box<Self>),
    List(Vec<Self>),
    Object(Vec<ObjectEntry>),
    Field(Box<Self>, String),
    Index(Box<Self>, Box<Self>),
    Block(Block),
    If(IfBlock),
    Jsx(JsxDef),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    FnCall(FnCall),
    Lambda(FnDef),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ObjectEntry {
    KeyValue(String, Expr),
    /// Rest/spread operator
    Rest(String),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    Null,
    Bool(bool),
    Num(f64),
    Str(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub op: BinaryOp,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Box<Expr>,
}

// See how Dioxus verifies JSX elements: https://github.com/DioxusLabs/dioxus/tree/master/packages/html#how-it-works
#[derive(PartialEq, Debug, Clone)]
pub struct JsxDef {
    pub name: String,
    pub props: Vec<(String, Expr)>,
    pub children: Vec<Expr>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct FnCall {
    pub fn_name: Box<Expr>,
    pub args: Vec<Expr>,
}
