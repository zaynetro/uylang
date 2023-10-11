use chumsky::span::SimpleSpan;

pub type Span = SimpleSpan<usize>;

/// Include spans to provide useful errors
pub type Spanned<T> = (T, Span);

#[derive(Debug, PartialEq)]
pub struct Module<'src> {
    pub imports: Vec<Import<'src>>,
    pub body: Vec<Stmt<'src>>,
}

#[derive(Debug, PartialEq)]
pub struct Import<'src> {
    /// Variable or function references
    pub refs: Vec<&'src str>,
    pub from: &'src str,
}

/// A single statement
#[derive(Debug, PartialEq, Clone)]
pub enum Stmt<'src> {
    /// A single line comment
    LineComment(&'src str),
    /// Variable definition
    VarDef(Spanned<VarDef<'src>>),
    /// Function definition
    FnDef(Spanned<FnDef<'src>>),
    /// Expression
    Expr(Spanned<Expr<'src>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarDef<'src> {
    pub exported: bool,
    pub pattern: MatchPattern<'src>,
    pub def: Expr<'src>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum MatchPattern<'src> {
    /// Skip value from this pattern
    Empty,
    /// Variable reference
    Ident(&'src str),
    /// Slice pattern with an optional rest item
    ///
    /// const [a, b, ...others] = [1, 2, 3, 4];
    /// console.log(a); // 1
    /// console.log(others); // [3, 4]
    Slice {
        items: Vec<Self>,
        rest: Option<&'src str>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnDef<'src> {
    pub exported: bool,
    pub name: &'src str,
    pub args: Vec<&'src str>,
    pub body: Block<'src>,
}

/// A single expression
#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'src> {
    // TODO: remove this (replace with Expr::Literal(Literal::Null) ?)
    Error,
    Literal(Literal<'src>),
    VarRef(&'src str),
    /// Expression in parentheses
    Grouped(Box<Self>),
    List(Vec<Spanned<Self>>),

    Field(Box<Self>, &'src str),
    Index(Box<Self>, Box<Self>),
    Object(Vec<ObjectEntry<'src>>),
    Binary(BinaryExpr<'src>),
    Unary(UnaryExpr<'src>),
    FnCall(FnCall<'src>),
    Block(Block<'src>),
    If(IfBlock<'src>),
    Jsx(JsxDef<'src>),
    Lambda(FnDef<'src>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ObjectEntry<'src> {
    KeyValue(&'src str, Spanned<Expr<'src>>),
    /// Rest/spread operator
    Rest(&'src str),
}

/// A block expression (everything inside curly braces).
#[derive(Debug, PartialEq, Clone)]
pub struct Block<'src> {
    pub statements: Vec<Stmt<'src>>,
    /// Block could have an optional last expression that doesn't end with a semicolon.
    /// This expression's result is implicit return value.
    pub out: Option<(Box<Expr<'src>>, SimpleSpan)>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct IfBlock<'src> {
    pub cond: Box<Spanned<Expr<'src>>>,
    pub block: Block<'src>,
    pub r#else: Option<Box<Expr<'src>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct JsxDef<'src> {
    pub name: &'src str,
    pub props: Vec<(&'src str, Expr<'src>)>,
    pub children: Vec<Expr<'src>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpr<'src> {
    pub left: Box<Spanned<Expr<'src>>>,
    pub op: BinaryOp,
    pub right: Box<Spanned<Expr<'src>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpr<'src> {
    pub op: UnaryOp,
    pub expr: Box<Spanned<Expr<'src>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnCall<'src> {
    pub fn_name: Box<Spanned<Expr<'src>>>,
    pub args: Vec<Spanned<Expr<'src>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    // Algebraic
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %

    // Boolean
    Eq,    // ==
    NotEq, // !=
    Lt,    // <
    Lte,   // <=
    Gt,    // >
    Gte,   // >=
    And,   // &&
    Or,    // ||

    // Assignment
    Assign, // =

    // Assignment algebraic
    AssignAdd, // +=
    AssignSub, // -=
    AssignMul, // *=
    AssignDiv, // /=
    AssignMod, // %=
}

impl BinaryOp {
    pub fn as_str(&self) -> &'static str {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::Eq => "==",
            BinaryOp::NotEq => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Lte => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Gte => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
            BinaryOp::Assign => "=",
            BinaryOp::AssignAdd => "+=",
            BinaryOp::AssignSub => "-=",
            BinaryOp::AssignMul => "*=",
            BinaryOp::AssignDiv => "/=",
            BinaryOp::AssignMod => "%=",
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Neg, // -
    Not, // !
}

impl UnaryOp {
    pub fn as_str(&self) -> &'static str {
        match self {
            UnaryOp::Neg => "-",
            UnaryOp::Not => "!",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'src> {
    Null,
    Bool(bool),
    Num(f64),
    Str(&'src str),
}
