use serde::Serialize;

use crate::lexer::IdentTyping;

#[derive(Debug, Clone, Serialize)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, Serialize)]
pub enum Statement {
    FunctionDecl(FunctionDecl),
    FunctionCall(FunctionCall),
    VarAssign(VarAssign),
    ArrayDecl(ArrayDecl),
    If(If),
    For(ForLoop),
    Repeat(RepeatLoop),
}

#[derive(Debug, Clone, Serialize)]
pub struct If {
    pub cond: Expr,
    pub statements: Vec<Statement>,
    pub else_if: Vec<Otherwise>,
}

#[derive(Debug, Clone, Serialize)]
pub enum Otherwise /* Feeling like a clown today */ {
    ElseIf {
        cond: Expr,
        statements: Vec<Statement>,
    },
    Else {
        statements: Vec<Statement>,
    },
}

impl Otherwise {
    pub fn statements(&self) -> &[Statement] {
        match self {
            Otherwise::ElseIf { statements, .. } => &statements,
            Otherwise::Else { statements } => &statements,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct ArrayDecl {
    pub ident: Ident,
    pub size: Expr,
}

#[derive(Debug, Clone, Serialize)]
pub struct ForLoop {
    pub name: Ident,
    pub from: Expr,
    pub to: Expr,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, Serialize)]
pub struct RepeatLoop {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Ident {
    pub name: String,
    pub ident_type: Option<IdentTyping>,
}

#[derive(Debug, Clone, Serialize)]
pub enum Expr {
    Function(FunctionCall),
    String(String),
    Integer(i64),
    Float(f64),
    Variable(Ident),
    Binary(BinaryExpr),
    Path(Vec<Ident>),
}

#[derive(Debug, Clone, Serialize)]
pub struct FunctionDecl {
    pub ident: Ident,
    pub params: Vec<(Ident, Expr)>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, Serialize)]
pub struct FunctionCall {
    pub ident: Ident,
    pub params: Vec<Expr>,
}

#[derive(Debug, Clone, Serialize)]
pub enum BinaryExprOp {
    Add,
    Sub,
    Mult,
    Div,
    Less,
    LessOrEqual,
    Equal,
    GreaterOrEqual,
    Greater,
    Different,
    BoolAnd,
    BoolOr,
    Xor,
}

#[derive(Debug, Clone, Serialize)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub op: BinaryExprOp,
}

#[derive(Debug, Clone, Default, Serialize)]
pub enum VarScope {
    Global,
    #[default]
    Local,
}

#[derive(Debug, Clone, Serialize)]
pub struct VarAssign {
    pub name: Ident,
    pub scope: Option<VarScope>,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone, Serialize)]
pub struct StructDecl {
    pub name: Ident,
    pub fields: Vec<Ident>,
}
