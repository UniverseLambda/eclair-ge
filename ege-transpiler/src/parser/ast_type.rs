mod expr;
mod func;
mod ident;
mod if_statement;
mod loops;
mod packed_type;
mod var_assign;

pub use expr::*;
pub use func::*;
pub use ident::*;
pub use if_statement::*;
pub use loops::*;
pub use packed_type::*;
pub use var_assign::*;

use anyhow::Result;
use std::io::Read;

use serde::Serialize;

use super::Parser;

pub trait Parsable: Sized {
    fn parse(parser: &mut Parser<impl Read>) -> Result<Self>;
}

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
pub struct ArrayDecl {
    pub ident: Ident,
    pub size: Expr,
}
