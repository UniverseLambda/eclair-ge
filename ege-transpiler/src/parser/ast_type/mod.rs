mod expr;
mod func;
mod ident;
mod ident_path;
mod if_statement;
mod insert;
mod loops;
mod packed_type;
mod var_assign;

pub use expr::*;
pub use func::*;
pub use ident::*;
pub use ident_path::*;
pub use if_statement::*;
pub use insert::*;
pub use loops::*;
pub use packed_type::*;
pub use var_assign::*;

use anyhow::Result;
use std::io::Read;

use serde::Serialize;

use crate::lexer::TokenTypeId;

use super::{expect_token_content, expect_token_type, Parser};

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
    PackedDecl(PackedDecl),
    Insert(Insert),
    NoData(NoDataStatement),
}

#[derive(Debug, Clone, Serialize)]
pub enum NoDataStatement {
    Exit,
}

impl Parsable for NoDataStatement {
    fn parse(parser: &mut Parser<impl Read>) -> Result<Self> {
        let zarma = parser.required_token()?;
        expect_token_type(&zarma, TokenTypeId::Keyword)?;
        parser.consume_token();

        Ok(match zarma.content.as_str() {
            "Exit" => Self::Exit,
            _ => return Err(expect_token_content(&zarma, "Exit").unwrap_err()),
        })
    }
}

impl Statement {
    pub fn is_inlinable(&self) -> bool {
        match self {
            Statement::FunctionCall(_) => true,
            Statement::VarAssign(var_assign) => var_assign.scope.is_none(),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct ArrayDecl {
    pub ident: Ident,
    pub size: Expr,
}
