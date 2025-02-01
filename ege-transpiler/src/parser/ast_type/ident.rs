use std::io::Read;

use anyhow::Result;
use serde::Serialize;

use crate::{
    lexer::{IdentTyping, TokenType, TokenTypeId},
    parser::Parser,
};

use super::Parsable;

#[derive(Debug, Clone, Serialize)]
pub struct Ident {
    pub name: String,
    pub ident_type: Option<IdentTyping>,
}

impl Parsable for Ident {
    fn parse(parser: &mut Parser<impl Read>) -> Result<Self> {
        let ident = parser.required_token()?;

        parser
            .expect_any_token_type(&ident, &[TokenTypeId::Ident, TokenTypeId::FunctionKeyword])?;

        let ident_type = match ident.token_type {
            TokenType::Ident(ident_type) => ident_type,
            TokenType::FunctionKeyword => None,
            _ => unreachable!(),
        };

        parser.consume_token();

        Ok(Ident {
            name: ident.content,
            ident_type,
        })
    }
}
