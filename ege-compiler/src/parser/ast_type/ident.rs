use std::io::Read;

use anyhow::Result;
use serde::Serialize;

use crate::{
    lexer::{IdentTyping, Token, TokenType, TokenTypeId},
    parser::{expect_any_token_type, expect_token_type, Parser},
};

use super::Parsable;

#[derive(Debug, Clone, Serialize)]
pub struct Ident {
    pub name: String,
    pub ident_type: Option<IdentTyping>,
}

impl Ident {
    pub fn from_token(token: Token) -> Result<Self> {
        expect_token_type(&token, TokenTypeId::Ident)?;

        let v = token.content;
        let TokenType::Ident(ident_type) = token.token_type else {
            unreachable!()
        };

        Ok(Ident {
            name: v,
            ident_type,
        })
    }
}

impl Parsable for Ident {
    fn parse(parser: &mut Parser<impl Read>) -> Result<Self> {
        let ident = parser.required_token()?;

        expect_any_token_type(&ident, &[TokenTypeId::Ident, TokenTypeId::FunctionKeyword])?;

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
