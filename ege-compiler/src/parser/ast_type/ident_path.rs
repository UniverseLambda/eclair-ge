use anyhow::Result;
use serde::Serialize;

use crate::{
    lexer::{IdentTyping, Token, TokenTypeId},
    parser::{expect_any_token_type, Parser},
};

use super::Parsable;

#[derive(Debug, Clone, Serialize)]
pub struct IdentPath {
    pub components: Vec<String>,
    pub final_type: Option<IdentTyping>,
}

impl IdentPath {
    pub fn from_token(token: Token) -> Result<Self> {
        expect_any_token_type(&token, &[TokenTypeId::Path, TokenTypeId::Ident])?;

        Ok(match token.token_type {
            crate::lexer::TokenType::Ident(id_type) => {
                Self::from_single_name_and_type(token.content, id_type)
            }
            crate::lexer::TokenType::Path(components, id_type) => Self {
                components,
                final_type: id_type,
            },
            _ => unreachable!(),
        })
    }

    pub fn from_single_name_and_type(name: String, ident_type: Option<IdentTyping>) -> Self {
        Self {
            final_type: ident_type,
            components: vec![name],
        }
    }
}

impl Parsable for IdentPath {
    fn parse(parser: &mut Parser<impl std::io::Read>) -> Result<Self> {
        let next_token = parser.required_token()?;
        parser.consume_token();

        Self::from_token(next_token)
    }
}
