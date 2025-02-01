use std::io::Read;

use anyhow::Result;
use serde::Serialize;

use crate::{lexer::TokenTypeId, parser::Parser};

use super::{Expr, Ident, Parsable, Statement};

#[derive(Debug, Clone, Serialize)]
pub struct FunctionCall {
    pub ident: Ident,
    pub params: Vec<Expr>,
}

impl Parsable for FunctionCall {
    fn parse(parser: &mut Parser<impl Read>) -> Result<Self> {
        eprintln!(
            "parse_function_call: current_token: {:?}, peeked: {:?}",
            parser.current_token()?,
            parser.peek_token()?
        );

        let ident = Ident::parse(parser)?;

        let expr_start = parser.required_token()?;

        let ends_with_parenth = if expr_start.is(TokenTypeId::Operator, "(") {
            parser.consume_token();

            true
        } else {
            false
        };

        let mut params = Vec::new();

        loop {
            let Some(expr_start) = parser.current_token()? else {
                break;
            };

            if (ends_with_parenth && expr_start.is(TokenTypeId::Operator, ")"))
                || expr_start.is(TokenTypeId::EndOfLine, "\n")
            {
                break;
            }

            params.push(Expr::parse(parser)?);

            if let Some(token) = parser.current_token()? {
                if token.is(TokenTypeId::Operator, ",") {
                    parser.consume_token();
                }
            }
        }

        if ends_with_parenth {
            let parenth = parser.required_token()?;

            parser.expect_token(&parenth, TokenTypeId::Operator, ")")?;
            parser.consume_token();
        }

        Ok(FunctionCall { ident, params })
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct FunctionDecl {
    pub ident: Ident,
    pub params: Vec<(Ident, Expr)>,
    pub statements: Vec<Statement>,
}
