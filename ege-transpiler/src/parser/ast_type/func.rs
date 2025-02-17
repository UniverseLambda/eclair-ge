use std::io::Read;

use anyhow::Result;
use log::debug;
use serde::Serialize;

use crate::{
    lexer::TokenTypeId,
    parser::{expect_token, expect_token_type, Parser},
};

use super::{Expr, Ident, Parsable, Statement};

#[derive(Debug, Clone, Serialize)]
pub struct FunctionCall {
    pub ident: Ident,
    pub params: Vec<Expr>,
}

impl Parsable for FunctionCall {
    fn parse(parser: &mut Parser<impl Read>) -> Result<Self> {
        debug!(
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

            expect_token(&parenth, TokenTypeId::Operator, ")")?;
            parser.consume_token();
        }

        Ok(FunctionCall { ident, params })
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct FunctionDecl {
    pub ident: Ident,
    pub params: Vec<(Ident, Option<Expr>)>,
    pub statements: Vec<Statement>,
}

impl Parsable for FunctionDecl {
    fn parse(parser: &mut Parser<impl Read>) -> Result<Self> {
        let func_keyword = parser.required_token()?;
        expect_token(&func_keyword, TokenTypeId::Keyword, "Function")?;
        parser.consume_token();

        let ident = Ident::parse(parser)?;

        let parenth_open = parser.required_next_token()?;
        expect_token(&parenth_open, TokenTypeId::Operator, "(")?;
        parser.consume_token();

        let mut params = Vec::new();

        loop {
            let current_token = parser.required_token()?;

            if current_token.is(TokenTypeId::Operator, ")") {
                parser.consume_token();
                break;
            }

            let param_name = Ident::parse(parser)?;
            let eq_or_comma = parser.required_next_token()?;

            if eq_or_comma.is(TokenTypeId::Operator, ",")
                || eq_or_comma.is(TokenTypeId::Operator, ")")
            {
                params.push((param_name, None));

                parser.consume_token();
                continue;
            }

            expect_token(&eq_or_comma, TokenTypeId::Operator, "=")?;
            parser.consume_token();

            let default_value = Expr::parse(parser)?;

            params.push((param_name, Some(default_value)));

            if parser.required_token()?.is(TokenTypeId::Operator, ",") {
                parser.consume_token();
            }
        }

        let lf = parser.required_token()?;
        expect_token_type(&lf, TokenTypeId::EndOfLine)?;
        parser.consume_token();

        let (statements, _) = parser.parse_statement_block("Function", false, &[])?;

        Ok(Self {
            ident,
            params,
            statements,
        })
    }
}
