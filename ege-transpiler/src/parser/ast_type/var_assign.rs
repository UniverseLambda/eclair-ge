use anyhow::bail;
use serde::Serialize;

use crate::{
    lexer::{TokenType, TokenTypeId},
    parser::expect_token,
};

use super::{Expr, IdentPath, Parsable};

#[derive(Debug, Clone, Default, Serialize)]
pub enum VarScope {
    Global,
    #[default]
    Local,
}

#[derive(Debug, Clone, Serialize)]
pub struct VarAssign {
    pub scope: Option<VarScope>,
    pub defs: Vec<(IdentPath, Option<Expr>)>,
}

impl Parsable for VarAssign {
    fn parse(parser: &mut crate::parser::Parser<impl std::io::Read>) -> anyhow::Result<Self> {
        eprintln!(
            "parse_expr: current_token: {:?}, peeked: {:?}",
            parser.current_token()?,
            parser.peek_token()?
        );

        let first_token = parser.required_token()?;

        let scope = if let TokenType::Keyword = first_token.token_type {
            parser.next_token()?;

            match first_token.content.as_str() {
                "Global" => Some(VarScope::Global),
                "Local" => Some(VarScope::Local),
                v => bail!("Unexpected token: {v}, expected Global or Local"),
            }
        } else {
            None
        };

        let mut defs = Vec::new();

        loop {
            let ident = IdentPath::parse(parser)?;
            let Some(operator) = parser.current_token()? else {
                defs.push((ident, None));
                break;
            };

            if let TokenType::EndOfLine = operator.token_type {
                defs.push((ident, None));
                break;
            }

            if operator.is(TokenTypeId::Operator, ",") {
                defs.push((ident, None));
                continue;
            }

            expect_token(&operator, TokenTypeId::Operator, "=")?;
            parser.consume_token();

            let value = Expr::parse(parser)?;

            defs.push((ident, Some(value)));

            let next_token = parser.current_token()?;

            if let Some(token) = next_token {
                if token.is(TokenTypeId::Operator, ",") {
                    parser.consume_token();
                    continue;
                }
            }
            break;
        }

        Ok(VarAssign { scope, defs })
    }
}
