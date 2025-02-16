use anyhow::bail;
use serde::Serialize;

use crate::{
    lexer::{TokenType, TokenTypeId},
    parser::expect_token,
};

use super::{Expr, Ident, Parsable};

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

        let ident = Ident::parse(parser)?;

        let mut res = VarAssign {
            name: ident,
            scope,
            value: None,
        };

        let Some(operator) = parser.current_token()? else {
            return Ok(res);
        };

        if let TokenType::EndOfLine = operator.token_type {
            return Ok(res);
        }

        expect_token(&operator, TokenTypeId::Operator, "=")?;
        parser.consume_token();

        res.value = Some(Expr::parse(parser)?);

        Ok(res)
    }
}
