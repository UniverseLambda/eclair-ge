use serde::Serialize;

use crate::{
    lexer::TokenTypeId,
    parser::{ext::TokenExt, Parser},
};

use super::{Expr, Parsable};

#[derive(Debug, Clone, Serialize)]
pub struct Return {
    pub value: Option<Expr>,
}

impl Parsable for Return {
    fn parse(parser: &mut Parser<impl std::io::Read>) -> anyhow::Result<Self> {
        parser
            .required_token()
            .expect_token(TokenTypeId::Keyword, "Return")?;

        let value = if parser
            .required_next_token()?
            .is(TokenTypeId::EndOfLine, "\n")
        {
            None
        } else {
            Some(Expr::parse(parser)?)
        };

        parser
            .required_token()
            .expect_type(TokenTypeId::EndOfLine)?;

        Ok(Self { value })
    }
}
