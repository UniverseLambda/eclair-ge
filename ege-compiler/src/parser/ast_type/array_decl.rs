use serde::Serialize;

use crate::{
    lexer::TokenTypeId,
    parser::{expect_token, expect_token_content, expect_token_type},
};

use super::{Expr, Ident, Parsable};

pub type ArrayDecls = Vec<ArrayDecl>;

#[derive(Debug, Clone, Serialize)]
pub struct ArrayDecl {
    pub ident: Ident,
    pub size: Vec<Expr>,
}

impl Parsable for ArrayDecls {
    fn parse(parser: &mut crate::parser::Parser<impl std::io::Read>) -> anyhow::Result<Self> {
        let keyword = parser.required_token()?;
        parser.consume_token();
        expect_token(&keyword, TokenTypeId::Keyword, "Dim")?;

        let mut decls = ArrayDecls::new();

        loop {
            let ident = Ident::parse(parser)?;
            let parenth = parser.required_token()?;
            expect_token(&parenth, TokenTypeId::Operator, "(")?;
            parser.consume_token();

            let mut size = vec![];

            loop {
                let size_value = Expr::parse(parser)?;

                size.push(size_value);

                let operator = parser.required_token()?;
                expect_token_type(&operator, TokenTypeId::Operator)?;

                if operator.content == ")" {
                    break;
                }

                expect_token_content(&operator, ",")?;
                parser.consume_token();
            }

            let parenth = parser.required_token()?;
            expect_token(&parenth, TokenTypeId::Operator, ")")?;
            parser.consume_token();

            decls.push(ArrayDecl { ident, size });

            // No need to pop the last parenth, as Expr::parse does treat this as a parenth expr.

            if let Some(token) = parser.current_token()? {
                if token.is(TokenTypeId::Operator, ",") {
                    parser.consume_token();
                    continue;
                }

                expect_token_type(&token, TokenTypeId::EndOfLine)?;
            }

            break;
        }

        Ok(decls)
    }
}
