use serde::Serialize;

use crate::{
    lexer::TokenTypeId,
    parser::{Parser, expect_token, expect_token_content, expect_token_type},
};

use super::{Ident, Parsable};

#[derive(Debug, Clone, Serialize)]
pub struct PackedDecl {
    pub name: Ident,
    pub fields: Vec<Ident>,
}

/*
 *
 * Exemple:
 *
 * Type ConsoleMsg
 *     Field txt$
 *     Field isCommand%
 *     Field r%,g%,b%
 * End Type
 *
 *
*/

impl Parsable for PackedDecl {
    fn parse(parser: &mut Parser<impl std::io::Read>) -> anyhow::Result<Self> {
        let type_keyword = parser.required_token()?;
        parser.consume_token();
        expect_token(&type_keyword, TokenTypeId::Keyword, "Type")?;

        let type_name = Ident::parse(parser)?;
        // FIXME: generate error when the type is not None

        let eof = parser.required_next_token()?;
        expect_token_type(&eof, TokenTypeId::EndOfLine)?;
        parser.consume_token();

        let mut fields = Vec::new();

        loop {
            let keyword = parser.required_token()?;
            parser.consume_token();
            expect_token_type(&keyword, TokenTypeId::Keyword)?;
            expect_token_content(&keyword, "Field").or(expect_token_content(&keyword, "End"))?;

            if keyword.content == "End" {
                break;
            }

            loop {
                fields.push(Ident::parse(parser)?);

                let next_token = parser.required_token()?;
                parser.consume_token();

                if next_token.is(TokenTypeId::EndOfLine, "\n") {
                    break;
                }

                expect_token(&next_token, TokenTypeId::Operator, ",")?;
            }
        }

        let end_token = parser.required_token()?;
        parser.consume_token();
        expect_token(&end_token, TokenTypeId::Keyword, "Type")?;

        Ok(Self {
            name: type_name,
            fields,
        })
    }
}
