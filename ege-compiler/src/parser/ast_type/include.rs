use std::path::Path;

use serde::Serialize;

use crate::{
    lexer::{TokenTypeId, Tokenizer},
    parser::{Parser, ext::TokenExt},
};

use super::{Parsable, Program};

#[derive(Debug, Clone, Serialize)]
pub struct Include {
    pub program: Program,
}

impl Parsable for Include {
    fn parse(parser: &mut Parser<impl std::io::Read>) -> anyhow::Result<Self> {
        let include_token = parser
            .required_token()
            .expect_token(TokenTypeId::Keyword, "Include")?;

        let path = parser
            .required_next_token()
            .expect_type(TokenTypeId::StringLiteral)?
            .content;
        parser.consume_token();

        let real_file_path = Path::new(&include_token.source_path)
            .parent()
            .unwrap()
            .join(path);

        let file = std::fs::File::open(&real_file_path)?;

        let lexer = Tokenizer::new(real_file_path.to_string_lossy().into_owned(), file);
        let program = Parser::new(lexer).parse_program()?;

        Ok(Self { program })
    }
}
