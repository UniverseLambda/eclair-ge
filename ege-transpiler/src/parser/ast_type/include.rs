use serde::Serialize;

use crate::{lexer::{TokenTypeId, Tokenizer}, parser::{ext::TokenExt, Parser}};

use super::{Parsable, Program};

#[derive(Debug, Clone, Serialize)]
pub struct Include {
	pub program: Program
}

impl Parsable for Include {
	fn parse(parser: &mut Parser<impl std::io::Read>) -> anyhow::Result<Self> {
		parser.required_token().expect_token(TokenTypeId::Keyword, "Include")?;

		let path = parser.required_next_token().expect_type(TokenTypeId::StringLiteral)?.content;
		parser.consume_token();

		let file = std::fs::File::open(&path)?;

		let lexer = Tokenizer::new(path, file);
		let program = Parser::new(lexer).parse_program()?;

		Ok(Self { program })
	}
}