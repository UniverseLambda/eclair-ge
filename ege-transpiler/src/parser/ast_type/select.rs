use anyhow::bail;
use serde::Serialize;

use crate::{lexer::TokenTypeId, parser::{ext::TokenExt, Parser}};

use super::{Expr, Parsable, Statement};

#[derive(Debug, Clone, Serialize)]
pub struct Select {
	pub value: Expr,
	pub cases: Vec<SelectCase>,
	pub default_case: Option<Vec<Statement>>,
}

#[derive(Debug, Clone, Serialize)]
pub struct SelectCase {
	pub values: Vec<Expr>,
	pub statements: Vec<Statement>,
}

impl Parsable for Select {
	fn parse(parser: &mut Parser<impl std::io::Read>) -> anyhow::Result<Self> {
		parser.required_token().expect_token(TokenTypeId::Keyword, "Select")?;
		parser.consume_token();

		let value = Expr::parse(parser)?;

		parser.required_token().expect_type(TokenTypeId::EndOfLine)?;

		let mut cases = Vec::new();
		let mut default_case = None;

		let mut start_token = parser.required_next_token()?;
		parser.consume_token();

		loop {
			parser.skip_line_returns()?;

			let token = start_token.expect_type(TokenTypeId::Keyword).expect_any_content(&["End", "Case", "Default"])?;

			let values =
			match token.content.as_str() {
				"End" => break,
				"Case" => {
					let mut values = Vec::new();

					loop {
						values.push(Expr::parse(parser)?);

						let next = parser.required_token().expect_any_type(&[TokenTypeId::EndOfLine, TokenTypeId::Operator]).expect_any_content(&["\n", ","])?;
						parser.consume_token();

						if next.content != "," {
							break;
						}
					}

					values
				}
				"Default" => {
					if default_case.is_some() {
						bail!("{}:{}:{}: default section for this select has already been defined", token.source_path, token.line, token.column);
					}

					Vec::new()
				},
				_ => unreachable!()
			};

			let (statements, stopper) = parser.parse_statement_block("End", true, &["Case", "Default"])?;

			start_token = stopper;

			if values.is_empty() {
				default_case = Some(statements)
			} else {
				cases.push(SelectCase { values, statements });
			}
		}

		parser.required_token().expect_token(TokenTypeId::Keyword, "Select")?;
		parser.consume_token();

		Ok(Self { value, cases, default_case })
	}
}
