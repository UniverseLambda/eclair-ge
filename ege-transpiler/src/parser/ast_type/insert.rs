use serde::Serialize;

use crate::{lexer::TokenTypeId, parser::{ext::TokenExt, Parser}};

use super::{Ident, Parsable};

// Insert <source> <rel_pos> <pivot> <collection>

#[derive(Debug, Clone, Serialize)]
pub struct Insert {
	pub source: Ident,
	pub rel_pos: InsertRelPos,
	pub pivot: InsertPivot,
	pub collection: Ident,
}

#[derive(Debug, Clone, Serialize)]
pub enum InsertRelPos {
	Before,
	After,
}


#[derive(Debug, Clone, Serialize)]
pub enum InsertPivot {
	First,
	Last,
}

impl Parsable for Insert {
	fn parse(parser: &mut Parser<impl std::io::Read>) -> anyhow::Result<Self> {
		parser.required_token()?.expect_token(TokenTypeId::Keyword, "Insert")?;
		parser.consume_token();

		let source= Ident::parse(parser)?;

		let rel_pos = parser.required_next_token()?
			.expect_type(TokenTypeId::Keyword)?
			.expect_any_content(&["Before", "After"])?;

		let rel_pos = match rel_pos.content.as_str() {
			"Before" => InsertRelPos::Before,
			"After" => InsertRelPos::After,
			_ => unreachable!()
		};

		let pivot = parser.required_next_token()?
			.expect_type(TokenTypeId::Keyword)?
			.expect_any_content(&["First", "Last"])?;


		let pivot = match pivot.content.as_str() {
			"First" => InsertPivot::First,
			"Last" => InsertPivot::Last,
			_ => unreachable!()
		};

		parser.consume_token();

		let collection = Ident::parse(parser)?;

		Ok(Self {
			source,
			rel_pos,
			pivot,
			collection,
		})

	}
}
