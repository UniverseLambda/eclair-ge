use anyhow::bail;

use crate::lexer::{Token, TokenTypeId};

use super::{expect_any_token_type, expect_token};

pub trait TokenExt: Sized {
	fn expect_any_type(self, tk_types: &[TokenTypeId]) -> anyhow::Result<Self>;
	fn expect_any_content(self, contents: &[&str]) -> anyhow::Result<Self>;
	fn expect_token(self, tk_type: TokenTypeId, content: &str) -> anyhow::Result<Self>;

	fn expect_type(self, tk_type: TokenTypeId) -> anyhow::Result<Self> {
		self.expect_any_type(&[tk_type])
	}

	fn expect_content(self, content: &str) -> anyhow::Result<Self> {
		self.expect_any_content(&[content])
	}

}

impl TokenExt for Token {
	fn expect_any_type(self, tk_types: &[TokenTypeId]) -> anyhow::Result<Self> {
		expect_any_token_type(&self, tk_types)?;

		Ok(self)
	}

	fn expect_any_content(self, contents: &[&str]) -> anyhow::Result<Self> {
		for content in contents {
			if *content == self.content {
				return Ok(self);
			}
		}

		bail!("{}:{}:{}: unexpected token: {} ({:?}), expected: {contents:?}", self.source_path, self.line, self.column, self.content, self.token_type.to_id());
	}

	fn expect_token(self, tk_type: TokenTypeId, content: &str) -> anyhow::Result<Self> {
		expect_token(&self, tk_type, content)?;

		Ok(self)
	}
}
