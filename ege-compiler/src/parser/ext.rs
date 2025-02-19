use anyhow::bail;

use crate::lexer::{Token, TokenTypeId};

use super::{expect_any_token_type, expect_token};

pub trait TokenExt: Sized {
    type Output;

    fn expect_any_type(self, tk_types: &[TokenTypeId]) -> Self::Output;
    fn expect_any_content(self, contents: &[&str]) -> Self::Output;
    fn expect_token(self, tk_type: TokenTypeId, content: &str) -> Self::Output;

    fn expect_type(self, tk_type: TokenTypeId) -> Self::Output {
        self.expect_any_type(&[tk_type])
    }
}

impl TokenExt for Token {
    type Output = anyhow::Result<Self>;

    fn expect_any_type(self, tk_types: &[TokenTypeId]) -> Self::Output {
        expect_any_token_type(&self, tk_types)?;

        Ok(self)
    }

    fn expect_any_content(self, contents: &[&str]) -> Self::Output {
        for content in contents {
            if *content == self.content {
                return Ok(self);
            }
        }

        bail!(
            "{}:{}:{}: unexpected token: {} ({:?}), expected: {contents:?}",
            self.source_path,
            self.line,
            self.column,
            self.content,
            self.token_type.to_id()
        );
    }

    fn expect_token(self, tk_type: TokenTypeId, content: &str) -> Self::Output {
        expect_token(&self, tk_type, content)?;

        Ok(self)
    }
}

impl TokenExt for anyhow::Result<Token> {
    type Output = Self;

    fn expect_any_type(self, tk_types: &[TokenTypeId]) -> Self::Output {
        self.and_then(|tk| tk.expect_any_type(tk_types))
    }

    fn expect_any_content(self, contents: &[&str]) -> Self::Output {
        self.and_then(|tk| tk.expect_any_content(contents))
    }

    fn expect_token(self, tk_type: TokenTypeId, content: &str) -> Self::Output {
        self.and_then(|tk| tk.expect_token(tk_type, content))
    }
}
