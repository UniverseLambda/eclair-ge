use std::io::Read;

use anyhow::Context;
use log::debug;
use serde::Serialize;

use crate::{
    lexer::TokenTypeId,
    parser::{Parser, expect_token, expect_token_type},
};

use super::{Expr, Ident, Parsable, Statement};

#[derive(Debug, Clone, Serialize)]
pub struct ForLoop {
    pub name: Ident,
    pub mode: ForLoopMode,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, Serialize)]
pub enum ForLoopMode {
    Range { from: Expr, to: Expr },
    Each { iterator: Ident },
}

impl Parsable for ForLoop {
    fn parse(parser: &mut Parser<impl Read>) -> anyhow::Result<Self> {
        debug!(
            "parse_for: current_token: {:#?}, peeked: {:#?}",
            parser.current_token()?,
            parser.peek_token()?
        );

        let for_keyword = parser.required_token()?;
        expect_token(&for_keyword, TokenTypeId::Keyword, "For")?;
        parser.consume_token();

        let it_ident = Ident::parse(parser)?;

        let eq_operator = parser.required_token()?;
        expect_token(&eq_operator, TokenTypeId::Operator, "=")?;

        let disc = parser.required_next_token()?;

        let mode = if disc.is(TokenTypeId::Keyword, "Each") {
            // TODO: support for Each here
            parser.required_next_token()?;

            ForLoopMode::Each {
                iterator: Ident::parse(parser)?,
            }
        } else {
            let initial_value = Expr::parse(parser)?;

            let to_keyword = parser.required_token()?;
            expect_token(&to_keyword, TokenTypeId::Keyword, "To")?;
            parser.consume_token();

            let final_value = Expr::parse(parser)?;

            ForLoopMode::Range {
                from: initial_value,
                to: final_value,
            }
        };

        let lf = parser.required_token()?;
        expect_token_type(&lf, TokenTypeId::EndOfLine)?;

        let (statements, _) = parser.parse_statement_block("Next", true, &[])?;

        Ok(ForLoop {
            name: it_ident,
            mode,
            statements,
        })
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct RepeatLoop {
    pub statements: Vec<Statement>,
}

impl Parsable for RepeatLoop {
    fn parse(parser: &mut Parser<impl Read>) -> anyhow::Result<Self> {
        // Skipping the "Repeat" keyword
        let Some(lf) = parser
            .next_token()
            .with_context(|| "Parser::parse_repeat")?
        else {
            return parser
                .unexpected_eof()
                .with_context(|| "Parser::parse_repeat");
        };

        expect_token_type(&lf, TokenTypeId::EndOfLine)?;

        let (statements, _) = parser
            .parse_statement_block("Forever", true, &[])
            .with_context(|| "Parser::parse_repeat")?;

        Ok(RepeatLoop { statements })
    }
}
