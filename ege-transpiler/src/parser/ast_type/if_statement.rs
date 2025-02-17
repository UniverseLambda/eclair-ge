use std::io::Read;

use serde::Serialize;

use crate::{
    lexer::TokenTypeId,
    parser::{expect_token, Parser},
};

use super::{Expr, Parsable, Statement};

#[derive(Debug, Clone, Serialize)]
pub struct If {
    pub cond: Expr,
    pub statements: Vec<Statement>,
    pub else_if: Vec<Otherwise>,
}

#[derive(Debug, Clone, Serialize)]
pub enum Otherwise /* Feeling like a clown today */ {
    ElseIf {
        cond: Expr,
        statements: Vec<Statement>,
    },
    Else {
        statements: Vec<Statement>,
    },
}

impl Parsable for If {
    fn parse(parser: &mut Parser<impl Read>) -> anyhow::Result<Self> {
        eprintln!(
            "parse_if: current_token: {:?}, peeked: {:?}",
            parser.current_token()?,
            parser.peek_token()?
        );

        let if_operator = parser.required_token()?;
        expect_token(&if_operator, TokenTypeId::Keyword, "If")?;
        parser.consume_token();

        let mut has_else = false;
        let mut blocks: Vec<(Expr, Vec<Statement>)> = Vec::new();
        let mut else_statements: Vec<Statement> = Vec::new();

        loop {
            let cond;

            if !has_else {
                cond = Expr::parse(parser)?;

                let then_keyword = parser.required_token()?;

                if then_keyword.is(TokenTypeId::Keyword, "Then") {
                    parser.consume_token();
                }
            } else {
                cond = Expr::Integer(1);
                parser.consume_token();
            }

            let next_token = parser.required_token()?;
            if !next_token.is(TokenTypeId::EndOfLine, "\n") {
                // UNWRAP: We know we have something else. So it will either be an error, or a statement, but not nothing.
                let statement = parser.parse_statement()?.unwrap();

                if has_else {
                    else_statements = vec![statement];
                } else {
                    blocks.push((cond, vec![statement]));
                }

                break;
            }

            let stoppers: &[&str] = if has_else { &[] } else { &["ElseIf", "Else"] };

            parser.consume_token();
            let (if_statement_block, stopper) =
                parser.parse_statement_block("If", false, stoppers)?;

            if has_else {
                else_statements = if_statement_block;
            } else {
                blocks.push((cond, if_statement_block));
            }

            if stopper == "Else" {
                has_else = true;
            } else if stopper != "ElseIf" {
                break;
            }
        }

        let (if_expr, if_statements) = blocks.remove(0);

        let mut otherwises: Vec<Otherwise> = blocks
            .drain(..)
            .map(|(cond, statements)| Otherwise::ElseIf { cond, statements })
            .collect();

        if has_else {
            otherwises.push(Otherwise::Else {
                statements: else_statements,
            });
        }

        Ok(If {
            cond: if_expr,
            statements: if_statements,
            else_if: otherwises,
        })
    }
}
