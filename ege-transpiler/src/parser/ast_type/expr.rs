use std::io::Read;

use anyhow::{bail, Context};
use serde::Serialize;

use crate::{
    lexer::{TokenType, TokenTypeId},
    parser::{expect_any_token_type, Parser},
};

use super::{FunctionCall, IdentPath, Parsable};

#[derive(Debug, Clone, Serialize)]
pub enum Expr {
    Function(FunctionCall),
    String(String),
    Integer(i64),
    Float(f64),
    Binary(BinaryExpr),
    Path(IdentPath),
}

#[derive(Debug, Clone, Serialize)]
pub enum BinaryExprOp {
    Add,
    Sub,
    Mult,
    Div,
    Less,
    LessOrEqual,
    Equal,
    GreaterOrEqual,
    Greater,
    Different,
    BoolAnd,
    BoolOr,
    Xor,
}

#[derive(Debug, Clone, Serialize)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub op: BinaryExprOp,
}

impl Parsable for Expr {
    fn parse(parser: &mut Parser<impl Read>) -> anyhow::Result<Self> {
        eprintln!(
            "parse_expr: current_token: {:?}, peeked: {:?}",
            parser.current_token()?,
            parser.peek_token()?
        );

        let mut current_expr: Expr = Self::parse_single_pass(parser)?;
        current_expr = parser.might_be_a_func_call(current_expr)?;

        loop {
            let Some(token) = parser
                .current_token()
                .with_context(|| "Parser::parse_expr")?
            else {
                break;
            };

            if token.is(TokenTypeId::EndOfLine, "\n")
                || token.is(TokenTypeId::Keyword, "Then")
                || token.is(TokenTypeId::Keyword, "To")
            {
                break;
            }

            expect_any_token_type(&token, &[TokenTypeId::Operator, TokenTypeId::Keyword])?;

            let binary_op = match token.content.as_str() {
                "+" => BinaryExprOp::Add,
                "-" => BinaryExprOp::Sub,
                "*" => BinaryExprOp::Mult,
                "/" => BinaryExprOp::Div,
                "<" => BinaryExprOp::Less,
                "<=" => BinaryExprOp::LessOrEqual,
                "=" => BinaryExprOp::Equal,
                ">=" => BinaryExprOp::GreaterOrEqual,
                ">" => BinaryExprOp::Greater,
                "<>" => BinaryExprOp::Different,
                "And" => BinaryExprOp::BoolAnd,
                "Or" => BinaryExprOp::BoolOr,
                "^" => BinaryExprOp::Xor,
                "," | ")" => break,
                v => bail!("Unexpected {:?}: {v}", token.token_type.to_id()),
            };

            parser.consume_token();

            let mut right_expr = Self::parse_single_pass(parser)?;
            right_expr = parser.might_be_a_func_call(right_expr)?;

            current_expr = Expr::Binary(BinaryExpr {
                left: Box::new(current_expr),
                right: Box::new(right_expr),
                op: binary_op,
            })
        }

        Ok(current_expr)
    }
}

impl Expr {
    // This function doesn't consume its last token.
    fn parse_single_pass(parser: &mut Parser<impl Read>) -> anyhow::Result<Self> {
        eprintln!(
            "parse_expr_single_pass: current_token: {:#?}",
            parser.current_token()?,
        );

        // At first we either have:
        // - an int: 42
        // - a float: 3.14
        // - a string: "zarma"
        // - a variable: banger$
        // - a parenthese
        // - a function call: pipoudou(...)
        // The trickiest of all is the function call,
        // because it's the only one we can't build from a single token
        // So we just ignore it because fuck it
        // And for parentheses expr, we just consume it, and call parse_expr

        // FIXME: Handle expressions that might be starting with keywords like "Not"

        eprintln!(
            "POST parse_expr_single_pass: current_token: {:#?}",
            parser.current_token()?,
        );

        let current_token = parser
            .required_token()
            .with_context(|| "Parser::parse_expr_single_pass")?;

        Ok(match (current_token.token_type, current_token.content) {
            (TokenType::FloatLiteral(v), _) => Expr::Float(v),
            (TokenType::IntegerLiteral(v), _) => Expr::Integer(v),
            (TokenType::StringLiteral, v) => Expr::String(v),
            (TokenType::Path(components, final_type), _) => Expr::Path(IdentPath {
                components,
                final_type,
            }),
            (TokenType::Ident(ident_type), name) => {
                Expr::Path(IdentPath::from_single_name_and_type(name, ident_type))
            }
            (TokenType::Operator, v) if v == "(" => {
                parser.consume_token();

                Expr::parse(parser)?
            }
            (t, v) => bail!("Unexpected token: `{v}` (type: {t:?})"),
        })
    }
}
