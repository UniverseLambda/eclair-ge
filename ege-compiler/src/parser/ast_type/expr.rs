use std::io::Read;

use anyhow::{Context, bail};
use log::{debug, trace};
use serde::Serialize;

use crate::{
    lexer::{TokenType, TokenTypeId},
    parser::{Parser, expect_any_token_type},
};

use super::{FunctionCall, Ident, IdentPath, Parsable};

#[derive(Debug, Clone, Serialize)]
pub enum Expr {
    Function(FunctionCall),
    String(String),
    Integer(i64),
    Float(f64),
    Null,
    Binary(BinaryExpr),
    Path(IdentPath),
    CollectionFirst(Ident),
    CollectionLast(Ident),
    New(Ident),
    Unary(UnaryExpr),
}

#[derive(Debug, Clone, Serialize)]
pub enum UnaryExprOp {
    Posate, // (?) // Lmao
    Negate,
    BitComplement,
}

#[derive(Debug, Clone, Serialize)]
pub struct UnaryExpr {
    pub op: UnaryExprOp,
    pub value: Box<Expr>,
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
    BitAnd,
    BitOr,
    BitXor,
    Pow,
}

#[derive(Debug, Clone, Serialize)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub op: BinaryExprOp,
}

impl Parsable for Expr {
    fn parse(parser: &mut Parser<impl Read>) -> anyhow::Result<Self> {
        debug!(
            "parse_expr: current_token: {:?}, peeked: {:?}",
            parser.current_token()?,
            parser.peek_token()?
        );

        let current_token = parser.required_token()?;

        if current_token.is(TokenTypeId::Operator, "-") {
            trace!("branching for unary expr '-'");
            parser.consume_token();

            return Ok(Self::Unary(UnaryExpr {
                op: UnaryExprOp::Negate,
                value: Box::new(Self::parse(parser)?),
            }));
        } else if current_token.is(TokenTypeId::Operator, "+") {
            trace!("branching for unary expr '+'");
            parser.consume_token();

            return Ok(Self::Unary(UnaryExpr {
                op: UnaryExprOp::Posate,
                value: Box::new(Self::parse(parser)?),
            }));
        } else if current_token.is(TokenTypeId::Operator, "~") {
            trace!("branching for unary expr '~'");
            parser.consume_token();

            return Ok(Self::Unary(UnaryExpr {
                op: UnaryExprOp::BitComplement,
                value: Box::new(Self::parse(parser)?),
            }));
        }

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
                "And" => BinaryExprOp::BitAnd,
                "Or" => BinaryExprOp::BitOr,
                "Xor" => BinaryExprOp::BitXor,
                "^" => BinaryExprOp::Pow,
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
        debug!(
            "parse_expr_single_pass: current_token: {:#?}",
            parser.current_token()?,
        );

        // At first we either have:
        // - an int: 42
        // - a float: 3.14
        // - a string: "zarma"
        // - a variable: banger$
        // - a parenthese
        // - a New/First/Last followed by a type (ident)
        // - a function call: pipoudou(...)
        // The trickiest of all is the function call,
        // because it's the only one we can't build from a single token
        // So we just ignore it because fuck it
        // And for parentheses expr, we just consume it, and call parse_expr

        // FIXME: Handle expressions that might be starting with keywords like "Not"

        trace!(
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
            (TokenType::Keyword, v) if v == "Null" => Expr::Null,
            (TokenType::Keyword, v) if v == "First" => {
                let type_token = parser.required_next_token()?;

                let ident = Ident::from_token(type_token)?;

                // TODO: Generate an error when a type is specified

                Expr::CollectionFirst(ident)
            }
            (TokenType::Keyword, v) if v == "Last" => {
                let type_token = parser.required_next_token()?;

                let ident = Ident::from_token(type_token)?;

                // TODO: Generate an error when a type is specified

                Expr::CollectionLast(ident)
            }
            (TokenType::Keyword, v) if v == "New" => {
                let type_token = parser.required_next_token()?;

                let ident = Ident::from_token(type_token)?;

                // TODO: Generate an error when a type is specified

                Expr::New(ident)
            }
            (TokenType::Operator, v) if v == "(" => {
                parser.consume_token();

                Expr::parse(parser)?
            }
            (t, v) => bail!(
                "{}:{}:{}; Unexpected token: `{v}` (type: {t:?})",
                current_token.source_path,
                current_token.line,
                current_token.column
            ),
        })
    }
}
