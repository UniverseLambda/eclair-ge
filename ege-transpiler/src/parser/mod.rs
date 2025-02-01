use std::io::Read;

use anyhow::{bail, Context, Ok, Result};
use ast_type::{
    BinaryExpr, BinaryExprOp, Expr, ForLoop, FunctionCall, Ident, If, Otherwise, Program,
    RepeatLoop, Statement, VarAssign, VarScope,
};

use crate::lexer::{Token, TokenType, TokenTypeId, Tokenizer};

mod ast_type;

pub struct Parser<R: Read> {
    token_source: Tokenizer<R>,
    _current_token: Option<Token>,
    _peeked_token: Option<Token>,
}

impl<R: Read> Parser<R> {
    pub fn new(token_source: Tokenizer<R>) -> Self {
        Self {
            token_source,
            _current_token: None,
            _peeked_token: None,
        }
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        let mut statements = Vec::new();

        while let Some(statement) = self
            .parse_statement()
            .with_context(|| "Parser::parse_program")?
        {
            eprintln!("Statement: {statement:?}");
            statements.push(statement);
        }

        Ok(Program { statements })
    }

    pub fn parse_statement(&mut self) -> Result<Option<Statement>> {
        self.skip_line_returns()?;

        let Some(disc) = self.current_token()? else {
            return Ok(None);
        };

        self.expect_any_token_type(
            &disc,
            &[
                TokenTypeId::Keyword,
                TokenTypeId::Ident,
                TokenTypeId::FunctionKeyword,
            ],
        )?;

        let statement = match (disc.token_type, disc.content.as_str()) {
            (TokenType::Keyword, "Function") => {
                /* TODO: Function declaration parsing */
                todo!()
            }
            (TokenType::Keyword, "Local" | "Global") => {
                self.parse_var_assign().map(Statement::VarAssign)?
            }
            (TokenType::Keyword, "If") => self.parse_if().map(Statement::If)?,
            (TokenType::Keyword, "For") => self.parse_for().map(Statement::For)?,
            (TokenType::Keyword, "Repeat") => Statement::Repeat(self.parse_repeat()?),
            (TokenType::Ident(_), _) => self.parse_statement_from_ident()?,
            (TokenType::FunctionKeyword, _) => self
                .parse_function_call()
                .map(|v| Statement::FunctionCall(v))?,
            (t, v) => bail!("Unexpected token: `{v}` (type: {t:?})"),
        };

        Ok(Some(statement))
    }

    fn parse_statement_from_ident(&mut self) -> Result<Statement> {
        eprintln!(
            "parse_statement_from_ident: current_token: {:?}, peeked: {:?}",
            self.current_token()?,
            self.peek_token()?
        );

        let Some(peeked) = self.peek_token()? else {
            return self
                .unexpected_eof()
                .with_context(|| "Parser::parse_statement_from_ident");
        };

        match peeked.content.as_str() {
            "=" => self.parse_var_assign().map(|v| Statement::VarAssign(v)),
            "(" => self
                .parse_function_call()
                .map(|v| Statement::FunctionCall(v)),
            v => bail!("Unexpected token: `{v}` (type: {:?})", peeked.token_type),
        }
    }

    fn parse_ident(&mut self) -> Result<Ident> {
        let ident = self.required_token()?;

        self.expect_any_token_type(&ident, &[TokenTypeId::Ident, TokenTypeId::FunctionKeyword])?;

        let ident_type = match ident.token_type {
            TokenType::Ident(ident_type) => ident_type,
            TokenType::FunctionKeyword => None,
            _ => unreachable!(),
        };

        self.consume_token();

        Ok(Ident {
            name: ident.content,
            ident_type,
        })
    }

    fn parse_for(&mut self) -> Result<ForLoop> {
        eprintln!(
            "parse_for: current_token: {:#?}, peeked: {:#?}",
            self.current_token()?,
            self.peek_token()?
        );

        let for_keyword = self.required_token()?;
        self.expect_token(&for_keyword, TokenTypeId::Keyword, "For")?;
        self.consume_token();

        let it_ident = self.parse_ident()?;

        let eq_operator = self.required_token()?;
        self.expect_token(&eq_operator, TokenTypeId::Operator, "=")?;
        self.consume_token();

        // TODO: support for Each here

        let initial_value = self.parse_expr()?;

        let to_keyword = self.required_token()?;
        self.expect_token(&to_keyword, TokenTypeId::Keyword, "To")?;
        self.consume_token();

        let final_value = self.parse_expr()?;

        let lf = self.required_token()?;
        self.expect_token_type(&lf, TokenTypeId::EndOfLine)?;

        let (statements, _) = self.parse_statement_block("Next", true, &[])?;

        Ok(ForLoop {
            name: it_ident,
            from: initial_value,
            to: final_value,
            statements,
        })
    }

    fn parse_if(&mut self) -> Result<If> {
        eprintln!(
            "parse_if: current_token: {:?}, peeked: {:?}",
            self.current_token()?,
            self.peek_token()?
        );

        let if_operator = self.required_token()?;
        self.expect_token(&if_operator, TokenTypeId::Keyword, "If")?;
        self.consume_token();

        let mut has_else = false;
        let mut blocks: Vec<(Expr, Vec<Statement>)> = Vec::new();
        let mut else_statements: Vec<Statement> = Vec::new();

        loop {
            let cond;

            if !has_else {
                cond = self.parse_expr()?;

                let then_keyword = self.required_token()?;
                self.expect_token(&then_keyword, TokenTypeId::Keyword, "Then")?;
            } else {
                cond = Expr::Integer(1)
            }

            let next_token = self.required_next_token()?;
            if !next_token.is(TokenTypeId::EndOfLine, "\n") {
                // UNWRAP: We know we have something else. So it will either be an error, or a statement, but not nothing.
                let statement = self.parse_statement()?.unwrap();

                if has_else {
                    else_statements = vec![statement];
                } else {
                    blocks.push((cond, vec![statement]));
                }

                break;
            }

            let stoppers: &[&str] = if has_else { &[] } else { &["ElseIf", "Else"] };

            self.consume_token();
            let (if_statement_block, stopper) =
                self.parse_statement_block("If", false, stoppers)?;

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

    fn parse_function_call(&mut self) -> Result<FunctionCall> {
        eprintln!(
            "parse_function_call: current_token: {:?}, peeked: {:?}",
            self.current_token()?,
            self.peek_token()?
        );

        let ident = self.parse_ident()?;

        let expr_start = self.required_token()?;

        let ends_with_parenth = if expr_start.is(TokenTypeId::Operator, "(") {
            self.consume_token();

            true
        } else {
            false
        };

        let mut params = Vec::new();

        loop {
            let Some(expr_start) = self.current_token()? else {
                break;
            };

            if (ends_with_parenth && expr_start.is(TokenTypeId::Operator, ")"))
                || expr_start.is(TokenTypeId::EndOfLine, "\n")
            {
                break;
            }

            params.push(self.parse_expr()?);

            if let Some(token) = self.current_token()? {
                if token.is(TokenTypeId::Operator, ",") {
                    self.consume_token();
                }
            }
        }

        if ends_with_parenth {
            let parenth = self.required_token()?;

            self.expect_token(&parenth, TokenTypeId::Operator, ")")?;
            self.consume_token();
        }

        Ok(FunctionCall { ident, params })
    }

    fn parse_var_assign(&mut self) -> Result<VarAssign> {
        eprintln!(
            "parse_expr: current_token: {:?}, peeked: {:?}",
            self.current_token()?,
            self.peek_token()?
        );

        let first_token = self.required_token()?;

        let scope = if let TokenType::Keyword = first_token.token_type {
            self.next_token()?;

            match first_token.content.as_str() {
                "Global" => Some(VarScope::Global),
                "Local" => Some(VarScope::Local),
                v => bail!("Unexpected token: {v}, expected Global or Local"),
            }
        } else {
            None
        };

        let ident_token = self.required_token()?;
        self.expect_token_type(&ident_token, TokenTypeId::Ident)?;

        let TokenType::Ident(ident_typing) = ident_token.token_type else {
            unreachable!()
        };

        let mut res = VarAssign {
            name: Ident {
                name: ident_token.content,
                ident_type: ident_typing,
            },
            scope,
            value: None,
        };

        let Some(operator) = self.next_token()? else {
            return Ok(res);
        };

        if let TokenType::EndOfLine = operator.token_type {
            return Ok(res);
        }

        self.expect_token(&operator, TokenTypeId::Operator, "=")?;
        self.consume_token();

        res.value = Some(self.parse_expr()?);

        Ok(res)
    }

    fn parse_repeat(&mut self) -> Result<RepeatLoop> {
        // Skipping the "Repeat" keyword
        let Some(lf) = self.next_token().with_context(|| "Parser::parse_repeat")? else {
            return self
                .unexpected_eof()
                .with_context(|| "Parser::parse_repeat");
        };

        self.expect_token_type(&lf, TokenTypeId::EndOfLine)?;

        let (statements, _) = self
            .parse_statement_block("Forever", true, &[])
            .with_context(|| "Parser::parse_repeat")?;

        Ok(RepeatLoop { statements })
    }

    fn parse_statement_block(
        &mut self,
        block_end: &str,
        no_end_token: bool,
        early_stoppers: &[&str],
    ) -> Result<(Vec<Statement>, String)> {
        let mut statements = Vec::new();

        let concat_end_token = if no_end_token {
            String::new()
        } else {
            format!("End{block_end}")
        };

        let mut block_stopper;

        'main_loop: loop {
            self.skip_line_returns()?;

            let Some(token) = self.current_token()? else {
                return self
                    .unexpected_eof()
                    .with_context(|| "Parser::parse_statement_block");
            };

            if no_end_token {
                if token.is(TokenTypeId::Keyword, block_end) {
                    block_stopper = block_end.to_string();
                    break;
                }
            } else if token.is(TokenTypeId::Keyword, &concat_end_token) {
                block_stopper = concat_end_token.to_string();
                break;
            } else if token.is(TokenTypeId::Keyword, "End") {
                if let Some(token) = self.next_token()? {
                    self.expect_token_type(&token, TokenTypeId::Keyword)?;

                    self.expect_token_content(&token, block_end)?;
                    block_stopper = block_end.to_string();
                    break;
                } else {
                    return self
                        .unexpected_eof()
                        .with_context(|| "Parser::parse_statement_block");
                }
            }

            for stopper in early_stoppers {
                if token.is(TokenTypeId::Keyword, stopper) {
                    block_stopper = stopper.to_string();
                    break 'main_loop;
                }
            }

            let Some(statement) = self.parse_statement()? else {
                return self
                    .unexpected_eof()
                    .with_context(|| "Parser::parse_statement_block");
            };

            statements.push(statement);
        }

        self.consume_token();

        Ok((statements, block_stopper))
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        eprintln!(
            "parse_expr: current_token: {:?}, peeked: {:?}",
            self.current_token()?,
            self.peek_token()?
        );

        let mut current_expr: Expr = self.parse_expr_single_pass()?;
        current_expr = self.might_be_a_func_call(current_expr)?;

        loop {
            let Some(token) = self.current_token().with_context(|| "Parser::parse_expr")? else {
                break;
            };

            if token.is(TokenTypeId::EndOfLine, "\n")
                || token.is(TokenTypeId::Keyword, "Then")
                || token.is(TokenTypeId::Keyword, "To")
            {
                break;
            }

            self.expect_any_token_type(&token, &[TokenTypeId::Operator, TokenTypeId::Keyword])?;

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

            self.consume_token();

            let mut right_expr = self.parse_expr_single_pass()?;
            right_expr = self.might_be_a_func_call(right_expr)?;

            current_expr = Expr::Binary(BinaryExpr {
                left: Box::new(current_expr),
                right: Box::new(right_expr),
                op: binary_op,
            })
        }

        Ok(current_expr)
    }

    fn might_be_a_func_call(&mut self, current_expr: Expr) -> Result<Expr> {
        eprintln!(
            "might_be_a_func_call: current_token: {:?}, peeked: {:?}",
            self.current_token()?,
            self.peek_token()?
        );

        if let Expr::Variable(ident) = current_expr {
            let Some(peeked_token) = self.peek_token()? else {
                self.consume_token();

                return Ok(Expr::Variable(ident));
            };

            if peeked_token.token_type.to_id() == TokenTypeId::Operator
                && peeked_token.content == "("
            {
                self.parse_function_call().map(|v| Expr::Function(v))
            } else {
                self.consume_token();

                Ok(Expr::Variable(ident))
            }
        } else {
            self.consume_token();

            Ok(current_expr)
        }
    }

    // This function doesn't consume its last token.
    fn parse_expr_single_pass(&mut self) -> Result<Expr> {
        eprintln!(
            "parse_expr_single_pass: current_token: {:#?}",
            self.current_token()?,
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
            self.current_token()?,
        );

        let Some(current_token) = self
            .current_token()
            .with_context(|| "Parser::parse_expr_single_pass")?
        else {
            return self.unexpected_eof();
        };

        Ok(match (current_token.token_type, current_token.content) {
            (TokenType::FloatLiteral(v), _) => Expr::Float(v),
            (TokenType::IntegerLiteral(v), _) => Expr::Integer(v),
            (TokenType::StringLiteral, v) => Expr::String(v),
            (TokenType::Ident(ident_type), name) => Expr::Variable(Ident { name, ident_type }),
            (TokenType::Operator, v) if v == "(" => {
                self.consume_token();

                self.parse_expr()?
            }
            (t, v) => bail!("Unexpected token: `{v}` (type: {t:?})"),
        })
    }

    pub fn consume_token(&mut self) {
        self._current_token = self._peeked_token.take();
    }

    pub fn current_token(&mut self) -> Result<Option<Token>> {
        if let Some(current) = self._current_token.clone() {
            Ok(Some(current))
        } else {
            self.next_token().with_context(|| "Parser::current_token")
        }
    }

    pub fn required_next_token(&mut self) -> Result<Token> {
        self.consume_token();

        self.required_token()
    }

    pub fn required_token(&mut self) -> Result<Token> {
        if let Some(token) = self.current_token()? {
            Ok(token)
        } else {
            self.unexpected_eof()
        }
    }

    pub fn next_token(&mut self) -> Result<Option<Token>> {
        self._current_token = self._peeked_token.take();

        if self._current_token.is_none() {
            self._current_token = self.read_token()?;
        }

        Ok(self
            ._current_token
            .clone()
            .inspect(|token| eprintln!("TOKEN: {token:?}")))
    }

    pub fn peek_token(&mut self) -> Result<Option<Token>> {
        if let Some(peeked) = self._peeked_token.clone() {
            Ok(Some(peeked))
        } else {
            self._peeked_token = self.read_token().with_context(|| "Parser::peek_token")?;

            Ok(self._peeked_token.clone())
        }
    }

    fn read_token(&mut self) -> Result<Option<Token>> {
        self.token_source
            .next_token()
            .with_context(|| "Parser::read_token")
    }

    fn expect_token(&self, token: &Token, tktype: TokenTypeId, content: &str) -> Result<()> {
        if token.token_type.to_id() == tktype && token.content == content {
            Ok(())
        } else {
            bail!("Unexpected token: {token:?}, expected: {content:?} (type: {tktype:?})");
        }
    }

    fn expect_token_type(&self, token: &Token, tktype: TokenTypeId) -> Result<()> {
        self.expect_any_token_type(token, &[tktype])
    }

    fn expect_token_content(&self, token: &Token, content: &str) -> Result<()> {
        if token.content == content {
            Ok(())
        } else {
            bail!("Unexpected token: {token:?}, expected: {content:?}");
        }
    }

    fn expect_any_token_type(&self, token: &Token, tktypes: &[TokenTypeId]) -> Result<()> {
        let curr = token.token_type.to_id();

        for tktype in tktypes {
            if *tktype == curr {
                return Ok(());
            }
        }

        bail!("Unexpected token: {token:?}, expected: {tktypes:?}");
    }

    fn skip_line_returns(&mut self) -> Result<()> {
        while let Some(token) = self.current_token()? {
            if token.token_type.to_id() != TokenTypeId::EndOfLine {
                break;
            }

            self.next_token()?;
        }

        Ok(())
    }

    fn unexpected_eof<T>(&self) -> Result<T> {
        bail!("Unexpected End-Of-File");
    }
}
