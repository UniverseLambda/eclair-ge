use std::io::Read;

use anyhow::{bail, Context, Ok, Result};
pub use ast_type::{
    ArrayDecl, ArrayDecls, BinaryExpr, BinaryExprOp, Expr, ForLoop, ForLoopMode, FunctionCall,
    FunctionDecl, IdentPath, If, Include, Insert, InsertPivot, InsertRelPos, NoDataStatement,
    Otherwise, PackedDecl, Parsable, Program, RepeatLoop, Return, Select, SelectCase, Statement,
    UnaryExprOp, VarAssign, VarScope,
};
use ext::TokenExt;
use log::{debug, trace, warn};

use crate::lexer::{Token, TokenType, TokenTypeId, Tokenizer};

mod ast_type;
mod ext;

pub struct Parser<R: Read> {
    token_source: Tokenizer<R>,
    _current_token: Option<Token>,
    _peeked_token: Option<Token>,
    expect_inlinable: bool,
}

impl<R: Read> Parser<R> {
    pub fn new(token_source: Tokenizer<R>) -> Self {
        Self {
            token_source,
            _current_token: None,
            _peeked_token: None,
            expect_inlinable: false,
        }
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        let mut statements = Vec::new();

        while let Some(statement) = self
            .parse_statement()
            .with_context(|| "Parser::parse_program")?
        {
            statements.push(statement);
        }

        Ok(Program { statements })
    }

    pub fn parse_statement(&mut self) -> Result<Option<Statement>> {
        self.skip_line_returns()?;

        let Some(disc) = self.current_token()? else {
            return Ok(None);
        };

        expect_any_token_type(
            &disc,
            &[
                TokenTypeId::Keyword,
                TokenTypeId::Ident,
                TokenTypeId::Path,
                TokenTypeId::FunctionKeyword,
            ],
        )?;

        let statement = match (disc.token_type, disc.content.as_str()) {
            (TokenType::Keyword, "Function") => {
                FunctionDecl::parse(self).map(Statement::FunctionDecl)?
            }
            (TokenType::Keyword, "Local" | "Global") => {
                VarAssign::parse(self).map(Statement::VarAssign)?
            }
            (TokenType::Keyword, "If") => If::parse(self).map(Statement::If)?,
            (TokenType::Keyword, "For") => ForLoop::parse(self).map(Statement::For)?,
            (TokenType::Keyword, "Repeat") => RepeatLoop::parse(self).map(Statement::Repeat)?,
            (TokenType::Keyword, "Type") => PackedDecl::parse(self).map(Statement::PackedDecl)?,
            (TokenType::Keyword, "Exit") => NoDataStatement::parse(self).map(Statement::NoData)?,
            (TokenType::Keyword, "Insert") => Insert::parse(self).map(Statement::Insert)?,
            (TokenType::Keyword, "Include") => Include::parse(self).map(Statement::Include)?,
            (TokenType::Keyword, "Select") => Select::parse(self).map(Statement::Select)?,
            (TokenType::Keyword, "Dim") => ArrayDecls::parse(self).map(Statement::ArrayDecl)?,
            (TokenType::Keyword, "Return") => Return::parse(self).map(Statement::Return)?,
            (TokenType::Ident(_), _) => self.parse_statement_from_ident()?,
            (TokenType::Path(_, _), _) => VarAssign::parse(self).map(Statement::VarAssign)?,
            (TokenType::FunctionKeyword, _) => {
                FunctionCall::parse(self).map(Statement::FunctionCall)?
            }
            (t, v) => bail!(
                "{}:{}:{}: unexpected token: `{v}` (type: {t:?})",
                disc.source_path,
                disc.line,
                disc.column
            ),
        };

        if self.expect_inlinable && !statement.is_inlinable() {
            bail!(
                "{}:{}:{}: statement {statement:?} is not inlinable.",
                disc.source_path,
                disc.line,
                disc.column
            );
        }

        self.expect_inlinable = false;

        if let Some(token) = self.current_token()? {
            if token.is(TokenTypeId::Operator, ":") {
                self.consume_token();

                self.expect_inlinable = true;

                if !statement.is_inlinable() {
                    bail!(
                        "{}:{}:{}: statement {statement:?} is not inlinable.",
                        disc.source_path,
                        disc.line,
                        disc.column
                    );
                }
            }
        }

        Ok(Some(statement))
    }

    fn parse_statement_from_ident(&mut self) -> Result<Statement> {
        debug!(
            "parse_statement_from_ident: current_token: {:?}, peeked: {:?}",
            self.current_token()?,
            self.peek_token()?
        );

        let Some(peeked) = self.peek_token()? else {
            return self
                .unexpected_eof()
                .with_context(|| "Parser::parse_statement_from_ident");
        };

        match peeked.expect_any_content(&["=", "("])?.content.as_str() {
            "=" => VarAssign::parse(self).map(Statement::VarAssign),
            "(" => FunctionCall::parse(self).map(Statement::FunctionCall),
            _ => unreachable!(),
        }
    }

    fn parse_statement_block(
        &mut self,
        block_end: &str,
        no_end_token: bool,
        early_stoppers: &[&str],
    ) -> Result<(Vec<Statement>, Token)> {
        let mut statements = Vec::new();

        let concat_end_token = if no_end_token {
            String::new()
        } else {
            format!("End{block_end}")
        };

        let block_stopper;

        'main_loop: loop {
            self.skip_line_returns()?;

            let Some(token) = self.current_token()? else {
                return self
                    .unexpected_eof()
                    .with_context(|| "Parser::parse_statement_block");
            };

            if no_end_token {
                if token.is(TokenTypeId::Keyword, block_end) {
                    block_stopper = token.clone();
                    break;
                }
            } else if token.is(TokenTypeId::Keyword, &concat_end_token) {
                block_stopper = token.clone();
                break;
            } else if token.is(TokenTypeId::Keyword, "End") {
                if let Some(nxt_token) = self.next_token()? {
                    if nxt_token.is(TokenTypeId::EndOfLine, "\n") {
                        warn!(
                            "{}:{}:{}: missing terminator after End (LF)",
                            nxt_token.source_path, nxt_token.line, nxt_token.column
                        );

                        block_stopper = token;
                        break;
                    }

                    expect_token_type(&nxt_token, TokenTypeId::Keyword)?;
                    expect_token_content(&nxt_token, block_end)?;

                    block_stopper = nxt_token.clone();
                    break;
                } else {
                    warn!(
                        "{}:{}:{}: missing terminator after End (EOF)",
                        token.source_path, token.line, token.column
                    );

                    block_stopper = token;
                    break;
                }
            }

            for stopper in early_stoppers {
                if token.is(TokenTypeId::Keyword, stopper) {
                    block_stopper = token.clone();
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

    fn might_be_a_func_call(&mut self, current_expr: Expr) -> Result<Expr> {
        debug!(
            "might_be_a_func_call: current_token: {:?}, peeked: {:?}",
            self.current_token()?,
            self.peek_token()?
        );

        if let Expr::Path(ident_path) = current_expr {
            if ident_path.components.len() != 1 {
                self.consume_token();
                return Ok(Expr::Path(ident_path));
            }

            let Some(peeked_token) = self.peek_token()? else {
                self.consume_token();

                return Ok(Expr::Path(ident_path));
            };

            if peeked_token.token_type.to_id() == TokenTypeId::Operator
                && peeked_token.content == "("
            {
                FunctionCall::parse(self).map(Expr::Function)
            } else {
                self.consume_token();

                Ok(Expr::Path(ident_path))
            }
        } else {
            self.consume_token();

            Ok(current_expr)
        }
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
            .inspect(|token| trace!("TOKEN: {token:?}")))
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

    fn skip_line_returns(&mut self) -> Result<()> {
        while let Some(token) = self.current_token()? {
            if token.token_type.to_id() != TokenTypeId::EndOfLine {
                break;
            }

            if self.expect_inlinable {
                bail!(
                    "{}:{}:{}: expected inlinable statement, but got line return.",
                    token.source_path,
                    token.line,
                    token.column
                );
            }

            self.next_token()?;
        }

        Ok(())
    }

    fn unexpected_eof<T>(&self) -> Result<T> {
        bail!(
            "{}:{}:{}: unexpected End-Of-File",
            self.token_source.source_path(),
            self.token_source.line(),
            self.token_source.column()
        );
    }
}

fn expect_token(token: &Token, tktype: TokenTypeId, content: &str) -> Result<()> {
    if token.token_type.to_id() == tktype && token.content == content {
        Ok(())
    } else {
        bail!(
            "{}:{}:{}: unexpected token: {token:?}, expected: {content:?} (type: {tktype:?})",
            token.source_path,
            token.line,
            token.column
        );
    }
}

fn expect_token_type(token: &Token, tktype: TokenTypeId) -> Result<()> {
    expect_any_token_type(token, &[tktype])
}

fn expect_token_content(token: &Token, content: &str) -> Result<()> {
    if token.content == content {
        Ok(())
    } else {
        bail!(
            "{}:{}:{}: unexpected token: {token:?}, expected: {content:?}",
            token.source_path,
            token.line,
            token.column
        );
    }
}

fn expect_any_token_type(token: &Token, tktypes: &[TokenTypeId]) -> Result<()> {
    let curr = token.token_type.to_id();

    for tktype in tktypes {
        if *tktype == curr {
            return Ok(());
        }
    }

    bail!(
        "{}:{}:{}: unexpected token: {token:?}, expected: {tktypes:?}",
        token.source_path,
        token.line,
        token.column
    );
}
