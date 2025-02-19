use std::{
    collections::VecDeque,
    io::{BufRead, BufReader, Read},
};

use anyhow::{anyhow, bail, Result};
use serde::Serialize;

const KEYWORDS: [&str; 37] = [
    "Global", "Local", "If", "Then", "ElseIf", "Else", "EndIf", "Include", "While", "Wend", "Null",
    "Select", "Case", "Default", "End", "Repeat", "Forever", "Until", "Dim", "For", "To", "Each",
    "Next", "Exit", "New", "Insert", "Before", "After", "First", "Last", "Type", "Field",
    "Function", "Return", "And", "Or", "Not", // Syntaxical keywords
];

const FUNCTION_KEYWORDS: [&str; 14] = [
    "Print",
    "AppTitle",
    "Graphics",
    "Cls",
    "Text",
    "DrawImage",
    "MaskImage",
    "PlaySound",
    "PlaySound_Strict",
    "FreeEntity",
    "PositionEntity",
    "ResetEntity",
    "ShowEntity",
    "AASetFont",
];

const OPERATORS: [&str; 16] = [
    "(", ")", "=", "<>", "<", ">", "<=", ">=", "+", "-", "*", "/", "^", ",", ".", ":",
];

const IDENT_STRING_SUFFIX: char = '$';
const IDENT_FLOAT_SUFFIX: char = '#';
const IDENT_INT_SUFFIX: char = '%';
const IDENT_TYPE_SUFFIX: char = '.';
const IDENT_PATH_SUFFIX: char = '\\';

// const IDENT_SUFFIXES: [char; 5] = [
//     IDENT_STRING_SUFFIX,
//     IDENT_FLOAT_SUFFIX,
//     IDENT_INT_SUFFIX,
//     IDENT_TYPE_SUFFIX,
//     IDENT_PATH_SUFFIX,
// ];

#[derive(Clone, Debug, Serialize)]
pub enum IdentTyping {
    String,
    Float,
    Integer,
    Type(String),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenTypeId {
    Keyword,
    FunctionKeyword,
    Ident,
    Path,
    Operator,
    StringLiteral,
    IntegerLiteral,
    FloatLiteral,
    EndOfLine,
}

#[derive(Clone, Debug)]
pub enum TokenType {
    Keyword,
    FunctionKeyword,
    Ident(Option<IdentTyping>),
    Path(Vec<String>, Option<IdentTyping>),
    Operator,
    StringLiteral,
    IntegerLiteral(i64),
    FloatLiteral(f64),
    EndOfLine,
}

impl TokenType {
    #[inline]
    pub fn to_id(&self) -> TokenTypeId {
        match self {
            TokenType::Keyword => TokenTypeId::Keyword,
            TokenType::FunctionKeyword => TokenTypeId::FunctionKeyword,
            TokenType::Ident(_) => TokenTypeId::Ident,
            TokenType::Path(_, _) => TokenTypeId::Path,
            TokenType::Operator => TokenTypeId::Operator,
            TokenType::StringLiteral => TokenTypeId::StringLiteral,
            TokenType::IntegerLiteral(_) => TokenTypeId::IntegerLiteral,
            TokenType::FloatLiteral(_) => TokenTypeId::FloatLiteral,
            TokenType::EndOfLine => TokenTypeId::EndOfLine,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub content: String,
    pub token_type: TokenType,
    pub source_path: String,
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn is(&self, tk_type: TokenTypeId, content: &str) -> bool {
        tk_type == self.token_type.to_id() && content == self.content
    }
}

pub struct Tokenizer<R: Read> {
    source_path: String,
    source: BufReader<R>,
    chars: VecDeque<char>,
    current_line: usize,
    current_column: usize,
    token_line: usize,
    token_column: usize,
}

impl<R: Read> Tokenizer<R> {
    pub fn line(&self) -> usize {
        self.current_line
    }

    pub fn column(&self) -> usize {
        self.current_column
    }

    pub fn source_path(&self) -> String {
        self.source_path.clone()
    }

    pub fn new(source_path: String, source: R) -> Self {
        Self {
            source_path,
            source: BufReader::new(source),
            chars: VecDeque::new(),
            current_line: 0,
            current_column: 1,
            token_line: 0,
            token_column: 1,
        }
    }

    pub fn next_token(&mut self) -> Result<Option<Token>> {
        loop {
            let Some(curr_char) = self.current_char()? else {
                return Ok(None);
            };

            if curr_char == ';' {
                // We just clear the remaining of the line
                self.chars.clear();
            } else if curr_char != ' ' && curr_char != '\t' {
                break;
            }

            self.next_char()?;
        }

        // SAFETY: we know it is Some as it is checked in the previous loop.
        let current_char = self.current_char()?.unwrap();

        self.token_line = self.current_line;
        self.token_column = self.current_column;

        if current_char.is_alphabetic() {
            self.handle_word()
        } else if current_char == '"' {
            self.handle_string()
        } else if current_char.is_numeric() {
            self.handle_number()
        } else if current_char == '\n' {
            self.next_char()?;
            Ok(Token {
                content: "\n".to_string(),
                source_path: self.source_path.clone(),
                token_type: TokenType::EndOfLine,
                column: self.token_column,
                line: self.token_line,
            })
        } else {
            self.handle_operator()
        }
        .map(Option::Some)
    }

    fn handle_word(&mut self) -> Result<Token> {
        let mut word_buffer = String::new();

        word_buffer.push(
            self.current_char()?
                .expect("Should be called AFTER the character is checked"),
        );

        while let Some(v) = self.next_char()? {
            if v.is_alphanumeric() || v == '_' {
                word_buffer.push(v);
            } else {
                break;
            }
        }

        let token_type = match self.current_char()? {
            Some(IDENT_STRING_SUFFIX) => TokenType::Ident(Some(IdentTyping::String)),
            Some(IDENT_FLOAT_SUFFIX) => TokenType::Ident(Some(IdentTyping::Float)),
            Some(IDENT_INT_SUFFIX) => TokenType::Ident(Some(IdentTyping::Integer)),
            Some(IDENT_TYPE_SUFFIX) => TokenType::Ident(Some(IdentTyping::Type({
                self.next_char()?;

                // FIXME: Generate error when the Ident has a typing

                self.handle_word()?.content
            }))),
            Some(IDENT_PATH_SUFFIX) => {
                let mut path_components = vec![word_buffer.clone()];
                let last_ident_type;

                {
                    self.next_char()?;

                    // FIXME: Generate error when the Ident has a typing

                    let next = self.handle_word()?;

                    match next.token_type {
                        // TODO: Handle Keyword and FunctionKeyword
                        // TokenType::Keyword => todo!(),
                        // TokenType::FunctionKeyword => todo!(),
                        TokenType::Ident(ident_type) => {
                            path_components.push(next.content);

                            last_ident_type = ident_type;
                        }
                        TokenType::Path(components, ident_type) => {
                            path_components.extend(components);
                            last_ident_type = ident_type;
                        }
                        _ => unreachable!(),
                    }
                }

                TokenType::Path(path_components, last_ident_type)
            }
            _ if KEYWORDS.contains(&word_buffer.as_str()) => TokenType::Keyword,
            _ if FUNCTION_KEYWORDS.contains(&word_buffer.as_str()) => TokenType::FunctionKeyword,
            _ => TokenType::Ident(None),
        };

        if let TokenType::Ident(Some(ref v)) = token_type {
            match v {
                IdentTyping::Type(_) => {}
                _ => {
                    self.next_char()?;
                }
            }
        }

        Ok(Token {
            content: word_buffer,
            token_type,
            source_path: self.source_path.clone(),
            column: self.token_column,
            line: self.token_line,
        })
    }

    fn handle_string(&mut self) -> Result<Token> {
        let mut string_buffer = String::new();

        let initial_delimiter = self.current_char()?;
        if !matches!(initial_delimiter, Some('"')) {
            return Err(anyhow!("Unexpected delimiter: {initial_delimiter:?}"));
        }

        loop {
            match self.next_char()? {
                Some('"') => break,
                Some(v) => string_buffer.push(v),
                None => {
                    return Err(anyhow!(
                        "Unexpected end of line while expecting string delimiter"
                    ))
                }
            }
        }

        // Consuming the last delimiter
        let _ = self.next_char();

        Ok(Token {
            content: string_buffer,
            token_type: TokenType::StringLiteral,
            source_path: self.source_path.clone(),
            column: self.token_column,
            line: self.token_line,
        })
    }

    fn handle_number(&mut self) -> Result<Token> {
        let mut is_float = false;
        let mut number_buffer = String::new();

        let initial_number = self.current_char()?;

        if !initial_number.is_some_and(|c| c.is_ascii_digit()) {
            bail!(
                "handle_number called but current_char is not a digit (found {initial_number:?})"
            );
        }

        number_buffer.push(initial_number.unwrap());

        while let Some(zarma) = self.next_char()? {
            if zarma.is_ascii_digit() {
                number_buffer.push(zarma);
            } else if zarma == '.' {
                if is_float {
                    bail!("handle_number encountered multiple . in a float number")
                }

                number_buffer.push(zarma);
                is_float = true;
            } else {
                break;
            }
        }

        let token_type = if is_float {
            TokenType::FloatLiteral(number_buffer.parse()?)
        } else {
            TokenType::IntegerLiteral(number_buffer.parse()?)
        };

        Ok(Token {
            content: number_buffer,
            token_type,
            source_path: self.source_path.clone(),
            column: self.token_column,
            line: self.token_line,
        })
    }

    fn handle_operator(&mut self) -> Result<Token> {
        let mut operator_buffer = String::new();

        operator_buffer.push(
            self.current_char()?
                .expect("Should be called AFTER the character is checked"),
        );

        let mut match_exact: Option<usize> = None;
        let mut match_start: Vec<usize> = Vec::new();

        for (idx, op) in OPERATORS.iter().enumerate() {
            if *op == operator_buffer {
                match_exact = Some(idx);
            } else if op.starts_with(&operator_buffer) {
                match_start.push(idx);
            }
        }

        if let Some(next_char) = self.next_char()? {
            for idx in match_start {
                if OPERATORS[idx].ends_with(next_char) {
                    self.next_char()?;

                    return Ok(Token {
                        content: OPERATORS[idx].to_string(),
                        token_type: TokenType::Operator,
                        source_path: self.source_path.clone(),
                        column: self.token_column,
                        line: self.token_line,
                    });
                }
            }
        }

        if match_exact.is_some() {
            return Ok(Token {
                content: operator_buffer,
                token_type: TokenType::Operator,
                source_path: self.source_path.clone(),
                column: self.token_column,
                line: self.token_line,
            });
        }

        Err(anyhow!("Unknown operator: {operator_buffer}"))
    }

    fn current_char(&mut self) -> Result<Option<char>> {
        if let Some(c) = self.chars.front() {
            Ok(Some(*c))
        } else {
            self.next_char()
        }
    }

    fn next_char(&mut self) -> Result<Option<char>> {
        self.chars.pop_front();

        if let Some(c) = self.chars.front() {
            self.current_column += 1;
            Ok(Some(*c))
        } else {
            let mut buf = String::new();

            // Thanks BASIC syntax which allows only one statement per line
            self.source.read_line(&mut buf)?;

            self.chars = buf.chars().collect::<VecDeque<char>>();

            self.current_line += 1;
            self.current_column = 1;

            Ok(self.chars.front().cloned())
        }
    }
}
