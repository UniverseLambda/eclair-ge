use std::collections::HashMap;

use anyhow::bail;
use serde::Serialize;

use crate::lexer::IdentTyping;

mod analyze;
mod expr;

pub use analyze::analyze_program;

#[derive(Clone, Debug, Serialize)]
pub struct AnalyzedProgram {
    pub structs: HashMap<String, StructInfo>,
    pub functions: HashMap<String, FunctionInfo>,
    pub global_vars: HashMap<String, VarInfo>,

    pub builtin_constants: HashMap<String, Constant>,
}

#[derive(Clone, Debug, Serialize)]
pub struct StructInfo {
    pub name: String,
    pub fields: Vec<VarInfo>,
}

#[derive(Clone, Debug, Serialize)]
pub struct FunctionInfo {
    pub name: String,
    pub return_type: Typing,
    pub args: HashMap<String, ArgInfo>,
    pub args_order: Vec<String>,
    pub vars: HashMap<String, VarInfo>,
    pub phase0_checked: bool,
}

#[derive(Clone, Debug, Serialize)]
pub struct VarInfo {
    pub name: String,
    pub typing: Typing,
}

#[derive(Clone, Debug, Serialize)]
pub struct ArgInfo {
    pub var_info: VarInfo,
    pub default_value: Option<Constant>,
}

#[derive(Clone, Debug, Serialize)]
pub enum Constant {
    Float(f64),
    Int(i64),
    String(String),
    Null,
}

impl TryFrom<crate::parser::Expr> for Constant {
    type Error = anyhow::Error;

    fn try_from(value: crate::parser::Expr) -> Result<Self, Self::Error> {
        match value {
            crate::parser::Expr::String(v) => Ok(Constant::String(v)),
            crate::parser::Expr::Integer(v) => Ok(Constant::Int(v)),
            crate::parser::Expr::Float(v) => Ok(Constant::Float(v)),
            crate::parser::Expr::Null => Ok(Constant::Null),
            // TODO: Trying to const eval those values.
            // crate::parser::Expr::Binary(binary_expr) => todo!(),
            // crate::parser::Expr::Unary(unary_expr) => todo!(),
            _ => bail!("Expected constant value"),
        }
    }
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum Typing {
    Void,
    Integer,
    Float,
    String,
    Array {
        inner_type: Box<Typing>,
        dimension_count: usize,
    },
    Struct(String),
}

impl From<Option<IdentTyping>> for Typing {
    fn from(value: Option<IdentTyping>) -> Self {
        match value.unwrap_or(IdentTyping::Integer) {
            IdentTyping::String => Typing::String,
            IdentTyping::Float => Typing::Float,
            IdentTyping::Integer => Typing::Integer,
            IdentTyping::Type(struct_name) => Typing::Struct(struct_name),
        }
    }
}
