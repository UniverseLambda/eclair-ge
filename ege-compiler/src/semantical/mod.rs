use std::collections::HashMap;

use analyze::TypedGenerator;
use anyhow::{anyhow, bail};
use expr::{TypedExpr, TypedExprValue};
use serde::Serialize;
use statement::TypedStatement;

use crate::lexer::IdentTyping;

mod analyze;
mod expr;
mod statement;

pub use analyze::analyze_program;

#[derive(Clone, Debug, Serialize)]
pub struct AnalyzedProgram {
    pub structs: HashMap<String, StructInfo>,
    pub functions: HashMap<String, FunctionInfo>,
    pub global_vars: HashMap<String, VarInfo>,

    pub builtin_constants: HashMap<String, Constant>,

    pub statements: Vec<TypedStatement>,
}

impl AnalyzedProgram {
    pub fn get_struct_info(&self, name: &String) -> anyhow::Result<&StructInfo> {
        self.structs
            .get(name)
            .ok_or_else(|| anyhow!("undefined Type `{name}`"))
    }

    pub fn get_function_info_mut(&mut self, name: &String) -> anyhow::Result<&mut FunctionInfo> {
        self.functions
            .get_mut(name)
            .ok_or_else(|| anyhow!("undefined function `{name}`"))
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct StructInfo {
    pub name: String,
    pub fields: Vec<VarInfo>,
}

impl StructInfo {
    pub fn field_type(&self, field_name: &str) -> anyhow::Result<Typing> {
        self.fields
            .iter()
            .find_map(|v| {
                if v.name == field_name {
                    Some(v.typing.clone())
                } else {
                    None
                }
            })
            .ok_or_else(|| anyhow!("no field named `{}` in Type `{}`", field_name, self.name))
    }

    pub fn as_type(&self) -> Typing {
        Typing::Struct(self.name.clone())
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct FunctionInfo {
    pub name: String,
    pub return_type: Typing,
    pub args: HashMap<String, ArgInfo>,
    pub args_order: Vec<String>,
    pub vars: HashMap<String, VarInfo>,
    pub statements: Vec<TypedStatement>,

    pub phase0_checked: bool,
}

#[derive(Clone, Debug, Serialize)]
pub struct ForScope<'a> {
    pub var_name: String,
    pub var_type: Typing,
    pub previous: Option<&'a ForScope<'a>>,
}

impl<'a> ForScope<'a> {
    pub fn new(var_name: String, var_type: Typing, previous: Option<&'a ForScope<'a>>) -> Self {
        Self {
            var_name,
            var_type,
            previous,
        }
    }
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

impl TypedGenerator for &Constant {
    type TypedOutput = TypedExpr;

    fn generate_typed(
        self,
        _: &mut AnalyzedProgram,
        _: &mut Option<FunctionInfo>,
        _: Option<&ForScope>,
    ) -> anyhow::Result<Self::TypedOutput> {
        Ok(match self {
            Constant::Float(v) => TypedExpr::new_float(TypedExprValue::Float(*v)),
            Constant::Int(v) => TypedExpr::new_int(TypedExprValue::Integer(*v)),
            Constant::String(v) => TypedExpr::new_string(TypedExprValue::String(v.clone())),
            Constant::Null => TypedExpr::new_null(),
        })
    }
}
