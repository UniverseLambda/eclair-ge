use serde::Serialize;

use crate::parser::{InsertPivot, InsertRelPos};

use super::{expr::{TypedExpr, TypedFunctionCall}, Typing};

#[derive(Debug, Clone, Serialize)]
pub struct TypedStatement {
	pub output_value: Typing,
	pub inner: TypedStatementInner,
}

#[derive(Debug, Clone, Serialize)]
pub enum TypedStatementInner {
    FunctionCall(TypedFunctionCall),
    VarAssign(TypedVarAssign),
    ArrayDecl(TypedArrayDecl),
    If(TypedIf),
    For(TypedForLoop),
    Repeat(TypedRepeatLoop),
    Insert(Insert),
    Exit,
    Select(TypedSelect),
    Return(Return),
}

#[derive(Debug, Clone, Serialize)]
pub struct TypedVarAssign {
	pub var_name: String,
	pub var_type: Typing,
	pub value: TypedExpr,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypedArrayDecl {
	pub name: String,
	pub array_type: Typing,
	pub size: Vec<TypedExpr>,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypedIf {
	pub cond: TypedExpr,
	pub statements: Vec<TypedStatement>,
	pub else_if: Vec<TypedOtherwise>,
}

#[derive(Debug, Clone, Serialize)]
pub enum TypedOtherwise {
	ElseIf {
		cond: TypedExpr,
        statements: Vec<TypedStatement>,
    },
    Else {
		statements: Vec<TypedStatement>,
    },
}

#[derive(Debug, Clone, Serialize)]
pub struct TypedForLoop {
	pub name: String,
	pub statements: Vec<TypedStatement>,
	pub mode: TypedForLoopMode,
}


#[derive(Debug, Clone, Serialize)]
pub enum TypedForLoopMode {
	Range { from: TypedExpr, to: TypedExpr },
    Each { iterator: TypedExpr },
}


#[derive(Debug, Clone, Serialize)]
pub struct TypedRepeatLoop {
	pub statements: Vec<TypedStatement>,
}


// Insert <source> <rel_pos> <pivot> <collection>

#[derive(Debug, Clone, Serialize)]
pub struct Insert {
	pub source_name: String,
	pub rel_pos: InsertRelPos,
	pub pivot: InsertPivot,
	pub collection: String,
}


#[derive(Debug, Clone, Serialize)]
pub struct TypedSelect {
	pub value: TypedExpr,
	pub cases: Vec<TypedSelectCase>,
	pub default_case: Option<Vec<TypedStatement>>,
}


#[derive(Debug, Clone, Serialize)]
pub struct TypedSelectCase {
	pub values: Vec<TypedExpr>,
	pub statements: Vec<TypedStatement>,
}


#[derive(Debug, Clone, Serialize)]
pub struct Return {
	pub value: Option<TypedExpr>,
	pub output_type: Typing,
}
