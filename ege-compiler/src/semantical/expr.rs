use serde::Serialize;

use crate::parser::{BinaryExprOp, UnaryExprOp};

use super::Typing;

#[derive(Debug, Clone, Serialize)]
pub struct TypedExpr {
    pub output_type: Typing,
    pub value: TypedExprValue,
}

#[derive(Debug, Clone, Serialize)]
pub enum TypedExprValue {
    String(String),
    Integer(i64),
    Float(f64),
    Null,
	FunctionCall(TypedFunctionCall),
    Binary(TypedBinaryExpr),
	VariableAccess(VarAccess),
	FieldAccess(FieldAccess),
	CollectionAcces(CollectionAccess),
	AllocStruct(AllocStruct),
	Unary(UnaryExprOp),

	// Internals only
    ConcatStr(Box<TypedExpr>, Box<TypedExpr>),
    ConcatInt(Box<TypedExpr>, Box<TypedExpr>),
    ConcatFloat(Box<TypedExpr>, Box<TypedExpr>),
    IntToFloat(Box<TypedExpr>),
    FloatToInt(Box<TypedExpr>),
}


#[derive(Debug, Clone, Serialize)]
pub struct TypedFunctionCall {
    pub name: String,
	pub output_type: Typing,
    pub params: Vec<TypedExpr>,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypedBinaryExpr {
    pub left: Box<TypedExpr>,
    pub right: Box<TypedExpr>,
    pub op: BinaryExprOp,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypedUnaryExpr {
    pub value: Box<TypedExpr>,
    pub op: UnaryExprOp,
}

#[derive(Debug, Clone, Serialize)]
pub struct FieldAccess {
	pub parent: Box<TypedExpr>,
	pub field_name: String,
	pub field_type: Typing,
}

#[derive(Debug, Clone, Serialize)]
pub struct VarAccess {
	pub var_name: String,
	pub var_type: Typing,
}

#[derive(Debug, Clone, Serialize)]
pub struct CollectionAccess {
	pub struct_name: String,
	pub last: bool,
	pub output_type: Typing,
}


#[derive(Debug, Clone, Serialize)]
pub struct AllocStruct {
	pub struct_name: String,
	pub output_type: Typing,
}
