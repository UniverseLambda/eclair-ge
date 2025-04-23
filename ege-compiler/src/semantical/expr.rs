use anyhow::bail;
use log::warn;
use serde::Serialize;

use crate::{
    parser::{BinaryExpr, BinaryExprOp, Expr, FunctionCall, IdentPath, UnaryExprOp},
    semantical::analyze::get_variable_type,
};

use super::{
    AnalyzedProgram, ForScope, FunctionInfo, Typing,
    analyze::{TypedGenerator, expect_typing, get_function_args_info, get_function_return_type},
};

#[derive(Debug, Clone, Serialize)]
pub struct TypedExpr {
    pub output_type: Typing,
    pub value: TypedExprValue,
}

impl TypedExpr {
    pub fn new(output_type: Typing, value: TypedExprValue) -> Self {
        Self { output_type, value }
    }

    // FIXME: new_*: always check the type of value
    pub fn new_int(value: TypedExprValue) -> Self {
        Self {
            output_type: Typing::Integer,
            value,
        }
    }

    pub fn new_float(value: TypedExprValue) -> Self {
        Self {
            output_type: Typing::Float,
            value,
        }
    }

    pub fn new_string(value: TypedExprValue) -> Self {
        Self {
            output_type: Typing::String,
            value,
        }
    }

    pub fn new_null() -> Self {
        Self {
            output_type: Typing::Integer,
            value: TypedExprValue::Null,
        }
    }

    fn new_float_to_int(value: TypedExpr) -> anyhow::Result<Self> {
        Ok(Self {
            output_type: Typing::Integer,
            value: TypedExprValue::FloatToInt(Box::new(value)),
        })
    }

    fn new_int_to_float(value: TypedExpr) -> anyhow::Result<Self> {
        Ok(Self {
            output_type: Typing::Float,
            value: TypedExprValue::IntToFloat(Box::new(value)),
        })
    }

    fn new_string_to_int(value: TypedExpr) -> anyhow::Result<Self> {
        Ok(Self {
            output_type: Typing::Integer,
            value: TypedExprValue::StringToInt(Box::new(value)),
        })
    }

    fn new_string_to_float(value: TypedExpr) -> anyhow::Result<Self> {
        Ok(Self {
            output_type: Typing::Float,
            value: TypedExprValue::StringToFloat(Box::new(value)),
        })
    }

    fn new_int_to_string(value: TypedExpr) -> anyhow::Result<Self> {
        Ok(Self {
            output_type: Typing::String,
            value: TypedExprValue::IntToString(Box::new(value)),
        })
    }

    fn new_float_to_string(value: TypedExpr) -> anyhow::Result<Self> {
        Ok(Self {
            output_type: Typing::String,
            value: TypedExprValue::FloatToString(Box::new(value)),
        })
    }

    pub fn cast_to(self, target_type: Typing) -> anyhow::Result<Self> {
        match target_type {
            Typing::Integer => self.cast_to_int(),
            Typing::Float => self.cast_to_float(),
            Typing::String => self.cast_to_string(),
            Typing::Void => unreachable!(),
            v => bail!("cannot cast to {v:?} (only Int, Float or String)"),
        }
    }

    pub fn cast_to_int(self) -> anyhow::Result<Self> {
        match self.output_type {
            Typing::Integer => Ok(self),
            Typing::Float => TypedExpr::new_float_to_int(self),
            Typing::String => TypedExpr::new_string_to_int(self),
            Typing::Void => unreachable!(),
            v => bail!("cannot cast {v:?} to Int (expected Float or String)"),
        }
    }

    pub fn cast_to_float(self) -> anyhow::Result<Self> {
        match self.output_type {
            Typing::Integer => TypedExpr::new_int_to_float(self),
            Typing::Float => Ok(self),
            Typing::String => TypedExpr::new_string_to_float(self),
            Typing::Void => unreachable!(),
            v => bail!("cannot cast {v:?} to Float (expected Int or String)"),
        }
    }

    pub fn cast_to_string(self) -> anyhow::Result<Self> {
        match self.output_type {
            Typing::Integer => TypedExpr::new_int_to_string(self),
            Typing::Float => TypedExpr::new_float_to_string(self),
            Typing::String => Ok(self),
            Typing::Void => unreachable!(),
            v => bail!("cannot cast {v:?} to Float (expected Int or Float)"),
        }
    }
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
    CollectionAccess(CollectionAccess),
    AllocStruct(AllocStruct),
    Unary(Box<TypedExpr>, UnaryExprOp),

    // Internals only
    ConcatStr(Box<TypedExpr>, Box<TypedExpr>),
    ConcatInt(Box<TypedExpr>, Box<TypedExpr>),
    ConcatFloat(Box<TypedExpr>, Box<TypedExpr>),
    IntToFloat(Box<TypedExpr>),
    FloatToInt(Box<TypedExpr>),
    IntToString(Box<TypedExpr>),
    FloatToString(Box<TypedExpr>),
    StringToFloat(Box<TypedExpr>),
    StringToInt(Box<TypedExpr>),
}

#[derive(Debug, Clone, Serialize)]
pub struct TypedFunctionCall {
    pub name: String,
    pub output_type: Typing,
    pub args: Vec<TypedExpr>,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypedBinaryExpr {
    pub left: Box<TypedExpr>,
    pub right: Box<TypedExpr>,
    pub op: BinaryExprOp,
    pub output_type: Typing,
}

impl TypedBinaryExpr {
    pub fn numeric_insert_cast(
        mut left: TypedExpr,
        mut right: TypedExpr,
    ) -> anyhow::Result<(TypedExpr, TypedExpr)> {
        if !matches!(left.output_type, Typing::Integer | Typing::Float)
            || !matches!(right.output_type, Typing::Integer | Typing::Float)
        {
            anyhow::bail!("expected numeric value (Int or Float)");
        }

        let final_type = if left.output_type == Typing::Float || right.output_type == Typing::Float
        {
            Typing::Float
        } else {
            Typing::Integer
        };

        if final_type == Typing::Float && left.output_type != right.output_type {
            if left.output_type == Typing::Integer {
                left = TypedExpr::new_int_to_float(left)?;
            } else {
                right = TypedExpr::new_int_to_float(right)?;
            }
        }
        Ok((left, right))
    }
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

macro_rules! gen_typed {
    ($value:ident($program:ident,$func:ident, $for_scope:ident), $target:ident, $typing_field:ident) => {{
        let res = ($value).generate_typed($program, $func, $for_scope)?;

        Ok(TypedExpr {
            output_type: res.$typing_field.clone(),
            value: TypedExprValue::$target(res),
        })
    }};
}

impl TypedGenerator for Expr {
    type TypedOutput = TypedExpr;

    fn generate_typed(
        self,
        pgm: &mut AnalyzedProgram,
        func: &mut Option<FunctionInfo>,
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<Self::TypedOutput> {
        match self {
            Expr::Integer(v) => Ok(TypedExpr::new_int(TypedExprValue::Integer(v))),
            Expr::Float(v) => Ok(TypedExpr::new_float(TypedExprValue::Float(v))),
            Expr::String(v) => Ok(TypedExpr::new_string(TypedExprValue::String(v))),
            Expr::Null => Ok(TypedExpr::new_null()),
            Expr::Function(function_call) => {
                gen_typed!(
                    function_call(pgm, func, for_scope),
                    FunctionCall,
                    output_type
                )
            }
            Expr::Binary(binary_expr) => binary_expr.generate_typed(pgm, func, for_scope),
            Expr::Path(ident_path) => ident_path.generate_typed(pgm, func, for_scope),
            Expr::CollectionFirst(ident) => {
                // FIXME: Check whether or not the type exists.
                let typing = Typing::from(ident.ident_type);

                Ok(TypedExpr {
                    output_type: typing.clone(),
                    value: TypedExprValue::CollectionAccess(CollectionAccess {
                        struct_name: ident.name,
                        last: false,
                        output_type: typing,
                    }),
                })
            }
            Expr::CollectionLast(ident) => {
                // FIXME: Check whether or not the type exists.
                let typing = Typing::from(ident.ident_type);

                Ok(TypedExpr {
                    output_type: typing.clone(),
                    value: TypedExprValue::CollectionAccess(CollectionAccess {
                        struct_name: ident.name,
                        last: true,
                        output_type: typing,
                    }),
                })
            }
            Expr::New(ident) => {
                // FIXME: Check whether or not the type exists.
                let typing = Typing::from(ident.ident_type);

                let struct_name = match typing {
                    Typing::Struct(ref v) => v,
                    v => bail!("expected Type, but got {v:?}"),
                };

                Ok(TypedExpr {
                    output_type: Typing::Struct(struct_name.clone()),
                    value: TypedExprValue::AllocStruct(AllocStruct {
                        struct_name: struct_name.clone(),
                        output_type: typing,
                    }),
                })
            }
            Expr::Unary(unary_expr) => {
                let value = Box::new(unary_expr.value.generate_typed(pgm, func, for_scope)?);

                Ok(TypedExpr {
                    output_type: value.output_type.clone(),
                    value: TypedExprValue::Unary(value, unary_expr.op),
                })
            }
        }
    }
}

impl TypedGenerator for FunctionCall {
    type TypedOutput = TypedFunctionCall;

    fn generate_typed(
        self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<Self::TypedOutput> {
        // TODO: implement Array access
        // FIXME: generate an error or a warning when type from ident does not match the one from the registrar.

        let name = self.ident.name;
        let mut params = self.params.generate_typed(program, function, for_scope)?;
        let func_in = get_function_args_info(program, &name)?;
        let func_out = get_function_return_type(program, &name)?;

        for (idx, param) in params.iter().enumerate() {
            let Some(expected_param) = func_in.get(idx) else {
                bail!(
                    "too many argument in call to `{name}`: expected at most {} arguments, but got only {} arguments",
                    func_in.len(),
                    params.len()
                );
            };

            expect_typing(&param.output_type, &expected_param.var_info.typing)?;
        }

        if params.len() < func_in.len() {
            for (idx, to_fill) in func_in.iter().enumerate().skip(params.len()) {
                let Some(ref default_value) = to_fill.default_value else {
                    bail!(
                        "missing argument in call to `{name}`: no default value for argument at index {idx}"
                    );
                };

                params.push(default_value.generate_typed(program, function, for_scope)?);
            }
        }

        Ok(TypedFunctionCall {
            name,
            output_type: func_out,
            args: params,
        })
    }
}

impl TypedGenerator for BinaryExpr {
    type TypedOutput = TypedExpr;

    fn generate_typed(
        self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<Self::TypedOutput> {
        let left = self.left.generate_typed(program, function, for_scope)?;
        let right = self.right.generate_typed(program, function, for_scope)?;
        let op = self.op;

        if left.output_type == Typing::String {
            let res = match right.output_type {
                Typing::Integer => TypedExprValue::ConcatInt(Box::new(left), Box::new(right)),
                Typing::Float => TypedExprValue::ConcatFloat(Box::new(left), Box::new(right)),
                Typing::String => TypedExprValue::ConcatStr(Box::new(left), Box::new(right)),
                v => bail!("cannot concat {v:?} to a string (expected Int, Float or String)"),
            };

            Ok(TypedExpr {
                output_type: Typing::String,
                value: res,
            })
        } else {
            let (left, right) = TypedBinaryExpr::numeric_insert_cast(left, right)?;

            let value = TypedBinaryExpr {
                output_type: left.output_type.clone(),
                left: Box::new(left),
                right: Box::new(right),
                op,
            };

            Ok(TypedExpr {
                output_type: value.output_type.clone(),
                value: TypedExprValue::Binary(value),
            })
        }
    }
}

impl TypedGenerator for IdentPath {
    type TypedOutput = TypedExpr;

    fn generate_typed(
        mut self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<Self::TypedOutput> {
        let root_component = self.components.remove(0);
        let root_struct = get_variable_type(program, function, for_scope, &root_component)?;
        let mut expr = TypedExpr::new(
            root_struct.clone(),
            TypedExprValue::VariableAccess(VarAccess {
                var_name: root_component,
                var_type: root_struct,
            }),
        );

        for field_name in self.components.drain(..) {
            let parent_struct_name = match expr.output_type {
                Typing::Struct(ref v) => v,
                v => bail!("expected Type, but got {v:?}"),
            };

            let parent_struct = program.get_struct_info(parent_struct_name)?;
            let field_type = parent_struct.field_type(&field_name)?;

            expr = TypedExpr::new(
                field_type.clone(),
                TypedExprValue::FieldAccess(FieldAccess {
                    parent: Box::new(expr),
                    field_name,
                    field_type,
                }),
            )
        }

        let final_type = Typing::from(self.final_type);
        if final_type != expr.output_type {
            warn!(
                "ident path typed as {:?}, but type analysis returned {final_type:?}",
                expr.output_type
            );
        }

        Ok(expr)
    }
}
