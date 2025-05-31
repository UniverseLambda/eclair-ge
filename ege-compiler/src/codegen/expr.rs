use std::ffi::CString;

use either::Either;
use inkwell::{
    FloatPredicate, IntPredicate,
    values::{BasicValueEnum, CallSiteValue, InstructionValue},
};

use crate::{
    parser::BinaryExprOp,
    semantical::{
        Constant, TypedBinaryExpr, TypedExpr, TypedExprValue, TypedFunctionCall, Typing, VarAccess,
    },
};

use super::{Codegen, CodegenScopeInfo, CodegenState};

impl<'a, 'ctx> Codegen<'a, 'ctx> for TypedExpr
where
    'a: 'ctx,
{
    type CodegenOutput = inkwell::values::BasicValueEnum<'ctx>;

    fn codegen(
        &self,
        cg: CodegenState<'a, 'ctx>,
        scope: &'a CodegenScopeInfo,
    ) -> anyhow::Result<Self::CodegenOutput> {
        Ok(match &self.value {
            TypedExprValue::String(v) => cg
                .builder
                .build_global_string_ptr(v.as_str(), "const_str")? // TODO: make one global string per unique string
                .as_pointer_value()
                .into(),
            TypedExprValue::Integer(v) => cg.context.i64_type().const_int(*v as u64, true).into(),
            TypedExprValue::Float(v) => cg.context.f64_type().const_float(*v).into(),
            TypedExprValue::Null => cg.context.i64_type().const_zero().into(),
            TypedExprValue::FunctionCall(typed_function_call) => typed_function_call
                .codegen(cg, scope)?
                .try_as_basic_value()
                .expect_left("tried to get the return value of a void function"), // FIXME: Check during semantical analysis whether the code function called in an Expr is void
            TypedExprValue::Binary(typed_binary_expr) => typed_binary_expr.codegen(cg, scope)?,
            TypedExprValue::VariableAccess(var_access) => var_access.codegen(cg, scope)?,
            TypedExprValue::FieldAccess(field_access) => todo!(),
            TypedExprValue::CollectionAccess(collection_access) => todo!(),
            TypedExprValue::AllocStruct(alloc_struct) => todo!(),
            TypedExprValue::Unary(typed_expr, unary_expr_op) => todo!(),
            TypedExprValue::ConcatStr(typed_expr, typed_expr1) => todo!(),
            TypedExprValue::ConcatInt(typed_expr, typed_expr1) => todo!(),
            TypedExprValue::ConcatFloat(typed_expr, typed_expr1) => todo!(),
            TypedExprValue::IntToFloat(typed_expr) => cg
                .builder
                .build_signed_int_to_float(
                    typed_expr.codegen(cg, scope)?.into_int_value(),
                    cg.context.f64_type(),
                    "i64_to_f64",
                )?
                .into(),
            TypedExprValue::FloatToInt(typed_expr) => cg
                .builder
                .build_float_to_signed_int(
                    typed_expr.codegen(cg, scope)?.into_float_value(),
                    cg.context.i64_type(),
                    "f64_to_i64",
                )?
                .into(),
            TypedExprValue::IntToString(typed_expr) => todo!(),
            TypedExprValue::FloatToString(typed_expr) => todo!(),
            TypedExprValue::StringToFloat(typed_expr) => todo!(),
            TypedExprValue::StringToInt(typed_expr) => todo!(),
        })
    }
}

impl<'a, 'ctx> Codegen<'a, 'ctx> for TypedBinaryExpr
where
    'a: 'ctx,
{
    type CodegenOutput = BasicValueEnum<'ctx>;

    fn codegen(
        &self,
        cg_state: CodegenState<'a, 'ctx>,
        scope: &'a CodegenScopeInfo,
    ) -> anyhow::Result<Self::CodegenOutput> {
        let lhs = self.left.codegen(cg_state, scope)?;
        let rhs = self.right.codegen(cg_state, scope)?;

        Ok(if self.output_type == Typing::Integer {
            let lhs = lhs.into_int_value();
            let rhs = rhs.into_int_value();

            // TODO: Refacto this hell-hole

            match self.op {
                BinaryExprOp::Add => cg_state.builder.build_int_add(lhs, rhs, "iadd")?.into(),
                BinaryExprOp::Sub => cg_state.builder.build_int_sub(lhs, rhs, "isub")?.into(),
                BinaryExprOp::Mult => cg_state.builder.build_int_mul(lhs, rhs, "imul")?.into(),
                BinaryExprOp::Div => cg_state
                    .builder
                    .build_int_signed_div(lhs, rhs, "idiv")?
                    .into(),
                BinaryExprOp::Less => cg_state
                    .builder
                    .build_int_compare(IntPredicate::SLT, lhs, rhs, "icmp_slt")?
                    .into(),
                BinaryExprOp::LessOrEqual => cg_state
                    .builder
                    .build_int_compare(IntPredicate::SLE, lhs, rhs, "icmp_sle")?
                    .into(),
                BinaryExprOp::Equal => cg_state
                    .builder
                    .build_int_compare(IntPredicate::EQ, lhs, rhs, "icmp_eq")?
                    .into(),
                BinaryExprOp::GreaterOrEqual => cg_state
                    .builder
                    .build_int_compare(IntPredicate::SGE, lhs, rhs, "icmp_sge")?
                    .into(),
                BinaryExprOp::Greater => cg_state
                    .builder
                    .build_int_compare(IntPredicate::SGT, lhs, rhs, "icmp_sgt")?
                    .into(),
                BinaryExprOp::Different => cg_state
                    .builder
                    .build_int_compare(IntPredicate::NE, lhs, rhs, "icmp_ne")?
                    .into(),
                BinaryExprOp::BitAnd => cg_state.builder.build_xor(lhs, rhs, "bitand")?.into(),
                BinaryExprOp::BitOr => cg_state.builder.build_xor(lhs, rhs, "bitor")?.into(),
                BinaryExprOp::BitXor => cg_state.builder.build_xor(lhs, rhs, "bitxor")?.into(),
                // TODO: Function for Pow
                BinaryExprOp::Pow => todo!(),
            }
        } else {
            let lhs = lhs.into_float_value();
            let rhs = rhs.into_float_value();

            match self.op {
                BinaryExprOp::Add => cg_state.builder.build_float_add(lhs, rhs, "fadd")?.into(),
                BinaryExprOp::Sub => cg_state.builder.build_float_sub(lhs, rhs, "fsub")?.into(),
                BinaryExprOp::Mult => cg_state.builder.build_float_mul(lhs, rhs, "fmul")?.into(),
                BinaryExprOp::Div => cg_state.builder.build_float_div(lhs, rhs, "fdiv")?.into(),
                BinaryExprOp::Less => cg_state
                    .builder
                    .build_float_compare(FloatPredicate::OLT, lhs, rhs, "fcmp_olt")?
                    .into(),
                BinaryExprOp::LessOrEqual => cg_state
                    .builder
                    .build_float_compare(FloatPredicate::OLE, lhs, rhs, "fcmp_ole")?
                    .into(),
                BinaryExprOp::Equal => cg_state
                    .builder
                    .build_float_compare(FloatPredicate::OEQ, lhs, rhs, "fcmp_oeq")?
                    .into(),
                BinaryExprOp::GreaterOrEqual => cg_state
                    .builder
                    .build_float_compare(FloatPredicate::OGE, lhs, rhs, "fcmp_oge")?
                    .into(),
                BinaryExprOp::Greater => cg_state
                    .builder
                    .build_float_compare(FloatPredicate::OGT, lhs, rhs, "fcmp_ogt")?
                    .into(),
                BinaryExprOp::Different => cg_state
                    .builder
                    .build_float_compare(FloatPredicate::ONE, lhs, rhs, "fcmp_one")?
                    .into(),
                // FIXME: Forbid bit ops on Floats
                BinaryExprOp::BitAnd => todo!(),
                BinaryExprOp::BitOr => todo!(),
                BinaryExprOp::BitXor => todo!(),
                BinaryExprOp::Pow => todo!(),
            }
        })
    }
}

impl<'a, 'ctx> Codegen<'a, 'ctx> for VarAccess
where
    'a: 'ctx,
{
    type CodegenOutput = BasicValueEnum<'ctx>;

    fn codegen(
        &self,
        _: CodegenState<'a, 'ctx>,
        scope: &'a CodegenScopeInfo,
    ) -> anyhow::Result<Self::CodegenOutput> {
        let var_def = scope.get_variable_prototype(&self.var_name)?;

        Ok(var_def.value)
    }
}

impl<'a, 'ctx> Codegen<'a, 'ctx> for TypedFunctionCall
where
    'a: 'ctx,
{
    type CodegenOutput = CallSiteValue<'ctx>;

    fn codegen(
        &self,
        cg_state: CodegenState<'a, 'ctx>,
        scope: &'a CodegenScopeInfo,
    ) -> anyhow::Result<Self::CodegenOutput> {
        let func = cg_state.module.get_function(&self.name).unwrap();

        let mut args = vec![];

        for arg in self.args.iter() {
            args.push(arg.codegen(cg_state, scope)?.into());
        }

        Ok(cg_state.builder.build_call(func, &args, "func_call")?)
    }
}

impl<'a, 'ctx> Codegen<'a, 'ctx> for Constant
where
    'a: 'ctx,
{
    type CodegenOutput = BasicValueEnum<'ctx>;

    fn codegen(
        &self,
        cg: CodegenState<'a, 'ctx>,
        scope: &'a CodegenScopeInfo,
    ) -> anyhow::Result<Self::CodegenOutput> {
        match self {
            Constant::Float(f) => TypedExpr::new(Typing::Float, TypedExprValue::Float(*f)),
            Constant::Int(i) => TypedExpr::new(Typing::Integer, TypedExprValue::Integer(*i)),
            Constant::String(s) => TypedExpr::new(Typing::Float, TypedExprValue::String(s.clone())),
            Constant::Null => TypedExpr::new_null(),
        }
        .codegen(cg, scope)
    }
}
