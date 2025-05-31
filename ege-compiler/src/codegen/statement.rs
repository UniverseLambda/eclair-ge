use inkwell::values::{AnyValue, BasicValue, BasicValueEnum, InstructionValue};

use crate::semantical::{TypedReturn, TypedStatement, TypedStatementInner, TypedVarAssign, Typing};

use super::Codegen;

impl<'a, 'ctx> Codegen<'a, 'ctx> for TypedStatement
where
    'a: 'ctx,
{
    type CodegenOutput = ();

    fn codegen(
        &self,
        cg: super::CodegenState<'a, 'ctx>,
        scope: &'a super::CodegenScopeInfo,
    ) -> anyhow::Result<Self::CodegenOutput> {
        match &self.inner {
            TypedStatementInner::FunctionCall(typed_function_call) => {
                typed_function_call.codegen(cg, scope)?;
            }
            TypedStatementInner::VarAssign(typed_var_assign) => {
                typed_var_assign.codegen(cg, scope)?;
            }
            TypedStatementInner::ArrayDecl(typed_array_decl) => todo!(),
            TypedStatementInner::If(typed_if) => todo!(),
            TypedStatementInner::ForRange(typed_for_range) => todo!(),
            TypedStatementInner::ForEach(typed_for_each) => todo!(),
            TypedStatementInner::Repeat(typed_repeat_loop) => todo!(),
            TypedStatementInner::Insert(typed_insert) => todo!(),
            TypedStatementInner::Exit => todo!(),
            TypedStatementInner::Select(typed_select) => todo!(),
            TypedStatementInner::Return(typed_return) => todo!(),
        }

        Ok(())
    }
}

impl<'a, 'ctx> Codegen<'a, 'ctx> for TypedVarAssign
where
    'a: 'ctx,
{
    type CodegenOutput = InstructionValue<'ctx>;

    fn codegen(
        &self,
        cg: super::CodegenState<'a, 'ctx>,
        scope: &'a super::CodegenScopeInfo,
    ) -> anyhow::Result<Self::CodegenOutput> {
        let var = match &self.var_expr {
            either::Either::Left(l) => l.codegen(cg, scope)?,
            either::Either::Right(r) => todo!(),
        };

        let value = self.value.codegen(cg, scope)?;

        Ok(cg.builder.build_store(var.into_pointer_value(), value)?)
    }
}

impl<'a, 'ctx> Codegen<'a, 'ctx> for TypedReturn
where
    'a: 'ctx,
{
    type CodegenOutput = InstructionValue<'ctx>;

    fn codegen(
        &self,
        cg: super::CodegenState<'a, 'ctx>,
        scope: &'a super::CodegenScopeInfo,
    ) -> anyhow::Result<Self::CodegenOutput> {
        let value = self
            .value
            .as_ref()
            .map(|v| v.codegen(cg, scope))
            .transpose()?;

        Ok(cg.builder.build_return(value.as_ref().map(|v| v as _))?)
    }
}
