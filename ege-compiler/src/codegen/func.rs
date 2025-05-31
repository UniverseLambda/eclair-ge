use inkwell::{
    AddressSpace,
    types::BasicMetadataTypeEnum,
    values::{BasicValue, FunctionValue},
};

use crate::semantical::{ArgInfo, FunctionInfo, Typing};

use super::{Codegen, CodegenScopeInfo, CodegenState};

impl<'a, 'ctx> Codegen<'a, 'ctx> for FunctionInfo
where
    'a: 'ctx,
{
    type CodegenOutput = FunctionValue<'ctx>;

    fn codegen(
        &self,
        cg: CodegenState<'a, 'ctx>,
        scope: &'a CodegenScopeInfo,
    ) -> anyhow::Result<Self::CodegenOutput> {
        let func = gen_proto(&self, cg)?;

        let bb = cg
            .context
            .append_basic_block(func, &format!("fn_body_{}", self.name));
        cg.builder.position_at_end(bb);

        scope.current_function.replace(Some(self.name.clone()));

        for (i, arg) in func.get_param_iter().enumerate() {
            let arg_name = self.args[i].var_info.name.as_str();
            let alloca = cg.builder.build_alloca(arg.get_type(), arg_name).unwrap();

            cg.builder.build_store(alloca, arg).unwrap();
        }

        for statement in self.statements.iter() {
            statement.codegen(cg, scope)?;
        }

        Ok(func)
    }
}

pub fn gen_proto<'a, 'ctx>(
    info: &FunctionInfo,
    cg: CodegenState<'a, 'ctx>,
) -> anyhow::Result<FunctionValue<'ctx>> {
    let mut args_types: Vec<BasicMetadataTypeEnum<'ctx>> = vec![];

    for ArgInfo { var_info: arg, .. } in info.args.iter() {
        args_types.push(match &arg.typing {
            Typing::Integer => cg.context.i64_type().into(),
            Typing::Float => cg.context.f64_type().into(),
            Typing::String | Typing::Array { .. } | Typing::Struct(_) => {
                cg.context.ptr_type(AddressSpace::default()).into()
            }
            Typing::Void => unreachable!(),
        });
    }

    let fn_type = match info.return_type {
        Typing::Void => cg.context.void_type().fn_type(&args_types, false),
        Typing::Integer => cg.context.i64_type().fn_type(&args_types, false),
        Typing::Float => cg.context.f64_type().fn_type(&args_types, false),
        Typing::String | Typing::Array { .. } | Typing::Struct(_) => cg
            .context
            .ptr_type(AddressSpace::default())
            .fn_type(&args_types, false),
    };

    let fn_val = cg.module.add_function(&info.name, fn_type, None);

    for (i, arg) in fn_val.get_param_iter().enumerate() {
        arg.as_basic_value_enum()
            .set_name(&info.args[i].var_info.name);
    }

    Ok(fn_val)
}
