use std::time::SystemTime;

use inkwell::{AddressSpace, types::BasicMetadataTypeEnum, values::FunctionValue};

use crate::{
    parser::FunctionCall,
    semantical::{ArgInfo, FunctionInfo, Typing},
};

use super::{Codegen, CodegenScopeInfo, CodegenState, CurrentFunction, VariableDef};

impl<'ctx> Codegen<'ctx> for FunctionInfo {
    type CodegenOutput = FunctionValue<'ctx>;

    fn codegen(
        &self,
        cg: CodegenState<'_, 'ctx>,
        scope: &CodegenScopeInfo<'ctx>,
    ) -> anyhow::Result<Self::CodegenOutput> {
        let (func, current_func) = gen_proto(&self, cg)?;

        let bb = cg
            .context
            .append_basic_block(func, &format!("fn_body_{}", self.name));
        cg.builder.position_at_end(bb);

        scope.current_function.replace(Some(current_func));

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
    cg: CodegenState<'_, 'ctx>,
) -> anyhow::Result<(FunctionValue<'ctx>, CurrentFunction<'ctx>)> {
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

    let mut curr = CurrentFunction {
        name: info.name.clone(),
        variables: vec![],
    };

    for (i, arg) in fn_val.get_param_iter().enumerate() {
        let name = info.args[i].var_info.name.clone();

        arg.set_name(&name);
        curr.variables.push(VariableDef {
            name,
            value: arg.clone(),
        });
    }

    Ok((fn_val, curr))
}

pub fn codegen_custom_func<'ctx>(
    cg: CodegenState<'_, 'ctx>,
    name: Option<String>,
    return_type: Typing,
    args: Vec<(Option<String>, Typing)>,
) -> anyhow::Result<FunctionValue<'ctx>> {
    let mut args_types: Vec<BasicMetadataTypeEnum<'ctx>> = vec![];

    for (_, arg_type) in args.iter() {
        args_types.push(match &arg_type {
            Typing::Integer => cg.context.i64_type().into(),
            Typing::Float => cg.context.f64_type().into(),
            Typing::String | Typing::Array { .. } | Typing::Struct(_) => {
                cg.context.ptr_type(AddressSpace::default()).into()
            }
            Typing::Void => unreachable!(),
        });
    }

    let fn_type = match return_type {
        Typing::Void => cg.context.void_type().fn_type(&args_types, false),
        Typing::Integer => cg.context.i64_type().fn_type(&args_types, false),
        Typing::Float => cg.context.f64_type().fn_type(&args_types, false),
        Typing::String | Typing::Array { .. } | Typing::Struct(_) => cg
            .context
            .ptr_type(AddressSpace::default())
            .fn_type(&args_types, false),
    };

    let fn_val = cg.module.add_function(
        &name.unwrap_or_else(|| {
            format!(
                "__zarma_rand{}",
                SystemTime::now()
                    .duration_since(SystemTime::UNIX_EPOCH)
                    .unwrap()
                    .as_micros()
            )
        }),
        fn_type,
        None,
    );

    for (i, arg) in fn_val.get_param_iter().enumerate() {
        let (arg_name, _) = args.get(i).as_ref().unwrap();

        if let Some(arg_name) = arg_name {
            arg.set_name(arg_name);
        }
    }

    Ok(fn_val)
}
