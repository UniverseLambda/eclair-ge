use std::{
    cell::{OnceCell, RefCell},
    collections::HashMap,
    sync::atomic::AtomicUsize,
};

use anyhow::anyhow;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicValueEnum, FunctionValue},
};
use log::debug;

use crate::semantical::{AnalyzedProgram, Constant, FunctionInfo, Typing};

mod expr;
mod func;
mod statement;

#[derive(Debug, Clone, Copy)]
pub struct CodegenState<'a, 'ctx>
where
    'ctx: 'a,
{
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
}

#[derive(Debug)]
pub struct CodegenScopeInfo<'ctx> {
    pub functions: HashMap<String, FunctionDef>,
    pub global_variables: HashMap<String, VariableDef<'ctx>>,
    pub current_function: RefCell<Option<CurrentFunction<'ctx>>>,
    pub loops: Vec<LoopDescriptor<'ctx>>,
    pub loop_id: AtomicUsize,
}

#[derive(Debug)]
pub struct CurrentFunction<'ctx> {
    name: String,
    variables: Vec<VariableDef<'ctx>>,
}

impl<'ctx> CodegenScopeInfo<'ctx> {
    // pub fn get_function_prototype(&self, name: &String) -> anyhow::Result<&FunctionDef> {
    //     self.functions
    //         .get(name)
    //         .ok_or_else(|| anyhow!("no function named `{name}`"))
    // }

    pub fn get_function_cg_name(&self, name: &String) -> anyhow::Result<String> {
        self.functions
            .get(name)
            .ok_or_else(|| anyhow!("no function named `{name}`"))
            .map(|f| f.cg_name.clone())
    }

    pub fn get_variable_value(&self, name: &String) -> anyhow::Result<BasicValueEnum<'ctx>> {
        self.loops
            .iter()
            .rfind(|v| v.var.as_ref().map_or(false, |v| &v.name == name))
            .map(|v| v.var.as_ref().unwrap().value.clone())
            .or_else(|| {
                self.current_function.borrow().as_ref().and_then(|v| {
                    v.variables
                        .iter()
                        .find(|v| &v.name == name)
                        .map(|v| v.value.clone())
                })
            })
            .or_else(|| self.global_variables.get(name).map(|v| v.value.clone()))
            .ok_or_else(|| anyhow!("no variable named `{name}`"))
    }
}

#[derive(Debug)]
pub struct FunctionDef {
    pub name: String,
    pub cg_name: String,
    pub return_typing: Typing,
    pub args: Vec<ArgDef>,
}

#[derive(Debug)]
pub struct ArgDef {
    pub name: String,
    pub default_value: Option<Constant>,
}

#[derive(Debug)]
pub struct VariableDef<'ctx> {
    pub name: String,
    pub value: BasicValueEnum<'ctx>,
}

#[derive(Debug)]
pub struct LoopDescriptor<'ctx> {
    pub loop_id: usize,
    pub var: Option<VariableDef<'ctx>>,
}

pub trait Codegen<'ctx> {
    type CodegenOutput;

    fn codegen(
        &self,
        cg: CodegenState<'_, 'ctx>,
        scope: &CodegenScopeInfo<'ctx>,
    ) -> anyhow::Result<Self::CodegenOutput>;
}

pub fn codegen(analyzed_program: AnalyzedProgram) -> anyhow::Result<()> {
    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("eclair-ge-pgm");

    let cg = CodegenState {
        builder: &builder,
        module: &module,
        context: &context,
    };

    let mut global_scope = CodegenScopeInfo {
        functions: HashMap::new(),
        global_variables: HashMap::new(),
        current_function: RefCell::new(None),
        loops: vec![],
        loop_id: AtomicUsize::new(0),
    };

    for (name, builtin) in analyzed_program.builtin_functions.iter() {
        func::codegen_custom_func(
            cg,
            Some(builtin.cg_name.clone()),
            builtin.return_type.clone(),
            builtin
                .args
                .iter()
                .map(|a| (Some(a.var_info.name.clone()), a.var_info.typing.clone()))
                .collect(),
        )?;

        global_scope.functions.insert(
            name.clone(),
            FunctionDef {
                name: name.clone(),
                cg_name: builtin.cg_name.clone(),
                return_typing: builtin.return_type.clone(),
                args: builtin
                    .args
                    .iter()
                    .map(|a| ArgDef {
                        name: a.var_info.name.clone(),
                        default_value: None,
                    })
                    .collect(),
            },
        );
    }

    for (name, func) in analyzed_program.functions.iter() {
        debug!("Pre-gen func {name}...");

        let args = func
            .args
            .iter()
            .map(|a| {
                Ok(ArgDef {
                    name: a.var_info.name.clone(),
                    default_value: a.default_value.clone(),
                })
            })
            .collect::<anyhow::Result<Vec<ArgDef>>>()?;

        global_scope.functions.insert(
            name.clone(),
            FunctionDef {
                name: name.clone(),
                cg_name: func.cg_name.clone(),
                return_typing: func.return_type.clone(),
                args,
            },
        );
    }

    for (_, func) in analyzed_program.functions.iter() {
        func.codegen(cg.clone(), &global_scope)?;
    }

    let main = FunctionInfo {
        name: "__main_789456123".to_string(),
        cg_name: "__main_789456123".to_string(),
        return_type: Typing::Void,
        args: vec![],
        vars: HashMap::new(),
        statements: analyzed_program.statements,
        phase0_checked: true,
    };

    main.codegen(cg, &global_scope)?;

    module.print_to_stderr();

    Ok(())
}
