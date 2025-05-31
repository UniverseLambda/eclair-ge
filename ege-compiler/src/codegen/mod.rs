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

use crate::semantical::{AnalyzedProgram, Constant, TypedExpr, Typing};

mod expr;
mod func;
mod statement;

#[derive(Debug, Clone, Copy)]
pub struct CodegenState<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
}

#[derive(Debug)]
pub struct CodegenScopeInfo<'ctx> {
    pub functions: HashMap<String, FunctionDef<'ctx>>,
    pub global_variables: HashMap<String, VariableDef<'ctx>>,
    pub current_function: RefCell<Option<String>>,
    pub loops: Vec<LoopDescriptor<'ctx>>,
    pub loop_id: AtomicUsize,
}

impl<'ctx> CodegenScopeInfo<'ctx> {
    pub fn get_function_prototype<'a>(&'a self, name: &String) -> anyhow::Result<&'a FunctionDef>
    where
        'a: 'ctx,
    {
        self.functions
            .get(name)
            .ok_or_else(|| anyhow!("no function named `{name}`"))
    }

    pub fn get_variable_prototype<'a>(&'a self, name: &String) -> anyhow::Result<&'a VariableDef>
    where
        'a: 'ctx,
    {
        self.loops
            .iter()
            .rfind(|v| v.var.as_ref().map_or(false, |v| &v.name == name))

            .map(|v| v.var.as_ref().unwrap())
            .or_else(|| {
                self.current_function.borrow().as_ref().and_then(|v| {
                    self.functions
                        .get(v)
                        .unwrap()
                        .args
                        .iter()
                        .find(|arg| &arg.name == name)
                })
            })
            .or_else(|| self.global_variables.get(name))
            .ok_or_else(|| anyhow!("no variable named `{name}`"))
    }
}

#[derive(Debug)]
pub struct FunctionDef<'ctx> {
    pub name: String,
    pub return_typing: Typing,
    pub args: Vec<ArgDef>,
    pub function: OnceCell<FunctionValue<'ctx>>,
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

pub trait Codegen<'a, 'ctx>
where
    'a: 'ctx,
{
    type CodegenOutput;

    fn codegen(
        &self,
        cg: CodegenState<'a, 'ctx>,
        scope: &'a CodegenScopeInfo,
    ) -> anyhow::Result<Self::CodegenOutput>;
}

pub fn codegen(analyzed_program: AnalyzedProgram) -> anyhow::Result<()> {
    let context = Context::create();
    let module = context.create_module("eclair-ge-pgm");
    let builder = context.create_builder();

    let cg = CodegenState {
        context: &context,
        module: &module,
        builder: &builder,
    };

    let mut global_scope = CodegenScopeInfo {
        functions: HashMap::new(),
        global_variables: HashMap::new(),
        current_function: RefCell::new(None),
        loops: vec![],
        loop_id: AtomicUsize::new(0),
    };

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
                return_typing: func.return_type.clone(),
                args,
                function: OnceCell::new(),
            },
        );
    }

    Ok(())
}
