use std::{cell::RefCell, collections::HashMap, sync::atomic::AtomicUsize};

use anyhow::anyhow;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicValueEnum, FunctionValue},
};

use crate::semantical::{AnalyzedProgram, Typing};

mod expr;
mod func;

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
    pub fn get_function_prototype(&self, name: &String) -> anyhow::Result<&FunctionDef> {
        self.functions
            .get(name)
            .ok_or_else(|| anyhow!("no function named `{name}`"))
    }

    pub fn get_variable_prototype(&self, name: &String) -> anyhow::Result<&VariableDef> {
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
    pub args: Vec<VariableDef<'ctx>>,
    pub function: FunctionValue<'ctx>,
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
    todo!()
}
