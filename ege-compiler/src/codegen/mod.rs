use std::{
    collections::HashMap,
    sync::{Arc, atomic::AtomicUsize},
};

use anyhow::anyhow;
use inkwell::{builder::Builder, context::Context, module::Module};

use crate::semantical::Typing;

#[derive(Debug, Clone, Copy)]
pub struct CodegenState<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
}

#[derive(Debug)]
pub struct CodegenScopeInfo {
    pub functions: HashMap<String, FunctionPrototype>,
    pub global_variables: HashMap<String, VariablePrototype>,
    pub current_function: Option<String>,
    pub loops: Vec<LoopDescriptor>,
    pub loop_id: AtomicUsize,
}

impl CodegenScopeInfo {
    pub fn get_function_prototype(&self, name: &String) -> anyhow::Result<&FunctionPrototype> {
        self.functions
            .get(name)
            .ok_or_else(|| anyhow!("no function named `{name}`"))
    }

    pub fn get_variable_prototype(&self, name: &String) -> anyhow::Result<&VariablePrototype> {
        self.loops
            .iter()
            .rfind(|v| v.var.as_ref().map_or(false, |v| &v.name == name)).map(|v| v.var.as_ref().unwrap())
            .or_else(|| self.current_function.as_ref().and_then(|v| {
                self.functions.get(v).unwrap().args.iter().find(|v| &v.name == name)
            }))
            .or_else(|| self.global_variables.get(name))
            .ok_or_else(|| anyhow!("no variable named `{name}`"))
    }
}

#[derive(Debug)]
pub struct FunctionPrototype {
    pub name: String,
    pub return_typing: Typing,
    pub args: Vec<VariablePrototype>,
}

#[derive(Debug)]
pub struct VariablePrototype {
    pub name: String,
    pub typing: Typing,
}

#[derive(Debug)]
pub struct LoopDescriptor {
    pub loop_id: usize,
    pub var: Option<VariablePrototype>,
}

pub trait Codegen {
    type CodegenOutput;

    fn codegen(
        &self,
        cg_state: CodegenState,
        scope: &CodegenScopeInfo,
    ) -> anyhow::Result<Self::CodegenOutput>;
}
