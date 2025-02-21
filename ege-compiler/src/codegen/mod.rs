use inkwell::{builder::Builder, context::Context, module::Module};

#[derive(Debug, Clone, Copy)]
pub struct CodegenState<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
}

pub struct CodegenScopeInfo {

}

pub trait Codegen {
    type CodegenOutput;

    fn codegen(&self, cg_state: CodegenState) -> anyhow::Result<Self::CodegenOutput>;
}
