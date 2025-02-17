use inkwell::{builder::Builder, context::Context, module::Module};

pub struct CodegenState<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
}
