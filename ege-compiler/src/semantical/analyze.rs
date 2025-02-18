use std::collections::HashMap;

use anyhow::bail;

use crate::parser::{
    ArrayDecl, FunctionDecl, PackedDecl, Program, Select, SelectCase, Statement, VarAssign, VarScope
};

use super::{AnalyzedProgram, ArgInfo, Constant, FunctionInfo, StructInfo, Typing, VarInfo};

pub trait Analyzable {
    fn extract_declarations(
        &self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
    ) -> anyhow::Result<()>;
}

pub fn analyze_program(program: Program) -> anyhow::Result<AnalyzedProgram> {
    let mut result = AnalyzedProgram {
        structs: HashMap::new(),
        functions: HashMap::new(),
        global_vars: HashMap::new(),
        builtin_constants: HashMap::new(),
    };

    program.extract_declarations(&mut result, &mut None)?;

    Ok(result)
}

pub fn insert_local_variable(
    program: &mut AnalyzedProgram,
    function: &mut Option<FunctionInfo>,
    var_info: VarInfo,
) -> anyhow::Result<()> {
    if let Some(func) = function.as_mut() {
        if let Some(var_info) = func.vars.insert(var_info.name.clone(), var_info) {
            bail!(
                "Multiple definition of local variable {} in function {}",
                var_info.name,
                func.name
            );
        }
    } else {
        insert_global_variable(program, var_info)?;
    }

    Ok(())
}

pub fn insert_global_variable(
    program: &mut AnalyzedProgram,
    var_info: VarInfo,
) -> anyhow::Result<()> {
    if let Some(var_info) = program.global_vars.insert(var_info.name.clone(), var_info) {
        bail!(
            "Multiple definition of variable {} in global scope",
            var_info.name
        );
    }

    Ok(())
}

pub fn insert_or_ignore_variable(
    program: &mut AnalyzedProgram,
    function: &mut Option<FunctionInfo>,
    var_info: VarInfo,
) {
    if let Some(func) = function.as_mut() {
        if !func.vars.contains_key(&var_info.name) {
            func.vars.insert(var_info.name.clone(), var_info);
        }
    } else {
        if !program.global_vars.contains_key(&var_info.name) {
            program.global_vars.insert(var_info.name.clone(), var_info);
        }
    }
}

impl Analyzable for Program {
    fn extract_declarations(
        &self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
    ) -> anyhow::Result<()> {
        self.statements.extract_declarations(program, function)
    }
}

impl<T: Analyzable> Analyzable for Vec<T> {
    fn extract_declarations(
        &self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
    ) -> anyhow::Result<()> {
        for v in self.iter() {
            v.extract_declarations(program, function)?;
        }

        Ok(())
    }
}

impl Analyzable for Statement {
    fn extract_declarations(
        &self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
    ) -> anyhow::Result<()> {
        match self {
            Statement::VarAssign(var_assign) => var_assign.extract_declarations(program, function),
            Statement::If(if_decl) => if_decl.statements.extract_declarations(program, function),
            Statement::PackedDecl(packed_decl) => {
                packed_decl.extract_declarations(program, function)
            }
            Statement::Include(include) => include.program.extract_declarations(program, function),
            Statement::For(for_loop) => for_loop.statements.extract_declarations(program, function),
            Statement::Repeat(repeat_loop) => repeat_loop
                .statements
                .extract_declarations(program, function),
            Statement::ArrayDecl(array_decls) => {
                array_decls.extract_declarations(program, function)
            }
            Statement::FunctionDecl(function_decl) => function_decl.extract_declarations(program, function),
            Statement::Select(select) => select.extract_declarations(program, function),
            Statement::Insert(_)
            | Statement::NoData(_)
            | Statement::FunctionCall(_) => Ok(()),
        }
    }
}

impl Analyzable for VarAssign {
    fn extract_declarations(
        &self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
    ) -> anyhow::Result<()> {
        for (name, _) in self.defs.iter() {
            let var_info = VarInfo {
                name: name.components.last().unwrap().clone(),
                typing: name.final_type.clone().into(),
            };

            match self.scope {
                Some(_) if name.components.len() > 1 => {
                    bail!(
                        "Scope not allowed when assigning a member of a struct: {:?}",
                        name.components
                    );
                }
                Some(VarScope::Global) => insert_global_variable(program, var_info)?,
                Some(VarScope::Local) => insert_local_variable(program, function, var_info)?,
                None if name.components.len() > 1 => (),
                None => insert_or_ignore_variable(program, function, var_info),
            }
        }

        Ok(())
    }
}

impl Analyzable for PackedDecl {
    fn extract_declarations(
        &self,
        program: &mut AnalyzedProgram,
        _: &mut Option<FunctionInfo>,
    ) -> anyhow::Result<()> {
        let info = StructInfo {
            name: self.name.name.clone(),
            fields: self
                .fields
                .iter()
                .cloned()
                .map(|v| VarInfo {
                    name: v.name,
                    typing: v.ident_type.into(),
                })
                .collect(),
        };

        if let Some(previous) = program.structs.insert(info.name.clone(), info) {
            bail!("Multiple definition of Type {}", previous.name);
        }

        Ok(())
    }
}

impl Analyzable for ArrayDecl {
    fn extract_declarations(
        &self,
        program: &mut AnalyzedProgram,
        _: &mut Option<FunctionInfo>,
    ) -> anyhow::Result<()> {
        let info = VarInfo {
            name: self.ident.name.clone(),
            typing: Typing::Array {
                inner_type: Box::new(self.ident.ident_type.clone().into()),
                dimension_count: self.size.len(),
            },
        };

        insert_or_ignore_variable(program, &mut None, info);

        Ok(())
    }
}

impl Analyzable for FunctionDecl {
    fn extract_declarations(
        &self,
        program: &mut AnalyzedProgram,
        _: &mut Option<FunctionInfo>,
    ) -> anyhow::Result<()> {
        let mut args = HashMap::new();
        let mut args_order = vec![];

        for (name, default_value) in self.params.iter().cloned() {
            let var_info = VarInfo {
                name: name.name,
                typing: name.ident_type.into(),
            };

            let default_value = if let Some(expr) = default_value {
				Some(Constant::try_from(expr)?)
            } else {
                None
            };

			args_order.push(var_info.name.clone());
			args.insert(var_info.name.clone(), ArgInfo {
				var_info,
				default_value,
			});
        }

        let mut func_info = Some(FunctionInfo {
            name: self.ident.name.clone(),
            return_type: self.ident.ident_type.clone().into(),
            args,
            args_order,
            vars: HashMap::new(),
        });

		self.statements.extract_declarations(program, &mut func_info)?;

		if let Some(previous) = program.functions.insert(func_info.as_ref().unwrap().name.clone(), func_info.unwrap()) {
			bail!("Multiple definition of function {}", previous.name);
		}

        Ok(())
    }
}

impl Analyzable for Select {
	fn extract_declarations(
			&self,
			program: &mut AnalyzedProgram,
			function: &mut Option<FunctionInfo>,
		) -> anyhow::Result<()> {
		self.cases.extract_declarations(program, function)
	}
}

impl Analyzable for SelectCase {
	fn extract_declarations(
		&self,
		program: &mut AnalyzedProgram,
		function: &mut Option<FunctionInfo>,
	) -> anyhow::Result<()> {
		self.statements.extract_declarations(program, function)
	}
}
