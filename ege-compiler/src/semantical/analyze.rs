use std::collections::HashMap;

use anyhow::bail;
use log::debug;

use crate::parser::{
    ArrayDecl, FunctionDecl, PackedDecl, Program, Return, Select, SelectCase, VarAssign, VarScope,
};

use super::{
    AnalyzedProgram, ArgInfo, Constant, ForScope, FunctionInfo, StructInfo, Typing, VarInfo,
};

macro_rules! insert_function_info {
    ($map:ident, $value:expr) => {{
        let res = $value;

        $map.insert(res.name.clone(), res);
    }};
}

macro_rules! builtin_function {
    (fn $name:ident($($arg:ident: $arg_type:ident $(= $default_value:expr)?),*) -> $ret_type:ident) => {{
        FunctionInfo {
            name: stringify!($name).into(),
            args: vec![$(
                ArgInfo {
                    var_info: VarInfo {
                        name: stringify!($arg).into(),
                        typing: Typing::$arg_type,
                    },
                    default_value: builtin_function!(@DEFAULT_VALUE@ $($default_value)? ),
            }),*],
            return_type: Typing::$ret_type,
            phase0_checked: true,
            statements: vec![],
            vars: HashMap::new(),
        }
    }};
    (@DEFAULT_VALUE@) => { None };
    (@DEFAULT_VALUE@ $val:expr) => { Some($val) };
}

pub trait Analyzable {
    fn extract_declarations(
        &self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<()>;
}

pub trait TypedGenerator {
    type TypedOutput;

    fn generate_typed(
        self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<Self::TypedOutput>;
}

pub fn analyze_program(program: Program) -> anyhow::Result<AnalyzedProgram> {
    debug!("semantic analysis...");

    let mut result = AnalyzedProgram {
        structs: HashMap::new(),
        functions: HashMap::new(),
        global_vars: HashMap::new(),
        builtin_constants: HashMap::new(),
        builtin_functions: builtin_functions(),
        statements: vec![],
    };

    debug!("sem: phase0: extracting declarations...");

    program.extract_declarations(&mut result, &mut None, None)?;

    debug!("sem: phase1: properly typing the code tree...");

    program.generate_typed(&mut result, &mut None, None)?;

    Ok(result)
}

fn builtin_functions() -> HashMap<String, FunctionInfo> {
    let mut res = HashMap::new();

    insert_function_info!(res, builtin_function!(fn AppTitle(title: String) -> Void));
    insert_function_info!(
        res,
        builtin_function!(fn Graphics(width: Integer, height: Integer, depth: Integer, video_mode: Integer = Constant::Int(0)) -> Void)
    );

    insert_function_info!(
        res,
        builtin_function!(fn CreateTimer(time: Integer) -> Integer)
    );

    insert_function_info!(res, builtin_function!(fn CurrentTime() -> String));
    insert_function_info!(
        res,
        builtin_function!(fn Left(source: String, len: Integer) -> String)
    );
    insert_function_info!(
        res,
        builtin_function!(fn Mid(source: String, offset: Integer, len: Integer) -> String)
    );
    insert_function_info!(
        res,
        builtin_function!(fn Right(source: String, len: Integer) -> String)
    );

    insert_function_info!(res, builtin_function!(fn Cls() -> Void));
    insert_function_info!(
        res,
        builtin_function!(fn Color(red: Integer, blue: Integer, green: Integer) -> Void)
    );

    insert_function_info!(
        res,
        builtin_function!(fn Text(x: Integer, y: Integer, string: String, center_x: Integer = Constant::Int(0), center_y: Integer = Constant::Int(0)) -> Void)
    );

    insert_function_info!(res, builtin_function!(fn WaitTimer(timer: Integer) -> Void));

    res
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
    _: Option<&ForScope>,
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

pub fn get_variable_type(
    program: &AnalyzedProgram,
    function: &Option<FunctionInfo>,
    mut for_scope: Option<&ForScope>,
    var_name: &String,
) -> anyhow::Result<Typing> {
    let mut var_type = None;

    while let Some(current) = for_scope {
        if &current.var_name == var_name {
            var_type = Some(current.var_type.clone());

            break;
        }

        for_scope = current.previous;
    }

    if var_type.is_none() {
        var_type = function
            .as_ref()
            .and_then(|v| {
                v.vars.get(var_name).or_else(|| {
                    v.args
                        .iter()
                        .find(|v| &v.var_info.name == var_name)
                        .map(|v| &v.var_info)
                })
            })
            .map(|v| v.typing.clone());
    }

    if var_type.is_none() {
        var_type = program.global_vars.get(var_name).map(|v| v.typing.clone());
    }

    let Some(var_type) = var_type else {
        bail!("undefined variable: {var_name}");
    };

    Ok(var_type)
}

pub fn get_function_return_type(
    program: &AnalyzedProgram,
    func_name: &String,
) -> anyhow::Result<Typing> {
    let Some(func) = program
        .functions
        .get(func_name)
        .or_else(|| program.builtin_functions.get(func_name))
    else {
        bail!("undefined function: {func_name}");
    };

    Ok(func.return_type.clone())
}

pub fn get_function_args_info(
    program: &AnalyzedProgram,
    func_name: &String,
) -> anyhow::Result<Vec<ArgInfo>> {
    let Some(func) = program
        .functions
        .get(func_name)
        .or_else(|| program.builtin_functions.get(func_name))
    else {
        bail!("undefined function: {func_name}");
    };

    Ok(func.args.clone())
}

pub fn expect_typing(got: &Typing, expected: &Typing) -> anyhow::Result<()> {
    if got != expected {
        bail!("expected type {expected:?}, but got {got:?}");
    } else {
        Ok(())
    }
}

impl Analyzable for Program {
    fn extract_declarations(
        &self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<()> {
        self.statements
            .extract_declarations(program, function, for_scope)
    }
}

impl TypedGenerator for Program {
    type TypedOutput = ();

    fn generate_typed(
        self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<Self::TypedOutput> {
        program.statements = self
            .statements
            .generate_typed(program, function, for_scope)
            .map(|v| v.concat())?;

        Ok(())
    }
}

impl<T: Analyzable> Analyzable for Vec<T> {
    fn extract_declarations(
        &self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<()> {
        for v in self.iter() {
            v.extract_declarations(program, function, for_scope)?;
        }

        Ok(())
    }
}

impl<T: TypedGenerator> TypedGenerator for Vec<T> {
    type TypedOutput = Vec<T::TypedOutput>;

    fn generate_typed(
        self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<Self::TypedOutput> {
        let mut result = vec![];

        for elem in self {
            result.push(elem.generate_typed(program, function, for_scope)?);
        }

        Ok(result)
    }
}

impl Analyzable for VarAssign {
    fn extract_declarations(
        &self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
        for_scope: Option<&ForScope>,
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
                None => insert_or_ignore_variable(program, function, for_scope, var_info),
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
        _: Option<&ForScope>,
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
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<()> {
        let info = VarInfo {
            name: self.ident.name.clone(),
            typing: Typing::Array {
                inner_type: Box::new(self.ident.ident_type.clone().into()),
                dimension_count: self.size.len(),
            },
        };

        insert_or_ignore_variable(program, &mut None, for_scope, info);

        Ok(())
    }
}

impl Analyzable for FunctionDecl {
    fn extract_declarations(
        &self,
        program: &mut AnalyzedProgram,
        _: &mut Option<FunctionInfo>,
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<()> {
        let mut args = Vec::new();

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

            args.push(ArgInfo {
                var_info,
                default_value,
            });
        }

        let mut func_info = Some(FunctionInfo {
            name: self.ident.name.clone(),
            return_type: self.ident.ident_type.clone().into(),
            args,
            vars: HashMap::new(),
            phase0_checked: false,
            statements: vec![],
        });

        self.statements
            .extract_declarations(program, &mut func_info, for_scope)?;

        if let Some(previous) = program
            .functions
            .insert(func_info.as_ref().unwrap().name.clone(), func_info.unwrap())
        {
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
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<()> {
        self.cases
            .extract_declarations(program, function, for_scope)
    }
}

impl Analyzable for SelectCase {
    fn extract_declarations(
        &self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<()> {
        self.statements
            .extract_declarations(program, function, for_scope)
    }
}

impl Analyzable for Return {
    fn extract_declarations(
        &self,
        _: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
        _: Option<&ForScope>,
    ) -> anyhow::Result<()> {
        let Some(function) = function else {
            bail!("Return statement outside of a function");
        };

        if !function.phase0_checked && function.return_type == Typing::Integer {
            if self.value.is_some() {
                function.return_type = Typing::Integer;
            } else {
                function.return_type = Typing::Void;
            }
        }
        function.phase0_checked = true;

        Ok(())
    }
}
