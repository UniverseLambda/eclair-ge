use anyhow::bail;
use either::Either;
use serde::Serialize;

use crate::parser::{
    ArrayDecl, ForLoop, ForLoopMode, FunctionDecl, If, InsertPivot, InsertRelPos, Otherwise,
    Statement, VarAssign,
};

use super::{
    analyze::{Analyzable, TypedGenerator},
    expr::{FieldAccess, TypedExpr, TypedExprValue, TypedFunctionCall, VarAccess},
    AnalyzedProgram, ForScope, FunctionInfo, Typing,
};

#[derive(Debug, Clone, Serialize)]
pub struct TypedStatement {
    pub output_value: Typing,
    pub inner: TypedStatementInner,
}

impl TypedStatement {
    pub fn new_void(inner: TypedStatementInner) -> Self {
        Self {
            output_value: Typing::Void,
            inner,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum TypedStatementInner {
    FunctionCall(TypedFunctionCall),
    VarAssign(TypedVarAssign),
    ArrayDecl(TypedArrayDecl),
    If(TypedIf),
    ForRange(TypedForRange),
    ForEach(TypedForEach),
    Repeat(TypedRepeatLoop),
    Insert(Insert),
    Exit,
    Select(TypedSelect),
    Return(TypedReturn),
}

#[derive(Debug, Clone, Serialize)]
pub struct TypedVarAssign {
    pub var_expr: Either<VarAccess, FieldAccess>,
    pub var_type: Typing,
    pub value: TypedExpr,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypedArrayDecl {
    pub name: String,
    pub array_type: Typing,
    pub size: Vec<TypedExpr>,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypedIf {
    pub cond: TypedExpr,
    pub statements: Vec<TypedStatement>,
    pub else_ifs: Vec<(TypedExpr, Vec<TypedStatement>)>,
    pub else_statements: Option<Vec<TypedStatement>>,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypedForEach {
    pub name: String,
    pub statements: Vec<TypedStatement>,
    pub iterator: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypedForRange {
    pub name: String,
    pub statements: Vec<TypedStatement>,
    pub from: TypedExpr,
    pub to: TypedExpr,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypedRepeatLoop {
    pub statements: Vec<TypedStatement>,
}

// Insert <source> <rel_pos> <pivot> <collection>

#[derive(Debug, Clone, Serialize)]
pub struct Insert {
    pub source_name: String,
    pub rel_pos: InsertRelPos,
    pub pivot: InsertPivot,
    pub collection: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypedSelect {
    pub value: TypedExpr,
    pub cases: Vec<TypedSelectCase>,
    pub default_case: Option<Vec<TypedStatement>>,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypedSelectCase {
    pub values: Vec<TypedExpr>,
    pub statements: Vec<TypedStatement>,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypedReturn {
    pub value: Option<TypedExpr>,
    pub output_type: Typing,
}

impl Analyzable for Statement {
    fn extract_declarations(
        &self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<()> {
        match self {
            Statement::VarAssign(var_assign) => {
                var_assign.extract_declarations(program, function, for_scope)
            }
            Statement::If(if_decl) => if_decl
                .statements
                .extract_declarations(program, function, for_scope),
            Statement::PackedDecl(packed_decl) => {
                packed_decl.extract_declarations(program, function, for_scope)
            }
            Statement::Include(include) => include
                .program
                .extract_declarations(program, function, for_scope),
            Statement::For(for_loop) => {
                let for_scope = ForScope::new(
                    for_loop.name.name.clone(),
                    for_loop.name.ident_type.clone().into(),
                    for_scope,
                );

                for_loop
                    .statements
                    .extract_declarations(program, function, Some(&for_scope))
            }
            Statement::Repeat(repeat_loop) => repeat_loop
                .statements
                .extract_declarations(program, function, for_scope),
            Statement::ArrayDecl(array_decls) => {
                array_decls.extract_declarations(program, function, for_scope)
            }
            Statement::FunctionDecl(function_decl) => {
                function_decl.extract_declarations(program, function, for_scope)
            }
            Statement::Select(select) => select.extract_declarations(program, function, for_scope),
            Statement::Return(ret) => ret.extract_declarations(program, function, for_scope),
            Statement::Insert(_) | Statement::NoData(_) | Statement::FunctionCall(_) => Ok(()),
        }
    }
}

impl TypedGenerator for Statement {
    type TypedOutput = Vec<TypedStatement>;

    fn generate_typed(
        self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<Self::TypedOutput> {
        match self {
            Statement::FunctionDecl(function_decl) => {
                function_decl.generate_typed(program, function, for_scope)
            }
            Statement::FunctionCall(function_call) => function_call
                .generate_typed(program, function, for_scope)
                .map(|v| {
                    vec![TypedStatement::new_void(TypedStatementInner::FunctionCall(
                        v,
                    ))]
                }),
            Statement::VarAssign(var_assign) => {
                var_assign.generate_typed(program, function, for_scope)
            }
            Statement::ArrayDecl(array_decls) => {
                array_decls.generate_typed(program, function, for_scope)
            }
            Statement::If(if_statement) => if_statement
                .generate_typed(program, function, for_scope)
                .map(|v| vec![v]),
            Statement::For(for_loop) => for_loop.generate_typed(program, function, for_scope).map(|v| vec![v]),
            Statement::PackedDecl(_) => Ok(vec![]),
            Statement::Repeat(repeat_loop) => todo!(),
            Statement::Insert(insert) => todo!(),
            Statement::NoData(no_data_statement) => todo!(),
            Statement::Include(include) => todo!(),
            Statement::Select(select) => todo!(),
            Statement::Return(_) => todo!(),
        }
    }
}

impl TypedGenerator for FunctionDecl {
    type TypedOutput = Vec<TypedStatement>;

    fn generate_typed(
        self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<Self::TypedOutput> {
        if let Some(func) = function {
            bail!("function declaration (here `{}`) inside another function (here `{}`) is not allowed", self.ident.name, func.name);
        }

        let mut statements = vec![];
        // We can throw out this one as it should not be modified
        let mut function = Some(program.get_function_info_mut(&self.ident.name)?.clone());
        // TODO: think about adding check to see if this value is different from the one stored.

        for statement in self.statements {
            let gen_statements = statement.generate_typed(program, &mut function, for_scope)?;

            statements.extend(gen_statements.into_iter());
        }

        // TODO: think about adding check to see if statements is empty or not
        program
            .get_function_info_mut(&self.ident.name)
            .unwrap()
            .statements = statements;

        Ok(vec![])
    }
}

impl TypedGenerator for VarAssign {
    type TypedOutput = Vec<TypedStatement>;

    fn generate_typed(
        self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<Self::TypedOutput> {
        let mut res = vec![];

        // TODO: think about validating scope?

        for (name, value) in self
            .defs
            .into_iter()
            .filter_map(|(ident_path, v)| v.map(|v| (ident_path, v)))
        {
            let (var_expr, var_type) =
                match name.generate_typed(program, function, for_scope)?.value {
                    TypedExprValue::VariableAccess(var_access) => {
                        let typing = var_access.var_type.clone();

                        (Either::Left(var_access), typing)
                    }
                    TypedExprValue::FieldAccess(field_access) => {
                        let typing = field_access.field_type.clone();

                        (Either::Right(field_access), typing)
                    }
                    _ => unreachable!(),
                };

            res.push(TypedStatement::new_void(TypedStatementInner::VarAssign(
                TypedVarAssign {
                    var_expr,
                    var_type: var_type.clone(),
                    value: value
                        .generate_typed(program, function, for_scope)?
                        .cast_to(var_type)?,
                },
            )));
        }

        Ok(res)
    }
}

impl TypedGenerator for ArrayDecl {
    type TypedOutput = TypedStatement;

    fn generate_typed(
        self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<Self::TypedOutput> {
        Ok(TypedStatement::new_void(TypedStatementInner::ArrayDecl(
            TypedArrayDecl {
                name: self.ident.name,
                array_type: self.ident.ident_type.into(),
                size: self
                    .size
                    .into_iter()
                    .map(|v| v.generate_typed(program, function, for_scope))
                    .collect::<anyhow::Result<Vec<TypedExpr>>>()?,
            },
        )))
    }
}

impl TypedGenerator for If {
    type TypedOutput = TypedStatement;

    fn generate_typed(
        self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<Self::TypedOutput> {
        let cond = self.cond.generate_typed(program, function, for_scope)?;
        let statements = self
            .statements
            .generate_typed(program, function, for_scope)?
            .concat();
        let mut else_ifs = vec![];
        let mut else_statements = None;

        for otherwise in self.else_if.into_iter() {
            match otherwise {
                Otherwise::ElseIf { cond, statements } => else_ifs.push((
                    cond.generate_typed(program, function, for_scope)?,
                    statements
                        .generate_typed(program, function, for_scope)?
                        .concat(),
                )),
                Otherwise::Else { statements } => {
                    else_statements = Some(
                        statements
                            .generate_typed(program, function, for_scope)?
                            .concat(),
                    )
                }
            }
        }

        Ok(TypedStatement::new_void(TypedStatementInner::If(TypedIf {
            cond,
            statements,
            else_ifs,
            else_statements,
        })))
    }
}

impl TypedGenerator for ForLoop {
    type TypedOutput = TypedStatement;

    fn generate_typed(
        self,
        program: &mut AnalyzedProgram,
        function: &mut Option<FunctionInfo>,
        for_scope: Option<&ForScope>,
    ) -> anyhow::Result<Self::TypedOutput> {
        Ok(TypedStatement::new_void(match self.mode {
            ForLoopMode::Range { from, to } => TypedStatementInner::ForRange(TypedForRange {
                name: self.name.name,
                statements: self
                    .statements
                    .generate_typed(program, function, for_scope)?
                    .concat(),
                from: from.generate_typed(program, function, for_scope)?,
                to: to.generate_typed(program, function, for_scope)?,
            }),
            ForLoopMode::Each { iterator } => {
                // TODO: warn when iterator.ident_type is not None
                let struct_typing = program.get_struct_info(&iterator.name)?.as_type();
                let for_scope = ForScope::new(self.name.name.clone(), struct_typing, for_scope);

                TypedStatementInner::ForEach(TypedForEach {
                    name: self.name.name,
                    statements: self
                        .statements
                        .generate_typed(program, function, Some(&for_scope))?
                        .concat(),
                    iterator: iterator.name,
                })
            }
        }))
    }
}
