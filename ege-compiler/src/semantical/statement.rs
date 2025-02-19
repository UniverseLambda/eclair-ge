use serde::Serialize;

use crate::parser::{InsertPivot, InsertRelPos, Statement};

use super::{
    analyze::{Analyzable, TypedGenerator},
    expr::{TypedExpr, TypedFunctionCall},
    AnalyzedProgram, FunctionInfo, Typing,
};

#[derive(Debug, Clone, Serialize)]
pub struct TypedStatement {
    pub output_value: Typing,
    pub inner: TypedStatementInner,
}

#[derive(Debug, Clone, Serialize)]
pub enum TypedStatementInner {
    FunctionCall(TypedFunctionCall),
    VarAssign(TypedVarAssign),
    ArrayDecl(TypedArrayDecl),
    If(TypedIf),
    For(TypedForLoop),
    Repeat(TypedRepeatLoop),
    Insert(Insert),
    Exit,
    Select(TypedSelect),
    Return(TypedReturn),
}

#[derive(Debug, Clone, Serialize)]
pub struct TypedVarAssign {
    pub var_name: String,
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
    pub else_if: Vec<TypedOtherwise>,
}

#[derive(Debug, Clone, Serialize)]
pub enum TypedOtherwise {
    ElseIf {
        cond: TypedExpr,
        statements: Vec<TypedStatement>,
    },
    Else {
        statements: Vec<TypedStatement>,
    },
}

#[derive(Debug, Clone, Serialize)]
pub struct TypedForLoop {
    pub name: String,
    pub statements: Vec<TypedStatement>,
    pub mode: TypedForLoopMode,
}

#[derive(Debug, Clone, Serialize)]
pub enum TypedForLoopMode {
    Range { from: TypedExpr, to: TypedExpr },
    Each { iterator: TypedExpr },
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
            Statement::FunctionDecl(function_decl) => {
                function_decl.extract_declarations(program, function)
            }
            Statement::Select(select) => select.extract_declarations(program, function),
            Statement::Return(ret) => ret.extract_declarations(program, function),
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
    ) -> anyhow::Result<Self::TypedOutput> {
        todo!()
    }
}
