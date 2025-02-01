use serde::Serialize;

use super::Ident;

#[derive(Debug, Clone, Serialize)]
pub struct PackedDecl {
    pub name: Ident,
    pub fields: Vec<Ident>,
}
