//! Higher level parsers and constants on top of tree-sitter primitives.

pub(crate) const NODE_KIND_COMMENT: &str = "comment";
pub(crate) const NODE_KIND_FUNC_DEF: &str = "function_definition";
pub(crate) const NODE_KIND_METHOD_DECL: &str = "method_declaration";
pub(crate) const NODE_KIND_OPEN_BRACE: &str = "{";

pub(crate) mod function;
pub mod normalize;
