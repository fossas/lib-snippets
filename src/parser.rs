//! Higher level parsers and constants on top of tree-sitter primitives.

pub mod bytes;
pub mod iter;
pub mod normalize;
pub mod stack;

#[cfg(feature = "lang-java-11")]
pub mod java;

pub(crate) const NODE_KIND_COMMENT: &str = "comment";
pub(crate) const NODE_KIND_FUNC_DEF: &str = "function_definition";
pub(crate) const NODE_KIND_METHOD_DECL: &str = "method_declaration";
pub(crate) const NODE_KIND_CONSTRUCTOR_DECL: &str = "constructor_declaration";
pub(crate) const NODE_KIND_OPEN_BRACE: &str = "{";
pub(crate) const NODE_KIND_CLOSE_BRACE: &str = "}";
pub(crate) const NODE_KIND_SEMI: &str = ";";
