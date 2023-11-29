//! Higher level parsers and constants on top of tree-sitter primitives.

use getset::{CopyGetters, Getters};
use typed_builder::TypedBuilder;

pub mod bytes;
pub mod normalize;

pub(crate) const NODE_KIND_COMMENT: &str = "comment";
pub(crate) const NODE_KIND_FUNC_DEF: &str = "function_definition";
pub(crate) const NODE_KIND_METHOD_DECL: &str = "method_declaration";
pub(crate) const NODE_KIND_CONSTRUCTOR_DECL: &str = "constructor_declaration";
pub(crate) const NODE_KIND_OPEN_BRACE: &str = "{";

/// A symbol in the source code.
#[derive(Clone, Eq, PartialEq, Debug, TypedBuilder, Getters, CopyGetters)]
pub struct Symbol {
    /// The name of the symbol; what is represented in source code.
    #[builder(setter(transform = |input: impl ToString| input.to_string()))]
    #[get = "pub"]
    name: String,

    /// The location in the source code file in which the symbol was found.
    #[get_copy = "pub"]
    location: bytes::Location,
}
