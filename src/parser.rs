//! Higher level parsers and constants on top of tree-sitter primitives.

use derive_more::Constructor;
use getset::{CopyGetters, Getters};

pub mod bytes;
pub mod normalize;

#[cfg(feature = "lang-java-11")]
pub mod java;

pub(crate) const NODE_KIND_COMMENT: &str = "comment";
pub(crate) const NODE_KIND_FUNC_DEF: &str = "function_definition";
pub(crate) const NODE_KIND_METHOD_DECL: &str = "method_declaration";
pub(crate) const NODE_KIND_CONSTRUCTOR_DECL: &str = "constructor_declaration";
pub(crate) const NODE_KIND_OPEN_BRACE: &str = "{";

/// A symbol in the source code.
#[derive(Clone, Eq, PartialEq, Debug, Getters, CopyGetters, Constructor)]
pub struct Symbol<P> {
    /// The fully qualified path of the symbol.
    #[get = "pub"]
    path: P,

    /// The location in the source code in which the function was found.
    #[get_copy = "pub"]
    location: bytes::Location,
}

/// A function declaration in source code.
/// This is a specialized form of [`Symbol`].
#[derive(Clone, Eq, PartialEq, Debug, Getters, CopyGetters, Constructor)]
pub struct FunctionDeclaration<P> {
    symbol: Symbol<P>,
}
