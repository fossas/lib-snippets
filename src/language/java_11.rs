//! Implements an [`Extractor`] for the Java programming language.
//!
//! # Version
//!
//! This is based on the grammar available at https://github.com/tree-sitter/tree-sitter-java.
//! It's not 100% clear which version of Java the grammar supports,
//! but some of the contents of the codebase imply that it supports Java 11.
//!
//! That being said, this extractor should generally support newer syntax,
//! so long as the new syntax doesn't prevent parsing functions out of the source code.
//!
//! [`Extractor`]: crate::Extractor

use crate::impl_prelude::*;

#[tracing::instrument]
pub(crate) fn parser() -> Result<tree_sitter::Parser, ExtractorError> {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(tree_sitter_java::language())
        .map_err(ExtractorError::configure)?;
    Ok(parser)
}
