//! Provides debugging helpers to snippet extractors.

use tap::Pipe;
use tracing::trace;
use tree_sitter::Node;

use crate::impl_prelude::*;

/// Conversion trait for types that can be represented with [`EscapedText`].
pub trait ToDisplayEscaped {
    fn display_escaped(&self) -> EscapedText;
}

impl<T: AsRef<[u8]>> ToDisplayEscaped for T {
    fn display_escaped(&self) -> EscapedText {
        EscapedText { buf: self.as_ref() }
    }
}

/// Wraps a byte buffer, escaping all non-ascii bytes when invoked by `Display`.
#[derive(Debug)]
pub struct EscapedText<'a> {
    buf: &'a [u8],
}

impl<'a> std::fmt::Display for EscapedText<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for c in self.buf.iter().map(|c| c.escape_ascii()) {
            write!(f, "{c}")?;
        }
        Ok(())
    }
}

#[tracing::instrument(skip_all)]
pub(crate) fn inspect_node(node: &Node<'_>, content: &[u8]) {
    let location = node.byte_range().pipe(SnippetLocation::from);
    if node.is_error() {
        let start = node.start_position();
        let end = node.end_position();
        trace!(
            %location,
            content = %location.extract_from(content).display_escaped(),
            kind = %"syntax_error",
            line_start = start.row,
            line_end = end.row,
            col_start = start.column,
            col_end = end.column,
        );
    } else {
        trace!(
            %location,
            content = %location.extract_from(content).display_escaped(),
            kind = %node.kind(),
        );
    }
}
