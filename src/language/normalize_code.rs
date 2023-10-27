use std::borrow::Cow;

use tap::Pipe;

use super::{normalize_comments, snippet_context::SnippetContext};
use crate::text::normalize_space;

/// Perform both comment and code normalization.
///
/// Uses [`super::normalize_comments`] and [`crate::text::normalize_space`].
#[tracing::instrument(skip_all)]
pub fn normalize_code<'a>(context: &'a SnippetContext) -> Cow<'a, [u8]> {
    normalize_comments(context)
        .pipe_borrow(normalize_space)
        .pipe(Vec::from)
        .into()
}

#[cfg(test)]
mod tests {

    use crate::impl_prelude::SnippetLocation;
    use crate::language::snippet_context::SnippetContext;
    use tree_sitter_traversal::{traverse_tree, Order};

    #[cfg(feature = "lang-c99-tc3")]
    #[test]
    fn normalizes_code() {
        //! Technically, `[normalize_code]` has applications beyond C.
        //! This is meant to be a very basic test and uses C.
        //! Language specific tests should also be done against this as they are implemented.
        let text = r#"int main() {
  printf("Hello, world!"); // comment
  /* A longer comment */
}"#
        .as_bytes();
        let expected_text = r#"int main() { printf("Hello, world!"); }"#;

        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(tree_sitter_c::language())
            .expect("Could not set language");

        let tree = parser.parse(text, None).expect("Couldn't parse test text");
        let context = SnippetContext::from_nodes(
            traverse_tree(&tree, Order::Pre),
            SnippetLocation::builder()
                .byte_offset(0)
                .byte_len(text.len())
                .build(),
            text,
        );

        let out_text = super::normalize_code(&context);
        assert_eq!(
            std::str::from_utf8(out_text.as_ref()).expect("Could not parse out text"),
            expected_text
        );
    }
}
