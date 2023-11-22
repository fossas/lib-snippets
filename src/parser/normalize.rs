//! Normalization primitives.

use std::borrow::Cow;

use lazy_regex::regex_replace_all;
use tap::Pipe;

use crate::impl_prelude::*;

use super::NODE_KIND_COMMENT;

/// Perform both comment and code normalization.
///
/// Equivalent to [`comments`] followed by [`space`].
#[tracing::instrument(skip_all)]
pub fn code<'a>(context: &'a SnippetContext) -> Cow<'a, [u8]> {
    comments(context)
        .pipe_borrow(space)
        .pipe(Vec::from)
        .pipe(Cow::Owned)
}

/// Remove all comment node text from the given content.
///
/// Because tree sitter parsers can define their own node types,
/// it's possible a comment node may have a kind that isn't expected.
///
/// Extractors that use this function should test each language-specific comment syntax at least once.
#[tracing::instrument(skip_all)]
pub fn comments<'a>(context: &'a SnippetContext) -> Vec<u8> {
    let comment_nodes = context
        .nodes()
        .iter()
        .filter(|n| n.kind() == NODE_KIND_COMMENT);
    context.content_around(comment_nodes)
}

/// Normalize any consecutive whitespace into a single space.
#[tracing::instrument(skip_all)]
pub fn space(text: &[u8]) -> Cow<'_, [u8]> {
    regex_replace_all!(r"\s+"B, text, b" ")
}

/// Normalize `\r\n` into `\n`.
#[tracing::instrument(skip_all)]
pub fn crlf(text: &[u8]) -> Cow<'_, [u8]> {
    regex_replace_all!("\r\n"B, text, b"\n")
}

#[cfg(test)]
mod tests {

    use super::*;
    use tree_sitter_traversal::{traverse_tree, Order};

    #[cfg(feature = "lang-c99-tc3")]
    #[test]
    fn normalizes_code() {
        //! Technically, `[normalize_code]` has applications beyond C.
        //! This is meant to be a very basic test and uses C.
        //! Language specific tests should also be done against this as they are implemented.
        let text = indoc::indoc! {r#"
            int main() {
                printf("Hello, world!"); // comment
                /* A longer comment */
            }
        "#}
        .as_bytes();
        let expected_text = r#"int main() { printf("Hello, world!"); } "#;

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

        let out_text = code(&context);
        pretty_assertions::assert_eq!(
            std::str::from_utf8(out_text.as_ref()).expect("Could not parse out text"),
            expected_text
        );
    }

    #[cfg(feature = "lang-c99-tc3")]
    #[test]
    fn normalizes_comments() {
        //! Technically, `[normalize_comments]` has applications beyond C.
        //! This is meant to be a very basic test and uses C.
        //! Language specific tests should also be done against this during full fingerprinting.
        let text = indoc::indoc! {r#"
            int main() {
                printf("Hello, world!"); // comment
                /* A longer comment */
            }
        "#}
        .as_bytes();
        let expected_text = indoc::indoc! {r#"
            int main() {
                printf("Hello, world!"); 
                
            }
        "#};

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

        let out_text = comments(&context);
        pretty_assertions::assert_eq!(
            std::str::from_utf8(out_text.as_ref()).expect("Could not parse out text"),
            expected_text
        );
    }

    #[test]
    fn normalizes_crlf() {
        let input = "foo\nbar\r\nbaz";
        let expected = "foo\nbar\nbaz";
        let normalized = crlf(input.as_bytes());
        let normalized = String::from_utf8_lossy(&normalized);

        assert_eq!(normalized, expected);
    }
}
