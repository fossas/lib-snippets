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

use getset::Getters;
use tap::{Pipe, Tap};
use tracing::{debug, warn};
use tree_sitter::Node;
use tree_sitter_traversal::{traverse_tree, Order};

use crate::{
    content::Content,
    debugging::{inspect_node, ToDisplayEscaped},
    impl_language,
    impl_prelude::*,
    parser::{Symbol, NODE_KIND_CONSTRUCTOR_DECL, NODE_KIND_METHOD_DECL},
};

/// This module implements support for Java 11.
///
/// Review module documentation for more details.
#[derive(Copy, Clone)]
pub struct Language;

impl SnippetLanguage for Language {
    const NAME: &'static str = "java_11";
    const STRATEGY: LanguageStrategy = LanguageStrategy::Static;
}

impl_language!(Language);

/// An empty struct used when no options are accepted.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmptyOptions;

/// Extracts standard snippets from source code.
///
/// All targets extracted are extracted with the equivalent of
/// [`SnippetTarget::Function`], [`SnippetKind::Full`], and [`SnippetMethod::Raw`].
pub struct Extractor;

impl SnippetExtractor for Extractor {
    type Options = EmptyOptions;
    type Output = Vec<Snippet<Language>>;

    #[tracing::instrument(skip_all, fields(content_len = content.as_bytes().len()))]
    fn extract(_: &Self::Options, content: &Content) -> Result<Self::Output, Error> {
        let mut parser = parser()?;

        let content = content.as_bytes();
        let Some(tree) = parser.parse(content, None) else {
            warn!("provided content did not parse to a tree");
            return Vec::new().pipe(Ok);
        };

        traverse_tree(&tree, Order::Pre)
            // Report syntax errors as warnings.
            // Always write a debugging line for each node, regardless of the kind of node.
            .inspect(|node| inspect_node(node, content))
            // Nodes that are not "named" are syntax,
            // which this function currently ignores.
            //
            // Reference:
            // https://tree-sitter.github.io/tree-sitter/using-parsers#named-vs-anonymous-nodes
            .filter(|node| node.is_named())
            // Filter to the kinds this extractor cares about.
            .filter(|node| {
                let kind = node.kind();
                kind == NODE_KIND_METHOD_DECL || kind == NODE_KIND_CONSTRUCTOR_DECL
            })
            // Hand each node off to be processed into a possible snippet,
            // based on the provided options.
            .map(|node| {
                node.byte_range()
                    .pipe(SnippetLocation::from)
                    .pipe(|loc| extract_function(loc, node, content))
            })
            // Then just collect all the produced snippets and done!
            // `From<Iterator> for Result<T, E>` stops iteration on the first error as well.
            .collect()
    }
}

#[tracing::instrument(skip_all)]
fn extract_function<L>(
    loc: SnippetLocation,
    node: Node<'_>,
    content: &[u8],
) -> Result<Snippet<L>, Error> {
    // The raw content here is just extracted for debugging.
    let raw = loc.extract_from(content);
    debug!(raw = %raw.display_escaped());

    // The actual context, the part the snippet scanner cares about, is built here.
    //
    // In the future, this will likely be extended to support other kinds of context,
    // similar to the C/C++ languages.
    let context = SnippetContext::new(node, loc, content);
    debug!(context = %context.content().display_escaped());

    // Transformations would normally be applied on text extracted from the context.
    // However, this language doesn't support transforms yet, so no transforms are performed.
    let text = context.content();
    debug!(text = %text.display_escaped());

    // The more exact location generated above overwrites the overall node location,
    // otherwise users would just always see the whole node.
    //
    // For now, this also asserts their kind and method.
    // This is done so that the output shape is compatible with other snippets.
    let meta = SnippetMetadata::new(SnippetKind::Full, SnippetMethod::Raw, context.location());
    Snippet::from(meta, text)
        .tap(|snippet| debug!(fingerprint = %snippet.fingerprint()))
        .pipe(Ok)
}

/// Snippets returned by the Java extractor also contain callgraph information.
///
/// Other languages may wish to adopt this kind of snippet in the future;
/// at the moment this is unique to Java since the two pre-existing languages (C and C++)
/// do not contain this information.
///
/// If this kind of snippet becomes more common, we should probably
/// alter the standard snippet type, special casing C/C++ instead.
///
/// Note that this type also assumes the embedded [`Snippet`] is [`SnippetTarget::Function`].
/// This
#[derive(Clone, Eq, PartialEq, Getters)]
pub struct CallGraphEntry {
    /// The name of the function that denotes this node in a call graph.
    name: Symbol,

    /// The functions this function calls.
    calls: Vec<Symbol>,
}

/// Extracts function call graphs from source code.
pub struct CallGraphExtractor;

impl SnippetExtractor for CallGraphExtractor {
    type Options = EmptyOptions;
    type Output = Vec<CallGraphEntry>;

    #[tracing::instrument(skip_all, fields(content_len = content.as_bytes().len()))]
    fn extract(_: &Self::Options, content: &Content) -> Result<Self::Output, Error> {
        let mut parser = parser()?;

        let content = content.as_bytes();
        let Some(tree) = parser.parse(content, None) else {
            warn!("provided content did not parse to a tree");
            return Vec::new().pipe(Ok);
        };

        todo!()
    }
}

#[tracing::instrument]
pub(crate) fn parser() -> Result<tree_sitter::Parser, Error> {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(tree_sitter_java::language())
        .map_err(Error::configure)?;
    Ok(parser)
}
