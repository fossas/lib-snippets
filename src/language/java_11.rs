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

use tap::{Pipe, Tap};
use tracing::{debug, warn};
use tree_sitter_traversal::{traverse_tree, Order};

use crate::{
    content::Content,
    debugging::{inspect_node, NodeInspector, ToDisplayEscaped},
    impl_language,
    impl_prelude::*,
    parser::{
        java, stack::Stack, Argument, Arguments, Label, Parameter, Parameters, Scope, Visibility,
        NODE_KIND_CONSTRUCTOR_DECL, NODE_KIND_METHOD_DECL,
    },
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
    node: tree_sitter::Node<'_>,
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

/// Extracts function call graphs from source code.
pub struct CallGraphExtractor;

impl SnippetExtractor for CallGraphExtractor {
    type Options = EmptyOptions;
    type Output = Stack<Node>;

    #[tracing::instrument(skip_all)]
    fn extract(_: &Self::Options, content: &Content) -> Result<Self::Output, Error> {
        let mut parser = parser()?;

        let content = content.as_bytes();
        let Some(tree) = parser.parse(content, None) else {
            warn!("provided content did not parse to a tree");
            return Stack::default().pipe(Ok);
        };

        // As the content is parsed, it can't be collapsed
        // into something simple like a lookup table;
        // this is because Java is a scoped language so the same
        // name may indicate multiple symbol paths at different scopes.
        //
        // Instead, this parser builds a stack of symbols,
        // performing a naive search of the entire stack (from front to back)
        // to resolve names into their fully qualified symbols.
        //
        // Symbols that are not found in the stack are assumed syntactically correct,
        // and that they are declared in a different file in the same package.
        //
        // Note that this is not a generalized parser for Java;
        // it is specific to _methods_ and therefore only stores
        // symbols that are required for resolving methods to their
        // declarations. As a concrete example, this parser ignores enums,
        // because they are not relevant for looking up methods.
        let mut stack = Stack::default();

        // Build the stack. Reporting the call graph is a two-phase operation
        // becuase each given symbol may depend on things that come
        // later in the file.
        for node in traverse_tree(&tree, Order::Pre).inspect_nodes(content) {
            if let Some(scope) = java::scope(node, content) {
                match scope {
                    Scope::Enter(location) => stack.enter(location),
                    Scope::Exit(location) => stack.exit(location),
                }
                continue;
            }

            if let Some(symbol) = java::symbol(node, content) {
                stack.push(symbol);
                continue;
            }
        }

        Ok(stack)
    }
}

/// A node in the file syntax tree.
#[derive(Clone, Eq, PartialEq, Debug, Hash, strum::Display)]
#[strum(serialize_all = "snake_case")]
#[non_exhaustive]
pub enum Node {
    /// Represents a package name.
    Package { name: Label },

    /// Represents an import.
    Import { name: Label },

    /// Represents a class name.
    Class { name: Label, visibility: Visibility },

    /// Represents a constructor of a class.
    Constructor {
        name: Label,
        params: Parameters,
        visibility: Visibility,
    },

    /// Represents a method on a class.
    Method {
        name: Label,
        params: Parameters,
        visibility: Visibility,
    },

    /// Represents a method invocation.
    ///
    /// `target` is the name of the symbol on which the method was invoked;
    /// this likely needs to be resolved in the current scope
    /// (for example it is likely a variable or an import).
    Invocation {
        name: Label,
        args: Arguments,
        target: Label,
    },

    /// Represents a variable.
    Variable {
        name: Label,
        type_name: Label,
        visibility: Visibility,
    },
}

impl Node {
    pub fn new_package(name: impl Into<Label>) -> Self {
        Self::Package { name: name.into() }
    }

    pub fn new_import(name: impl Into<Label>) -> Self {
        Self::Import { name: name.into() }
    }

    pub fn new_class(visibility: Visibility, name: impl Into<Label>) -> Self {
        Self::Class {
            name: name.into(),
            visibility,
        }
    }

    pub fn new_constructor(
        visibility: Visibility,
        name: impl Into<Label>,
        params: impl IntoIterator<Item = Parameter>,
    ) -> Self {
        Self::Constructor {
            name: name.into(),
            params: Parameters::new(params),
            visibility,
        }
    }

    pub fn new_variable(
        visibility: Visibility,
        name: impl Into<Label>,
        type_name: impl Into<Label>,
    ) -> Self {
        Self::Variable {
            name: name.into(),
            type_name: type_name.into(),
            visibility,
        }
    }

    pub fn new_method(
        visibility: Visibility,
        name: impl Into<Label>,
        params: impl IntoIterator<Item = Parameter>,
    ) -> Self {
        Self::Method {
            name: name.into(),
            params: Parameters::new(params),
            visibility,
        }
    }

    pub fn new_invocation(
        name: impl Into<Label>,
        target: impl Into<Label>,
        args: impl IntoIterator<Item = Argument>,
    ) -> Self {
        Self::Invocation {
            name: name.into(),
            target: target.into(),
            args: Arguments::new(args),
        }
    }
}

impl From<&Node> for Node {
    fn from(value: &Node) -> Self {
        value.clone()
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
