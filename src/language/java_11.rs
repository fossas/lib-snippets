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

use std::fmt::Write;

use colored::Colorize;
use derive_more::Constructor;
use getset::{CopyGetters, Getters};
use tap::{Pipe, Tap};
use tracing::{debug, info, warn};
use tree_sitter::Node;
use tree_sitter_traversal::{traverse_tree, Order};

use crate::{
    content::Content,
    debugging::{inspect_node, NodeInspector, ToDisplayEscaped},
    ext::vec::FunctionalVec,
    impl_language,
    impl_prelude::*,
    parser::{
        bytes::Location,
        java::{scope, symbol, Kind, Scope},
        stack::{Entry, Stack},
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

/// Call graphs are made up of functions found in the source code, which call 0 or more other functions.
#[derive(Debug, Clone, Eq, PartialEq, Getters)]
pub struct CallGraphEntry {
    /// The function that denotes this node in a call graph.
    ///
    /// For example, given the file:
    /// ```not_rust
    /// import java.util.logging.Logger;
    ///
    /// public class TestFunctions {
    ///
    ///   private static final Logger logger = Logger.getLogger(TestFunctions.class.getName());
    ///
    ///   public void simpleMethod() {
    ///     logger.info("simpleMethod called");
    ///     methodWithParam(5); // Calling another method
    ///   }
    /// }
    /// ```
    ///
    /// When reporting the `simpleMethod` method, `target` consists of:
    /// ```not_rust
    /// Function {
    ///   name: "default.TestFunctions::simpleMethod",
    ///   ...
    /// }
    /// ```
    target: MethodDeclaration,

    /// The functions this function calls.
    /// These are denoted as symbols because they are unresolved.
    ///
    /// For example, given the file:
    /// ```not_rust
    /// import java.util.logging.Logger;
    ///
    /// public class TestFunctions {
    ///
    ///   private static final Logger logger = Logger.getLogger(TestFunctions.class.getName());
    ///
    ///   public void simpleMethod() {
    ///     logger.info("simpleMethod called");
    ///     methodWithParam(5); // Calling another method
    ///   }
    /// }
    /// ```
    ///
    /// When reporting the `simpleMethod` method, `calls` consists of:
    /// ```not_rust
    /// Vec [
    ///   Symbol { name: "java.util.logging.Logger::info", ... },
    ///   Symbol { name: "default.TestFunctions::methodWithParam", ... },
    /// ]
    /// ```
    ///
    /// These are "unresolved symbols" even though they indicate the full path,
    /// because this data structure doesn't itself provide
    /// the recursive data of these functions:
    /// namely what they call and where they are declared.
    calls: Vec<Kind>,
}

/// Extracts function call graphs from source code.
pub struct CallGraphExtractor;

impl SnippetExtractor for CallGraphExtractor {
    type Options = EmptyOptions;
    type Output = Vec<CallGraphEntry>;

    #[tracing::instrument(skip_all)]
    fn extract(_: &Self::Options, content: &Content) -> Result<Self::Output, Error> {
        let mut parser = parser()?;

        let content = content.as_bytes();
        let Some(tree) = parser.parse(content, None) else {
            warn!("provided content did not parse to a tree");
            return Vec::new().pipe(Ok);
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
        let mut stack = Stack::<Kind>::default();

        // Build the stack. Reporting the call graph is a two-phase operation
        // becuase each given symbol may depend on things that come
        // later in the file.
        for node in traverse_tree(&tree, Order::Pre).inspect_nodes(content) {
            if let Some(scope) = scope(node, content) {
                match scope {
                    Scope::Enter(location) => stack.enter(location),
                    Scope::Exit(location) => stack.exit(location),
                }
                continue;
            }

            if let Some(symbol) = symbol(node, content) {
                stack.push(symbol);
                continue;
            }
        }

        // Once a method declaration has been resolved using the stack,
        // it's copied here for export. This way the stack can be freely
        // modified without changing the exported results,
        // which may depend on elements in the stack.
        // let mut entries = Vec::new();

        for entry in stack.iter() {
            let Entry::Symbol(symbol) = entry else {
                continue;
            };

            if let Kind::Invocation { .. } = symbol.inner() {
                info!("Invocation stack:");
                info!("{:?}", symbol.inner());
                for (i, parent) in stack.retrace_from(symbol).enumerate() {
                    let indent = "  ".repeat(i + 1);
                    info!("{indent}← {:?}", parent.inner());
                }

                let context = stack
                    .retrace_from(symbol)
                    .map(|s| s.location())
                    .collect::<Vec<_>>()
                    .reversed();
                debug!("Context:\n{}", render_overlaid_context(content, &context));

                info!("-----");
            }
        }

        Ok(vec![])
    }
}

fn render_overlaid_context(content: &[u8], context: &[Location]) -> String {
    let in_context =
        |offset: usize| -> bool { context.iter().any(|loc| loc.as_range().contains(&offset)) };

    let mut output = String::new();

    let content = content.iter().copied().map(char::from).map(String::from);
    for (b, c) in content.enumerate() {
        if in_context(b) {
            write!(&mut output, "{c}").expect("write to buffer");
        } else {
            write!(&mut output, "{}", c.dimmed()).expect("write to buffer");
        }
    }

    output
}

/// A method declaration in source code.
///
/// Together, `MethodDeclaration` and `MethodInvocation`
/// form the backbone of the call graph that this package exports.
#[derive(Clone, Eq, PartialEq, Debug, Getters, CopyGetters, Constructor)]
pub struct MethodDeclaration {
    /// The full path to the method declaration.
    ///
    /// Does not include the method declaration itself.
    /// For example with the following code:
    /// ```not_rust
    /// public class TestFunctions {
    ///     public void simpleMethod() {
    ///         // ...
    ///     }
    /// }
    /// ```
    ///
    /// The path to `simpleMethod` is:
    /// ```not_rust
    /// Path([
    ///   Symbol::Package{ label: "default" },
    ///   Symbol::Class{ label: "TestFunctions" },
    /// ])
    /// ```
    ///
    /// To build a full path including this declaration,
    /// append the appropriate [`Symbol`] type
    /// to the path.
    ///
    /// Note: the "default" package is implicit in Java programs
    /// if no other package is declared.
    path: Vec<Kind>,

    /// The signature of the method being declared.
    ///
    /// `signature` + `path` is enough to uniquely identify a method on a class
    /// without knowing if the signature refers to a static or instance method
    /// because Java does not allow classes to have both kinds with the same signature.
    signature: Signature,

    /// The location of this declaration in the source code.
    location: Location,

    /// The methods invoked by this method.
    ///
    /// For example, in the following code:
    /// ```not_rust
    /// import java.util.logging.Logger;
    ///
    /// public class TestFunctions {
    ///
    ///     // Logger for logging messages
    ///     private static final Logger logger =
    ///         Logger.getLogger(TestFunctions.class.getName());
    ///
    ///     // Default constructor
    ///     public TestFunctions() {
    ///         logger.info("Constructor called");
    ///     }
    ///
    ///     public void simpleMethod() {
    ///         methodWithParam(5);
    ///     }
    /// }
    /// ```
    ///
    /// The value of `invokes` for `simpleMethod` is:
    /// ```not_rust
    /// Vec[
    ///   MethodInvocation{
    ///     path: NonEmpty[
    ///       Symbol::Package{ label: "default" },
    ///       Symbol::Class{ label: "TestFunctions" },
    ///     ],
    ///     target: NonEmpty[
    ///       Symbol::Package{ label: "default" },
    ///       Symbol::Class{ label: "TestFunctions" },
    ///     ],
    ///     signature: "methodWithParam(int)",
    ///   }
    /// ]
    /// ```
    ///
    /// Note that `Logger.getLogger` is called _when the class is first initialized_,
    /// not when an instance is constructed (note the `static` keyword).
    /// Initializing the class can happen when:
    /// 1. An instance of the class is created.
    /// 2. A static method of the class is invoked.
    /// 3. A static field of the class is assigned or accessed.
    /// 4. The class is referenced via reflection
    ///    (note that reflection is not supported by this parser).
    ///
    /// To avoid such methods from getting lost in the reported graph,
    /// we'll report methods called during static initialization
    /// as part of the `invokes` list of anything that _could_ result in
    /// the initializer being called.
    ///
    /// For example, the value of `invokes` for the constructor is:
    /// ```not_rust
    /// Vec[
    ///   MethodInvocation{
    ///     path: NonEmpty[
    ///       Symbol::Package{ label: "default" },
    ///       Symbol::Class{ label: "TestFunctions" },
    ///     ],
    ///     target: NonEmpty[
    ///       Symbol::Package{ label: "java" },
    ///       Symbol::Package{ label: "util" },
    ///       Symbol::Package{ label: "logging" },
    ///       Symbol::Class{ label: "Logger" },
    ///     ],
    ///     signature: "getLogger(String)",
    ///   },
    ///   MethodInvocation{
    ///     path: NonEmpty[
    ///       Symbol::Package{ label: "default" },
    ///       Symbol::Class{ label: "TestFunctions" },
    ///       Symbol::Constructor{ signature: "TestFunctions()" },
    ///     ],
    ///     target: NonEmpty[
    ///       Symbol::Package{ label: "java" },
    ///       Symbol::Package{ label: "util" },
    ///       Symbol::Package{ label: "logging" },
    ///       Symbol::Class{ label: "Logger" },
    ///     ],
    ///     signature: "info(String)",
    ///   },
    /// ]
    /// ```
    ///
    /// This is because the constructor:
    /// - _May_ cause the static initializer to run,
    ///   implicitly calling `Logger::getLogger`.
    /// - Explicitly runs `Logger::info`.
    ///
    /// The reasoning here is that if a class is loaded, it's being used;
    /// if used, any entrypoint call path can result in initialization;
    /// therefore all entrypoints should record that they invoke
    /// static initializers for the purposes of our call graph.
    ///
    /// While we're here, it's worth noting that the
    /// intention of this package, and therefore this property,
    /// is to report all _build-time defined_ edges between methods,
    /// regardless of _runtime_ behavior.
    /// This means that if for example a method is only called on
    /// a specific platform, or in a specific runtime scenario,
    /// it's always reported in the graph.
    ///
    /// Given this, the decision to report static initializers
    /// is consistent with the overall theme of the library.
    invokes: Vec<MethodInvocation>,
}

/// A method invocation in source code.
///
/// Together, `MethodDeclaration` and `MethodInvocation`
/// form the backbone of the call graph that this package exports.
#[derive(Clone, Eq, PartialEq, Debug, Getters, CopyGetters, Constructor)]
pub struct MethodInvocation {
    /// The full path to the method invocation.
    ///
    /// Does not include the method invocation itself.
    /// For example with the following code:
    /// ```not_rust
    /// public class TestFunctions {
    ///     public void simpleMethod() {
    ///         methodWithParam(5);
    ///     }
    /// }
    /// ```
    ///
    /// The path to the `methodWithParams` call inside `simpleMethod` is:
    /// ```not_rust
    /// Path([
    ///   Symbol::Package{ label: "default" },
    ///   Symbol::Class{ label: "TestFunctions" },
    ///   Symbol::MethodDeclaration{ signature: "simpleMethod()" },
    /// ])
    /// ```
    ///
    /// The "default" package is implicit in Java programs
    /// if no other package is declared.
    ///
    /// To build a full path including this declaration,
    /// append the appropriate [`Symbol`] type
    /// to the path.
    path: Vec<Kind>,

    /// The target symbol on which the method is being called.
    ///
    /// All method invocations _should_ have an identifiable target
    /// at parse time, even when performing file-by-file parsing,
    /// with a couple exceptions.
    ///
    /// For example, in the following code:
    /// ```not_rust
    /// import java.util.logging.Logger;
    ///
    /// public class TestFunctions {
    ///
    ///     private static final Logger logger =
    ///         Logger.getLogger(TestFunctions.class.getName());
    ///
    ///     public TestFunctions() {
    ///         logger.info("Constructor called");
    ///     }
    ///
    ///     public void simpleMethod() {
    ///         methodWithParam(5);
    ///     }
    /// }
    /// ```
    ///
    /// We can statically determine that:
    /// - `default.TestFunctions::constructor()` calls `java.util.logging.Logger::info(String)`.
    /// - `default.TestFunctions::simpleMethod()` calls `default.TestFunctions::methodWithParam(int)`.
    ///
    /// This is because:
    /// - Users cannot define methods on a class in a separate file
    ///   (they must subclass or compose to do this).
    /// - All method invocations must be performed on a known type.
    ///
    /// The primary exception to this is classes referenced in another file that is part
    /// of the same package. To work around this, all classes for which the source package
    /// is not determined are attached to the current package for the file;
    /// this way when multiple files that make up a package are parsed,
    /// the resultant symbols can be resolved across files within the same package.
    ///
    /// The other exception to this is wildcard imports, which are not currently supported
    /// by this library. These allow the use of classes implicitly imported from a package,
    /// making it difficult or impossible to statically determine the package of a given class.
    /// For example, in the following code:
    /// ```not_rust
    /// import java.util.logging.*;
    ///
    /// public class TestFunctions {
    ///
    ///     private static final Logger logger =
    ///         Logger.getLogger(TestFunctions.class.getName());
    ///
    ///     public TestFunctions() {
    ///         logger.info("Constructor called");
    ///     }
    ///
    ///     public void simpleMethod() {
    ///         methodWithParam(5);
    ///     }
    /// }
    /// ```
    ///
    /// Currently, this library assumes that `Logger`
    /// (which is not resolvable due to the wildcard import)
    /// belongs to the package declared in the file (in this case, implicitly `default`),
    /// due to the behavior required to model multi-file packages.
    /// As a result, the response from parsing this file looks like:
    /// - `default.TestFunctions::constructor()` calls `default.Logger::info(String)`.
    /// - `default.TestFunctions::simpleMethod()` calls `default.TestFunctions::methodWithParam(int)`.
    ///
    /// While we could assume in this particular case that `Logger`
    /// belongs to the only wildcard import present in the package,
    /// most Java programs that use any wildcard imports are unlikely to stop at one.
    /// Additionally, reporting an association only some of the time is more confusing
    /// to users than just not supporting this association at all.
    target: Vec<Kind>,

    /// The signature of the method being called.
    ///
    /// `signature` + `target` is enough to uniquely identify a method on a class
    /// without knowing if the signature refers to a static or instance method,
    /// because Java does not allow a class to declare both kinds with the same signature.
    signature: Signature,

    /// The location of this invocation in the source code.
    location: Location,
}

/// The signature of a method.
///
/// # Overloads
///
/// Java allows overloading methods on:
/// - Number of arguments
/// - Type of arguments
///
/// This means that the following methods are distinct:
/// ```not_rust
/// public void example();
/// public void example(int a);
/// public void example(int a, String b);
/// public void example(String a, int b);
/// ```
///
/// Varargs complicate this as well.
/// The following methods are considered ambiguous:
/// ```not_rust
/// public void example(int ... a);
/// public void example(int a, int b);
/// ```
///
/// Happily, neither return type nor argument names
/// are considered overloads, meaning that methods which
/// attempt to overload on these are considered ambiguous.
///
/// Based on these rules, this type consists of the
/// minimal signature required to disambiguate overloads.
///
/// The above methods are recorded as:
/// ```not_rust
/// Signature ( "example()" )
/// Signature ( "example(int...)" )
/// Signature ( "example(int)" )
/// Signature ( "example(int, String)" )
/// Signature ( "example(String, int)" )
/// ```
///
/// # Wildcards
///
/// Sometimes the types cannot be inferred statically.
/// For example, consider this invocation:
/// ```not_rust
/// import java.util.logging.Logger;
/// private static final Logger logger =
///   Logger.getLogger(TestFunctions.class.getName());
/// ```
///
/// Since we're not modeling Java execution, we don't
/// know that `getName()` returns a `String` since we don't
/// see its definition (it's implicit to Java and doesn't appear
/// in the source code).
///
/// Such signatures are represented with an underscore per argument:
/// ```not_rust
/// Signature ( "java.util.logging.Logger::getLogger(_)" )
/// ```
///
/// Specifically, in signatures `_` functions as a wildcard that matches
/// one argument of any type.
#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct Signature(String);

impl Signature {
    /// Create a new instance with the provided value.
    pub fn new(signature: impl Into<String>) -> Self {
        signature.into().pipe(Self)
    }
}

impl<S: Into<String>> From<S> for Signature {
    fn from(value: S) -> Self {
        value.into().pipe(Self)
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
