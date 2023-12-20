//! Parsers and types for Java source code.
//!
//! Existing well-known combinatorial parsers (e.g. `nom`)
//! are generally designed to parse text, and making them support
//! more complicated structures requires a lot of work.
//!
//! Writing our own parsers using `nom` for Java text isn't _too_ awful,
//! but a goal of using treesitter in the first place is to lift the responsibility
//! for this off our shoulders (especially in regards to maintenance),
//! so this isn't considered at this time.
//!
//! Instead of spending the time adapting `nom` to [`tree_sitter::Node`]s,
//! or putting together our own fancy combinatorial parsers,
//! this module implements hand-rolled parsers specific to
//! specific kinds of syntax.
//!
//! In order to make this more convenient to implement,
//! this module does not provide a way to choose which parser is
//! appropriate for a given syntactical construction.
//!
//! As we add more implementations, we should consider providing
//! a generalized parser abstraction to make this safer to implement.

use std::borrow::Cow;

use itertools::Itertools;
use lazy_regex::regex_is_match;
use once_cell::sync::Lazy;
use tap::{Pipe, Tap};
use tracing::trace;
use tree_sitter::Node as TSNode;

use crate::{
    debugging::NodeInspector,
    ext::vec::FunctionalVec,
    language::java_11::Node,
    parser::{iter::some, Argument, Parameter, Symbol, Visibility, NODE_KIND_SEMI},
};

use super::{bytes::Location, Scope, NODE_KIND_CLOSE_BRACE, NODE_KIND_OPEN_BRACE};

/// A parser is any function that implements this type.
type Parser<T> = fn(TSNode<'_>, &[u8]) -> Option<T>;

/// Wrap a [`Parser`] with the provided `content` so that it fits the shape
/// expected by [`crate::parser::iter`] (which expects a function that
/// accepts a single argument).
macro_rules! parser {
    ($content:expr, $parser:expr) => {
        |node: TSNode<'_>| $parser(node, $content)
    };
    ($content:expr, $parser:expr, $($args:expr),* $(,)*) => {
        |node: TSNode<'_>| $parser(node, $content, $($args,)*)
    };
}

/// Use [`crate::parser::iter::some`] on the context,
/// early returning [`None`] from the enclosing function if no parse
/// is successful.
macro_rules! parse_some {
    ($context:ident => $($tail:tt)*) => {
        match crate::parser::iter::some(&mut $context, parser!($($tail)*)) {
            Some(result) => result,
            None => return None,
        }
    };
}

/// Construct a new [`Symbol`], trace it in context, and wrap it in [`Some`].
macro_rules! symbol {
    ($($args:expr),* $(,)*) => {
        Symbol::new($($args,)*).tap(|k| trace!(loc = %k.location(), kind = ?k.inner())).pipe(Some)
    };
}

/// Ensure the node is of the specified kind,
/// else early return from the enclosing function.
macro_rules! ensure_node_kind {
    ($node:expr, $kind:expr) => {{
        if $node.kind() != $kind {
            return None;
        }
    }};
    ($node:expr, $kind_a:expr, $kind_b:expr) => {{
        if $node.kind() != $kind_a && $node.kind() != $kind_b {
            return None;
        }
    }};
}

/// A lazily initialized stack of parsers.
type ParserStack<T> = Lazy<Vec<Parser<T>>>;

/// Instantiate a [`ParserStack`] with the provided parsers.
///
/// Usage:
/// ```ignore
/// parser_stack!(T, parsers...)
/// ```
///
/// Creates a static variable in the current scope with the name `PARSER_STACK`
/// containing a stack of parsers that result in type `T`.
///
/// Using this and [`some`], one can form an analogue to an "alternative" parser combinator:
/// ```ignore
/// pub fn scope(node: TSNode<'_>, content: &[u8]) -> Option<Scope> {
///     parser_stack!(Scope => scope_enter, scope_exit);
///     some(&mut PARSER_STACK.iter(), |parser| parser(node, content))
/// }
/// ```
macro_rules! parser_stack {
    ($type:ty => $($args:expr),* $(,)*) => {
        static PARSER_STACK: ParserStack<$type> = Lazy::new(|| vec![$($args,)*]);
    };
}

/// Parse the node as a scope event.
#[tracing::instrument(skip_all)]
pub fn scope(node: TSNode<'_>, content: &[u8]) -> Option<Scope> {
    parser_stack!(Scope => scope_enter, scope_exit);
    some(&mut PARSER_STACK.iter(), |parser| parser(node, content))
}

#[tracing::instrument(skip_all)]
fn scope_enter(node: TSNode<'_>, _: &[u8]) -> Option<Scope> {
    ensure_node_kind!(node, NODE_KIND_OPEN_BRACE);
    Scope::Enter(Location::from(node)).pipe(Some)
}

#[tracing::instrument(skip_all)]
fn scope_exit(node: TSNode<'_>, _: &[u8]) -> Option<Scope> {
    ensure_node_kind!(node, NODE_KIND_CLOSE_BRACE);
    Scope::Exit(Location::from(node)).pipe(Some)
}

/// Attempt to parse the node as any supported symbol.
#[tracing::instrument(skip_all)]
pub fn symbol(node: TSNode<'_>, content: &[u8]) -> Option<Symbol<Node>> {
    parser_stack!(
        Symbol<Node> =>
        invocation,
        method,
        field,
        constructor,
        class,
        import,
        package
    );
    some(&mut PARSER_STACK.iter(), |parser| parser(node, content))
}

#[tracing::instrument(skip_all)]
fn package(node: TSNode<'_>, content: &[u8]) -> Option<Symbol<Node>> {
    ensure_node_kind!(node, "package_declaration");

    let mut ctx = traverse_statement(node, content);
    let name = parse_some!(ctx => content, if_kind, "scoped_identifier");

    symbol!(Node::new_package(name.inner), name.loc)
}

#[tracing::instrument(skip_all)]
fn import(node: TSNode<'_>, content: &[u8]) -> Option<Symbol<Node>> {
    ensure_node_kind!(node, "import_declaration");

    let mut ctx = traverse_statement(node, content);
    let name = parse_some!(ctx => content, if_kind, "scoped_identifier");
    symbol!(Node::new_import(name.inner), name.loc)
}

#[tracing::instrument(skip_all)]
fn class(node: TSNode<'_>, content: &[u8]) -> Option<Symbol<Node>> {
    ensure_node_kind!(node, "class_declaration");

    // Since visibility modifiers come first, but are optional,
    // to parse these we need backtracking in the iterator parser.
    // I don't have this built yet, so just assume all modifiers are public
    // for now.
    let visibility = Visibility::Public;

    let mut ctx = traverse_while(node, content, |node| node.kind() != "class_body");
    let name = parse_some!(ctx => content, if_kind, "identifier");

    symbol!(Node::new_class(visibility, name.inner), name.loc,)
}

#[tracing::instrument(skip_all)]
fn constructor(node: TSNode<'_>, content: &[u8]) -> Option<Symbol<Node>> {
    ensure_node_kind!(node, "constructor_declaration");

    // Since visibility modifiers come first, but are optional,
    // to parse these we need backtracking in the iterator parser.
    // I don't have this built yet, so just assume all modifiers are public
    // for now.
    let visibility = Visibility::Public;

    let mut ctx = traverse_while(node, content, |node| node.kind() != "constructor_body");
    let name = parse_some!(ctx => content, if_kind, "identifier");
    let args = parse_some!(ctx => content, method_params);

    symbol!(
        Node::new_constructor(visibility, name.inner, args.inner),
        name.loc + args.loc,
    )
}

#[tracing::instrument(skip_all)]
fn field(node: TSNode<'_>, content: &[u8]) -> Option<Symbol<Node>> {
    ensure_node_kind!(node, "field_declaration");

    // Since visibility modifiers come first, but are optional,
    // to parse these we need backtracking in the iterator parser.
    // I don't have this built yet, so just assume all modifiers are public
    // for now.
    let visibility = Visibility::Public;

    let mut ctx = traverse_statement(node, content);
    let type_name = parse_some!(ctx => content, if_kind, "type_identifier");
    let name = parse_some!(ctx => content, if_kind, "identifier");

    symbol!(
        Node::new_variable(visibility, name.inner, type_name.inner),
        type_name.loc + name.loc,
    )
}

#[tracing::instrument(skip_all)]
fn method(node: TSNode<'_>, content: &[u8]) -> Option<Symbol<Node>> {
    ensure_node_kind!(node, "method_declaration");

    // Since visibility modifiers come first, but are optional,
    // to parse these we need backtracking in the iterator parser.
    // I don't have this built yet, so just assume all modifiers are public
    // for now.
    let visibility = Visibility::Public;

    let mut ctx = traverse_while(node, content, |node| node.kind() != "block");
    let name = parse_some!(ctx => content, if_kind, "identifier");
    let args = parse_some!(ctx => content, method_params);

    symbol!(
        Node::new_method(visibility, name.inner, args.inner),
        name.loc + args.loc
    )
}

#[tracing::instrument(skip_all)]
fn invocation(node: TSNode<'_>, content: &[u8]) -> Option<Symbol<Node>> {
    ensure_node_kind!(node, "method_invocation");

    // Invocations are commonly in the form `target.name()`, but can also just be `name()`.
    // In the latter case, `target` is assumed to be the current class.
    // Invocations can also use a multipart path, like `System.out.println`.
    let mut ctx = traverse_statement(node, content).peekable();
    let mut target = Vec::new();
    let name = loop {
        let name_or_target = parse_some!(ctx => content, if_kind, "identifier");
        if ctx.peek().map(|n| n.kind()) == Some(".") {
            target.push(name_or_target);
        } else {
            break name_or_target;
        }
    };
    let args = parse_some!(ctx => content, invocation_arguments);

    let target = if target.is_empty() {
        Extracted::new("this", name.loc)
    } else {
        target
            .into_iter()
            .fold(Extracted::<Vec<String>>::default(), |acc, item| {
                if acc.is_empty() {
                    item.map(|inner| vec![inner.to_string()])
                } else {
                    Extracted {
                        inner: acc.inner.pushed(item.inner.to_string()),
                        loc: acc.loc + item.loc,
                    }
                }
            })
            .map(|inner| inner.join("."))
    };

    symbol!(
        Node::new_invocation(name.inner, target.inner, args.inner),
        target.loc + name.loc + args.loc,
    )
}

#[tracing::instrument(skip_all)]
fn method_params<'a>(node: TSNode<'_>, content: &'a [u8]) -> Option<Extracted<Vec<Parameter>>> {
    // treesitter provides "argument_list" when reading a method invocation,
    // and "formal_parameters" when reading a method declaration.
    ensure_node_kind!(node, "formal_parameters");

    // In the future, we probably need to use nom to parse the declared types for method declarations.
    // This should work because one has to declare the type of arguments in declarations.
    // Remember complex types types exist (e.g. `Map<String, int> map`),
    // so simply splitting on `,` or spaces isnt' good enough.
    //
    // For now, just handle the cases of "no arguments" or "some arguments";
    // the latter becomes a wildcard variadic argument, so it can accept
    // any number of any type of argument.
    extract(node, content)
        .map(|inner| {
            if inner == "()" {
                Vec::new()
            } else {
                vec![Parameter::Variadic(Argument::Any)]
            }
        })
        .pipe(Some)
}

#[tracing::instrument(skip_all)]
fn invocation_arguments<'a>(
    node: TSNode<'_>,
    content: &'a [u8],
) -> Option<Extracted<Vec<Argument>>> {
    ensure_node_kind!(node, "argument_list");

    // In the future, we probably need to use treesitter's node kinds to determine input types.
    // For example, `kind=string_literal` maps to `String`.
    // This will likely need to also be able to indicate a variable that can be looked up in the stack
    // by the caller to determine its type; this is likely at least one additional
    // variant to the `Argument` type.
    //
    // For now, just turn every distinct argument into a wildcard.
    let raw = &*extract(node, content).inner;

    // Early check: if this is an empty set of parethesis, or there is only whitespace between them,
    // just return an empty argument set.
    if regex_is_match!(r"\(\s*\)", raw) {
        return Extracted::new(Vec::new(), node).pipe(Some);
    }

    // Otherwise, count the comma nodes emitted by treesitter to find the argument count.
    // Since the "no arguments" case was already checked, assume "no commas" means "one argument"
    // (so really, "argument count" is just "comma count + 1").
    let count = traverse(node, content)
        .filter(|node| node.kind() == ",")
        .count();
    let args = std::iter::repeat(Argument::Any)
        .take(count + 1)
        .collect_vec();
    Extracted::new(args, node).pipe(Some)
}

/// Since visibility modifiers come first, but are optional,
/// to parse these we need backtracking in the iterator parser.
///
/// This is not built yet, so we assume all modifiers are public for now;
/// leaving this parser unused- leaving it in the code though because
/// it _should_ be good to go once backtracking is implemented.
#[tracing::instrument(skip_all)]
fn visibility(node: TSNode<'_>, content: &[u8]) -> Option<Extracted<Visibility>> {
    ensure_node_kind!(node, "modifiers");
    extract(node, content)
        .map(|modifiers| {
            if modifiers.contains("public") {
                Visibility::Public
            } else {
                Visibility::Private
            }
        })
        .tap(|modifiers| trace!(loc = %modifiers.loc, modifiers = %modifiers.inner))
        .pipe(Some)
}

fn if_kind<'a>(
    node: TSNode<'_>,
    content: &'a [u8],
    kind: impl AsRef<str>,
) -> Option<Extracted<Cow<'a, str>>> {
    ensure_node_kind!(node, kind.as_ref());
    extract(node, content).pipe(Some)
}

/// Traverse children of a node in the syntax tree
/// until the statement ends (a semicolon is observed).
fn traverse_statement<'a>(node: TSNode<'a>, content: &'a [u8]) -> impl Iterator<Item = TSNode<'a>> {
    traverse_while(node, content, |node| node.kind() != NODE_KIND_SEMI)
}

/// Traverse children of a node in the syntax tree
/// while the predicate returns true.
fn traverse_while<'a>(
    node: TSNode<'a>,
    content: &'a [u8],
    pred: impl FnMut(&TSNode<'a>) -> bool,
) -> impl Iterator<Item = TSNode<'a>> {
    traverse(node, content).take_while(pred)
}

/// Traverse children of a node in the syntax tree.
fn traverse<'a>(node: TSNode<'a>, content: &'a [u8]) -> impl Iterator<Item = TSNode<'a>> {
    tree_sitter_traversal::traverse(node.walk(), tree_sitter_traversal::Order::Pre)
        .inspect_nodes(content)
}

#[derive(Debug, Default)]
struct Extracted<T> {
    inner: T,
    loc: Location,
}

impl<T> Extracted<T> {
    /// Construct a new instance.
    fn new(inner: impl Into<T>, loc: impl Into<Location>) -> Self {
        Extracted {
            inner: inner.into(),
            loc: loc.into(),
        }
    }

    /// Use the provided closure to map the interior data.
    fn map<U>(self, map: impl FnOnce(T) -> U) -> Extracted<U> {
        Extracted {
            inner: map(self.inner),
            loc: self.loc,
        }
    }

    /// Whether the extracted item is empty.
    fn is_empty(&self) -> bool {
        self.loc.is_empty()
    }
}

impl From<Extracted<Node>> for Symbol<Node> {
    fn from(value: Extracted<Node>) -> Self {
        Self::new(value.inner, value.loc)
    }
}

impl std::iter::Sum for Extracted<String> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Extracted::<Vec<String>>::default(), |mut acc, item| {
            if acc.is_empty() {
                item.map(|inner| vec![inner])
            } else {
                acc.loc += item.loc;
                acc.inner.push(item.inner);
                acc
            }
        })
        .map(|inner| inner.concat())
    }
}

#[tracing::instrument(skip_all)]
fn extract<'a>(node: TSNode<'_>, content: &'a [u8]) -> Extracted<Cow<'a, str>> {
    let loc = node.byte_range().pipe(Location::from);
    let text = loc.extract_from_lossy(content);
    Extracted { inner: text, loc }
}
