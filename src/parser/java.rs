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

use enum_common_fields::EnumCommonFields;
use once_cell::sync::Lazy;
use tap::{Pipe, Tap};
use tracing::trace;
use tree_sitter::Node;
use tree_sitter_traversal::{traverse, Order};

use crate::{debugging::NodeInspector, parser::NODE_KIND_SEMI};

use super::{bytes::Location, stack::Symbol, NODE_KIND_CLOSE_BRACE, NODE_KIND_OPEN_BRACE};

/// A parser is any function that implements this type.
type Parser = fn(Node<'_>, &[u8]) -> Option<Symbol<Kind>>;

/// Wrap a [`Parser`] with the provided `content` so that it fits the shape
/// expected by [`crate::parser::iter`] (which expects a function that
/// accepts a single argument).
macro_rules! parser {
    ($content:expr, $parser:expr) => {
        |node: Node<'_>| $parser(node, $content)
    };
    ($content:expr, $parser:expr, $($args:expr),* $(,)*) => {
        |node: Node<'_>| $parser(node, $content, $($args,)*)
    };
}

/// Use [`crate::parser::iter::parse_some`] on the context,
/// early returning [`None`] from the enclosing function if no parse
/// is successful.
macro_rules! parse_some {
    ($context:ident => $($tail:tt)*) => {
        match crate::parser::iter::parse_some(&mut $context, parser!($($tail)*)) {
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
    ($node:expr, $content:expr, $kind:expr) => {{
        if $node.kind() != $kind {
            return None;
        }
    }};
}

/// Parse the node as a scope event.
#[tracing::instrument(skip_all)]
pub fn scope(node: Node<'_>) -> Option<Scope> {
    let node_loc = || node.byte_range().pipe(Location::from);
    if node.kind() == NODE_KIND_OPEN_BRACE {
        let loc = node_loc();
        trace!(%loc, kind = "scope_enter");
        Scope::Enter(loc).pipe(Some)
    } else if node.kind() == NODE_KIND_CLOSE_BRACE {
        let loc = node_loc();
        trace!(%loc, kind = "scope_exit");
        Scope::Exit(loc).pipe(Some)
    } else {
        None
    }
}

/// Attempt to parse the node as any supported symbol.
#[tracing::instrument(skip_all)]
pub fn symbol(node: Node<'_>, content: &[u8]) -> Option<Symbol<Kind>> {
    // Not critical, but try to keep the list in order of most commonly seen symbols.
    static PARSERS: Lazy<Vec<Parser>> = Lazy::new(|| {
        vec![
            invocation,
            method,
            field,
            constructor,
            class,
            import,
            package,
        ]
    });
    crate::parser::iter::parse_some(&mut PARSERS.iter(), |parser| parser(node, content))
}

#[tracing::instrument(skip_all)]
fn package(node: Node<'_>, content: &[u8]) -> Option<Symbol<Kind>> {
    ensure_node_kind!(node, content, "package_declaration");

    let mut ctx = traverse_statement(node, content);
    let name = parse_some!(ctx => content, if_kind, "package_declaration");
    symbol!(Kind::new_import(name.inner), name.loc)
}

#[tracing::instrument(skip_all)]
fn import(node: Node<'_>, content: &[u8]) -> Option<Symbol<Kind>> {
    ensure_node_kind!(node, content, "import_declaration");

    let mut ctx = traverse_statement(node, content);
    let name = parse_some!(ctx => content, if_kind, "import_declaration");
    symbol!(Kind::new_import(name.inner), name.loc)
}

#[tracing::instrument(skip_all)]
fn class(node: Node<'_>, content: &[u8]) -> Option<Symbol<Kind>> {
    ensure_node_kind!(node, content, "class_declaration");

    let mut ctx = traverse_while(node, content, |node| node.kind() != "class_body");
    let visibility = parse_some!(ctx => content, visibility);
    let name = parse_some!(ctx => content, if_kind, "identifier");

    symbol!(
        Kind::new_class(visibility.inner, name.inner),
        visibility.loc + name.loc,
    )
}

#[tracing::instrument(skip_all)]
fn constructor(node: Node<'_>, content: &[u8]) -> Option<Symbol<Kind>> {
    ensure_node_kind!(node, content, "constructor_declaration");

    let mut ctx = traverse_while(node, content, |node| node.kind() != "constructor_body");
    let visibility = parse_some!(ctx => content, visibility);
    let name = parse_some!(ctx => content, if_kind, "identifier");
    let params = parse_some!(ctx => content, if_kind, "formal_parameters");

    symbol!(
        Kind::new_method(visibility.inner, name.inner + params.inner),
        visibility.loc + name.loc + params.loc,
    )
}

#[tracing::instrument(skip_all)]
fn field(node: Node<'_>, content: &[u8]) -> Option<Symbol<Kind>> {
    ensure_node_kind!(node, content, "field_declaration");

    let mut stmt = traverse_statement(node, content);
    let visibility = parse_some!(stmt => content, visibility);
    let type_name = parse_some!(stmt => content, if_kind, "type_identifier");
    let name = parse_some!(stmt => content, if_kind, "identifier");

    symbol!(
        Kind::new_variable(visibility.inner, name.inner, type_name.inner),
        visibility.loc + type_name.loc + name.loc,
    )
}

#[tracing::instrument(skip_all)]
fn method(node: Node<'_>, content: &[u8]) -> Option<Symbol<Kind>> {
    ensure_node_kind!(node, content, "method_declaration");

    let mut ctx = traverse_while(node, content, |node| node.kind() != "block");
    let visibility = parse_some!(ctx => content, visibility);
    let name = parse_some!(ctx => content, if_kind, "identifier");

    // TODO: need to collapse to abstract types
    let params = parse_some!(ctx => content, if_kind, "formal_parameters");

    symbol!(
        Kind::new_method(visibility.inner, name.inner + params.inner),
        visibility.loc + name.loc + params.loc,
    )
}

#[tracing::instrument(skip_all)]
fn invocation(node: Node<'_>, content: &[u8]) -> Option<Symbol<Kind>> {
    ensure_node_kind!(node, content, "method_invocation");

    let mut ctx = traverse_statement(node, content);
    let target = parse_some!(ctx => content, if_kind, "identifier");
    let name = parse_some!(ctx => content, if_kind, "identifier");

    // TODO: need to collapse to abstract types
    let params = parse_some!(ctx => content, if_kind, "argument_list");

    symbol!(
        Kind::new_invocation(name.inner + params.inner, target.inner),
        target.loc + name.loc + params.loc,
    )
}

#[tracing::instrument(skip_all)]
fn visibility(node: Node<'_>, content: &[u8]) -> Option<Extracted<Visibility>> {
    ensure_node_kind!(node, content, "modifiers");
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
    node: Node<'_>,
    content: &'a [u8],
    kind: impl AsRef<str>,
) -> Option<Extracted<Cow<'a, str>>> {
    ensure_node_kind!(node, content, kind.as_ref());
    extract(node, content).pipe(Some)
}

/// Traverses the children of a node in the syntax tree until the
/// statement ends (a semicolon is observed).
fn traverse_statement<'a>(node: Node<'a>, content: &'a [u8]) -> impl Iterator<Item = Node<'a>> {
    traverse_while(node, content, |node| node.kind() != NODE_KIND_SEMI)
}

fn traverse_while<'a>(
    node: Node<'a>,
    content: &'a [u8],
    pred: impl FnMut(&Node<'a>) -> bool,
) -> impl Iterator<Item = Node<'a>> {
    traverse(node.walk(), Order::Pre)
        .inspect_nodes(content)
        .take_while(pred)
}

struct Extracted<T> {
    inner: T,
    loc: Location,
}

impl<T> Extracted<T> {
    fn map<U>(self, map: impl FnOnce(T) -> U) -> Extracted<U> {
        Extracted {
            inner: map(self.inner),
            loc: self.loc,
        }
    }
}

impl From<Extracted<Kind>> for Symbol<Kind> {
    fn from(value: Extracted<Kind>) -> Self {
        Self::new(value.inner, value.loc)
    }
}

#[tracing::instrument(skip_all)]
fn extract<'a>(node: Node<'_>, content: &'a [u8]) -> Extracted<Cow<'a, str>> {
    let loc = node.byte_range().pipe(Location::from);
    let text = loc.extract_from_lossy(content);
    Extracted { inner: text, loc }
}

/// The label of a generic symbol that is not a method.
///
/// This can be thought of as the equivalent to [`Label`],
/// without the implied necessity of checking for overloads
/// when resolving.
#[derive(Clone, Eq, PartialEq, Debug, Hash, derive_more::Display)]
pub struct Label(String);

impl Label {
    /// Create a new instance with the provided value.
    pub fn new(label: impl Into<String>) -> Self {
        label.into().pipe(Self)
    }
}

impl<S: Into<String>> From<S> for Label {
    fn from(value: S) -> Self {
        value.into().pipe(Self)
    }
}

/// Part of a fully qualified path.
#[derive(Clone, Eq, PartialEq, Debug, Hash, strum::Display, EnumCommonFields)]
#[common_field(name: Label)]
#[strum(serialize_all = "snake_case")]
pub enum Kind {
    /// Represents a package name.
    Package { name: Label },

    /// Represents an import.
    Import { name: Label },

    /// Represents a class name.
    Class { name: Label, visibility: Visibility },

    /// Represents a constructor of a class.
    Constructor { name: Label, visibility: Visibility },

    /// Represents a method on a class.
    Method { name: Label, visibility: Visibility },

    /// Represents a method invocation.
    ///
    /// `target` is the name of the symbol on which the method was invoked;
    /// this likely needs to be resolved in the current scope
    /// (for example it is likely a variable or an import).
    Invocation { name: Label, target: Label },

    /// Represents a variable.
    Variable {
        name: Label,
        type_name: Label,
        visibility: Visibility,
    },
}

impl Kind {
    /// Create a symbol representing the "default" package.
    pub fn default_package() -> Self {
        Self::Package {
            name: Label::new("default"),
        }
    }

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
        name: impl AsRef<str>,
        params: impl AsRef<str>,
    ) -> Self {
        Self::Constructor {
            name: format!("{}{}", name.as_ref(), params.as_ref()).into(),
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

    pub fn new_method(visibility: Visibility, name: impl Into<Label>) -> Self {
        Self::Method {
            name: name.into(),
            visibility,
        }
    }

    pub fn new_invocation(name: impl Into<Label>, target: impl Into<Label>) -> Self {
        Self::Invocation {
            name: name.into(),
            target: target.into(),
        }
    }
}

impl From<&Kind> for Kind {
    fn from(value: &Kind) -> Self {
        value.clone()
    }
}

/// The visibility for a symbol.
///
/// Note that this is not necessarily all the same visibility levels as Java;
/// mostly we just care "is it public or not", not all the other stuff like
/// "how visible" or "is it final".
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, strum::Display)]
#[strum(serialize_all = "snake_case")]
pub enum Visibility {
    /// Users outside the package can access this symbol.
    Public,

    /// The symbol is only accessible within the package.
    Private,
}

/// Kinds of scope that can be parsed.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, strum::Display)]
#[strum(serialize_all = "snake_case")]
pub enum Scope {
    /// Denotes a scope entry.
    Enter(Location),

    /// Denotes a scope exit.
    Exit(Location),
}
