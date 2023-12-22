//! Higher level parsers and constants on top of tree-sitter primitives.

use delegate::delegate;
use getset::{CopyGetters, Getters};
use itertools::Itertools;
use tap::Pipe;

use self::bytes::Location;

pub mod bytes;
pub mod iter;
pub mod normalize;
pub mod stack;

#[cfg(feature = "lang-java-11")]
pub mod java;

pub(crate) const NODE_KIND_COMMENT: &str = "comment";
pub(crate) const NODE_KIND_FUNC_DEF: &str = "function_definition";
pub(crate) const NODE_KIND_METHOD_DECL: &str = "method_declaration";
pub(crate) const NODE_KIND_CONSTRUCTOR_DECL: &str = "constructor_declaration";
pub(crate) const NODE_KIND_OPEN_BRACE: &str = "{";
pub(crate) const NODE_KIND_CLOSE_BRACE: &str = "}";
pub(crate) const NODE_KIND_SEMI: &str = ";";

/// Equality isn't the only form of comparison.
/// This trait expresses another form: checking whether an item satisfies a constraint.
pub trait Constraint<T> {
    /// Report whether `self` (the constraint) is satisfied by
    /// `other` (the item being checked against the constraint).
    fn satisfied_by(&self, other: T) -> bool;
}

/// Kinds of scope that can be parsed.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, strum::Display)]
#[strum(serialize_all = "snake_case")]
#[non_exhaustive]
pub enum Scope {
    /// Denotes a scope entry.
    Enter(Location),

    /// Denotes a scope exit.
    Exit(Location),
}

impl Scope {
    /// Create a new variant from the provided location.
    pub fn new_enter(location: impl Into<Location>) -> Self {
        location.into().pipe(Self::Enter)
    }

    /// Create a new variant from the provided location.
    pub fn new_exit(location: impl Into<Location>) -> Self {
        location.into().pipe(Self::Exit)
    }
}

/// The label of a generic symbol.
#[derive(Clone, Eq, PartialEq, Hash, derive_more::Display)]
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

impl std::fmt::Debug for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// The visibility for a symbol.
///
/// Note that this is not necessarily all the same visibility levels as a given language;
/// mostly this library just cares whether a symbol is public or not.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, strum::Display)]
#[strum(serialize_all = "snake_case")]
#[non_exhaustive]
pub enum Visibility {
    /// The symbol is accessible from outside the package.
    Public,

    /// The symbol is only accessible within the package.
    Private,
}

/// Contains multiple parameters for a specific function.
#[derive(Clone, Eq, PartialEq, Hash, Debug, Default)]
pub struct Parameters(Vec<Parameter>);

impl Parameters {
    /// Construct a new instance with the provided parameters.
    pub fn new(params: impl IntoIterator<Item = Parameter>) -> Self {
        params.into_iter().collect_vec().pipe(Self)
    }

    /// Construct a new instance from a set of arguments.
    /// Assumes all arguments become single parameters.
    pub fn from_args(args: impl IntoIterator<Item = Argument>) -> Self {
        args.into_iter()
            .map(Parameter::Single)
            .collect_vec()
            .pipe(Self)
    }

    delegate! {
        to self.0 {
            /// Test whether the set is empty.
            pub fn is_empty(&self) -> bool;

            /// Iterate through the set.
            pub fn iter(&self) -> impl Iterator<Item = &Parameter>;
        }
    }
}

impl IntoIterator for Parameters {
    type Item = Parameter;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, I: IntoIterator<Item = &'a Argument>> Constraint<I> for Parameters {
    /// Check whether the set of arguments satisfies the set of parameters.
    /// This is distinct from equality.
    ///
    /// The key difference is that parameters are really _constraints_ on arguments:
    /// - A single parameter is a constraint: "give me a single argument of this type".
    /// - A variadic parameter is a looser constraint: "give me any number of arguments of this type".
    /// - A generic parameter is even looser: "give me an argument that matches this interface".
    ///
    /// This method checks whether the set of arguments provided matches
    /// the constraints imposed by the set of parameters.
    fn satisfied_by(&self, args: I) -> bool {
        let mut args = args.into_iter().peekable();
        for parameter in self.0.iter() {
            // Automatically handles variadic parameters.
            if !parameter.satisfied_by(args.next()) {
                return false;
            }

            // If the parameter is variadic, eagerly consume all the arguments that match it.
            if matches!(parameter, Parameter::Variadic(_)) {
                args.peeking_take_while(|arg| parameter.satisfied_by(*arg))
                    .for_each(|_| {});
            }
        }

        // Non-matching conditions early return with false.
        true
    }
}

/// Kinds of function parameters that can be parsed.
///
/// Distinct from [`Argument`] in that parameters are part of _declarations_,
/// while arguments are part of _invocations_. The key difference is that
/// parameters are really _constraints_ on arguments:
///
/// - A single parameter is a constraint: "give me a single argument of this type".
/// - A variadic parameter is a looser constraint: "give me any number of arguments of this type".
/// - A generic parameter is even looser: "give me an argument that matches this interface".
///
/// This type is meant to encapsulate this concept.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
#[non_exhaustive]
pub enum Parameter {
    /// Accepts a single argument of the specified type.
    Single(Argument),

    /// Accepts any number of arguments of the specified type.
    Variadic(Argument),
}

impl std::fmt::Display for Parameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Parameter::Single(arg) => write!(f, "{arg}"),
            Parameter::Variadic(arg) => write!(f, "{arg}..."),
        }
    }
}

impl Constraint<Option<&Argument>> for Parameter {
    /// Check whether an argument satisfies this parameter.
    ///
    /// This is distinct from equality:
    /// - If the constraint is any, any argument satisfies it.
    /// - Similarly, if the argument is any, it satisfies any constraint.
    /// - Otherwise, the constraint is only satisfied if they are equal.
    ///
    /// Note that variadic parameters accept _any number_ of arguments,
    /// including zero. So if the provided argument is [`None`],
    /// _and_ the parameter is variadic, the parameter is satisfied.
    fn satisfied_by(&self, arg: Option<&Argument>) -> bool {
        match (self, arg) {
            (Parameter::Single(_), None) => false,
            (Parameter::Single(constraint), Some(arg)) => constraint.satisfied_by(arg),
            (Parameter::Variadic(_), None) => true,
            (Parameter::Variadic(constraint), Some(arg)) => constraint.satisfied_by(arg),
        }
    }
}

impl Constraint<&Argument> for Parameter {
    /// Check whether an argument satisfies this parameter.
    ///
    /// This is distinct from equality:
    /// - If the constraint is any, any argument satisfies it.
    /// - Similarly, if the argument is any, it satisfies any constraint.
    /// - Otherwise, the constraint is only satisfied if they are equal.
    ///
    /// Since this is called with an explicitly non-zero argument,
    /// whether this parameter is single or variadic is ignored.
    fn satisfied_by(&self, arg: &Argument) -> bool {
        match self {
            Parameter::Single(constraint) => constraint.satisfied_by(arg),
            Parameter::Variadic(constraint) => constraint.satisfied_by(arg),
        }
    }
}

impl IntoIterator for Parameter {
    type Item = Parameter;

    type IntoIter = std::iter::Once<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        std::iter::once(self)
    }
}

/// Contains multiple arguments for a specific function.
#[derive(Clone, Eq, PartialEq, Hash, Debug, Default)]
pub struct Arguments(Vec<Argument>);

impl Arguments {
    /// Construct a new instance with the provided arguments.
    pub fn new(args: impl IntoIterator<Item = Argument>) -> Self {
        args.into_iter().collect_vec().pipe(Self)
    }

    delegate! {
        to self.0 {
            /// Test whether the set is empty.
            pub fn is_empty(&self) -> bool;

            /// Iterate through the set.
            pub fn iter(&self) -> impl Iterator<Item = &Argument>;
        }
    }
}

impl IntoIterator for Arguments {
    type Item = Argument;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

/// Kinds of arguments that can be parsed.
///
/// Distinct from [`Parameter`] in that parameters are part of _declarations_,
/// while arguments are part of _invocations_.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
#[non_exhaustive]
pub enum Argument {
    /// An argument with a specific type.
    Typed(Label),

    /// An argument of indeterminate type.
    Any,
}

impl Constraint<&Argument> for Argument {
    /// Treat this argument as a parameter- namely, as a constraint,
    /// then check whether the provided argument satisfies the constraint.
    ///
    /// This is distinct from equality:
    /// - If the constraint is any, any argument satisfies it.
    /// - Similarly, if the argument is any, it satisfies any constraint.
    /// - Otherwise, the constraint is only satisfied if they are equal.
    fn satisfied_by(&self, arg: &Argument) -> bool {
        match (self, arg) {
            (Argument::Any, _) | (_, Argument::Any) => true,
            (Argument::Typed(constraint), Argument::Typed(arg)) => constraint == arg,
        }
    }
}

impl Constraint<Option<&Argument>> for Argument {
    /// Treat this argument as a parameter- namely, as a constraint,
    /// then check whether the provided argument satisfies the constraint.
    ///
    /// If the argument is [`None`], it is considered to not satisfy the constraint.
    ///
    /// This is distinct from equality:
    /// - If the constraint is any, any argument satisfies it.
    /// - Similarly, if the argument is any, it satisfies any constraint.
    /// - Otherwise, the constraint is only satisfied if they are equal.
    fn satisfied_by(&self, arg: Option<&Argument>) -> bool {
        arg.map(|arg| self.satisfied_by(arg)).unwrap_or(false)
    }
}

impl std::fmt::Display for Argument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Argument::Typed(ty) => write!(f, "{ty}"),
            Argument::Any => write!(f, "_"),
        }
    }
}

impl IntoIterator for Argument {
    type Item = Argument;

    type IntoIter = std::iter::Once<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        std::iter::once(self)
    }
}

/// A symbol in the source code.
///
/// Different languages may have additional
/// data they track about the symbol;
/// at its base level this is just a location in the source code.
///
/// Language implementations can customize the generic type
/// to replace the included metadata as appropriate.
#[derive(Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct Symbol<T> {
    /// The inner type of the symbol.
    /// This may be specific to the language being parsed.
    #[getset(get = "pub")]
    inner: T,

    /// The location of the symbol in source code.
    #[getset(get_copy = "pub")]
    location: Location,
}

impl<T> Symbol<T> {
    /// Construct a new instance with the specified inner type and location.
    pub fn new(inner: impl Into<T>, location: impl Into<Location>) -> Self {
        Self {
            inner: inner.into(),
            location: location.into(),
        }
    }
}

impl<T: Clone> From<&Symbol<T>> for Symbol<T> {
    fn from(value: &Symbol<T>) -> Self {
        value.clone()
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Symbol<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} @ {:?}", &self.inner, self.location.as_range())
    }
}

impl<T> Symbol<T> {
    /// The byte length of the symbol.
    pub fn len(&self) -> usize {
        self.location.byte_len().as_usize()
    }

    /// Whether the symbol is empty.
    pub fn is_empty(&self) -> bool {
        self.location.is_empty()
    }
}

/// A [`Symbol`] that has no metadata attached to it, only a location.
pub type UnitSymbol = Symbol<()>;

impl<T: Into<Location>> From<T> for UnitSymbol {
    fn from(value: T) -> Self {
        Symbol::new((), value)
    }
}
