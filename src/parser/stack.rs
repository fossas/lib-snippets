use delegate::delegate;
use getset::{CopyGetters, Getters};
use tap::Pipe;
use tracing::trace;

use crate::{ext::iter::PeekingExt, parser::bytes::Location};

/// A stack models the types in a given source code file.
///
/// # Long lived scopes
///
/// For any given scope, an element in the scope can't be fully
/// resolved as they're parsed because its definition may come later.
/// Similarly, the contents of nested scopes can't be resolved when
/// they close because they may rely on definitions in an ancestor scope
/// that haven't yet been parsed.
///
/// This means we can't limit the stack to only "in scope items"
/// like a real stack in an executing program.
/// Instead, each scope has to be long lived to power lookups
/// after the file is fully parsed.
///
/// # Typed modeling
///
/// This is not meant to fully model execution of a program.
/// Generally the intention is to model type mappings.
///
/// This is hard to really express, but easy to show;
/// the comments and code below signify the kinds of inferences
/// the modeling is meant to support:
/// ```not_rust
/// // Record the package for the file.
/// package com.example.MyApp;
///
/// // Record imports for the file,
/// // meaning that the symbol `Logger`
/// // signifies the type `java.util.logging.Logger`.
/// import java.util.logging.Logger;
///
/// // Record that the class is defined in this package,
/// // meaning `AppFunctions` is `com.example.MyApp.AppFunctions`.
/// public class AppFunctions {
///
///   // Record the mapping of "the variable `logger`
///   // refers to the type `java.util.logging.Logger`".
///   //
///   // Also, implicitly attach the method invocation
///   // of `java.util.logging.Logger::getLogger(_)`
///   // to the scope so that when building the call graph,
///   // public static and constructor methods include this
///   // in their call graphs.
///   private static final Logger logger =
///     Logger.getLogger(TestFunctions.class.getName());
///
///   // Record that `AppFunctions` is a constructor
///   // inside the scope of the class `AppFunctions`,
///   // meaning its full path is
///   // `com.example.MyApp.AppFunctions::AppFunctions()`.
///   public AppFunctions() {
///     // Resolve `logger` to the type `java.util.logging.Logger`,
///     // meaning that `com.example.MyApp.AppFunctions::AppFunctions()`
///     // calls `java.util.logging.Logger::info(String)`.
///     logger.info("Constructor called");
///   }
///
///   // Record that `simpleMethod` is a method
///   // inside the scope of the class `AppFunctions`,
///   // meaning its full path is
///   // `com.example.MyApp.AppFunctions::simpleMethod()`.
///   public void simpleMethod() {
///     // Resolve `logger` to the type `java.util.logging.Logger`,
///     // meaning that `com.example.MyApp.AppFunctions::simpleMethod()`
///     // calls `java.util.logging.Logger::info(String)`.
///     logger.info("simpleMethod called");
///
///     // Resolve `doThing(int)` to the type `AppFunctions`,
///     // meaning that `com.example.MyApp.AppFunctions::simpleMethod()`
///     // calls `com.example.MyApp.AppFunctions::doThing(int)`.
///     doThing(5);
///
///     // Resolve `computeThing()` to the type `AppFunctions`,
///     // meaning that `com.example.MyApp.AppFunctions::simpleMethod()`
///     // calls `com.example.MyApp.AppFunctions::computeThing()`.
///     //
///     // Additionally, record that it returns a `String`,
///     // so that we can map it to the correct overload
///     // (which here is `java.util.logging.Logger::info(String)`).
///     logger.info(computeThing());
///   }
///
///   // Record that `doThing(int)` is a method
///   // inside the scope of the class `AppFunctions`,
///   // meaning its full path is
///   // `com.example.MyApp.AppFunctions::doThing(int)`.
///   public int doThing(int a) {
///     return a;
///   }
///
///   // Record that `computeThing()` is a method
///   // inside the scope of the class `AppFunctions`,
///   // meaning its full path is
///   // `com.example.MyApp.AppFunctions::computeThing()`.
///   public string computeThing() {
///     return "";
///   }
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stack<T: PartialEq> {
    entries: Vec<Entry<T>>,
    scope_enters: usize,
    scope_exits: usize,
}

macro_rules! scope_indent {
    ($stack:ident => $($arg:tt)*) => {{
        let inner = format!($($arg)*);
        format!("{:indent$}{inner}", "", indent = $stack.scope_level())
    }};
}

impl<T: PartialEq> Stack<T> {
    /// Enter a new scope on the stack.
    pub fn enter(&mut self, location: impl Into<Location>) {
        let loc = location.into();
        let entry = UnitSymbol::from(loc).pipe(Entry::Enter);
        self.entries.push(entry);

        trace!(%loc, "{}", scope_indent!(self => "scope_enter"));
        self.scope_enters += 1;
    }

    /// Exit the current scope on the stack.
    pub fn exit(&mut self, location: impl Into<Location>) {
        let loc = location.into();
        let entry = UnitSymbol::from(loc).pipe(Entry::Exit);
        self.entries.push(entry);

        self.scope_exits += 1;
        trace!(%loc, "{}", scope_indent!(self => "scope_exit"));

        debug_assert!(
            self.scope_enters >= self.scope_exits,
            "scope enter count ({}) must be >= exit count ({}) when a scope is exited",
            self.scope_enters,
            self.scope_exits,
        );
    }

    /// Push a new symbol onto the stack.
    pub fn push(&mut self, symbol: impl Into<Symbol<T>>) {
        let symbol = symbol.into();
        trace!(loc = %symbol.location, "{}", scope_indent!(self => "push: {symbol:?}"));

        let symbol = Entry::Symbol(symbol);
        self.entries.push(symbol);
    }

    /// Retrace the stack, reporting all the symbols in scope.
    /// Symbols are reported in the inverse order that they are added to the stack.
    pub fn retrace(&self) -> impl Iterator<Item = &Symbol<T>> {
        self.entries.iter().rev().pipe(ScopedStackIterator::new)
    }

    /// Retrace the stack, reporting all the symbols in scope
    /// starting at the given symbol in the stack,
    /// not including the search symbol itself.
    /// Symbols are reported in the inverse order that they are added to the stack.
    ///
    /// If the given symbol is not found in the stack, the returned vector is empty.
    pub fn retrace_from<'a, 'b: 'a>(
        &'a self,
        symbol: &'b Symbol<T>,
    ) -> impl Iterator<Item = &'a Symbol<T>> {
        self.entries
            .iter()
            .rev()
            .peeking_skip_until(move |entry| entry == symbol)
            .skip(1)
            .pipe(ScopedStackIterator::new)
    }

    fn scope_level(&self) -> usize {
        self.scope_enters.saturating_sub(self.scope_exits)
    }

    delegate! {
        to self.entries {
            /// Get the last entry in the stack.
            pub fn last(&self) -> Option<&Entry<T>>;
        }
    }
}

/// Manually implemented because the derive based implementation
/// generates code that for some reason requires `T` to implement
/// `Default`, even though this doesn't generate a default
/// value of `T` (just a default `Vec<T>` that holds it).
impl<T: PartialEq> Default for Stack<T> {
    fn default() -> Self {
        Self {
            entries: Default::default(),
            scope_enters: Default::default(),
            scope_exits: Default::default(),
        }
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
///
/// Regardless of additional information provided,
/// the stack solely uses the source code location for equality.
#[derive(Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct Symbol<T: PartialEq> {
    /// The inner type of the symbol.
    /// This may be specific to the language being parsed.
    #[getset(get = "pub")]
    inner: T,

    /// The location of the symbol in source code.
    #[getset(get_copy = "pub")]
    location: Location,
}

impl<T: PartialEq> Symbol<T> {
    /// Construct a new instance with the specified inner type and location.
    pub fn new(inner: impl Into<T>, location: impl Into<Location>) -> Self {
        Self {
            inner: inner.into(),
            location: location.into(),
        }
    }
}

impl<T: Clone + PartialEq> From<&Symbol<T>> for Symbol<T> {
    fn from(value: &Symbol<T>) -> Self {
        value.clone()
    }
}

impl<T: PartialEq> std::fmt::Debug for Symbol<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.location.as_range())
    }
}

impl<T: PartialEq> Symbol<T> {
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
type UnitSymbol = Symbol<()>;

impl<T: Into<Location>> From<T> for UnitSymbol {
    fn from(value: T) -> Self {
        Symbol::new((), value)
    }
}

/// Tracks the items required for the stack to perform its duties.
///
/// Note that since entries can be used as cursors, it's important
/// that entries contain information that allows different entries
/// with the same name to be disambiguated.
/// An easy way to do this is by including their location:
/// a given symbol can only appear in the source code in one place.
///
/// Some types (such as [`Symbol`]) already include location
/// and are therefore inherently able to disambuguate entries.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Entry<T: PartialEq> {
    /// Indicates that a scope has been entered.
    Enter(UnitSymbol),

    /// Indicates that a scope has been closed.
    Exit(UnitSymbol),

    /// Indicates a symbol in the currently active scope.
    Symbol(Symbol<T>),
}

impl<T: PartialEq> Entry<T> {
    /// The location of the entry in the source code, regardless of variant.
    pub fn location(&self) -> Location {
        match self {
            Entry::Enter(s) => s.location(),
            Entry::Exit(s) => s.location(),
            Entry::Symbol(s) => s.location(),
        }
    }
}

impl<T: PartialEq> PartialEq<Symbol<T>> for &Entry<T> {
    fn eq(&self, other: &Symbol<T>) -> bool {
        match self {
            Entry::Symbol(symbol) => symbol == other,
            _ => false,
        }
    }
}

/// Iterates over entries in [`Stack`],
/// dropping entries that are not in the current scope.
///
/// Important: this assumes it is iterating the stack in reverse.
/// Since this is so easy to misuse, this type really shouldn't be public.
struct ScopedStackIterator<I> {
    iter: I,
}

impl<I> ScopedStackIterator<I> {
    /// Drops stack entries that are not in the current scope.
    /// Important: this assumes it is iterating the stack in reverse.
    fn new(iter: I) -> Self {
        Self { iter }
    }
}

impl<'a, T, I> Iterator for ScopedStackIterator<I>
where
    Self: 'a,
    T: 'a + PartialEq,
    I: Iterator<Item = &'a Entry<T>>,
{
    type Item = &'a Symbol<T>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next()? {
            // Multiple exits may come one after another.
            // Keep a running count until the same number of enters have been passed.
            Entry::Exit(_) => {
                let mut exit_count = 1;
                for entry in self.iter.by_ref() {
                    match entry {
                        Entry::Enter(_) => exit_count -= 1,
                        Entry::Exit(_) => exit_count += 1,
                        _ => {}
                    }
                    if exit_count == 0 {
                        break;
                    }
                }
                self.next()
            }
            // Don't touch exit count here:
            // this is only reached when walking unclosed scope.
            Entry::Enter(_) => self.next(),
            Entry::Symbol(symbol) => Some(symbol),
        }
    }
}
