use std::{borrow::Cow, ops::Range};

use bstr::ByteSlice;
use colored::Colorize;
use delegate::delegate;
use tap::Pipe;
use tracing::trace;

use crate::parser::{bytes::Location, Symbol, UnitSymbol};

use super::Scope;

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
#[derive(Debug, Clone)]
pub struct Stack<T: Node> {
    entries: Vec<Entry<T>>,
    scope_enters: usize,
    scope_exits: usize,
}

impl<T: Node> PartialEq for Stack<T> {
    fn eq(&self, other: &Self) -> bool {
        self.entries == other.entries
    }
}

impl<T: Node> Eq for Stack<T> {}

/// All stack nodes must comply with this trait to facilitate operation & debugging.
pub trait Node: PartialEq + std::fmt::Debug {}
impl<T: PartialEq + std::fmt::Debug> Node for T {}

macro_rules! scope_indent {
    ($stack:ident => $($arg:tt)*) => {{
        let inner = format!($($arg)*);
        format!("{:indent$}{inner}", "", indent = $stack.scope_level())
    }};
}

impl<T: Node> Stack<T> {
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
    #[tracing::instrument(skip_all)]
    pub fn retrace(&self) -> impl Iterator<Item = &Symbol<T>> {
        self.entries.iter().rev().pipe(ScopedStackIterator::new)
    }

    /// Retrace the stack, reporting all the symbols in scope
    /// starting at the given symbol in the stack,
    /// not including the search symbol itself.
    /// Symbols are reported in the inverse order that they are added to the stack.
    ///
    /// If the given symbol is not found in the stack, the returned vector is empty.
    #[tracing::instrument(skip_all)]
    pub fn retrace_from<'a, 'b: 'a>(
        &'a self,
        symbol: &'b Symbol<T>,
    ) -> impl Iterator<Item = &'a Symbol<T>> {
        self.entries
            .iter()
            .rev()
            .skip_while(move |entry| entry != symbol)
            .skip(1)
            .pipe(ScopedStackIterator::new)
    }

    /// Iterate through the symbols in the stack.
    #[tracing::instrument(skip_all)]
    pub fn symbols(&self) -> impl Iterator<Item = &Symbol<T>> {
        self.entries.iter().filter_map(|entry| match entry {
            Entry::Symbol(sy) => Some(sy),
            _ => None,
        })
    }

    /// Returns a formatted string representing the input,
    /// where the search symbol and its scope are highlighted.
    ///
    /// - Italic text is the actual symbol for which scope is being reported.
    /// - Dimmed text in the string is not part of the current scope.
    ///   It may be out of scope, or may not have been parsed at all.
    /// - Normal text in the string is part of the current scope.
    #[tracing::instrument(skip_all)]
    pub fn render_scope(&self, content: &[u8], search: &Symbol<T>) -> String {
        content
            .grapheme_indices()
            .map(|(start, end, chunk)| {
                // The end index provided is exclusive, but `Location` assumes
                // inclusive end index.
                (start..end.saturating_sub(1), chunk)
            })
            .map(|(location, chunk)| {
                if search.location.encloses_range(&location) {
                    Cow::Owned(chunk.italic().to_string())
                } else if self.in_context_from(search, &location) {
                    Cow::Borrowed(chunk)
                } else {
                    Cow::Owned(chunk.dimmed().to_string())
                }
            })
            .collect()
    }

    /// Report whether a given byte range is in the context of the parsed stack,
    /// starting from the provided search node.
    #[tracing::instrument(skip_all)]
    fn in_context_from(&self, symbol: &Symbol<T>, search: &Range<usize>) -> bool {
        // No attempt being made to build a more efficient index
        // because premature optimization.
        // If this becomes a problem, consider using an interval tree
        // or some similar structure.
        self.retrace_from(symbol)
            .any(|entry| entry.location().encloses_range(search))
    }

    fn scope_level(&self) -> usize {
        self.scope_enters.saturating_sub(self.scope_exits)
    }

    delegate! {
        to self.entries {
            /// Iterate over the entries in the stack.
            ///
            /// Note that `retrace` and `retrace_from`
            /// are usually more useful.
            pub fn iter(&self) -> impl Iterator<Item = &Entry<T>>;

            /// Get the last entry in the stack.
            pub fn last(&self) -> Option<&Entry<T>>;

            /// Check whether the stack is empty.
            pub fn is_empty(&self) -> bool;
        }
    }
}

impl<T: Node> FromIterator<Entry<T>> for Stack<T> {
    fn from_iter<I: IntoIterator<Item = Entry<T>>>(iter: I) -> Self {
        let mut stack = Self::default();
        for entry in iter {
            match entry {
                Entry::Enter(entry) => stack.enter(entry.location),
                Entry::Exit(entry) => stack.exit(entry.location),
                Entry::Symbol(sy) => stack.push(sy),
            }
        }
        stack
    }
}

/// Manually implemented because the derive based implementation
/// generates code that for some reason requires `T` to implement
/// `Default`, even though this doesn't generate a default
/// value of `T` (just a default `Vec<T>` that holds it).
impl<T: Node> Default for Stack<T> {
    fn default() -> Self {
        Self {
            entries: Default::default(),
            scope_enters: Default::default(),
            scope_exits: Default::default(),
        }
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
pub enum Entry<T: Node> {
    /// Indicates that a scope has been entered.
    Enter(UnitSymbol),

    /// Indicates that a scope has been closed.
    Exit(UnitSymbol),

    /// Indicates a symbol in the currently active scope.
    Symbol(Symbol<T>),
}

impl<T: Node> Entry<T> {
    /// The location of the entry in the source code, regardless of variant.
    pub fn location(&self) -> Location {
        match self {
            Entry::Enter(s) => s.location(),
            Entry::Exit(s) => s.location(),
            Entry::Symbol(s) => s.location(),
        }
    }
}

impl<T: Node> PartialEq<Symbol<T>> for &Entry<T> {
    fn eq(&self, other: &Symbol<T>) -> bool {
        match self {
            Entry::Symbol(symbol) => symbol == other,
            _ => false,
        }
    }
}

impl<T: Node> From<Symbol<T>> for Entry<T> {
    fn from(value: Symbol<T>) -> Self {
        Self::Symbol(value)
    }
}

impl<T: Node> From<Scope> for Entry<T> {
    fn from(value: Scope) -> Self {
        match value {
            Scope::Enter(loc) => Symbol::new((), loc).pipe(Self::Enter),
            Scope::Exit(loc) => Symbol::new((), loc).pipe(Self::Exit),
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
    T: 'a + Node,
    I: Iterator<Item = &'a Entry<T>>,
{
    type Item = &'a Symbol<T>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next()? {
            // Multiple exits may come one after another.
            // Keep a running count until the same number of enters have been passed.
            Entry::Exit(_) => {
                let mut exit_count = 1usize;
                for entry in self.iter.by_ref() {
                    match entry {
                        Entry::Enter(_) => exit_count = exit_count.saturating_sub(1),
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
