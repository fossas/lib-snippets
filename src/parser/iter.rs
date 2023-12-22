//! Types and helpers for treating iterators
//! in a manner similar to combinatorial parsers.
//!
//! The goal isn't to replicate a full combinatorial parser;
//! just steal some ideas from the concept.
//!
//! # Future work
//!
//! ## Wrap around an existing parser combinator
//!
//! Nom is adaptable to other data types than bytes,
//! but it's kind of complicated to do, so I timeboxed it.
//!
//! Depending on how complicated our parser needs get,
//! this may be worth revisiting.
//!
//! ## Backtracking (`alternative`)
//!
//! Ideal, borderline needed, for parsing true alternative arrangements:
//! - Optional visibility modifiers (`private foo()` vs `foo()`).
//! - Optional method targets (`this.foo()` vs `foo()`).
//!
//! Without backtracking, the most we can do is invert `some`
//! to form an "alternative-like" over a single node,
//! instead of the ideal of an alternative series of parsers.
//!
//! Can probably be implemented with `itertools::MultiPeek`
//! + a wrapper around a `MultiPeek` that translates `next` to `peek`
//! so that any parser can be turned into a backtracking parser.
//!
//! Then if the parse is successful, the `alternative` function
//! just consumes items from the underlying iterator.
//!
//! ## Ownership of input
//!
//! Probably needed for backtracking.
//!
//! Parsers functions take ownership of the input,
//! changing their signatures from `Parser<I, T>: Fn(I) -> Option<T>`
//! to `Parser<I, O, T>: Fn(I) -> (O, Option<T>)`
//! where `I` is the input and `O` is the unparsed remainder of the input.

/// Describes a generic parser that works with this module.
pub trait Parser<I, T>: Fn(I) -> Option<T> {}
impl<I, F: Fn(I) -> Option<T>, T> Parser<I, T> for F {}

/// Parses the input, searching for the first item
/// for which the provided parser returns `Some`.
/// Once found, returns that parsed result.
///
/// Items read from the iterator that fail to parse are dropped.
pub fn some<I: Iterator, T>(input: &mut I, parser: impl Parser<I::Item, T>) -> Option<T> {
    input.find_map(parser)
}

/// Parses while the predicate returns true, searching for the first item
/// for which the provided parser returns `Some`.
/// Once found, returns that parsed result.
///
/// The item for which the predicate
/// returns false is consumed from the iterator.
/// Items read from the iterator that fail to parse are dropped.
pub fn some_while<I: Iterator, T>(
    input: &mut I,
    pred: impl Fn(&I::Item) -> bool,
    parser: impl Parser<I::Item, T>,
) -> Option<T> {
    some(&mut input.take_while(pred), parser)
}

/// Greedily performs [`some`] until the input is done.
/// In the case that no items were parsed, the returned vector is empty.
pub fn many<'a, I: Iterator, T>(
    input: &'a mut I,
    parser: impl Parser<I::Item, T> + 'a,
) -> impl Iterator<Item = T> + 'a {
    std::iter::from_fn(move || some(input, &parser))
}

/// Greedily performs [`some_while`] until the input is done.
/// In the case that no items were parsed, the returned vector is empty.
pub fn many_while<'a, I: Iterator, T>(
    input: &'a mut I,
    pred: impl Fn(&I::Item) -> bool + 'a,
    parser: impl Parser<I::Item, T> + 'a,
) -> impl Iterator<Item = T> + 'a {
    std::iter::from_fn(move || some_while(input, &pred, &parser))
}
