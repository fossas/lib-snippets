//! Types and helpers for treating iterators
//! in a manner similar to combinatorial parsers.
//!
//! The goal isn't to replicate a full combinatorial parser;
//! just steal some ideas from the concept.
//!
//! Longer term I'd like to either convert this to a full blown
//! combinatorial parser library, or adapt nom/something similar
//! to work over arbitrary types instead of bytes
//! (this can be done today but is very complicated,
//! so I didn't take the time).

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
