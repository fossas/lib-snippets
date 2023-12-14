//! Types and helpers for treating iterators
//! in a manner similar to combinatorial parsers.
//!
//! The goal isn't to replicate a full combinatorial parser yet;
//! just steal some ideas from the concept.

/// Describes a generic parser that works with this module.
pub trait Parser<I: Iterator, T>: Fn(I::Item) -> Option<T> {}
impl<I: Iterator, F: Fn(I::Item) -> Option<T>, T> Parser<I, T> for F {}

/// Iterates until the first item for which the provided parser returns `Some`.
///
/// Items read from the iterator until that point are dropped.
/// The returned `Some` value is the result of this function.
pub fn parse_some<I: Iterator, T>(iter: &mut I, parser: impl Parser<I, T>) -> Option<T> {
    for item in iter {
        if let Some(parsed) = parser(item) {
            return Some(parsed);
        }
    }
    None
}

/// Copies the provided context alongside each iterator entry.
pub struct ContextIter<I, C> {
    iter: I,
    context: C,
}

/// An item in the iterator that has context.
pub type ContextItem<T, C> = (T, C);

impl<I: Iterator, C: Copy> Iterator for ContextIter<I, C> {
    type Item = ContextItem<I::Item, C>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|item| (item, self.context))
    }
}

/// Adaptor to map the provided context into each iterator entry.
pub trait ToContextIter<I, C> {
    /// Copies the provided context alongside each iterator entry.
    fn context(self, context: C) -> ContextIter<I, C>;
}

impl<I: Iterator, C: Copy> ToContextIter<I, C> for I {
    fn context(self, context: C) -> ContextIter<I, C> {
        ContextIter {
            iter: self,
            context,
        }
    }
}
