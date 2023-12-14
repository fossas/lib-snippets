//! General extensions to iterators.

use std::iter::Peekable;

/// Extensions to peekable iterators.
pub trait PeekingExt: Iterator + Sized {
    /// Skip elements until the next iteration will yield
    /// the element for which the closure returns `true`.
    ///
    /// After the closure has returned `true` once, further items are passed on
    /// without additional consideration.
    fn peeking_skip_until<F>(self, should_yield: F) -> PeekingSkipUntil<Self, F>
    where
        F: FnMut(&Self::Item) -> bool;
}

impl<I: Iterator> PeekingExt for I {
    fn peeking_skip_until<F>(self, should_yield: F) -> PeekingSkipUntil<Self, F>
    where
        Self: Sized,
        F: FnMut(&Self::Item) -> bool,
    {
        PeekingSkipUntil::new(self, should_yield)
    }
}

/// An iterator adaptor that skips items while the closure returns `true`.
/// After the closure has returned `false` once, further items are passed on
/// without additional consideration.
pub struct PeekingSkipUntil<I: Iterator, F> {
    iter: Peekable<I>,
    should_yield: F,
    passthrough: bool,
}

impl<I: Iterator, F> PeekingSkipUntil<I, F> {
    fn new(iter: I, should_yield: F) -> Self {
        Self {
            iter: iter.peekable(),
            passthrough: false,
            should_yield,
        }
    }
}

impl<I: Iterator, F: FnMut(&I::Item) -> bool> Iterator for PeekingSkipUntil<I, F> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.passthrough {
            return self.iter.next();
        }

        // Haven't called next yet, so the first peek yields the future next.
        while let Some(upcoming) = self.iter.peek() {
            if (self.should_yield)(upcoming) {
                self.passthrough = true;
                break;
            } else {
                self.iter.next();
            }
        }

        // This is only hit if there were no items to peek
        // (so the iterator was empty), or once the iterator
        // is operating in passthrough mode.
        self.iter.next()
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use pretty_assertions::assert_eq;

    use super::PeekingExt;

    #[test]
    fn skips_properly() {
        let items = [1, 2, 3, 4, 5];
        let got = items
            .into_iter()
            .peeking_skip_until(|item| *item > 2)
            .collect_vec();
        let expected = vec![3, 4, 5];
        assert_eq!(got, expected);
    }

    #[test]
    fn empty_iter() {
        let items = std::iter::empty::<i32>();
        let got = items
            .into_iter()
            .peeking_skip_until(|item| *item > 2)
            .collect_vec();
        let expected = vec![];
        assert_eq!(got, expected);
    }

    #[test]
    fn first_item_yielded() {
        let items = [1, 2, 3, 4, 5];
        let got = items.into_iter().peeking_skip_until(|_| true).collect_vec();
        let expected = vec![1, 2, 3, 4, 5];
        assert_eq!(got, expected);
    }
}
