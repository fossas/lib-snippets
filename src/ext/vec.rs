//! Extensions to [`Vec`] or Vec-like types.

use nonempty::{nonempty, NonEmpty};

/// Extends [`Vec`] to make it usable in a more functional style.
pub trait FunctionalVec<T> {
    /// Push the new element into self, returning the modified form.
    fn pushed(self, new: T) -> Self;

    /// Extend self with the provided [`IntoIterator`], returning the modified form.
    fn extended(self, new: impl IntoIterator<Item = T>) -> Self;

    /// Reverse self, returning the modified form.
    fn reversed(self) -> Self;

    /// Prepend self with the provided item.
    fn prepended(self, new: T) -> Self;
}

impl<T> FunctionalVec<T> for Vec<T> {
    fn pushed(mut self, new: T) -> Self {
        self.push(new);
        self
    }

    fn extended(mut self, new: impl IntoIterator<Item = T>) -> Self {
        self.extend(new);
        self
    }

    fn reversed(mut self) -> Self {
        self.reverse();
        self
    }

    fn prepended(self, new: T) -> Self {
        vec![new].extended(self)
    }
}

impl<T> FunctionalVec<T> for NonEmpty<T> {
    fn pushed(mut self, new: T) -> Self {
        self.push(new);
        self
    }

    fn extended(mut self, new: impl IntoIterator<Item = T>) -> Self {
        self.extend(new);
        self
    }

    fn reversed(self) -> Self {
        let mut tail = self.tail;
        let old_head = self.head;

        if let Some(head) = tail.pop() {
            NonEmpty::new(head).extended(tail.reversed().pushed(old_head))
        } else {
            NonEmpty::new(old_head).extended(tail)
        }
    }

    fn prepended(self, new: T) -> Self {
        nonempty![new].extended(self)
    }
}

#[cfg(test)]
mod tests {
    use nonempty::nonempty;
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn vec() {
        let start = Vec::new;
        assert_eq!(Vec::<i32>::new(), start().reversed());
        assert_eq!(vec![1], start().pushed(1));
        assert_eq!(vec![1, 2], start().pushed(1).pushed(2));
        assert_eq!(vec![1, 2], start().extended([1, 2]));
        assert_eq!(vec![2, 1], start().extended([1, 2]).reversed());
    }

    #[test]
    fn nonempty() {
        let start = || nonempty![1];
        assert_eq!(nonempty![1], start());
        assert_eq!(nonempty![1], start().reversed());
        assert_eq!(nonempty![1, 2], start().pushed(2));
        assert_eq!(nonempty![1, 2, 3], start().extended([2, 3]));
        assert_eq!(nonempty![3, 2, 1], start().extended([2, 3]).reversed());
    }
}
