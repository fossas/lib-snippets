//! Types related to working with source code represented as bytes.

use std::{
    borrow::Cow,
    ops::{Range, RangeInclusive},
};

use derive_more::{Add, AddAssign, Display, Sub, SubAssign};
use getset::CopyGetters;
use miette::SourceSpan;
use tree_sitter::Node;
use typed_builder::TypedBuilder;

/// The location in a unit of source code from which content was extracted.
///
/// After opening the file (so a hypothetical reader is at byte offset `0`),
/// the reader then skips a number of bytes equal to `byte_offset`,
/// then reads a number of bytes equal to `byte_len`.
/// The bytes that were read compose the entire content indicated by this [`Location`].
///
/// For example, given the file:
/// ```not_rust
/// #include <stdio.h>
///
/// int main() {
///   printf("hello world\n");
///   return 0;
/// }
/// ```
///
/// In the representation the computer sees, it looks like this (using `⏎` to represent a newline):
/// ```not_rust
/// #include <stdio.h>⏎⏎int main() {⏎  printf("hello world\n");⏎  return 0;⏎}⏎
/// ^^^^                ^        ^
/// 0123...             20 <-9-> 29
/// ```
///
/// The [`Location`] below represents the `int main()` snippet in that example:
/// ```
/// # // ⏎ is a multi-byte symbol, so use an empty space for demonstration instead.
/// # let example = b"#include <stdio.h>  int main() {}";
/// # use snippets::parser::bytes::*;
/// let location = Location::builder().byte_offset(20).byte_len(10).build();
///
/// let range = location.as_range();
/// let snippet = &example[range];
///
/// let got = std::str::from_utf8(snippet)?;
/// assert_eq!(got, "int main()");
/// # Ok::<(), std::str::Utf8Error>(())
/// ```
//
// Note: we use a `TypedBuilder` instead of a `Constructor` here because this way we can accept
// a standard `usize` for each argument while still making it very clear in-code
// which argument is which.
//
// Basically, the intent is to straddle the line between newtype convenience and newtype safety.
#[derive(
    Debug, Clone, Copy, Default, PartialEq, Eq, Hash, PartialOrd, Ord, CopyGetters, TypedBuilder,
)]
#[getset(get_copy = "pub")]
pub struct Location {
    /// The byte offset at which the snippet began.
    #[builder(setter(transform = |input: usize| ByteOffset(input)))]
    byte_offset: ByteOffset,

    /// The number of bytes to read for the snippet from the file.
    #[builder(setter(transform = |input: usize| ByteLen(input)))]
    byte_len: ByteLen,
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..={}", self.start_byte(), self.end_byte())
    }
}

impl Location {
    /// Read a [`Location`] as a range, intended to be used to index a buffer of bytes.
    pub fn as_range(&self) -> std::ops::Range<usize> {
        let start = self.byte_offset.0;
        let len = self.byte_len.0;
        let end = start + len;
        start..end
    }

    /// The index of the first byte indicated for the provided location.
    pub fn start_byte(&self) -> usize {
        self.as_range().start
    }

    /// The index of the last byte indicated for the provided location.
    pub fn end_byte(&self) -> usize {
        let end = self.as_range().end;
        if end == 0 {
            0
        } else {
            end - 1 // as_range is not inclusive, so the last byte _to be read_ is less one.
        }
    }

    /// Whether the location is empty.
    pub fn is_empty(&self) -> bool {
        self.byte_len.0 == 0
    }

    /// Extract the bytes indicated by a [`Location`] from a buffer.
    ///
    /// # Example
    ///
    /// ```
    /// # use snippets::*;
    /// let example = "#include <stdio.h>  int main() {}";
    /// let location = Location::builder().byte_offset(20).byte_len(10).build();
    ///
    /// let got = location.extract_from(example.as_bytes());
    /// assert_eq!(got, b"int main()");
    /// ```
    pub fn extract_from<'a>(&self, buf: &'a [u8]) -> &'a [u8] {
        &buf[self.as_range()]
    }

    /// Extract the bytes indicated by a [`Location`] from a buffer,
    /// into a lossily converted [`String`].
    ///
    /// # Example
    ///
    /// ```
    /// # use snippets::*;
    /// let example = "#include <stdio.h>  int main() {}";
    /// let location = Location::builder().byte_offset(20).byte_len(10).build();
    ///
    /// let got = location.extract_from_lossy(example.as_bytes());
    /// assert_eq!(got, "int main()");
    /// ```
    pub fn extract_from_lossy<'a>(&self, buf: &'a [u8]) -> Cow<'a, str> {
        let bytes = self.extract_from(buf);
        String::from_utf8_lossy(bytes)
    }
}

impl From<Range<usize>> for Location {
    fn from(value: Range<usize>) -> Self {
        let start = value.start;
        let end = value.end;
        Self {
            byte_offset: ByteOffset(start),
            byte_len: ByteLen(end - start),
        }
    }
}

impl From<RangeInclusive<usize>> for Location {
    fn from(value: RangeInclusive<usize>) -> Self {
        let start = *value.start();
        let end = *value.end() + 1;
        Self {
            byte_offset: ByteOffset(start),
            byte_len: ByteLen(end - start),
        }
    }
}

impl From<Node<'_>> for Location {
    fn from(value: Node<'_>) -> Self {
        value.byte_range().into()
    }
}

impl From<Location> for SourceSpan {
    fn from(value: Location) -> Self {
        Self::new(
            value.byte_offset().as_usize().into(),
            value.byte_len().as_usize().into(),
        )
    }
}

impl std::ops::Add for Location {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let start = self.start_byte().min(rhs.start_byte());
        let end = self.end_byte().max(rhs.end_byte());
        (start..=end).into()
    }
}

impl std::ops::AddAssign for Location {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl std::iter::Sum for Location {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Self::default(), |a, b| a + b)
    }
}

/// The byte offset at which the snippet began.
///
/// Zero-based, meaning that if the snippet begins on the first byte of the file,
/// this offset is `0`.
///
/// Think of the offset as
/// "the number of bytes to skip from the start of the file to when this snippet begins".
#[derive(
    Debug,
    Clone,
    Copy,
    Default,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    Display,
    Add,
    AddAssign,
    Sub,
    SubAssign,
)]
pub struct ByteOffset(usize);

impl ByteOffset {
    /// View the offset as a usize.
    pub fn as_usize(self) -> usize {
        self.0
    }
}

/// The number of bytes to read for the snippet from the file.
#[derive(
    Debug,
    Clone,
    Copy,
    Default,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    Display,
    Add,
    AddAssign,
    Sub,
    SubAssign,
)]
pub struct ByteLen(usize);

impl ByteLen {
    /// View the length as a usize.
    pub fn as_usize(self) -> usize {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn location_add() {
        let a = Location::builder().byte_offset(10).byte_len(10).build();
        let b = Location::builder().byte_offset(20).byte_len(20).build();
        let expected = Location::builder().byte_offset(10).byte_len(30).build();
        assert_eq!(expected, a + b);

        let example = b"some text [ highlighted text ] other text";
        //              ^         ^           ^ ^    ^
        //     indexes: 0        10          22 24  29

        let a = Location::builder().byte_offset(10).byte_len(13).build();
        let b = Location::builder().byte_offset(24).byte_len(6).build();

        // Note that the space between them is captured.
        assert_eq!(a.extract_from_lossy(example), "[ highlighted");
        assert_eq!(b.extract_from_lossy(example), "text ]");
        assert_eq!((a + b).extract_from_lossy(example), "[ highlighted text ]");
    }

    #[test]
    fn location_add_assign() {
        let a = Location::builder().byte_offset(10).byte_len(10).build();
        let b = Location::builder().byte_offset(20).byte_len(20).build();

        let mut c = a;
        c += b;

        assert_eq!(a + b, c);
    }
}
