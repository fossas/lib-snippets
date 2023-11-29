//! Types related to working with source code represented as bytes.

use std::{
    borrow::Cow,
    ops::{Range, RangeInclusive},
};

use getset::CopyGetters;
use typed_builder::TypedBuilder;

/// The location in the unit of source code from which the snippet was extracted.
///
/// After opening the file (so a hypothetical reader is at byte offset `0`),
/// the reader then skips a number of bytes equal to `byte_offset`,
/// then reads a number of bytes equal to `byte_len`.
/// The bytes that were read compose the entire snippet.
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
/// # let example = "#include <stdio.h>  int main() {}";
/// # use snippets::*;
/// let location = Location::builder().byte_offset(20).byte_len(10).build();
///
/// let range = location.as_range();
/// let snippet = &example.as_bytes()[range];
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, CopyGetters, TypedBuilder)]
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

/// The byte offset at which the snippet began.
///
/// Zero-based, meaning that if the snippet begins on the first byte of the file,
/// this offset is `0`.
///
/// Think of the offset as
/// "the number of bytes to skip from the start of the file to when this snippet begins".
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, derive_more::Display)]
pub struct ByteOffset(usize);

impl ByteOffset {
    /// View the offset as a usize.
    pub fn as_usize(self) -> usize {
        self.0
    }
}

/// The number of bytes to read for the snippet from the file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, derive_more::Display)]
pub struct ByteLen(usize);

impl ByteLen {
    /// View the length as a usize.
    pub fn as_usize(self) -> usize {
        self.0
    }
}
