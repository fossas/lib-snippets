//! Specialized types for dealing with content provided to this library,
//! or reading content into this library.

use std::{borrow::Cow, path::Path};

use derivative::Derivative;
use derive_more::Index;
use tap::Pipe;

/// Specialized type to indicate the original content provided to the extractor,
/// distinct from a sliced section of that content.
#[derive(Clone, PartialEq, Eq, Derivative, Index)]
#[derivative(Debug = "transparent")]
pub struct Content(Vec<u8>);

impl Content {
    /// Create a new instance with the provided content.
    pub fn new(content: Vec<u8>) -> Self {
        Self(content)
    }

    /// Read a file on disk as content.
    pub fn from_file(path: impl AsRef<Path>) -> Result<Self, std::io::Error> {
        std::fs::read(path).map(Self::new)
    }

    /// View the content as a plain byte slice.
    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }
}

impl<U: AsRef<[u8]>> From<U> for Content {
    fn from(value: U) -> Self {
        value.as_ref().pipe(|v| v.to_vec()).pipe(Self)
    }
}

/// Common functionality for any type indicating a section of bytes to extract from [`Content`].
pub trait ByteCoordinate {
    /// The byte offset at which the function starts.
    fn byte_start(&self) -> usize;

    /// The byte offset at which the function ends.
    fn byte_end(&self) -> usize;

    /// Extract the text representing this part from the specified content.
    fn extract_from<'a>(&self, content: &'a Content) -> &'a [u8] {
        &content[self.byte_start()..self.byte_end()]
    }

    /// Extract the text representing this part from the specified content as a lossy string.
    fn extract_from_lossy<'a>(&self, content: &'a Content) -> Cow<'a, str> {
        let content = self.extract_from(content);
        String::from_utf8_lossy(content)
    }
}
