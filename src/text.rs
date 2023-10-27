//! Text based operations on byte streams and strings.

use std::{borrow::Cow, ops::Deref};

use base64::Engine;
use derive_more::{Deref, Index};
use sha2::{Digest, Sha256};
use tap::{Conv, Pipe};
use thiserror::Error;

use crate::parser::normalize;

/// Given a buffer, produce a fingerprint of its contents.
pub fn fingerprint(input: impl AsRef<[u8]>) -> Buffer {
    let normalized_line_endings = normalize::crlf(input.as_ref());
    let mut hasher = Sha256::new();
    for &c in normalized_line_endings.deref() {
        hasher.update([c]);
    }
    hasher.finalize().as_slice().pipe(Buffer::new)
}

/// Given a buffer, produce a base64 representation of its contents.
pub fn as_base64(input: impl AsRef<[u8]>) -> String {
    base64::engine::general_purpose::STANDARD_NO_PAD.encode(input)
}

/// Errors reported when decoding a buffer.
#[derive(Debug, Error)]
pub enum Error {
    #[error("decode base64 input")]
    DecodeBase64(#[from] DecodeBase64Error),
}

/// The kind of encoding to use when displaying the buffer.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Encoding {
    /// Indicate the buffer was created with arbitrary bytes.
    Bytes(Vec<u8>),

    /// Indicate the buffer was created with UTF8 text.
    UTF8(String),
}

/// A byte buffer that reports its value in a more human-readable form.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Index, Deref)]
pub struct Buffer {
    encoding: Encoding,
}

impl Buffer {
    fn new_internal(encoding: Encoding) -> Self {
        Self { encoding }
    }

    /// Create a new buffer from arbitrary bytes.
    pub fn new(input: impl Into<Vec<u8>>) -> Self {
        let input = input.into();
        String::from_utf8(input)
            .map(Encoding::UTF8)
            .unwrap_or_else(|err| err.into_bytes().pipe(Encoding::Bytes))
            .pipe(Self::new_internal)
    }

    /// Decode a string into an instance.
    pub fn utf8(input: impl Into<String>) -> Self {
        input
            .conv::<String>()
            .pipe(Encoding::UTF8)
            .pipe(Self::new_internal)
    }

    /// Decode a base64 string into an instance.
    pub fn base64(input: impl AsRef<str>) -> Result<Self, Error> {
        base64::engine::general_purpose::STANDARD_NO_PAD
            .decode(input.as_ref())
            .map_err(Error::from)
            .map(Self::new)
    }

    /// Read the buffer as a `Vec<u8>`.
    pub fn as_bytes(&self) -> &[u8] {
        match &self.encoding {
            Encoding::Bytes(v) => v,
            Encoding::UTF8(s) => s.as_bytes(),
        }
    }

    /// Read the buffer as a `String`.
    /// If the buffer isn't a valid string already, returns the base64 encoded equivalent.
    pub fn as_utf8(&self) -> Cow<'_, str> {
        match &self.encoding {
            Encoding::Bytes(v) => as_base64(v).pipe(Cow::from),
            Encoding::UTF8(s) => Cow::from(s),
        }
    }
}

impl std::fmt::Display for Buffer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let repr = match &self.encoding {
            Encoding::UTF8(s) => Cow::from(s),
            Encoding::Bytes(b) => as_base64(b).pipe(Cow::from),
        };
        f.write_str(&repr)
    }
}

impl std::fmt::Debug for Buffer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Buffer('{self}')")
    }
}

/// An error that occurs when trying to decode into a buffer.
#[derive(Debug, Error)]
#[error(transparent)]
pub struct DecodeBase64Error(#[from] base64::DecodeError);

impl From<base64::DecodeError> for Error {
    fn from(value: base64::DecodeError) -> Self {
        DecodeBase64Error(value).pipe(Self::DecodeBase64)
    }
}
