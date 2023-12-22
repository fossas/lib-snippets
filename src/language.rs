//! Languages supported by this library.
//!
//! Any consumer can build their own language support with the [`super::Extractor`] trait,
//! but this module provides prebuilt language support.

#[cfg(feature = "lang-c99-tc3")]
pub mod c99_tc3;

#[cfg(feature = "lang-cpp-98")]
pub mod cpp_98;

#[cfg(feature = "lang-java-11")]
pub mod java_11;
