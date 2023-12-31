#![doc = include_str!("../README.md")]
#![deny(clippy::invalid_regex)]

use std::{collections::HashSet, marker::PhantomData, path::Path, str::Utf8Error};

use content::Content;
use derivative::Derivative;
use derive_more::{Constructor, Deref, Index};
pub use fallible_iterator::FallibleIterator;
use flagset::{flags, FlagSet};
use getset::{CopyGetters, Getters};
use itertools::Itertools;
use once_cell::sync::OnceCell;
use parser::bytes::Location;
use strum::{Display, EnumIter};
use tap::{Conv, Pipe};
use thiserror::Error;
use tree_sitter::Node;
use tree_sitter_traversal::{traverse, Order};
use typed_builder::TypedBuilder;

pub mod content;
pub mod debugging;
pub(crate) mod ext;
pub mod language;
pub mod parser;
pub mod text;

/// Convenience import for all types that
/// an implementation of [`Extractor`] would likely need.
///
/// Some commonly-named types are renamed to reduce the likelihood of collisions
/// when imported via this prelude: e.g. [`Error`] becomes `ExtractorError`.
///
/// [`Extractor`]: crate::Extractor
/// [`Error`]: enum@crate::Error
pub mod impl_prelude {
    pub use super::parser::bytes::Location as SnippetLocation;
    pub use super::{
        Context as SnippetContext, Error, Extractor as SnippetExtractor, Kind as SnippetKind,
        Kinds as SnippetKinds, Language as SnippetLanguage, LanguageError,
        Metadata as SnippetMetadata, Method as SnippetMethod, Options as SnippetOptions, Snippet,
        Strategy as LanguageStrategy, Target as SnippetTarget, Transform as SnippetTransform,
        Transforms as SnippetTransforms,
    };
}

/// Errors reported by [`Extractor`].
#[derive(Debug, Error)]
#[non_exhaustive]
pub enum Error {
    #[error("configure parser")]
    Configure(#[from] LanguageError),

    #[error("read input as utf8")]
    DecodeUTF8(#[from] Utf8Error),

    #[error("read file content")]
    ReadFile(#[source] std::io::Error),
}

impl Error {
    fn configure(err: tree_sitter::LanguageError) -> Self {
        format!("{err}").pipe(LanguageError).into()
    }
}

/// An error that occurs when trying to assign an incompatible language to a parser.
// Note: Implementing it this way allows us to keep `tree_sitter` out of the public API.
//       More details: https://docs.rs/thiserror/latest/thiserror/
#[derive(Debug, Error)]
#[error("language: {0}")]
pub struct LanguageError(String);

/// An implementation of [`Extractor`] enables output to be extracted
/// from a given unit of source code (typically a file).
pub trait Extractor {
    /// The options accepted by the extractor.
    type Options;

    /// The output of the extractor.
    type Output;

    /// Reads the provided unit of source code for output, according to the provided options.
    fn extract(opts: &Self::Options, content: &Content) -> Result<Self::Output, Error>;

    /// Reads the provided file for output, according to the provided options.
    fn extract_file(opts: &Self::Options, path: &Path) -> Result<Self::Output, Error> {
        let content = Content::from_file(path).map_err(Error::ReadFile)?;
        Self::extract(opts, &content)
    }
}

/// Default options for extracting snippets.
/// Options are constructed via the `Options::builder` method.
///
/// Note that different extractors can provide unique option types.
///
/// # Best effort
///
/// Constructed combinations of options may not make sense.
/// For example, a hypothetical future [`Target`] type may not ever have comments
/// (imagine a `Target` that reports only a function name and a list of argument types for example),
/// and therefore [`Transform::Comment`] may not make sense in the context of that target.
///
/// It is up to the snippet implementation what happens in this scenario.
/// Consumers should not rely on any particular behavior here, and should expect to filter or handle
/// any combination of skippets that the provided options allow.
///
/// By default the recommendation is that the implementation should emit a snippet for all combinations,
/// and leave it up to consumers to decide what constitutes a duplicate snippet and de-duplicate as desired.
///
/// # Defaults and empty sets
///
/// With the exception of [`Options::transforms`], any empty set provided
/// is replaced with the default set
/// (as provided by the implementation of [`Default`] for [`Options`]).
///
/// By default, all kinds of snippet are extracted, and all normalizations are applied.
/// Providing an empty set to [`Options::kinds`] is equivalent to the default set
/// (namely, all [`Kind`]s).
///
/// # The [`Method`] type
///
/// [`Options::transforms`] is converted to [`Method`],
/// always implicitly attaching [`Method::Raw`].
///
/// Specifically, an empty [`Options::transforms`] still results in [`Method::Raw`]
/// fingerprints being provided by default.
///
/// # Examples
///
/// Defaults:
/// ```
/// # use snippets::*;
/// let options = Options::default();
///
/// assert_eq!(options.targets(), Targets::full());
/// assert_eq!(options.kinds(), Kinds::full());
/// assert_eq!(options.methods(), &Methods::full());
/// ```
///
/// Restricting the kinds of snippet extracted:
/// ```
/// # use snippets::*;
/// let options = Options::new(Target::Function, Kind::Signature, Transforms::full());
/// assert!(options.kinds().contains(Kind::Signature));
/// assert!(!options.kinds().contains(Kind::Body));
/// assert!(!options.kinds().contains(Kind::Full));
/// ```
///
/// Restricting the transforms applied:
/// ```
/// # use snippets::*;
/// let options = Options::new(Target::Function, Kinds::full(), Transform::Comment);
/// assert!(options.methods().contains_transform(Transform::Comment));
/// assert!(!options.methods().contains_transform(Transform::Space));
/// ```
///
/// Only use [`Method::Raw`]:
/// ```
/// # use snippets::*;
/// let options = Options::new(Target::Function, Kinds::full(), Methods::raw());
/// assert!(options.methods().contains(Method::Raw));
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct Options {
    /// The target units of source code to extract as snippets.
    #[getset(get_copy = "pub")]
    targets: Targets,

    /// The kinds of snippet to extract.
    #[getset(get_copy = "pub")]
    kinds: Kinds,

    /// The normalizations used to extract this snippet.
    #[getset(get = "pub")]
    methods: Methods,
}

impl Options {
    /// Create a new set of options for a snippet extractor.
    pub fn new(
        targets: impl Into<Targets>,
        kinds: impl Into<Kinds>,
        methods: impl Into<Methods>,
    ) -> Self {
        Self {
            targets: targets.conv::<Targets>().default_if_empty(),
            kinds: kinds.conv::<Kinds>().default_if_empty(),
            methods: methods.into(),
        }
    }

    /// Ensure that [`Method::Raw`] is disabled in the options.
    /// Ideally, just don't pass this method in the options in the first place.
    pub fn disable_raw(mut self) -> Self {
        self.methods.0.remove(&Method::Raw);
        self
    }

    /// Report the cartesian product of the configured [`Kind`]s of snippets to extract
    /// with configured [`Method`]s to apply.
    pub fn cartesian_product(&self) -> impl Iterator<Item = (Target, Kind, Method)> {
        itertools::iproduct!(self.targets.iter(), self.kinds.iter(), self.methods.iter())
    }
}

impl Default for Options {
    fn default() -> Self {
        Self {
            targets: Targets::full(),
            kinds: Kinds::full(),
            methods: Methods::full(),
        }
    }
}

/// Standardizes the description of languages supported by [`Extractor`] implementations.
///
/// Note: [`impl_language!`] is available for convenience trait implementations if desired.
///
/// # Example
///
/// ```
/// use snippets::Language;
///
/// pub struct CustomLanguage;
///
/// impl Language for CustomLanguage {
///     const NAME: &'static str = "example";
///     const STRATEGY: snippets::Strategy = snippets::Strategy::Static;
/// }
///
/// assert_eq!(&format!("{}", CustomLanguage::display()), "example/static");
/// ```
///
/// # Customization
///
/// ```
/// use snippets::Language;
///
/// pub struct CustomLanguage;
///
/// impl Language for CustomLanguage {
///     const NAME: &'static str = "example";
///     const STRATEGY: snippets::Strategy = snippets::Strategy::Static;
///
///     fn display() -> &'static str {
///         "custom name"
///     }
/// }
///
/// assert_eq!(&format!("{}", CustomLanguage::display()), "custom name");
/// ```
pub trait Language {
    /// The name of the language.
    const NAME: &'static str;

    /// The strategy used for parsing the language.
    const STRATEGY: Strategy;

    /// Override the display of the language name if desired.
    fn display() -> &'static str {
        static DISPLAY: OnceCell<String> = OnceCell::new();
        DISPLAY.get_or_init(|| format!("{}/{}", Self::NAME, Self::STRATEGY))
    }
}

/// Convenience macro to implement standard traits for a [`Language`].
///
/// This library cannot auto implement debug implementations because
/// they may be foreign types for a foreign trait.
///
/// Trait | Default | Description
/// --- | --- | ---
/// `Debug` | Yes | Implement `std::fmt::Debug` with the same text as `Self::display()`.
/// `Display` | Yes | Implement `std::fmt::Display` with the same text as `Self::display()`.
///
/// Implement all "default" traits in the table above by calling this macro
/// with only the type name as an argument:
/// ```ignore
/// impl_language!(CustomLanguage);
/// ```
///
/// Implement a subset of traits by calling this macro with the type and trait:
/// ```ignore
/// impl_language!(CustomLanguage => Debug);
/// ```
///
/// Only traits in the table above are supported at all.
///
/// # Example
///
/// ```
/// use snippets::{impl_language, Language};
///
/// pub struct CustomLanguage;
///
/// impl Language for CustomLanguage {
///     const NAME: &'static str = "example";
///     const STRATEGY: snippets::Strategy = snippets::Strategy::Static;
/// }
///
/// impl_language!(CustomLanguage);
///
/// assert_eq!(format!("{}", CustomLanguage), format!("{}", CustomLanguage::display()));
/// assert_eq!(format!("{:?}", CustomLanguage), format!("{}", CustomLanguage));
/// ```
///
/// # Customization
///
/// Users may still customize the display of a language:
/// ```
/// use snippets::{impl_language, Language};
///
/// pub struct CustomLanguage;
///
/// impl Language for CustomLanguage {
///     const NAME: &'static str = "example";
///     const STRATEGY: snippets::Strategy = snippets::Strategy::Static;
///
///     fn display() -> &'static str {
///         "custom name"
///     }
/// }
///
/// impl_language!(CustomLanguage);
///
/// assert_eq!(&format!("{}", CustomLanguage), "custom name");
/// assert_eq!(&format!("{:?}", CustomLanguage), "custom name");
/// ```
#[macro_export]
macro_rules! impl_language {
    ($language:ty => Debug) => {
        impl std::fmt::Debug for $language {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", <Self as $crate::Language>::display())
            }
        }
    };
    ($language:ty => Display) => {
        impl std::fmt::Display for $language {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", <Self as $crate::Language>::display())
            }
        }
    };
    ($language:ty) => {
        $crate::impl_language!($language => Display);
        $crate::impl_language!($language => Debug);
    };

}

/// Many programming languages include compile-time metaprogramming,
/// for example C and C++ have [preprocessing macros],
/// Rust has [multiple types of macros],
/// Haskell has [Template Haskell], and more.
///
/// This type allows a [`Language`], specified for an [`Extractor`],
/// to advertise the kind of parsing strategy it employs to parse the language.
///
/// [preprocessing macros]: https://gcc.gnu.org/onlinedocs/cpp/Macros.html
/// [multiple types of macros]: https://doc.rust-lang.org/book/ch19-06-macros.html
/// [Template Haskell]: http://wiki.haskell.org/Template_Haskell
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display)]
#[strum(serialize_all = "snake_case")]
#[non_exhaustive]
pub enum Strategy {
    /// The extractor statically analyzes the code.
    /// No compile time metaprogramming is evaluated.
    Static,
}

/// An extracted snippet from the given unit of source code.
#[derive(Clone, Getters, CopyGetters, Index, Deref, Derivative, TypedBuilder)]
#[derivative(Ord, PartialEq, Eq)]
pub struct Snippet<L> {
    /// Metadata for the extracted snippet.
    #[getset(get_copy = "pub")]
    metadata: Metadata,

    /// The bytes of the snippet fingerprint.
    #[index]
    #[deref]
    #[getset(get = "pub")]
    #[derivative(PartialOrd = "ignore", Ord = "ignore")]
    fingerprint: text::Buffer,

    /// Reports the content that actually generated the fingerprint.
    #[getset(get = "pub")]
    #[derivative(PartialOrd = "ignore", Ord = "ignore", PartialEq = "ignore")]
    content: text::Buffer,

    /// Used to disambiguate snippets by source language.
    ///
    /// Technically this is evaluated for ordering and equality,
    /// but `PhantomData<T>` is always equal to itself for both checks.
    #[builder(default, setter(skip))]
    language: PhantomData<L>,
}

impl<L> PartialOrd for Snippet<L> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<L> Snippet<L> {
    /// Create a new snippet from the provided data.
    pub fn from(meta: Metadata, content: impl AsRef<[u8]>) -> Self {
        Self::builder()
            .content(text::Buffer::new(content.as_ref()))
            .fingerprint(text::fingerprint(content))
            .metadata(meta)
            .build()
    }
}

impl<L: Language> std::fmt::Display for Snippet<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", L::display(), self.metadata)
    }
}

impl<L: Language> std::fmt::Debug for Snippet<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Snippet")
            .field("language", &L::display())
            .field("metadata", &self.metadata)
            .field("fingerprint", &self.fingerprint)
            .field("content", &self.content)
            .finish()
    }
}

/// The metadata for an extracted snippet.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, CopyGetters, Constructor)]
#[getset(get_copy = "pub")]
pub struct Metadata {
    /// The kind of item this snippet represents.
    kind: Kind,

    /// The method used to extract this snippet.
    method: Method,

    /// The location at which the snippet was found.
    location: Location,
}

impl std::fmt::Display for Metadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}/{}", self.kind, self.method, self.location)
    }
}

flags! {
    /// The kind of item this snippet represents.
    ///
    /// # Specificity order
    ///
    /// Specificity is in the order specified by the implementation of [`Ord`] for this type,
    /// meaning that a [`Kind::Full`] variant is considered a more exact match
    /// than a [`Kind::Body`] variant, which is a more exact match
    /// than a [`Kind::Signature`] variant.
    ///
    /// Items with higher "specificity order" are sorted _higher_; meaning that a
    /// [`Kind::Full`] variant would be sorted later in a vector
    /// than a [`Kind::Signature`] variant:
    ///
    /// ```
    /// # use snippets::*;
    /// assert!(Kind::Full > Kind::Body);
    /// assert!(Kind::Body > Kind::Signature);
    /// ```
    #[derive(Hash, PartialOrd, Ord, EnumIter, Display)]
    #[strum(serialize_all = "snake_case")]
    #[non_exhaustive]
    pub enum Kind: u8 {
        /// The signature of the snippet.
        ///
        /// ```ignore
        /// fn say_happy_birthday(age: usize) -> String            // <- included
        /// {                                                      // <- omitted
        ///   println!("Happy birthday! You're {age} years old!"); // <- omitted
        /// }                                                      // <- omitted
        /// ```
        Signature,

        /// The body of the snippet.
        ///
        /// ```ignore
        /// fn say_happy_birthday(age: usize) -> String {          // <- omitted
        ///   println!("Happy birthday! You're {age} years old!"); // <- included
        /// }                                                      // <- omitted
        /// ```
        Body,

        /// Both signature and body in one snippet.
        ///
        /// ```ignore
        /// fn say_happy_birthday(age: usize) -> String {          // <- included
        ///   println!("Happy birthday! You're {age} years old!"); // <- included
        /// }                                                      // <- included
        /// ```
        Full,
    }
}

/// The kinds of snippet to extract.
///
/// # Examples
///
/// Single [`Kind`] in the set:
/// ```
/// # use snippets::*;
/// let kinds = Kinds::from(Kind::Signature);
/// assert!(kinds.contains(Kind::Signature));
/// ```
///
/// Multiple [`Kind`]s in the set:
/// ```
/// # use snippets::*;
/// let kinds = Kinds::from(Kind::Signature | Kind::Body);
/// assert!(kinds.contains(Kind::Signature));
/// assert!(kinds.contains(Kind::Body));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Kinds(FlagSet<Kind>);

impl Default for Kinds {
    fn default() -> Self {
        Self::full()
    }
}

impl Kinds {
    /// Check whether a given [`Kind`] is in the set.
    ///
    /// # Example
    ///
    /// ```
    /// # use snippets::*;
    /// let kinds = Kinds::from(Kind::Signature | Kind::Body);
    /// assert!(kinds.contains(Kind::Signature));
    /// assert!(kinds.contains(Kind::Body));
    /// ```
    pub fn contains(&self, kind: Kind) -> bool {
        self.0.contains(kind)
    }

    /// Check whether the set is empty.
    ///
    /// # Example
    ///
    /// ```
    /// # use snippets::*;
    /// let kinds = Kinds::from(Kind::Signature | Kind::Body);
    /// assert!(!kinds.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Create a new set of all [`Kind`]s.
    ///
    /// # Example
    ///
    /// ```
    /// # use snippets::*;
    /// let kinds = Kinds::full();
    /// assert!(kinds.contains(Kind::Signature));
    /// assert!(kinds.contains(Kind::Body));
    /// ```
    pub fn full() -> Self {
        Self(FlagSet::full())
    }

    /// Iterate over the [`Kind`]s in the set.
    pub fn iter(&self) -> impl Iterator<Item = Kind> + Clone {
        self.0.into_iter()
    }
}

impl<I: IntoIterator<Item = Kind>> From<I> for Kinds {
    fn from(value: I) -> Self {
        let mut value = value.into_iter();
        if let Some(first) = value.next() {
            let mut fs = FlagSet::from(first);
            for flag in value {
                fs |= flag;
            }
            Self(fs)
        } else {
            Self(FlagSet::default())
        }
    }
}

impl From<Kind> for Kinds {
    fn from(value: Kind) -> Self {
        Self(value.into())
    }
}

impl std::fmt::Display for Kinds {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let kinds = self
            .iter()
            .sorted_unstable()
            .map(|t| t.to_string())
            .collect_vec()
            .join(",");
        write!(f, "{kinds}")
    }
}

/// Methods used to extract snippets.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Methods(HashSet<Method>);

impl Methods {
    /// Convenience constructor for all methods.
    pub fn full() -> Self {
        Transforms::full().pipe(Self::raw_and)
    }

    /// Construct a set of methods which only contains the `Raw` method.
    pub fn raw() -> Self {
        HashSet::from_iter([Method::Raw]).pipe(Self)
    }

    /// Construct a set of methods including raw and the provided transforms.
    pub fn raw_and(transforms: Transforms) -> Self {
        Method::iter(transforms).collect::<HashSet<_>>().pipe(Self)
    }

    /// Construct an empty set of methods.
    pub fn empty() -> Self {
        HashSet::new().pipe(Self)
    }

    /// Add a method to the set.
    pub fn insert(&mut self, method: Method) {
        self.0.insert(method);
    }

    /// Iterate over methods in the set.
    pub fn iter(&self) -> impl Iterator<Item = Method> + Clone {
        self.0.iter().copied().collect_vec().into_iter()
    }

    /// Report whether a method is contained in the set.
    pub fn contains(&self, test: Method) -> bool {
        self.0.contains(&test)
    }

    /// Report whether a transform is contained in the set.
    pub fn contains_transform(&self, test: Transform) -> bool {
        for method in &self.0 {
            let Method::Normalized(transform) = method else {
                continue;
            };
            if *transform == test {
                return true;
            }
        }
        false
    }
}

impl From<Option<Transforms>> for Methods {
    fn from(value: Option<Transforms>) -> Self {
        match value {
            Some(transforms) => Self::from(transforms),
            None => Self::raw(),
        }
    }
}

impl From<Option<Transform>> for Methods {
    fn from(value: Option<Transform>) -> Self {
        Transforms::from(value).pipe(Self::from)
    }
}

impl From<Transform> for Methods {
    fn from(value: Transform) -> Self {
        Transforms::from(value).pipe(Self::from)
    }
}

impl From<Transforms> for Methods {
    fn from(value: Transforms) -> Self {
        Self::raw_and(value)
    }
}

impl std::fmt::Display for Methods {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let methods = self
            .iter()
            .sorted_unstable()
            .map(|t| t.to_string())
            .collect_vec()
            .join(",");
        write!(f, "{methods}")
    }
}

/// The method used to extract this snippet.
///
/// # Specificity order
///
/// Specificity is in the order specified by the implementation of [`Ord`] for this type,
/// meaning that a [`Method::Raw`] variant is considered a more exact match
/// than a [`Method::Normalized`] variant.
///
/// Items with higher "specificity order" are sorted _higher_; meaning that a
/// [`Method::Raw`] variant would be sorted later in a vector
/// than a [`Method::Normalized`] variant:
///
/// ```
/// # use snippets::*;
/// # let arbitrary = Transform::Space;
/// assert!(Method::Raw > Method::Normalized(arbitrary));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum Method {
    /// Generated from the text with the specified normalizations applied.
    Normalized(Transform),

    /// Generated from the text as written.
    ///
    /// ```ignore
    /// fn say_happy_birthday(age: usize) -> String {
    ///   // TODO: make 'years' smart plural.
    ///   println!("Happy birthday! You're {age} years old!");
    /// }
    /// ```
    Raw,
}

impl Method {
    /// Create an iterator over possible methods to use for snippet extraction,
    /// given the provided [`Transforms`].
    ///
    /// If the provided set is empty, this is equivalent to [`std::iter::once`] over [`Method::Raw`].
    pub fn iter(transforms: Transforms) -> impl Iterator<Item = Method> + Clone {
        // Implement with `Vec` so that the types for each branch line up.
        // Since each branch uses a macro to construct an appropriately-sized `Vec`,
        // this is no worse performance than e.g. `iter::once().chain(iter::once())`.
        if transforms.is_empty() {
            vec![Method::Raw]
        } else {
            std::iter::once(Method::Raw)
                .chain(transforms.iter().map(Method::Normalized))
                .collect_vec()
        }
        .into_iter()
    }
}

impl<I: IntoIterator<Item = Transform>> From<I> for Method {
    fn from(value: I) -> Self {
        let mut value = value.into_iter();
        if let Some(first) = value.next() {
            Self::Normalized(first)
        } else {
            Self::Raw
        }
    }
}

impl std::fmt::Display for Method {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Method::Normalized(transforms) => write!(f, "normalized({transforms})"),
            Method::Raw => write!(f, "raw"),
        }
    }
}

flags! {
    /// The normalization used to extract this snippet.
    ///
    /// # Specificity order
    ///
    /// Specificity is in the order specified by the implementation of [`Ord`] for this type,
    /// meaning that a [`Transform::Space`] variant is considered a more exact match
    /// than a [`Transform::Comment`] variant.
    ///
    /// Items with higher "specificity order" are sorted _higher_; meaning that a
    /// [`Transform::Space`] variant would be sorted later in a vector
    /// than a [`Transform::Comment`] variant:
    ///
    /// ```
    /// # use snippets::*;
    /// assert!(Transform::Space > Transform::Comment);
    /// ```
    #[derive(Hash, PartialOrd, Ord, EnumIter, Display)]
    #[strum(serialize_all = "snake_case")]
    #[non_exhaustive]
    pub enum Transform: u8 {
        /// Transform the text to have any comments removed and whitespace normalized.
        /// Equivalent to [`Transform::Comment`] followed by [`Transform::Space`].
        ///
        /// # Example
        ///
        /// The original input:
        /// ```ignore
        /// fn say_happy_birthday(age: usize) -> String {
        ///   // TODO: make 'years' smart plural.
        ///   println!("Happy birthday! You're {age} years old!");
        /// }
        /// ```
        ///
        /// Is normalized to this:
        /// ```ignore
        /// fn say_happy_birthday(age: usize) -> String { println!("Happy birthday! You're {age} years old!"); }
        /// ```
        Code,

        /// Generated with any comments removed. Exactly what constitutes a comment is up to the implementation
        /// of the [`Extractor`] for the language being analyzed.
        ///
        /// # Example
        ///
        /// The original input:
        /// ```ignore
        /// fn say_happy_birthday(age: usize) -> String {
        ///   // TODO: make 'years' smart plural.
        ///   println!("Happy birthday! You're {age} years old!");
        /// }
        /// ```
        ///
        /// Is normalized to this:
        /// ```ignore
        /// fn say_happy_birthday(age: usize) -> String {
        ///   println!("Happy birthday! You're {age} years old!");
        /// }
        /// ```
        Comment,

        /// Generated with any whitespace characters (including newlines) normalized to a single space.
        /// Contiguous spaces are also collapsed to a single space. The specific test for whether
        /// a character is considered "whitespace" is the Unicode property `White_Space=yes`.
        ///
        /// # Example
        ///
        /// The original input:
        /// ```ignore
        /// fn say_happy_birthday(age: usize) -> String {
        ///   // TODO: make 'years' smart plural.
        ///   println!("Happy birthday! You're {age} years old!");
        /// }
        /// ```
        ///
        /// Is normalized to this:
        /// ```ignore
        /// fn say_happy_birthday(age: usize) -> String { // TODO: make 'years' smart plural. println!("Happy birthday! You're {age} years old!"); }
        /// ```
        Space,
    }
}

/// The normalizations used to extract this snippet.
///
/// # Examples
///
/// Single [`Transform`] in the set:
/// ```
/// # use snippets::*;
/// let transforms = Transforms::from(Transform::Space);
/// assert!(transforms.contains(Transform::Space));
/// ```
///
/// Multiple [`Transform`]s in the set:
/// ```
/// # use snippets::*;
/// let transforms = Transforms::from(Transform::Space | Transform::Comment);
/// assert!(transforms.contains(Transform::Space));
/// assert!(transforms.contains(Transform::Comment));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Transforms(FlagSet<Transform>);

impl Default for Transforms {
    fn default() -> Self {
        Self::full()
    }
}

impl Transforms {
    /// Check whether a given [`Transform`] is in the set.
    ///
    /// # Example
    ///
    /// ```
    /// # use snippets::*;
    /// let transforms = Transforms::from(Transform::Space | Transform::Comment);
    /// assert!(transforms.contains(Transform::Space));
    /// assert!(transforms.contains(Transform::Comment));
    /// ```
    pub fn contains(&self, transform: Transform) -> bool {
        self.0.contains(transform)
    }

    /// Check whether the set is empty.
    ///
    /// # Example
    ///
    /// ```
    /// # use snippets::*;
    /// let transforms = Transforms::from(Transform::Space | Transform::Comment);
    /// assert!(!transforms.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Create a new set of all [`Transform`]s.
    ///
    /// # Example
    ///
    /// ```
    /// # use snippets::*;
    /// let transforms = Transforms::full();
    /// assert!(transforms.contains(Transform::Space));
    /// assert!(transforms.contains(Transform::Comment));
    /// ```
    pub fn full() -> Self {
        Self(FlagSet::full())
    }

    /// Create a new set with no [`Transform`]s.
    ///
    /// # Example
    ///
    /// ```
    /// # use snippets::*;
    /// let transforms = Transforms::none();
    /// assert!(!transforms.contains(Transform::Space));
    /// assert!(!transforms.contains(Transform::Comment));
    /// ```
    pub fn none() -> Self {
        Self(FlagSet::default())
    }

    /// Iterate over the [`Transform`]s in the set.
    pub fn iter(&self) -> impl Iterator<Item = Transform> + Clone {
        self.0.into_iter()
    }
}

impl std::fmt::Display for Transforms {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let transforms = self
            .iter()
            .sorted_unstable()
            .map(|t| t.to_string())
            .collect_vec()
            .join(",");
        write!(f, "{transforms}")
    }
}

impl<I: IntoIterator<Item = Transform>> From<I> for Transforms {
    fn from(value: I) -> Self {
        let mut value = value.into_iter();
        if let Some(first) = value.next() {
            let mut fs = FlagSet::from(first);
            for flag in value {
                fs |= flag;
            }
            Self(fs)
        } else {
            Self(FlagSet::default())
        }
    }
}

impl From<Transform> for Transforms {
    fn from(value: Transform) -> Self {
        Self(value.into())
    }
}

flags! {
    /// The targets of snippets to extract.
    ///
    /// # Specificity order
    ///
    /// Specificity is in the order specified by the implementation of [`Ord`] for this type.
    /// Currently only one variant exists, but the idea is similar to that of [`Kind`] or [`Transform`]:
    /// the more exact and meaningful the snippet target, the higher specificity.
    ///
    /// Items with higher "specificity order" are sorted _higher_; meaning that a
    /// higher specificity variant is sorted later in a vector
    /// than a lower specificity variant.
    #[derive(Hash, PartialOrd, Ord, EnumIter, Display)]
    #[strum(serialize_all = "snake_case")]
    #[non_exhaustive]
    pub enum Target: u8 {
        /// Targets function defintions as snippets.
        Function,
    }
}

/// The targets of snippet to extract.
///
/// # Examples
///
/// Single [`Target`] in the set:
/// ```
/// # use snippets::*;
/// let targets = Targets::from(Target::Function);
/// assert!(targets.contains(Target::Function));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Targets(FlagSet<Target>);

impl Default for Targets {
    fn default() -> Self {
        Self::full()
    }
}

impl Targets {
    /// Target function declarations as snippets.
    ///
    /// # Example
    ///
    /// ```
    /// # use snippets::*;
    /// let targets = Targets::from(Target::Function);
    /// assert!(targets.contains(Target::Function));
    /// ```
    pub fn contains(&self, target: Target) -> bool {
        self.0.contains(target)
    }

    /// Check whether the set is empty.
    ///
    /// # Example
    ///
    /// ```
    /// # use snippets::*;
    /// let targets = Targets::from(Target::Function);
    /// assert!(!targets.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Create a new set of all [`Target`]s.
    ///
    /// # Example
    ///
    /// ```
    /// # use snippets::*;
    /// let targets = Targets::full();
    /// assert!(targets.contains(Target::Function));
    /// ```
    pub fn full() -> Self {
        Self(FlagSet::full())
    }

    /// Iterate over the [`Target`]s in the set.
    pub fn iter(&self) -> impl Iterator<Item = Target> + Clone {
        self.0.into_iter()
    }
}

impl<I: IntoIterator<Item = Target>> From<I> for Targets {
    fn from(value: I) -> Self {
        let mut value = value.into_iter();
        if let Some(first) = value.next() {
            let mut fs = FlagSet::from(first);
            for flag in value {
                fs |= flag;
            }
            Self(fs)
        } else {
            Self(FlagSet::default())
        }
    }
}

impl From<Target> for Targets {
    fn from(value: Target) -> Self {
        Self(value.into())
    }
}

impl std::fmt::Display for Targets {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let targets = self
            .iter()
            .sorted_unstable()
            .map(|t| t.to_string())
            .collect_vec()
            .join(",");
        write!(f, "{targets}")
    }
}

impl DefaultIfEmpty for Targets {
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

impl DefaultIfEmpty for Kinds {
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

trait DefaultIfEmpty: Default {
    fn is_empty(&self) -> bool;

    fn default_if_empty(self) -> Self {
        if self.is_empty() {
            Self::default()
        } else {
            self
        }
    }
}

/// This structure represents a view into a larger piece of parsed text.
/// For snippet scanning, we generally look at just parts of a larger piece of text for each snippet.
#[derive(Debug, PartialEq, Getters, CopyGetters)]
pub struct Context<'a> {
    /// The location (in `content`) of this snippet.
    #[getset(get_copy = "pub")]
    location: Location,

    /// Parsed nodes representing the snippet.
    #[getset(get = "pub")]
    nodes: Vec<Node<'a>>,

    /// The full text in which this snippet resides.
    content: &'a [u8],
}

impl<'a> Context<'a> {
    /// Make a new instance from a parent node and its location within the original parsed text.
    ///
    /// Ensure that the content provided is the same as the content used to extract the parent node;
    /// byte offsets must line up for operations on this type to make sense.
    pub fn new(parent: Node<'a>, location: Location, content: &'a [u8]) -> Self {
        Self::from_nodes(traverse(parent.walk(), Order::Pre), location, content)
    }

    /// Make a new instance from a set of nodes and their location within the original parsed text.
    ///
    /// Ensure that the content provided is the same as the content used to extract the nodes;
    /// byte offsets must line up for operations on this type to make sense.
    pub fn from_nodes(
        nodes: impl IntoIterator<Item = Node<'a>>,
        location: Location,
        content: &'a [u8],
    ) -> Self {
        Context {
            nodes: nodes.into_iter().collect(),
            location,
            content,
        }
    }

    /// Extract the part of the content indicated by the location.
    pub fn content(&self) -> &[u8] {
        self.location.extract_from(self.content)
    }

    /// Get content from the snippet which is not in ranges covered by the provided nodes.
    pub fn content_around(&self, nodes: impl Iterator<Item = &'a Node<'a>>) -> Vec<u8> {
        let mut slices = Vec::new();
        let mut start_byte = self.location.start_byte();

        for node in nodes {
            let node_start_byte = node.start_byte();
            slices.push(&self.content[start_byte..node_start_byte]);
            start_byte = node.end_byte();
        }

        // `Location::end_byte` is inclusive.
        slices.push(&self.content[start_byte..=self.location.end_byte()]);
        slices.concat()
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn specificity_order_kind() {
        let mut input = vec![Kind::Body, Kind::Signature, Kind::Full];
        input.sort_unstable();
        assert_eq!(input, vec![Kind::Signature, Kind::Body, Kind::Full]);
    }

    #[test]
    fn specificity_order_method() {
        let arbitrary = Transform::Space;
        let mut input = vec![Method::Raw, Method::Normalized(arbitrary)];
        input.sort_unstable();

        let expected = vec![Method::Normalized(arbitrary), Method::Raw];
        assert_eq!(input, expected);
    }

    #[test]
    fn specificity_order_normalization() {
        let mut input = vec![Transform::Space, Transform::Comment];
        input.sort_unstable();
        assert_eq!(input, vec![Transform::Comment, Transform::Space]);
    }

    #[test]
    fn specificity_order_normalizations() {
        let mut input = vec![
            Method::Normalized(Transform::Comment),
            Method::Normalized(Transform::Code),
            Method::Normalized(Transform::Space),
        ];
        let expected = vec![
            Method::Normalized(Transform::Code),
            Method::Normalized(Transform::Comment),
            Method::Normalized(Transform::Space),
        ];

        input.sort_unstable();
        assert_eq!(input, expected);
    }

    #[test]
    fn slice_offset() -> Result<(), std::str::Utf8Error> {
        let example = "#include <stdio.h>  int main() {}";
        let location = Location::builder().byte_offset(20).byte_len(10).build();

        let range = location.as_range();
        let snippet = &example.as_bytes()[range];
        let got = std::str::from_utf8(snippet)?;
        assert_eq!(got, "int main()");

        Ok(())
    }

    #[test]
    fn location_round_trip() {
        let location = Location::builder().byte_offset(20).byte_len(10).build();
        let inclusive_range = 20..=29;
        let range = 20..30;

        assert_eq!(location.as_range(), range);
        assert_eq!(Location::from(inclusive_range), location);
        assert_eq!(Location::from(range), location);
    }

    #[test]
    fn location_bytes_round_trip() {
        let location = Location::builder().byte_offset(10).byte_len(10).build();

        let input = "0123456789helloworld_abcdefghijk";
        assert_eq!(location.extract_from(input.as_bytes()), b"helloworld");

        let range = location.start_byte()..=location.end_byte();
        let roundtrip = Location::from(range);
        assert_eq!(roundtrip, location);
        assert_eq!(roundtrip.extract_from(input.as_bytes()), b"helloworld");

        let range = location.as_range();
        let roundtrip = Location::from(range);
        assert_eq!(roundtrip, location);
        assert_eq!(roundtrip.extract_from(input.as_bytes()), b"helloworld");
    }

    #[test]
    fn location_extract() {
        let input = "0123456789helloworld0123456789";
        let location = Location::builder().byte_offset(10).byte_len(10).build();

        assert_eq!(location.extract_from(input.as_bytes()), b"helloworld");
        assert_eq!(location.start_byte(), 10);
        assert_eq!(location.end_byte(), 19);
    }

    #[test]
    fn location_extract_end() {
        let input = "0123456789helloworld";
        let location = Location::builder().byte_offset(10).byte_len(10).build();

        assert_eq!(location.extract_from(input.as_bytes()), b"helloworld");
    }
}
