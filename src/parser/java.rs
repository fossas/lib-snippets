//! Parsers and types for Java source code.

use derive_more::Constructor;
use enum_common_fields::EnumCommonFields;
use getset::{CopyGetters, Getters};
use nonempty::NonEmpty;

use super::bytes::Location;

/// The signature of a method.
///
/// Java allows overloading methods on:
/// - Number of arguments
/// - Type of arguments
///
/// This means that the following methods are distinct:
/// ```not_rust
/// public void example();
/// public void example(int a);
/// public void example(int a, String b);
/// public void example(String a, int b);
/// ```
///
/// Varargs complicate this as well.
/// The following methods are considered ambiguous:
/// ```not_rust
/// public void example(int ... a);
/// public void example(int a, int b);
/// ```
///
/// Happily, neither return type nor argument names
/// are considered overloads, meaning that methods which
/// attempt to overload on these are considered ambiguous.
///
/// Based on these rules, this type consists of the
/// minimal signature required to disambiguate overloads.
///
/// The above methods are recorded as:
/// ```not_rust
/// Signature ( "example()" )
/// Signature ( "example(int...)" )
/// Signature ( "example(int)" )
/// Signature ( "example(int, String)" )
/// Signature ( "example(String, int)" )
/// ```
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Signature(String);

/// The label of a generic symbol that is not a method.
///
/// This can be thought of as the equivalent to [`Signature`],
/// without the implied necessity of checking for overloads
/// when resolving.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Label(String);

/// A sequence of symbols that lead to a terminating symbol.
/// The terminating symbol is the last entry in the list.
///
/// For example, the value:
/// ```not_rust
/// Path([
///   Symbol::Package{ label: "default" },
///   Symbol::Class{ label: "TestFunctions" },
///   Symbol::ClassMethod{ signature: "simpleMethod()" },
/// ])
/// ```
/// indicates the class method `simpleMethod()`
/// on the class `TestFunctions` in the `default` package.
///
/// Symbols in the path are hierarchical:
/// a symbol preceding another symbol in the path indicates
/// that the latter symbol is defined within the former.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Path(NonEmpty<Symbol>);

/// Part of a fully qualified path.
#[derive(Clone, Eq, PartialEq, Debug, EnumCommonFields)]
#[common_field(location: Location)]
pub enum Symbol {
    /// Represents a package name.
    ///
    /// Nested packages are separate:
    /// ```not_rust
    /// java.util.logging.Logger
    /// ```
    /// is represented as:
    /// ```not_rust
    /// NonEmpty[
    ///   Symbol::Package{ label: "java" },
    ///   Symbol::Package{ label: "util" },
    ///   Symbol::Package{ label: "logging" },
    ///   Symbol::Class{ label: "Logger" },
    /// ]
    /// ```
    Package { label: Label, location: Location },

    /// Represents a class name.
    Class { label: Label, location: Location },

    /// Represents a constructor of a class.
    Constructor {
        signature: Signature,
        location: Location,
    },

    /// Represents an instance method on a class.
    InstanceMethod {
        signature: Signature,
        location: Location,
    },

    /// Represents a class method on a class.
    ClassMethod {
        signature: Signature,
        location: Location,
    },
}

/// A method declaration in source code.
///
/// Together, `MethodDeclaration` and `MethodInvocation`
/// form the backbone of the call graph that this package exports.
#[derive(Clone, Eq, PartialEq, Debug, Getters, CopyGetters, Constructor)]
pub struct MethodDeclaration {
    /// The full path to the method declaration.
    ///
    /// Does not include the method declaration itself.
    /// For example with the following code:
    /// ```not_rust
    /// public class TestFunctions {
    ///     public void simpleMethod() {
    ///         // ...
    ///     }
    /// }
    /// ```
    ///
    /// The path to `simpleMethod` is:
    /// ```not_rust
    /// Path([
    ///   Symbol::Package{ label: "default" },
    ///   Symbol::Class{ label: "TestFunctions" },
    /// ])
    /// ```
    ///
    /// To build a full path including this declaration,
    /// append the appropriate [`Symbol`] type
    /// to the path.
    ///
    /// Note: the "default" package is implicit in Java programs
    /// if no other package is declared.
    path: Path,

    /// The signature of the method being declared.
    ///
    /// `signature` + `path` is enough to uniquely identify a method on a class
    /// without knowing if the signature refers to a static or instance method
    /// because Java does not allow classes to have both kinds with the same signature.
    signature: Signature,

    /// The location of this declaration in the source code.
    location: Location,

    /// The methods invoked by this method.
    ///
    /// For example, in the following code:
    /// ```not_rust
    /// import java.util.logging.Logger;
    ///
    /// public class TestFunctions {
    ///
    ///     // Logger for logging messages
    ///     private static final Logger logger =
    ///         Logger.getLogger(TestFunctions.class.getName());
    ///
    ///     // Default constructor
    ///     public TestFunctions() {
    ///         logger.info("Constructor called");
    ///     }
    ///
    ///     public void simpleMethod() {
    ///         methodWithParam(5);
    ///     }
    /// }
    /// ```
    ///
    /// The value of `invokes` for `simpleMethod` is:
    /// ```not_rust
    /// Vec[
    ///   MethodInvocation{
    ///     path: NonEmpty[
    ///       Symbol::Package{ label: "default" },
    ///       Symbol::Class{ label: "TestFunctions" },
    ///     ],
    ///     target: NonEmpty[
    ///       Symbol::Package{ label: "default" },
    ///       Symbol::Class{ label: "TestFunctions" },
    ///     ],
    ///     signature: "methodWithParam(int)",
    ///   }
    /// ]
    /// ```
    ///
    /// Note that `Logger.getLogger` is called _when the class is first initialized_,
    /// not when an instance is constructed (note the `static` keyword).
    /// Initializing the class can happen when:
    /// 1. An instance of the class is created.
    /// 2. A static method of the class is invoked.
    /// 3. A static field of the class is assigned or accessed.
    /// 4. The class is referenced via reflection
    ///    (note that reflection is not supported by this parser).
    ///
    /// To avoid such methods from getting lost in the reported graph,
    /// we'll report methods called during static initialization
    /// as part of the `invokes` list of anything that _could_ result in
    /// the initializer being called.
    ///
    /// For example, the value of `invokes` for the constructor is:
    /// ```not_rust
    /// Vec[
    ///   MethodInvocation{
    ///     path: NonEmpty[
    ///       Symbol::Package{ label: "default" },
    ///       Symbol::Class{ label: "TestFunctions" },
    ///     ],
    ///     target: NonEmpty[
    ///       Symbol::Package{ label: "java" },
    ///       Symbol::Package{ label: "util" },
    ///       Symbol::Package{ label: "logging" },
    ///       Symbol::Class{ label: "Logger" },
    ///     ],
    ///     signature: "getLogger(String)",
    ///   },
    ///   MethodInvocation{
    ///     path: NonEmpty[
    ///       Symbol::Package{ label: "default" },
    ///       Symbol::Class{ label: "TestFunctions" },
    ///       Symbol::Constructor{ signature: "TestFunctions()" },
    ///     ],
    ///     target: NonEmpty[
    ///       Symbol::Package{ label: "java" },
    ///       Symbol::Package{ label: "util" },
    ///       Symbol::Package{ label: "logging" },
    ///       Symbol::Class{ label: "Logger" },
    ///     ],
    ///     signature: "info(String)",
    ///   },
    /// ]
    /// ```
    ///
    /// This is because the constructor:
    /// - _May_ cause the static initializer to run,
    ///   implicitly calling `Logger::getLogger`.
    /// - Explicitly runs `Logger::info`.
    ///
    /// The reasoning here is that if a class is loaded, it's being used;
    /// if used, any entrypoint call path can result in initialization;
    /// therefore all entrypoints should record that they invoke
    /// static initializers for the purposes of our call graph.
    ///
    /// While we're here, it's worth noting that the
    /// intention of this package, and therefore this property,
    /// is to report all _build-time defined_ edges between methods,
    /// regardless of _runtime_ behavior.
    /// This means that if for example a method is only called on
    /// a specific platform, or in a specific runtime scenario,
    /// it's always reported in the graph.
    ///
    /// Given this, the decision to report static initializers
    /// is consistent with the overall theme of the library.
    invokes: Vec<MethodInvocation>,
}

/// A method invocation in source code.
///
/// Together, `MethodDeclaration` and `MethodInvocation`
/// form the backbone of the call graph that this package exports.
#[derive(Clone, Eq, PartialEq, Debug, Getters, CopyGetters, Constructor)]
pub struct MethodInvocation {
    /// The full path to the method invocation.
    ///
    /// Does not include the method invocation itself.
    /// For example with the following code:
    /// ```not_rust
    /// public class TestFunctions {
    ///     public void simpleMethod() {
    ///         methodWithParam(5);
    ///     }
    /// }
    /// ```
    ///
    /// The path to the `methodWithParams` call inside `simpleMethod` is:
    /// ```not_rust
    /// Path([
    ///   Symbol::Package{ label: "default" },
    ///   Symbol::Class{ label: "TestFunctions" },
    ///   Symbol::MethodDeclaration{ signature: "simpleMethod()" },
    /// ])
    /// ```
    ///
    /// The "default" package is implicit in Java programs
    /// if no other package is declared.
    ///
    /// To build a full path including this declaration,
    /// append the appropriate [`Symbol`] type
    /// to the path.
    path: Path,

    /// The target symbol on which the method is being called.
    ///
    /// All method invocations _should_ have an identifiable target
    /// at parse time, even when performing file-by-file parsing,
    /// with a couple exceptions.
    ///
    /// For example, in the following code:
    /// ```not_rust
    /// import java.util.logging.Logger;
    ///
    /// public class TestFunctions {
    ///
    ///     private static final Logger logger =
    ///         Logger.getLogger(TestFunctions.class.getName());
    ///
    ///     public TestFunctions() {
    ///         logger.info("Constructor called");
    ///     }
    ///
    ///     public void simpleMethod() {
    ///         methodWithParam(5);
    ///     }
    /// }
    /// ```
    ///
    /// We can statically determine that:
    /// - `default.TestFunctions::constructor()` calls `java.util.logging.Logger::info(String)`.
    /// - `default.TestFunctions::simpleMethod()` calls `default.TestFunctions::methodWithParam(int)`.
    ///
    /// This is because:
    /// - Users cannot define methods on a class in a separate file
    ///   (they must subclass or compose to do this).
    /// - All method invocations must be performed on a known type.
    ///
    /// The primary exception to this is classes referenced in another file that is part
    /// of the same package. To work around this, all classes for which the source package
    /// is not determined are attached to the current package for the file;
    /// this way when multiple files that make up a package are parsed,
    /// the resultant symbols can be resolved across files within the same package.
    ///
    /// The other exception to this is wildcard imports, which are not currently supported
    /// by this library. These allow the use of classes implicitly imported from a package,
    /// making it difficult or impossible to statically determine the package of a given class.
    /// For example, in the following code:
    /// ```not_rust
    /// import java.util.logging.*;
    ///
    /// public class TestFunctions {
    ///
    ///     private static final Logger logger =
    ///         Logger.getLogger(TestFunctions.class.getName());
    ///
    ///     public TestFunctions() {
    ///         logger.info("Constructor called");
    ///     }
    ///
    ///     public void simpleMethod() {
    ///         methodWithParam(5);
    ///     }
    /// }
    /// ```
    ///
    /// Currently, this library assumes that `Logger`
    /// (which is not resolvable due to the wildcard import)
    /// belongs to the package declared in the file (in this case, implicitly `default`),
    /// due to the behavior required to model multi-file packages.
    /// As a result, the response from parsing this file looks like:
    /// - `default.TestFunctions::constructor()` calls `default.Logger::info(String)`.
    /// - `default.TestFunctions::simpleMethod()` calls `default.TestFunctions::methodWithParam(int)`.
    ///
    /// While we could assume in this particular case that `Logger`
    /// belongs to the only wildcard import present in the package,
    /// most Java programs that use any wildcard imports are unlikely to stop at one.
    /// Additionally, reporting an association only some of the time is more confusing
    /// to users than just not supporting this association at all.
    target: Path,

    /// The signature of the method being called.
    ///
    /// `signature` + `target` is enough to uniquely identify a method on a class
    /// without knowing if the signature refers to a static or instance method,
    /// because Java does not allow a class to declare both kinds with the same signature.
    signature: Signature,

    /// The location of this invocation in the source code.
    location: Location,
}
