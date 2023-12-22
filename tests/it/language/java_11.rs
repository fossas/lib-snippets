use pretty_assertions::assert_eq;
use snippets::{
    language::java_11::{self, Node},
    parser::{bytes::Location, stack::Stack, Argument, Parameter, Scope, Symbol, Visibility},
    Extractor, Kind, Metadata, Method, Snippet,
};

use crate::include_str_lf;

const PUBLIC: Visibility = Visibility::Public;
const ANY_PARAM: Parameter = Parameter::Variadic(Argument::Any);
const ANY_ARG: Argument = Argument::Any;

macro_rules! node {
    ($loc:expr, $constructor:ident, $($args:expr),* $(,)*) => {
        Symbol::new(Node::$constructor($($args,)*), $loc).into()
    };
}

macro_rules! scope_enter {
    ($loc:expr) => {
        Scope::new_enter($loc).into()
    };
}

macro_rules! scope_exit {
    ($loc:expr) => {
        Scope::new_exit($loc).into()
    };
}

#[test]
fn call_graph_smoke_test() {
    crate::tracing::setup();

    let options = java_11::EmptyOptions;
    let content = include_str_lf!("testdata/java_11/with_package.java");
    let extract = java_11::CallGraphExtractor::extract(&options, &content).expect("set up parser");

    // Using /**/ to indicate each level of scope and macros to keep it all on one line
    // so that this is at least somewhat readable...
    let expected = Stack::<java_11::Node>::from_iter([
        node!(8..25, new_package, "com.example.MyApp"),
        node!(35..52, new_import, "java.util.HashMap"),
        node!(61..85, new_import, "java.util.logging.Logger"),
        node!(164..176, new_class, PUBLIC, "AppFunctions"),
        scope_enter!(177..178),
        /**/ node!(240..253, new_variable, PUBLIC, "logger", "Logger"),
        /**/ node!(256..303, new_invocation, "getLogger", "Logger", []),
        /**/ node!(293..302, new_invocation, "getName", "this", []),
        /**/ node!(344..358, new_constructor, PUBLIC, "AppFunctions", []),
        /**/ scope_enter!(359..360),
        /**/ /**/ node!(369..402, new_invocation, "info", "logger", ANY_ARG),
        /**/ /**/ scope_exit!(408..409),
        /**/ node!(479..493, new_method, PUBLIC, "simpleMethod", []),
        /**/ scope_enter!(494..495),
        /**/ /**/ node!(504..538, new_invocation, "info", "logger", ANY_ARG),
        /**/ /**/ node!(548..566, new_invocation, "methodWithParam", "this", ANY_ARG),
        /**/ /**/ scope_exit!(598..599),
        /**/ node!(667..689, new_method, PUBLIC, "methodWithParam", ANY_PARAM),
        /**/ scope_enter!(690..691),
        /**/ /**/ node!(700..758, new_invocation, "info", "logger", ANY_ARG),
        /**/ /**/ scope_exit!(782..783),
        /**/ node!(845..874, new_method, PUBLIC, "methodWithParam", ANY_PARAM),
        /**/ scope_enter!(875..876),
        /**/ /**/ node!(885..965, new_invocation, "info", "logger", ANY_ARG),
        /**/ /**/ node!(975..989, new_invocation, "staticMethod", "this", []),
        /**/ /**/ scope_exit!(1044..1045),
        /**/ node!(1095..1105, new_class, PUBLIC, "InnerClass"),
        /**/ scope_enter!(1106..1107),
        /**/ /**/ node!(1123..1135, new_constructor, PUBLIC, "InnerClass", []),
        /**/ /**/ scope_enter!(1136..1137),
        /**/ /**/ /**/ node!(1150..1194, new_invocation, "info", "logger", ANY_ARG),
        /**/ /**/ /**/ scope_exit!(1204..1205),
        /**/ /**/ scope_exit!(1210..1211),
        /**/ node!(1275..1291, new_class, PUBLIC, "StaticInnerClass"),
        /**/ scope_enter!(1292..1293),
        /**/ /**/ node!(1309..1327, new_constructor, PUBLIC, "StaticInnerClass", []),
        /**/ /**/ scope_enter!(1328..1329),
        /**/ /**/ /**/ node!(1342..1392, new_invocation, "info", "logger", ANY_ARG),
        /**/ /**/ /**/ scope_exit!(1402..1403),
        /**/ /**/ scope_exit!(1408..1409),
        /**/ scope_exit!(1410..1411),
    ]);

    assert_eq!(expected, extract);
}

#[test]
fn smoke_test() {
    crate::tracing::setup();

    let kind = Kind::Full;
    let transform = None;

    let options = java_11::EmptyOptions;
    let content = include_str_lf!("testdata/java_11/smoke_test.java");
    let extract = java_11::Extractor::extract(&options, &content).expect("set up parser");

    let expected = vec![
        Snippet::from(
            Metadata::new(kind, Method::from(transform), Location::from(247..320)),
            "public TestFunctions() {\n        logger.info(\"Constructor called\");\n    }".as_bytes(),
        ),
        Snippet::from(
            Metadata::new(kind, Method::from(transform), Location::from(378..510)),
            "public void simpleMethod() {\n        logger.info(\"simpleMethod called\");\n        methodWithParam(5); // Calling another method\n    }".as_bytes(),
        ),
        Snippet::from(
            Metadata::new(kind, Method::from(transform), Location::from(567..694)),
            "public int methodWithParam(int a) {\n        logger.info(\"methodWithParam(int a) called with a = \" + a);\n        return a;\n    }".as_bytes(),
        ),
        Snippet::from(
            Metadata::new(kind, Method::from(transform), Location::from(745..956)),
            "public int methodWithParam(int a, int b) {\n        logger.info(\"methodWithParam(int a, int b) called with a = \" + a + \", b = \" + b);\n        staticMethod(); // Calling a static method\n        return a + b;\n    }".as_bytes(),
        ),
        Snippet::from(
            Metadata::new(kind, Method::from(transform), Location::from(983..1068)),
            "public static void staticMethod() {\n        logger.info(\"staticMethod called\");\n    }".as_bytes(),
        ),
        Snippet::from(
            Metadata::new(kind, Method::from(transform), Location::from(1096..1234)),
            "private void privateMethod() {\n        logger.info(\"privateMethod called\");\n        protectedMethod(); // Calling a protected method\n    }".as_bytes(),
        ),
        Snippet::from(
            Metadata::new(kind, Method::from(transform), Location::from(1264..1351)),
            "protected void protectedMethod() {\n        logger.info(\"protectedMethod called\");\n    }".as_bytes(),
        ),
        Snippet::from(
            Metadata::new(kind, Method::from(transform), Location::from(1396..1575)),
            "public void methodWithComplexParam(HashMap<String, Integer> map) {\n        logger.info(\"methodWithComplexParam called\");\n        privateMethod(); // Calling a private method\n    }".as_bytes(),
        ),
        Snippet::from(
            Metadata::new(kind, Method::from(transform), Location::from(1608..1918)),
            "public void methodWithVarargs(String... args) {\n        logger.info(\"methodWithVarargs called with \" + args.length + \" arguments\");\n        // Calling a method with a complex parameter\n        HashMap<String, Integer> map = new HashMap<>();\n        map.put(\"key\", 1);\n        methodWithComplexParam(map);\n    }".as_bytes(),
        ),
        Snippet::from(
            Metadata::new(kind, Method::from(transform), Location::from(1989..2078)),
            "public InnerClass() {\n            logger.info(\"InnerClass constructor called\");\n        }".as_bytes(),
        ),
        Snippet::from(
            Metadata::new(kind, Method::from(transform), Location::from(2175..2276)),
            "public StaticInnerClass() {\n            logger.info(\"StaticInnerClass constructor called\");\n        }".as_bytes(),
        ),
    ];

    assert_eq!(expected, extract);
}

#[test]
fn call_graph_hello_world() {
    crate::tracing::setup();

    let options = java_11::EmptyOptions;
    let content = include_str_lf!("testdata/java_11/hello_world.java");
    let extract = java_11::CallGraphExtractor::extract(&options, &content).expect("set up parser");

    const PUBLIC: Visibility = Visibility::Public;
    const ANY: Parameter = Parameter::Variadic(Argument::Any);

    // Using /**/ to indicate each level of scope and macros to keep it all on one line
    // so that this is at least somewhat readable...
    let expected = Stack::<java_11::Node>::from_iter([
        node!(7..17, new_class, PUBLIC, "HelloWorld"),
        scope_enter!(18..19),
        /**/ node!(43..62, new_method, PUBLIC, "main", ANY),
        /**/ scope_enter!(63..64),
        /**/ /**/ node!(73..108, new_invocation, "println", "System.out", ANY_ARG),
        /**/ /**/ scope_exit!(114..115),
        /**/ scope_exit!(116..117),
    ]);

    assert_eq!(expected, extract);
}

#[test]
fn full_raw_hello_world() {
    crate::tracing::setup();

    let kind = Kind::Full;
    let transform = None;
    let span = Location::from(24..115);

    let options = java_11::EmptyOptions;
    let content = include_str_lf!("testdata/java_11/hello_world.java");
    let extract = java_11::Extractor::extract(&options, &content).expect("set up parser");

    let expected = vec![Snippet::from(
        Metadata::new(kind, Method::from(transform), span),
        span.extract_from(content.as_bytes()),
    )];

    assert_eq!(expected, extract);
}
