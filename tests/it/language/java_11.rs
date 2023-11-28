use pretty_assertions::assert_eq;
use snippets::{language::java_11, Extractor, Kind, Location, Metadata, Method, Snippet};

use crate::include_str_lf;

// #[test]
// fn smoke_test() {
//     crate::tracing::setup();

//     let kind = Kind::Full;
//     let transform = None;
//     let span = Location::from(24..115);

//     let options = java_11::Options;
//     let content = include_str_lf!("testdata/java_11/smoke_test.java");
//     let extract = java_11::Extractor::extract(&options, &content).expect("set up parser");

//     let expected = vec![Snippet::from(
//         Metadata::new(kind, Method::from(transform), span),
//         span.extract_from(content.as_bytes()),
//     )];

//     assert_eq!(extract, expected);
// }

#[test]
fn smoke_test() {
    crate::tracing::setup();

    let kind = Kind::Full;
    let transform = None;

    let options = java_11::Options;
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

    assert_eq!(extract, expected);
}

#[test]
fn full_raw_hello_world() {
    crate::tracing::setup();

    let kind = Kind::Full;
    let transform = None;
    let span = Location::from(24..115);

    let options = java_11::Options;
    let content = include_str_lf!("testdata/java_11/hello_world.java");
    let extract = java_11::Extractor::extract(&options, &content).expect("set up parser");

    let expected = vec![Snippet::from(
        Metadata::new(kind, Method::from(transform), span),
        span.extract_from(content.as_bytes()),
    )];

    assert_eq!(extract, expected);
}
