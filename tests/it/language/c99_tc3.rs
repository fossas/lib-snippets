use itertools::Itertools;
use pretty_assertions::assert_eq;
use snippets::{
    content::Content, language::c99_tc3, parser::bytes::Location, Extractor, Kind, Kinds, Metadata,
    Method, Options, Snippet, Target, Targets, Transform, Transforms,
};
use tap::Pipe;

use crate::include_str_lf;

#[test]
fn full_raw_hello_world() {
    crate::tracing::setup();

    let kind = Kind::Full;
    let transform = None;
    let span = Location::from(21..74);

    let content = include_str_lf!("testdata/c99_tc3/hello_world.c");
    let opts = Options::new(Target::Function, kind, Transforms::from(transform));
    let extract = c99_tc3::Extractor::extract(&opts, &content).expect("must set up parser");

    let expected = vec![Snippet::from(
        Metadata::new(kind, Method::from(transform), span),
        span.extract_from(content.as_bytes()),
    )];

    assert_eq!(extract, expected);
}

#[test]
fn full_raw_hello_world_crlf_lf() {
    crate::tracing::setup();

    let kind = Kind::Full;
    let transform = None;
    let span_lf = Location::from(21..74);
    let span_crlf = Location::from(24..80);

    // This test runs on both Windows and other platforms, so it normalizes
    // to \n regardless of the actual example file and then expands that back to \r\n.
    //
    // On non-Windows the first replace will just effectively do nothing.
    let content_lf = include_str!("testdata/c99_tc3/hello_world.c").replace("\r\n", "\n");
    let content_crlf = content_lf.replace('\n', "\r\n").pipe(Content::from);
    let content_lf = Content::from(content_lf);

    let opts = Options::new(Target::Function, kind, Transforms::from(transform));
    let extract_lf = c99_tc3::Extractor::extract(&opts, &content_lf).unwrap();
    let extract_crlf = c99_tc3::Extractor::extract(&opts, &content_crlf).unwrap();

    // Even though the fingerprints themselves are normalized, they'll still be at different byte offsets.
    let expected_lf = vec![Snippet::from(
        Metadata::new(kind, Method::from(transform), span_lf),
        span_lf.extract_from(content_lf.as_bytes()),
    )];
    let expected_crlf = vec![Snippet::from(
        Metadata::new(kind, Method::from(transform), span_crlf),
        span_crlf.extract_from(content_crlf.as_bytes()),
    )];

    assert_eq!(extract_lf.clone(), expected_lf);
    assert_eq!(extract_crlf.clone(), expected_crlf);

    let fingerprints_lf = extract_lf
        .into_iter()
        .map(|snippet| snippet.fingerprint().clone())
        .collect_vec();
    let fingerprints_crlf = extract_crlf
        .into_iter()
        .map(|snippet| snippet.fingerprint().clone())
        .collect_vec();
    assert_eq!(fingerprints_lf, fingerprints_crlf);
}

#[test]
fn full_raw_hello_world_syntax_error() {
    crate::tracing::setup();

    let kind = Kind::Full;
    let transform = None;
    let span = Location::from(21..=68);

    let content = include_str_lf!("testdata/c99_tc3/hello_world_error.c");

    let opts = Options::new(Target::Function, kind, Transforms::from(transform));
    let extract = c99_tc3::Extractor::extract(&opts, &content).expect("must set up parser");

    let expected = vec![Snippet::from(
        Metadata::new(kind, Method::from(transform), span),
        span.extract_from(content.as_bytes()),
    )];

    assert_eq!(extract, expected);
}

#[test]
fn signature_raw_hello_world() {
    crate::tracing::setup();

    let kind = Kind::Signature;
    let transform = None;
    let span = Location::from(21..31);

    let content = include_str_lf!("testdata/c99_tc3/hello_world.c");
    let opts = Options::new(Target::Function, kind, Transforms::from(transform));
    let extract = c99_tc3::Extractor::extract(&opts, &content).expect("must set up parser");

    let expected = vec![Snippet::from(
        Metadata::new(kind, transform.into(), span),
        span.extract_from(content.as_bytes()),
    )];

    assert_eq!(extract, expected);
}

#[test]
fn body_raw_hello_world() {
    crate::tracing::setup();

    let kind = Kind::Body;
    let transform = None;
    let span = Location::from(32..74);

    let content = include_str_lf!("testdata/c99_tc3/hello_world.c");
    let opts = Options::new(Target::Function, kind, Transforms::from(transform));
    let extract = c99_tc3::Extractor::extract(&opts, &content).expect("must set up parser");

    let expected = vec![Snippet::from(
        Metadata::new(kind, transform.into(), span),
        span.extract_from(content.as_bytes()),
    )];

    assert_eq!(extract, expected);
}

#[test]
fn full_space_hello_world() {
    crate::tracing::setup();

    let kind = Kind::Full;
    let transform = Some(Transform::Space);
    let span = Location::from(21..74);

    let content = include_str_lf!("testdata/c99_tc3/hello_world.c");
    let opts = Options::new(Target::Function, kind, transform).disable_raw();
    let extract = c99_tc3::Extractor::extract(&opts, &content).expect("must set up parser");

    let expected = vec![Snippet::from(
        Metadata::new(kind, transform.into(), span),
        br#"int main() { printf("hello world\n"); return 0; }"#,
    )];

    assert_eq!(extract, expected);
}

#[test]
fn full_raw_hello_world_comment() {
    crate::tracing::setup();

    let kind = Kind::Full;
    let transform = None;
    let span = Location::from(84..1336);

    let content = include_str_lf!("testdata/c99_tc3/hello_world_comment.c");
    let opts = Options::new(Target::Function, kind, transform);
    let extract = c99_tc3::Extractor::extract(&opts, &content).expect("must set up parser");

    let expected = vec![Snippet::from(
        Metadata::new(kind, transform.into(), span),
        span.extract_from(content.as_bytes()),
    )];

    assert_eq!(extract, expected);
}

#[test]
fn signature_raw_hello_world_comment() {
    crate::tracing::setup();

    let kind = Kind::Signature;
    let transform = None;
    let span = Location::from(84..224);

    let content = include_str_lf!("testdata/c99_tc3/hello_world_comment.c");
    let opts = Options::new(Target::Function, kind, transform);
    let extract = c99_tc3::Extractor::extract(&opts, &content).expect("must set up parser");

    let expected = vec![Snippet::from(
        Metadata::new(kind, transform.into(), span),
        span.extract_from(content.as_bytes()),
    )];

    assert_eq!(extract, expected);
}

#[test]
fn body_raw_hello_world_comment() {
    crate::tracing::setup();

    let kind = Kind::Body;
    let transform = None;
    let span = Location::from(225..1336);

    let content = include_str_lf!("testdata/c99_tc3/hello_world_comment.c");
    let opts = Options::new(Target::Function, kind, transform);
    let extract = c99_tc3::Extractor::extract(&opts, &content).expect("must set up parser");

    let expected = vec![Snippet::from(
        Metadata::new(kind, transform.into(), span),
        span.extract_from(content.as_bytes()),
    )];

    assert_eq!(extract, expected);
}

#[test]
fn full_comment_hello_world_comment() {
    crate::tracing::setup();

    let kind = Kind::Full;
    let transform = Some(Transform::Comment);
    let span = Location::from(84..1336);

    let content = include_str_lf!("testdata/c99_tc3/hello_world_comment.c");
    let opts = Options::new(Target::Function, kind, transform).disable_raw();
    let extract = c99_tc3::Extractor::extract(&opts, &content).expect("must set up parser");

    let expected_content = r#"int  main  () 
{ 
  

  
  printf("hello world\n"  ); 

 return  0 ;

   }"#;

    let expected = vec![Snippet::from(
        Metadata::new(kind, transform.into(), span),
        expected_content.as_bytes(),
    )];

    assert_eq!(extract, expected);
}

#[test]
fn signature_comment_hello_world_comment() {
    crate::tracing::setup();

    let kind = Kind::Signature;
    let transform = Some(Transform::Comment);
    let span = Location::from(84..224);

    let content = include_str_lf!("testdata/c99_tc3/hello_world_comment.c");
    let opts = Options::new(Target::Function, kind, transform).disable_raw();
    let extract = c99_tc3::Extractor::extract(&opts, &content).expect("must set up parser");

    let expected_content = r#"int  main  () "#;

    let expected = vec![Snippet::from(
        Metadata::new(kind, transform.into(), span),
        expected_content.as_bytes(),
    )];

    assert_eq!(extract, expected);
}

#[test]
fn signature_comment_complicated() {
    crate::tracing::setup();

    let kind = Kind::Signature;
    let transform = Some(Transform::Code);
    let span = Location::from(100..510);

    let content = include_str_lf!("testdata/c99_tc3/signature_comment.c");
    let opts = Options::new(Target::Function, kind, transform).disable_raw();
    let extract = c99_tc3::Extractor::extract(&opts, &content).expect("must set up parser");

    let expected_content = r"static int comp_method_zlib_comp(LIBSSH2_SESSION *session, unsigned char *dest, size_t *dest_len, const unsigned char *src, size_t src_len, void **abstract)";
    let expected = vec![Snippet::from(
        Metadata::new(kind, transform.into(), span),
        expected_content.as_bytes(),
    )];

    assert_eq!(extract, expected);
}

#[test]
fn body_comment_complicated() {
    crate::tracing::setup();

    let kind = Kind::Body;
    let transform = Some(Transform::Code);
    let span = Location::from(511..1145);

    let content = include_str_lf!("testdata/c99_tc3/signature_comment.c");
    let opts = Options::new(Target::Function, kind, transform).disable_raw();
    let extract = c99_tc3::Extractor::extract(&opts, &content).expect("must set up parser");

    let expected_content = r#"{ z_stream *strm = *abstract; int out_maxlen = *dest_len; int status; strm->next_in = (unsigned char *) src; strm->avail_in = src_len; strm->next_out = dest; strm->avail_out = out_maxlen; status = deflate(strm, Z_PARTIAL_FLUSH); if((status == Z_OK) && (strm->avail_out > 0)) { *dest_len = out_maxlen - strm->avail_out; return 0; } _libssh2_debug(session, LIBSSH2_TRACE_TRANS, "unhandled zlib compression error %d, avail_out", status, strm->avail_out); return _libssh2_error(session, LIBSSH2_ERROR_ZLIB, "compression failure"); }"#;
    let expected = vec![Snippet::from(
        Metadata::new(kind, transform.into(), span),
        expected_content.as_bytes(),
    )];

    assert_eq!(extract, expected);
}

#[test]
fn body_comment_hello_world_comment() {
    crate::tracing::setup();

    let kind = Kind::Body;
    let transform = Some(Transform::Comment);
    let span = Location::from(225..1336);

    let content = include_str_lf!("testdata/c99_tc3/hello_world_comment.c");
    let opts = Options::new(Target::Function, kind, transform).disable_raw();
    let extract = c99_tc3::Extractor::extract(&opts, &content).expect("must set up parser");

    let expected_content = r#"{ 
  

  
  printf("hello world\n"  ); 

 return  0 ;

   }"#;

    let expected = vec![Snippet::from(
        Metadata::new(kind, transform.into(), span),
        expected_content.as_bytes(),
    )];

    assert_eq!(extract, expected);
}

#[test]
fn full_code_hello_world_comment() {
    crate::tracing::setup();

    let kind = Kind::Full;
    let transform = Some(Transform::Code);
    let span = Location::from(84..1336);

    let content = include_str_lf!("testdata/c99_tc3/hello_world_comment.c");
    let opts = Options::new(Target::Function, kind, transform).disable_raw();
    let extract = c99_tc3::Extractor::extract(&opts, &content).expect("must set up parser");

    let expected_content = r#"int main () { printf("hello world\n" ); return 0 ; }"#;

    let expected = vec![Snippet::from(
        Metadata::new(kind, transform.into(), span),
        expected_content.as_bytes(),
    )];

    assert_eq!(extract, expected);
}

#[test]
fn signature_code_hello_world_comment() {
    crate::tracing::setup();

    let kind = Kind::Signature;
    let transform = Some(Transform::Code);
    let span = Location::from(84..224);

    let content = include_str_lf!("testdata/c99_tc3/hello_world_comment.c");
    let opts = Options::new(Target::Function, kind, transform).disable_raw();
    let extract = c99_tc3::Extractor::extract(&opts, &content).expect("must set up parser");

    let expected_content = r#"int main () "#;

    let expected = vec![Snippet::from(
        Metadata::new(kind, transform.into(), span),
        expected_content.as_bytes(),
    )];

    assert_eq!(extract, expected);
}

#[test]
fn body_code_hello_world_comment() {
    crate::tracing::setup();

    let kind = Kind::Body;
    let transform = Some(Transform::Code);
    let span = Location::from(225..1336);

    let content = include_str_lf!("testdata/c99_tc3/hello_world_comment.c");
    let opts = Options::new(Target::Function, kind, transform).disable_raw();
    let extract = c99_tc3::Extractor::extract(&opts, &content).expect("must set up parser");

    let expected_content = r#"{ printf("hello world\n" ); return 0 ; }"#;

    let expected = vec![Snippet::from(
        Metadata::new(kind, transform.into(), span),
        expected_content.as_bytes(),
    )];

    assert_eq!(extract, expected);
}

#[test]
fn smoke_test() {
    crate::tracing::setup();

    let target = Targets::default();
    let kind = Kinds::default();
    let transform = Transforms::default();

    let content = include_str_lf!("testdata/c99_tc3/smoke_comp.c");
    let opts = Options::new(target, kind, transform).disable_raw();
    let extract = c99_tc3::Extractor::extract(&opts, &content).expect("extract snippets");
    assert!(!extract.is_empty(), "must have extracted snippets");
}
