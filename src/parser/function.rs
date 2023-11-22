//! Extracts function parts out of a set of nodes.

use std::cmp;

use getset::{CopyGetters, Getters};
use tree_sitter::Node;
use tree_sitter_traversal::{traverse, Order};

use crate::{
    content::{ByteCoordinate, Content},
    debugging::inspect_node,
};

/// Extracts parts of a function.
#[derive(Debug, Clone, Getters, CopyGetters)]
pub struct Function<'a> {
    /// The byte at which the overall function starts.
    #[get_copy = "pub"]
    byte_start: usize,

    /// The byte at which the overall function ends.
    #[get_copy = "pub"]
    byte_end: usize,

    /// The parts that make up the function.
    /// Which parts are reported depends on the method used to create this instance.
    #[get = "pub"]
    parts: Vec<Part<'a>>,
}

impl<'a> Function<'a> {
    /// Break a function into the nodes in the signature and body using the specified predicate to categorize a single body node.
    ///
    /// The first node for which the predicate returns `true` is considered the body,
    /// and everything that came before it is considered the signature.
    #[tracing::instrument(skip_all)]
    pub fn distinct_body(
        root: Node<'a>,
        content: &Content,
        is_body: impl Fn(Node<'_>) -> bool,
    ) -> Self {
        let nodes = traverse(root.walk(), Order::Pre)
            .inspect(|node| inspect_node(node, content.as_bytes()));

        let mut signature = Section::default();
        let mut body = None;
        for node in nodes {
            if is_body(node) {
                body = Some(Section::single(node));
                break;
            }

            // The root node is emitted when traversing the tree,
            // don't include it in the actual output.
            if node != root {
                signature.push(node);
            }
        }

        let mut parts = Vec::new();
        if !signature.is_empty() {
            parts.push(Part::Signature(signature));
        }
        if let Some(body) = body {
            parts.push(Part::Body(body));
        }

        Self {
            byte_start: root.start_byte(),
            byte_end: root.end_byte(),
            parts,
        }
    }
}

impl<'a> ByteCoordinate for Function<'a> {
    fn byte_start(&self) -> usize {
        self.byte_start
    }

    fn byte_end(&self) -> usize {
        self.byte_end
    }
}

/// A function is made up of multiple parts. These are described here.
#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum Part<'a> {
    /// The signature section of the function.
    /// Includes the function name, arguments, and any comments before the body.
    Signature(Section<'a>),

    /// The body section of the function.
    Body(Section<'a>),
}

impl<'a> ByteCoordinate for Part<'a> {
    fn byte_start(&self) -> usize {
        match self {
            Part::Signature(section) => section.byte_start,
            Part::Body(section) => section.byte_start,
        }
    }

    fn byte_end(&self) -> usize {
        match self {
            Part::Signature(section) => section.byte_end,
            Part::Body(section) => section.byte_end,
        }
    }
}

#[derive(Debug, Clone, Default, Getters, CopyGetters)]
pub struct Section<'a> {
    /// The byte at which the section starts.
    #[get_copy = "pub"]
    byte_start: usize,

    /// The byte at which the section ends.
    #[get_copy = "pub"]
    byte_end: usize,

    /// The nodes that make up the section.
    #[get = "pub"]
    nodes: Vec<Node<'a>>,
}

impl<'a> Section<'a> {
    fn single(node: Node<'a>) -> Self {
        Self {
            byte_start: node.start_byte(),
            byte_end: node.end_byte(),
            nodes: vec![node],
        }
    }

    fn push(&mut self, node: Node<'a>) {
        if self.nodes.is_empty() {
            self.byte_start = node.start_byte();
            self.byte_end = node.end_byte();
        } else {
            self.byte_start = cmp::min(node.start_byte(), self.byte_start);
            self.byte_end = cmp::max(node.end_byte(), self.byte_end);
        }
        self.nodes.push(node);
    }

    fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }
}

impl<'a> ByteCoordinate for Section<'a> {
    fn byte_start(&self) -> usize {
        self.byte_start
    }

    fn byte_end(&self) -> usize {
        self.byte_end
    }
}

/// Returns a function that can be used as a predicate to test whether a node kind equals the provided kind.
pub fn node_kind(kind: &str) -> impl Fn(Node<'_>) -> bool + '_ {
    move |node: Node<'_>| node.kind() == kind
}

/// Integration tests are also performed in the `tests/it` module,
/// so these tests don't have to be extremely broad.
#[cfg(test)]
mod tests {
    use indoc::indoc;
    use pretty_assertions::assert_eq;
    use tap::Pipe;
    use tree_sitter_traversal::traverse_tree;

    use crate::{language::java_11, parser::NODE_KIND_METHOD_DECL};

    use super::*;

    #[test]
    fn delimited_java() {
        let _ = tracing_subscriber::fmt::try_init();

        let content = indoc! {r#"
        class HelloWorld {
            public static void main(String[] args) {
                System.out.println("Hello, World!");
            }
        }
        "#}
        .pipe(Content::from);

        let mut parser = java_11::parser().expect("init parser");
        let tree = parser
            .parse(content.as_bytes(), None)
            .expect("parse content");

        let function = traverse_tree(&tree, Order::Pre)
            .inspect(|node| inspect_node(node, content.as_bytes()))
            .find(|node| node.kind() == NODE_KIND_METHOD_DECL)
            .expect("find method declaration");
        assert_eq!(function.byte_range(), (23..114), "snippet location");

        let function = Function::distinct_body(function, &content, node_kind("block"));
        let signature = function
            .parts
            .iter()
            .find(|part| matches!(part, Part::Signature(_)))
            .expect("find signature part");
        let body = function
            .parts
            .iter()
            .find(|part| matches!(part, Part::Body(_)))
            .expect("find signature part");

        assert_eq!(
            signature.extract_from_lossy(&content),
            "public static void main(String[] args)",
            "signature",
        );
        assert_eq!(
            body.extract_from_lossy(&content),
            "{\n        System.out.println(\"Hello, World!\");\n    }",
            "body"
        );
    }
}
