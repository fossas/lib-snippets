# v0.2.0

Adds support for Java.

Breaking:
- Normalizations have been centralized to `snippets::parser::normalize`.
- `snippets::text::buffer` has been merged into `snippets::text`.
- Some vestigal traits (such as `snippets::text::ConvertCRLFToLF`) have been removed.
- Implementation-specific constants such as `NODE_KIND_COMMENT` have been made private.
- Removed `tree-sitter` types from the API.

# v0.1.3

Adds support for C++.
Adds support for C.

This repository initially existed in FOSSA's [foundation-libs](https://github.com/fossas/foundation-libs/tree/master/snippets) monorepo.
History for this library earlier than v0.1.3 can be viewed there.
