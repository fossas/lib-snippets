Provides a framework and implementations for extracting snippets of programming languages from files.

# Aspirations

- Extensible over pedantic features
- Platform independent over platform optimized
- Reliable over performant

# Feature flags

The main library, which enables consumers to plug their own implementations, is available by default.
Features are most commonly used to enable support for languages,
but other kinds of flags exist; see the table below for details.

Name           | Description                                          | Kind
---------------|------------------------------------------------------|------------
`lang-all`     | Enables all features that are of the kind "Language" | Language
`lang-c99-tc3` | Enables support for C99 TC3                          | Language
`lang-cpp`     | Enables support for C++ 98.                          | Language
`sha2-asm`     | Enables hardware acceleration for SHA2               | Performance

# History

This repository initially existed in FOSSA's [foundation-libs](https://github.com/fossas/foundation-libs/tree/master/snippets) monorepo.
History for this library earlier than v0.1.3 can be viewed there.
