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

# Compatibility

FOSSA generally targets the latest releases of Rust, so there is no MSRV policy.
Releases will generally use the latest Rust features.

If this becomes a problem, open an issue and we can talk it out!

# Developing

## Release process

- [Check semver compatibility](https://crates.io/crates/cargo-semver-checks) to choose the version to be released.
  - For example, on the feature branch you can run `cargo semver-checks --baseline-rev main`.
- Run `cargo doc --open` to validate the public API appears as you expect.
  - Note that this will show docs for dependency crates as well; just look at the `snippets` crate.
- Update changelog and merge the branch to `main`.
- Release the version by tagging it. [`cargo-release`](https://crates.io/crates/cargo-release) is recommended.
