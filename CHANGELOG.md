# Changelog

All notable changes to this project are documented here. This project adheres
to [Semantic Versioning](https://semver.org).

## [0.5.0] — unreleased

Fixes a batch of correctness, diagnostic, and hygiene bugs found in an audit.
Contains **breaking changes** — see *Migrating from 0.4* below.

### Breaking

- Removed the public `unsafe trait Repeat`. Repetition now uses internal
  autoref-specialization. The `Borrow<[T]>` blanket impl is replaced by concrete
  impls for `Vec<T>`, `[T]`, `[T; N]`, `&T`, `&mut T` and `BTreeSet<T>` — a type
  that was iterated *only* via a hand-written `Borrow<[T]>` impl must now be
  passed as a slice/`Vec` or via `.iter()`.
- Interpolation-boundary spacing now matches `quote!`: a punctuation token
  immediately before an interpolation is emitted with `Spacing::Alone`. This
  prevents silent operator fusion (e.g. `-` + `>` becoming `->`) but changes
  some output strings to be spaced (e.g. `-#v` now yields `- 1i32`).

### Fixed

- Empty inline group `#{}` expands to nothing (previously a `usize` underflow: a
  debug-mode panic, or a `(): ToTokens` error in release).
- `#var` nested inside a group within a `#(if/for/while/let …)` header or an
  inline `#{ … }` block is now interpolated.
- The `let`-pattern parser accepts `mut`/`ref` bindings, leading `::` paths,
  non-trailing rest patterns (`[a, .., b]`), and struct-field sub-patterns
  (`S { a: (x, y) }`).
- A type implementing both `Borrow<[T]>` and `ToTokens` no longer fails with an
  `E0283` ambiguity in `#(#x)*` (it resolves to the `ToTokens` behavior).
- Malformed templates and `quote_configured!` / `quote_spanned!` inputs emit
  spanned diagnostics via `abort!` instead of raw `panic!`s; a repetition with
  no interpolation variable reports a clear, pointed message.
- Emitted internal identifiers use `Span::mixed_site()` and no longer collide
  with same-named user variables.
- The repetition guard honors the configurable `core` path in
  `quote_configured!`.

### Docs & internal

- New README (logo, quick start, feature overview); corrected examples and
  intra-doc links; documented `quote_spanned!` / `quote_configured!`.
- Added trybuild UI tests plus hygiene/pattern/spanned/configured regression
  tests; the crate is clippy- and rustfmt-clean, with CI.

### Migrating from 0.4

- If you implemented `template_quote::Repeat` manually, remove it — the trait no
  longer exists. Pass an iterator, a slice/`Vec`/array, or a `ToTokens` value.
- If you relied on a custom `Borrow<[T]>` type auto-iterating in `#(#x)*`, pass
  `x.iter()` or a slice instead.
- If you compare generated output as strings, note that spacing around
  interpolated punctuation now matches `quote!`.

## 0.4.4 and earlier

See the git history.
