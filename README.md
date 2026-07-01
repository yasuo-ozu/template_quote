<div align="center">
  <img src="https://raw.githubusercontent.com/yasuo-ozu/template_quote/main/logo.png" alt="template-quote logo" width="200">
  <p><em><code>quote!</code> with control flow.</em></p>
</div>

# template-quote

[![crates.io](https://img.shields.io/crates/v/template-quote.svg)](https://crates.io/crates/template-quote)
[![docs.rs](https://docs.rs/template-quote/badge.svg)](https://docs.rs/template-quote)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![CI](https://github.com/yasuo-ozu/template_quote/actions/workflows/ci.yml/badge.svg)](https://github.com/yasuo-ozu/template_quote/actions/workflows/ci.yml)

A drop-in [`quote!`](https://docs.rs/quote/) replacement that adds
**template-engine-like control flow** — write `if`, `for`, `while` and `let`
directly inside the macro instead of assembling `TokenStream`s by hand.

```rust
use template_quote::quote;
use proc_macro2::{Ident, Span};

let fields: Vec<Ident> = ["x", "y", "z"]
    .iter()
    .map(|n| Ident::new(n, Span::call_site()))
    .collect();
let derive_debug = true;

let tokens = quote! {
    #(if derive_debug) { #[derive(Debug)] }
    struct Point {
        #(for f in &fields) { #f: i32, }
    }
};
// #[derive(Debug)] struct Point { x: i32, y: i32, z: i32, }
# assert!(tokens.to_string().contains("struct Point"));
```

## Why template-quote?

- **Backward-compatible with `quote!`** — every existing `quote!` template works
  unchanged, so adopting it is zero-cost.
- **Inline control flow** — `#(if …)`, `#(else)`, `#(for x in …)`, `#(while …)`
  and `#(let … = …)` branch and loop *inside* the quotation.
- **Nested repetition** that classic `quote!` rejects, e.g. `#(#(#a #b)*)*`.
- **Inline expressions & statements** — `#{ expr }` splices a computed value;
  `#{ stmt; }` runs code mid-expansion.
- **`quote_spanned!` / `quote_configured!`** for span control and custom crate
  paths.

## Install

```sh
cargo add template-quote proc-macro2
```

The generated code refers to [`proc_macro2`](https://docs.rs/proc-macro2/), so
add it alongside `template-quote`. MSRV is **Rust 1.56**.

## How it compares

- vs [`quote`](https://docs.rs/quote/): a superset — every `quote!` template
  works unchanged, plus inline `if` / `for` / `while` / `let` and nested
  repetition.
- vs [`proc-quote`](https://crates.io/crates/proc-quote): the inspiration for
  this crate; `template-quote` keeps full `quote!` syntax compatibility.

# Interpolation

The original `quote!` macro syntax is fully supported. See [quote docs](https://docs.rs/quote/).

For backward compatibility, interpolation rules are the same as in the traditional `quote!` macro. Interpolation uses `#var` (similar to `$var` in `macro_rules!`). Most values from `syn` are interpolated via the [`ToTokens`] trait.

## Rules

Repetition uses syntax like `#(...)*` or `#(...),*`, repeating over the variables (`#var`) inside the pattern. A repeated `#var` may be any [`Iterator`], a slice / array / `Vec` (iterated by reference, not consumed), or a single [`ToTokens`] value (repeated unchanged on every iteration). At least one `#var` in the group must be a real iterator to bound the repetition.

- `#(...)*` repeats `...` with no separator. At least one variable must appear in `...`.
- `#(...),*` does the same, but inserts `,` as a separator.

## Problem

Classic interpolation is limited, so this crate introduces new template syntax. For example, the following code is not allowed because `#var1` cannot be nested in this way:

```
# use template_quote::quote;
let var1 = vec!['a', 'b'];
let var2 = vec![vec![1, 2], vec![3, 4]];
let tokens = quote!{
	#(#(#var1 #var2)*)*
};
assert_eq!("'a' 1i32 'a' 2i32 'b' 3i32 'b' 4i32", tokens.to_string());
```

# Template syntax

Template syntax is procedural-like and lets you use structured statements inside the macro.

## If syntax

This code iterates over `#i` (via interpolation) and emits `i32` into the `TokenStream` when the value meets the condition.

```
# use template_quote::quote;
let i = vec![1, 2, 3];
let tokens = quote!{
	#(
		#(if i > &2) {
			#i
		}
	)*
};
assert_eq!("3i32", tokens.to_string());
```

`if-else` and `if-else-if` are also supported.

```
# use template_quote::quote;
let i = vec![1, 2, 3];
let tokens = quote!{
	#(
		#(if i > &2) {
			+ #i
		}
		#(else) {
			- #i
		}
	)*
};
assert_eq!("- 1i32 - 2i32 + 3i32", tokens.to_string());
```

```
# use template_quote::quote;
let i = vec![1, 2, 3, 4, 5];
let tokens = quote!{
	#(
		#(if i % &2 == 0) {
			+ #i
		}
		#(else if i % &3 == 0) {
			- #i
		}
		#(else) {
			#i
		}
	)*
};
assert_eq!("1i32 + 2i32 - 3i32 + 4i32 5i32", tokens.to_string());
```

## For syntax

`for` syntax iterates over variables (similar to interpolation), but lets you explicitly choose which variable to iterate.

```
# use template_quote::quote;
let v1 = vec![1, 2];
let v2 = vec!['a', 'b'];
let tokens = quote!{
	#(for i1 in &v1) {
		#(for i2 in &v2) {
			#i1 -> #i2
		}
	}
};
assert_eq!("1i32 -> 'a' 1i32 -> 'b' 2i32 -> 'a' 2i32 -> 'b'", tokens.to_string());
```

The inner loop can be replaced with interpolation:

```
# use template_quote::quote;
let v1 = vec![1, 2];
let v2 = vec!['a', 'b'];
let tokens = quote!{
	#(for i1 in &v1) {
		#(
			#i1 -> #v2
		)*
	}
};
assert_eq!("1i32 -> 'a' 1i32 -> 'b' 2i32 -> 'a' 2i32 -> 'b'", tokens.to_string());
```

You can also specify a separator with a `for` statement.

```
# use template_quote::quote;
let v = vec![1, 2];
let tokens = quote!{
	#(for i in v) | { #i }
};
assert_eq!("1i32 | 2i32", tokens.to_string());
```

Interpolation cannot use variables bound by `for` syntax directly. For example:

```compile_fail
# use template_quote::quote;
let v = vec![vec![1, 2], vec![3]];
let tokens = quote!{
	#(
		#(for i in v) { #i }
	),*
};
assert_eq!("1i32 2i32 , 3i32", tokens.to_string());
```

This fails because no interpolation variable is available:

```text
error: Repetition group `#( # ( .. ) { .. } )*` contains no interpolation variable (`#var`)
 --> src/lib.rs
  |
  |         #(for i in v) { #i }
  |         ^^^^^^^^^^^^^^^^^^^^
  |
  = note: a repetition needs at least one `#var` inside it to drive the iteration
```

In this case, use `#(for i in #v)` to specify which variable to iterate via interpolation:

```
# use template_quote::quote;
let v = vec![vec![1, 2], vec![3]];
let tokens = quote!{
	#(
		#(for i in #v) { #i }
	),*
};
assert_eq!("1i32 2i32 , 3i32", tokens.to_string());
```

## While syntax

```
# use template_quote::quote;
let mut v = vec![1, 2].into_iter();
let tokens = quote!{
	#(while v.next().is_some()) { hello }
};
assert_eq!("hello hello", tokens.to_string());
```

## While-let syntax

```
# use template_quote::quote;
let mut v = vec![1, 2].into_iter();
let tokens = quote!{
	#(while let Some(i) = v.next()) { #i }
};
assert_eq!("1i32 2i32", tokens.to_string());
```

As with `for` syntax, variables bound in `while` are not iterable through interpolation. For example:

```compile_fail
# use template_quote::quote;
let mut v = vec![1, 2].into_iter();
quote!{
	#(
		#(while let Some(i) = v.next()) { #i }
	)*
};
```

This fails.

## Let syntax

`let` syntax binds new variables that can be used inside the block.

```
# use template_quote::quote;
let v = vec![(1, 'a'), (2, 'b')];
let tokens = quote!{
	#(for i in v), {
		#(let (n, c) = i) {
			#n -> #c
		}
	}
};
assert_eq!("1i32 -> 'a' , 2i32 -> 'b'", tokens.to_string());
```

Here, `#n` and `#c` are not iterable via interpolation.

## Inline expression

You can place inline expressions in `quote!`.

```
# use template_quote::quote;
let v = vec![1, 2];
let tokens = quote!{
	#(for i in v){
		#i -> #{ i.to_string() }
	}
};
assert_eq!("1i32 -> \"1\" 2i32 -> \"2\"", tokens.to_string());
```

The following example fails because the macro cannot determine which variable should be iterated:

```compile_fail
# use template_quote::quote;
let v = vec![1, 2];
let tokens = quote!{
	#(
		#{ v.to_string() }
	)*
};
assert_eq!("\"1\" \"2\"", tokens.to_string());
```

In this case, use `#i` inside the inline expression to specify the interpolation variable:

```
# use template_quote::quote;
let v = vec![1, 2];
let tokens = quote!{
	#(
		#{ #v.to_string() }
	)*
};
assert_eq!("\"1\" \"2\"", tokens.to_string());
```

## Inline statement

You can place arbitrary statements inside the macro. For example:

```
# use template_quote::quote;
let v = vec![1, 2, 3];
let tokens = quote!{
	#(
		#v
		#{ eprintln!("debug: {}", &v); }
	)*
};
assert_eq!("1i32 2i32 3i32", tokens.to_string());
```

This prints:

```text
debug: 1
debug: 2
debug: 3
```

To avoid ambiguity, all inline statements must end with `;`. For example, an `if` statement in inline-statement syntax needs an extra `;`:

```
# use template_quote::quote;
let v = vec![1, 2, 3];
quote!{
	#(
		#v
		#{ if v >= &2 { eprintln!("debug: {}", &v); } ; }
	)*
};
```

## Break, Continue

You can use control-flow statements like `break` and `continue` in inline statements, but this can be risky.

If you use `break;` inside a group (like `{ ... }` or `( ... )`), it aborts emission of the whole group, and nothing is emitted for that group. For example, the following code emits only one token:

```
# use template_quote::quote;
let v = vec![1, 2, 3];
let tokens = quote!{
	#(for i in v) {
		#i // emitted once
		// This block is not emitted
		{
			#i
			#{ break; }
		}
	}
};
assert_eq!("1i32", tokens.to_string());
```

`break` also affects interpolation syntax:

```
# use template_quote::quote;
let v = vec![1, 2, 3];
let tokens = quote!{
	#(
		#v
		#{ break; }
	),*
};
assert_eq!("1i32", tokens.to_string());
```

`break` can even escape outside the `quote!` macro. In this example, the internal `break` affects the outer `for` loop:

```
# use template_quote::quote;
let mut v = Vec::new();
for _ in 0..3 {
	let tokens = quote!{
		#{ break; }
	};
	v.push(tokens);
}
assert_eq!(v.len(), 0);
```

# Spanned and configured output

Besides [`quote!`], two sibling macros are provided:

- [`quote_spanned!`] works like `quote!` but applies an explicit span to every
  emitted token: `quote_spanned! { my_span => /* ... */ }`.
- [`quote_configured!`] overrides the crate paths and default span that the
  generated code refers to (useful when `proc_macro2` or this crate are
  re-exported under non-standard paths). A leading `{ key: value, ... } =>`
  block configures it; see its documentation for the accepted keys.

# Using this crate

This crate is useful for proc-macro development. A typical proc-macro crate using `template_quote` has the following `Cargo.toml`:

```Cargo.toml
[package]
name = "your_crate_name"
version = "0.0.0"
edition = "2021"

[lib]
proc-macro = true

[dependencies]
template-quote = "0.5"
proc-macro2 = "1.0"
```

And the following `src/lib.rs`:

```lib.rs
extern crate proc_macro;
extern crate proc_macro2;
extern crate template_quote;

use template_quote::quote;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;

#[proc_macro]
pub fn my_macro(_: TokenStream) -> TokenStream {
	quote! { /* something here */ }.into()
}
```

Then you can use it like this:

```ignore
extern crate your_crate_name;
use your_crate_name::my_macro;

my_macro!()
```
