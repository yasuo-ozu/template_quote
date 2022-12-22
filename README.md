# Rust Quasi-Quoting macro with pretty template-engine like syntax

This crate provides quasi-quoting macros like [quote](https://github.com/dtolnay/quote).
This crate has backward-compatibility with original `quote!` macro and also provides
new template-engine like syntax.

This crate is get some inspiration from [proc-quote](https://crates.io/crates/proc-quote).

## Using this crate

This crate is useful for developing proc-macro. Usually an proc-macro crate using template_quote is placed with following `Cargo.toml`:

```Cargo.toml
[package]
name = "your_crate_name"
version = "0.0.0"
edition = "2021"

[lib]
proc-macro = true

[dependencies]
template-quote = "0.2"
proc-macro2 = "1.0"
```

and with following `src/lib.rs` code:

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

then you will be able to use it like:

```rust
extern crate your_crate_name;
use your_crate_name::my_macro;

my_macro!()
```

## Original syntax

Original `quote!` macro syntax is fully supported. See [quote's doc](https://docs.rs/quote/1.0.23/quote/).

## Template-engine like syntax

### Conditional blocks

template_quote supports `if`, `else`,  `else if`, `for .. in ..`, `while ..`, `while let ...`, `loop` syntax like following:

```rust
let tokens = quote! {
	#(
		#(if v >= &10){
			#v
		}
		#(else){
			-#v
		}
	)*
};
assert_eq!("-1i32 10i32 -2i32 13i32 -4i32 19i32", tokens.to_string());
```

You can also set an separater for `for`, `while`, `loop`, placing a separater between `#(..)` and `{..}`.

### Inline statements / expressions

You can use inline expression by syntax `#{ ... }`.

```rust
let v = vec![1, 2, 3];
let tokens = quote! {
	#(
		#v => #{ format!("{}", v).as_str() }
	)*
};
assert_eq!(
	"1i32 => \"1\" 2i32 => \"2\" 3i32 => \"3\"",
	tokens.to_string()
);
```

## Limitation

- Let binding in `#{ ... }` within traditional repetition syntax `#( .. )*` does not work.
- If the punct token before '#' in the macro body has `Spacing::Join`, then the emitting punct also has same spacing, whether the '#' token is processed by the macro or not.
