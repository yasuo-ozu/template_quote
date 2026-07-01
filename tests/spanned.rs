//! Coverage for `quote_spanned!` — previously untested (entry point, the
//! bespoke `=>` scanner, interpolation, repetition, and span expressions).
extern crate proc_macro2;
extern crate template_quote;

use proc_macro2::{Ident, Span};
use template_quote::quote_spanned;

#[test]
fn spanned_basic() {
	let sp = Span::call_site();
	let tokens = quote_spanned! { sp => struct Foo; };
	assert_eq!(tokens.to_string(), "struct Foo ;");
}

#[test]
fn spanned_interpolation() {
	let sp = Span::call_site();
	let name = Ident::new("Bar", Span::call_site());
	let tokens = quote_spanned! { sp => struct #name ; };
	assert_eq!(tokens.to_string(), "struct Bar ;");
}

#[test]
fn spanned_repetition() {
	let sp = Span::call_site();
	let items = [1i32, 2, 3];
	let tokens = quote_spanned! { sp => #(#items),* };
	assert_eq!(tokens.to_string(), "1i32 , 2i32 , 3i32");
}

#[test]
fn spanned_scanner_handles_method_call_span_expr() {
	// The span expression is itself a non-trivial token sequence (`id.span()`
	// with `.` and `()`), exercising the `=>` scanner before the body.
	let id = Ident::new("z", Span::call_site());
	let tokens = quote_spanned! { id.span() => #id + 1 };
	assert_eq!(tokens.to_string(), "z + 1");
}

#[test]
fn spanned_matches_quote_output() {
	// The span does not affect the textual output, only the tokens' spans.
	let x = 5i32;
	let sp = Span::mixed_site();
	assert_eq!(
		quote_spanned! { sp => let y = #x ; }.to_string(),
		"let y = 5i32 ;"
	);
}
