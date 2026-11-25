//! Coverage for `quote_configured!` — the config parser (`ParseEnvironment`)
//! and the configured paths flowing into the generated code. (The failure
//! path is covered by tests/ui/bad_config.rs.)
extern crate proc_macro2;
extern crate template_quote;

use template_quote::quote_configured;

#[test]
fn configured_defaults_equivalent_to_quote() {
	let x = 5i32;
	let tokens = quote_configured! {
		{
			proc_macro2: ::proc_macro2,
			quote: ::template_quote,
			core: ::core,
		} =>
		let y = #x ;
	};
	assert_eq!(tokens.to_string(), "let y = 5i32 ;");
}

#[test]
fn configured_repetition_uses_configured_quote_path() {
	// Repetition routes through `#quote::__private::ext`, so this exercises the
	// configured `quote:` path end to end.
	let items = [1i32, 2, 3];
	let tokens = quote_configured! {
		{ proc_macro2: ::proc_macro2, quote: ::template_quote, core: ::core } =>
		#(#items),*
	};
	assert_eq!(tokens.to_string(), "1i32 , 2i32 , 3i32");
}

#[test]
fn configured_span_expression() {
	let sp = proc_macro2::Span::call_site();
	let tokens = quote_configured! { { span: sp } => struct Foo; };
	assert_eq!(tokens.to_string(), "struct Foo ;");
}

fn pick<T, U>(t: T, _u: U) -> T {
	t
}

#[test]
fn configured_span_expression_with_top_level_commas() {
	// The commas inside the turbofish must not end the `span` value; the one
	// before the next `key:` does.
	let sp = proc_macro2::Span::call_site();
	let tokens = quote_configured! {
		{ span: pick::<proc_macro2::Span, u8>(sp, 0), core: ::core } =>
		let y = 1 ;
	};
	assert_eq!(tokens.to_string(), "let y = 1 ;");
}

mod reexport {
	pub mod proc_macro2 {
		pub use ::proc_macro2::*;
	}
	pub mod template_quote {
		pub use ::template_quote::*;
	}
}

macro_rules! quote_via_crate_paths {
	($($tt:tt)*) => {
		quote_configured! {
			{
				proc_macro2: $crate::reexport::proc_macro2,
				quote: $crate::reexport::template_quote,
			} =>
			$($tt)*
		}
	};
}

macro_rules! quote_via_path_fragments {
	($pm2:path, $q:path; $($tt:tt)*) => {
		quote_configured! { { proc_macro2: $pm2, quote: $q } => $($tt)* }
	};
}

#[test]
fn configured_dollar_crate_paths() {
	let items = [1i32, 2];
	let tokens = quote_via_crate_paths! { #(#items),* };
	assert_eq!(tokens.to_string(), "1i32 , 2i32");
}

#[test]
fn configured_path_fragments() {
	// `$x:path` fragments reach the macro wrapped in invisible groups.
	let items = [1i32, 2];
	let tokens = quote_via_path_fragments! {
		::proc_macro2, crate::reexport::template_quote; #(#items)*
	};
	assert_eq!(tokens.to_string(), "1i32 2i32");
}
