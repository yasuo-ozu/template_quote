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
