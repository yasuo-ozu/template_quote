//! Regression tests for the Tier-1 audit findings (see AUDIT.md).
use template_quote::quote;

// ---- Tier-1 #3: empty inline group `#{}` must expand to nothing ----
// Previously panicked in debug (usize underflow at `inner.len() - 1`) and
// emitted `&{}` (== `()`, `(): ToTokens` error) in release.

#[test]
fn empty_inline_group_expands_to_nothing() {
	let tokens = quote! { #{} };
	assert_eq!(tokens.to_string(), "");
}

#[test]
fn empty_inline_group_between_tokens() {
	let tokens = quote! { a #{} b };
	assert_eq!(tokens.to_string(), "a b");
}

#[test]
fn nonempty_inline_group_still_works() {
	let x = 5usize;
	let tokens = quote! { #{ x + 1 } };
	assert_eq!(tokens.to_string(), "6usize");
}

// ---- Tier-1 #1: an interpolation boundary must not fuse the preceding punct
// with the interpolation's leading punct into a different operator. ----

#[test]
fn boundary_minus_does_not_form_arrow() {
	let x = quote!(> z); // interpolation starting with `>`
	let s = quote!( a -#x ).to_string();
	assert!(!s.contains("->"), "`-` fused with `>` into `->`: {s}");
}

#[test]
fn boundary_colon_does_not_form_triple_colon() {
	let p = quote!(::std::vec::Vec); // interpolation starting with `:`
	let s = quote!( let x:#p = y; ).to_string();
	assert!(!s.contains(":::"), "`:` fused into `:::`: {s}");
}

#[test]
fn boundary_plus_does_not_form_double_plus() {
	let x = quote!(+ y); // interpolation starting with `+`
	let s = quote!( a +#x ).to_string();
	assert!(!s.contains("++"), "`+` fused into `++`: {s}");
}

#[test]
fn boundary_minus_with_number_still_negates() {
	let v = 1i32;
	// Matches the reference `quote` crate: spaced, no fusion.
	assert_eq!(quote!( -#v ).to_string(), "- 1i32");
}

// ---- Tier-1 #2: `#var` nested inside a group within a conditional/iteration
// header or an inline `#{...}` block must be interpolated (not left literal).
// ----

#[test]
fn interp_inside_group_in_for_header() {
	let v = [1, 2, 3];
	let tokens = quote! {
		#(for i in (#v).iter()) { #i }
	};
	assert_eq!(tokens.to_string(), "1i32 2i32 3i32");
}

#[test]
fn interp_inside_group_in_if_condition() {
	let v = [1, 2, 3];
	let tokens = quote! {
		#(if (#v).len() == 3) { yes }
	};
	assert_eq!(tokens.to_string(), "yes");
}

#[test]
fn interp_inside_group_in_inline_block() {
	let v = [10, 20];
	let tokens = quote! {
		#(for _i in 0..2) { #{ (#v).len() } }
	};
	assert_eq!(tokens.to_string(), "2usize 2usize");
}

// ---- Tier-1 #5: the let-pattern parser must accept valid Rust patterns
// inside `#(let ...)`, `#(if let ...)` and `#(while let ...)` headers. ----

#[test]
#[allow(unused_mut)] // the generated `let (mut a, ..)` binding is only read
fn pattern_mut_ref_modifiers() {
	let tokens = quote! {
		#(let (mut a, ref b) = (1i32, 2i32)) { #a #b }
	};
	assert_eq!(tokens.to_string(), "1i32 2i32");
}

#[test]
fn pattern_leading_absolute_path() {
	let tokens = quote! {
		#(if let ::core::option::Option::Some(x) = Some(7i32)) { #x }
	};
	assert_eq!(tokens.to_string(), "7i32");
}

#[test]
fn pattern_nontrailing_rest_slice() {
	let arr = [1i32, 2, 3, 4];
	let tokens = quote! {
		#(let [a, .., b] = arr) { #a #b }
	};
	assert_eq!(tokens.to_string(), "1i32 4i32");
}

#[test]
fn pattern_leading_rest_tuple() {
	let tokens = quote! {
		#(let (.., last) = (1i32, 2i32, 3i32)) { #last }
	};
	assert_eq!(tokens.to_string(), "3i32");
}

#[test]
fn pattern_struct_field_subpattern() {
	struct Pair {
		a: (i32, i32),
		b: i32,
	}
	let pair = Pair {
		a: (1i32, 2i32),
		b: 3i32,
	};
	let tokens = quote! {
		#(let Pair { a: (x, y), b } = pair) { #x #y #b }
	};
	assert_eq!(tokens.to_string(), "1i32 2i32 3i32");
}

// ---- Tier-1 #6: a type implementing BOTH `Borrow<[U]>` and `ToTokens` must
// not cause an E0283 ambiguity in `#(#x)*`. Previously impls (b) and (c) both
// matched `&Both`, failing to compile and leaking an internal type. The
// autoref-specialization redesign resolves it deterministically to the
// ToTokens tier (the `Borrow<[T]>` blanket was replaced by concrete std-type
// slice impls, so a foreign `Both` type is never in the slice tier). ----

#[test]
fn repeat_type_impl_both_borrow_and_totokens() {
	use proc_macro2::{Ident, Span, TokenStream, TokenTree};
	use std::borrow::Borrow;

	struct Both(#[allow(dead_code)] Vec<u8>);
	impl Borrow<[u8]> for Both {
		fn borrow(&self) -> &[u8] {
			&self.0
		}
	}
	impl template_quote::ToTokens for Both {
		fn to_tokens(&self, tokens: &mut TokenStream) {
			tokens.extend(Some(TokenTree::Ident(Ident::new(
				"Both",
				Span::call_site(),
			))));
		}
	}

	let both = Both(vec![1, 2, 3]);
	let i = 0..3; // bounding iterator
			   // Compiles (no E0283) and treats `both` as a single repeated ToTokens value.
	let tokens = quote! { #(#i #both)* };
	assert_eq!(tokens.to_string(), "0i32 Both 1i32 Both 2i32 Both");
}

// ---- Tier-2 #10: the emitted internal identifiers must be hygienic
// (`Span::mixed_site()`) so that user bindings with the same textual name
// (`__template_quote_stream`, `__template_quote_idcnt`, and the per-var
// iteration helpers) do not collide with the macro's own locals. Previously
// these were `call_site`, so a user variable of the same name shadowed the
// macro's stream/counter and broke expansion. ----

#[test]
fn hygienic_internal_idents_no_user_collision() {
	// User variables whose names deliberately match the macro's internal locals.
	let __template_quote_stream = 111i32;
	let __template_quote_idcnt = 222i32;
	let __template_quote_it_0 = 333i32;
	let __template_quote_m_0 = 444i32;

	let x = [1i32, 2i32, 3i32];
	// A separated repetition exercises the counter (`id_counter`) and the
	// per-var iterator helpers (`__template_quote_it_*` / `__template_quote_m_*`).
	let tokens = quote! { #(#x),* };
	assert_eq!(tokens.to_string(), "1i32 , 2i32 , 3i32");

	// The user bindings must remain untouched and usable after the macro.
	assert_eq!(__template_quote_stream, 111i32);
	assert_eq!(__template_quote_idcnt, 222i32);
	assert_eq!(__template_quote_it_0, 333i32);
	assert_eq!(__template_quote_m_0, 444i32);

	// Interpolating the same-named user variables must yield their values,
	// not the macro internals.
	let s = quote! { #__template_quote_stream #__template_quote_idcnt };
	assert_eq!(s.to_string(), "111i32 222i32");
}

// ---- `#()` family: empty inline group `#{}` expands to nothing (tested
// above); a bare `#()` with no `*`/`{...}`/separator is not a template
// construct, so the `#` is emitted literally. (The empty *repetition* `#()*`
// with no interpolation variable is a compile error — see tests/ui.) ----

#[test]
fn bare_hash_paren_emits_literal_hash() {
	assert_eq!(quote! { a #() b }.to_string(), "a # () b");
	assert_eq!(quote! { #(x y) }.to_string(), "# (x y)");
}
