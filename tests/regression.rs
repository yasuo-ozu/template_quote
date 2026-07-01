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
	let v = vec![1, 2, 3];
	let tokens = quote! {
		#(for i in (#v).iter()) { #i }
	};
	assert_eq!(tokens.to_string(), "1i32 2i32 3i32");
}

#[test]
fn interp_inside_group_in_if_condition() {
	let v = vec![1, 2, 3];
	let tokens = quote! {
		#(if (#v).len() == 3) { yes }
	};
	assert_eq!(tokens.to_string(), "yes");
}

#[test]
fn interp_inside_group_in_inline_block() {
	let v = vec![10, 20];
	let tokens = quote! {
		#(for _i in 0..2) { #{ (#v).len() } }
	};
	assert_eq!(tokens.to_string(), "2usize 2usize");
}

// ---- Tier-1 #5: the let-pattern parser must accept valid Rust patterns
// inside `#(let ...)`, `#(if let ...)` and `#(while let ...)` headers. ----

#[test]
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
