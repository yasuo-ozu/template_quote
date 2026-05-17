use proc_macro2::{Ident, Span};
use template_quote::quote;

#[test]
fn test_quote_impl() {
	let tokens = quote! {
		impl<'a, T: ToTokens> ToTokens for &'a T {
			fn to_tokens(&self, tokens: &mut TokenStream) {
				(**self).to_tokens(tokens)
			}
		}
	};

	let expected = concat!(
		"impl < 'a , T : ToTokens > ToTokens for & 'a T { ",
		"fn to_tokens (& self , tokens : & mut TokenStream) { ",
		"(** self) . to_tokens (tokens) ",
		"} ",
		"}"
	);

	assert_eq!(expected, tokens.to_string());
}

#[test]
fn test_var_name_conflict() {
	// The implementation of `quote!` uses the variable `__stream` but it should
	// fine, if a little confusing when debugging.
	let __stream = 'a';
	let tokens = quote!( #__stream );
	let expected = "'a'";
	assert_eq!(expected, tokens.to_string());
}

#[test]
fn test_empty_quote() {
	let tokens = quote!();
	assert_eq!("", tokens.to_string());
}

#[test]
fn test_append_tokens() {
	let mut a = quote!(a);
	let b = quote!(b);
	a.extend(b);
	assert_eq!("a b", a.to_string());
}

#[test]
fn test_closure() {
	fn field_i(i: usize) -> Ident {
		Ident::new(&format!("__field{}", i), Span::call_site())
	}

	let fields = (0usize..3)
		.map(field_i as fn(_) -> _)
		.map(|var| quote!( #var ));

	let tokens = quote!( #(#fields)* );
	assert_eq!("__field0 __field1 __field2", tokens.to_string());
}

#[test]
fn test_raw_ident() {
	let tokens = quote!(r#struct r#the);
	let expected = "r#struct r#the";
	assert_eq!(expected, tokens.to_string());
}

#[test]
fn test_joint_punct_before_interpolation() {
	let v = 3;
	let tokens = quote!(a+=#v);
	let expected = "a += 3i32";
	assert_eq!(expected, tokens.to_string());
}

#[test]
fn test_hash_not_processed_is_unchanged() {
	let tokens = quote!(#[inline] a = 1;);
	let expected = "# [inline] a = 1 ;";
	assert_eq!(expected, tokens.to_string());
}

#[test]
fn test_joint_punct_before_interpolation_edge_cases() {
	let v = 3;
	assert_eq!("a -= 3i32", quote!(a-=#v).to_string());
	assert_eq!("a *= 3i32", quote!(a*=#v).to_string());
	assert_eq!("a /= 3i32", quote!(a/=#v).to_string());
	assert_eq!("a %= 3i32", quote!(a%=#v).to_string());
	assert_eq!("a &= 3i32", quote!(a&=#v).to_string());
	assert_eq!("a |= 3i32", quote!(a|=#v).to_string());
	assert_eq!("a ^= 3i32", quote!(a^=#v).to_string());
	assert_eq!("a == 3i32", quote!(a==#v).to_string());
	assert_eq!("a != 3i32", quote!(a!=#v).to_string());
	assert_eq!("a <= 3i32", quote!(a<=#v).to_string());
	assert_eq!("a >= 3i32", quote!(a>=#v).to_string());
}
