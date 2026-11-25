//! Regression test for issue #2 (`could not find proc_macro2 in the list of
//! imported crates`). This crate deliberately does not depend on
//! `proc-macro2`, so the generated code must reach it through `template_quote`.

#[cfg(test)]
mod tests {
	use template_quote::{quote, quote_configured, quote_spanned};

	#[test]
	fn quote_issue_repro() {
		assert_eq!(quote!(MyType1).to_string(), "MyType1");
	}

	#[test]
	fn quote_all_token_kinds() {
		// Idents, raw idents, puncts, groups, literals, repetition with a
		// separator and a conditional.
		let items = vec![1i32, 2, 3];
		let flag = true;
		let tokens = quote! {
			fn r#fn() -> i32 { #(#items)+* }
			#(if flag) { const S: &str = "lit"; }
		};
		assert_eq!(
			tokens.to_string(),
			"fn r#fn () -> i32 { 1i32 + 2i32 + 3i32 } const S : & str = \"lit\" ;"
		);
	}

	#[test]
	fn quote_spanned_without_proc_macro2_dependency() {
		let span = quote!(x).into_iter().next().unwrap().span();
		let items = [1i32, 2];
		let tokens = quote_spanned! { span => let v = [#(#items),*]; };
		assert_eq!(tokens.to_string(), "let v = [1i32 , 2i32] ;");
	}

	#[test]
	fn quote_configured_without_proc_macro2_dependency() {
		let span = quote!(x).into_iter().next().unwrap().span();
		assert_eq!(quote_configured!({} => a b).to_string(), "a b");
		assert_eq!(quote_configured!({ span: span } => a b).to_string(), "a b");
	}
}
