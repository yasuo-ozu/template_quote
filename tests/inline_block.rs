extern crate template_quote;

use template_quote::quote;

#[test]
fn test_block_expr() {
	let v = (vec![1, 2, 3], vec!["1", "2", "3"]);
	let tokens = quote! {
		#(
			#{ v.0 } => #{ v.1 }
		)*
	};
	assert_eq!(
		"1i32 => \"1\" 2i32 => \"2\" 3i32 => \"3\"",
		tokens.to_string()
	);
}
