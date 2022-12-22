extern crate template_quote;

use template_quote::quote;

#[test]
fn test_conditional_if() {
	let v = vec![1, 10, 2, 13, 4, 19];
	let tokens = quote! {
		#(
			#(if v >= &10){
				#v
			}
		)*
	};
	assert_eq!("10i32 13i32 19i32", tokens.to_string());
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
}

#[test]
fn test_conditional_for() {
	let v = vec![(1, "one"), (2, "two"), (3, "three")];
	let tokens = quote! {
		#(for (i, name) in v),{
			#i is #name
		}
	};
	assert_eq!(
		"1i32 is \"one\" , 2i32 is \"two\" , 3i32 is \"three\"",
		tokens.to_string()
	);
}

#[test]
fn test_conditional_let() {
	let v = vec![1, 2, 3];
	let b = vec![true];
	let tokens = quote! {
		#(
			#(let (i, s) = (&v, v.iter().map(|i| i.to_string()).collect::<Vec<_>>())){
				#(#i is #s),*
			}
			#b
		)*
	};
	assert_eq!(
		"1i32 is \"1\" , 2i32 is \"2\" , 3i32 is \"3\" true",
		tokens.to_string()
	);
}

#[test]
fn test_conditional_while() {
	let mut v = vec![1, 2, 3].into_iter();
	let tokens = quote! {
		#(while let Some(i) = v.next()){
			#i
		}
	};
	assert_eq!("1i32 2i32 3i32", tokens.to_string());
	let mut v = vec![1, 2, 3].into_iter();
	let tokens = quote! {
		#(while let Some(i) = v.next()){
			#i
			#{
				if i > 1 { break; };
			}
		}
	};
	assert_eq!("1i32 2i32", tokens.to_string());
}
