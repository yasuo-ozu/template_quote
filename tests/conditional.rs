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
	let v = vec![vec![1, 2, 3], vec![4, 5, 6]];
	let vv = vec![1];
	let tokens = quote! {
		#(
			#(for inner in &v);{
				[#( #inner ),*]
			}
			#vv
		)*
	};
	assert_eq!(
		"[1i32 , 2i32 , 3i32] ; [4i32 , 5i32 , 6i32] 1i32",
		tokens.to_string()
	);
	#[allow(unreachable_code)]
	let tokens = quote! {
		#(for inner in 0..10);{
			{
				#inner
				#{break;}
			}
		}
	};
	assert_eq!("", tokens.to_string());
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
	struct STuple(i32, i32);
	struct SStruct {
		a: i32,
		b: i32,
		_c: i32,
	}
	let vv = vec![true];
	let tup = STuple(123, 456);
	let sct = SStruct {
		a: 789,
		b: 12,
		_c: 345,
	};
	let tokens = quote! {
		#(
			#(let STuple(a, ..) = tup) {
				#a
			}
			#(let SStruct{ a: d, b, .. } = sct) {
				#d, #b
				#vv
			}
		)*
	};
	assert_eq!("123i32 789i32 , 12i32 true", tokens.to_string());
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
	let mut v = vec![vec![1, 2, 3], vec![4, 5]].into_iter();
	let tokens = quote! {
		{
			#(while let Some(i) = v.next()){
				#(#i),*
				#{break;}
			}
		}
	};
	assert_eq!("{ 1i32 , 2i32 , 3i32 }", tokens.to_string());
}

#[test]
fn test_break() {
	let v = vec![1, 2, 3];
	let tokens = quote! {
		#(
			#v
			#{ if v >= &2 { break; }; }
		)*
	};
	assert_eq!("1i32 2i32", tokens.to_string());
}

#[test]
fn test_punct() {
	let v = vec![vec![1, 2], vec![3]];
	let tokens = quote! {
		#(
			#(for i in #v) {
				#i
			}
		),*
	};
	assert_eq!("1i32 2i32 , 3i32", tokens.to_string());
	let tokens = quote! {
		#(
			#(for i in #v.iter().map(|a| a + 1)) {
				#i
			}
		),*
	};
	assert_eq!("2i32 3i32 , 4i32", tokens.to_string());
	let v = vec![1, 2, 3];
	let tokens = quote! {
		#(
			#(for i in 0..*#v) {
				#i
			}
		),*
	};
	assert_eq!("0i32 , 0i32 1i32 , 0i32 1i32 2i32", tokens.to_string());
	let tokens = quote! {
		#(
			#{#v.to_string()}
		),*
	};
	assert_eq!("\"1\" , \"2\" , \"3\"", tokens.to_string());
	let v = vec![1, 2, 3];
	let tokens = quote! {
		#(
			#(if #v > &2) {
				+ #v
			}
			#(else) {
				- #v
			}
		)*
	};
	assert_eq!("- 1i32 - 2i32 + 3i32", tokens.to_string());
}

#[test]
fn test_complex_assign() {
	let v = vec![1, 2, 3];
	let v = Some(v.as_slice());
	let tokens = quote! {
		#(if let Option::Some([a, b, c]) = v) {
			#a - #b - #c
		}
	};
	assert_eq!("1i32 - 2i32 - 3i32", tokens.to_string());
}
