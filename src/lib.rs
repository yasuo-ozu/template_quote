//! This create provide [`quote!`] macro.

/// The entrypoint with fully backward-compatibility with traditional `quote!`
/// macro.
///
/// This macro is intended to use in your proc-macro, to generate `TokenStream`
/// expanding variable interporation and expanding templates.
///
/// This macro is constructed based on `proc_macro` crate.
///
/// # Interporation
///
/// For backward compatibility, interporation rule is same as traditional
/// `quote!` macro. The interporation is done with `#var` (similar to the
/// variable `$var` in `macro_rules!`). Most variables in `Syn` crate are
/// interporated using [`::proc_quote::ToTokens`] trait.
///
/// ## Rules
///
/// Repetition is done using syntax like `#(...)*` or `#(...),*`. It repeats the
/// variables (`#var`) inside this syntax, which implements
/// [`::proc_quote::Repeat`].
///
/// - `#(...)*` - repeat ... with no separators. at least one variable should be
///   included in ...
/// - `#(...),*` - same as before, but interporates with separator ','.
///
/// ## Problem
///
/// The interporation rule is **rough**, so I implemented new 'template' syntax.
/// For example, the following code will not allowed, because `#var1` cannot be
/// iterated double.
///
/// ```
/// # use template_quote::quote;
/// let var1 = vec!['a', 'b'];
/// let var2 = vec![vec![1, 2], vec![3, 4]];
/// let tokens = quote!{
/// 	#(#(#var1 #var2)*)*
/// };
/// assert_eq!("'a' 1i32 'a' 2i32 'b' 3i32 'b' 4i32", tokens.to_string());
/// ```
///
/// # Template syntax
///
/// Template syntax is proceedual-like syntax, which allows you to use structual
/// statementsinside the macro.
///
/// ## If syntax
///
/// This code iterates around `#i` (with interporation), and emits `i32` into
/// `TokenStream` while the number meets the condition.
///
/// ```
/// # use template_quote::quote;
/// let i = vec![1, 2, 3];
/// let tokens = quote!{
/// 	#(
/// 		#(if i > &2) {
/// 			#i
/// 		}
/// 	)*
/// };
/// assert_eq!("3i32", tokens.to_string());
/// ```
///
/// The if-else and if-else-if is also allowed.
///
/// ```
/// # use template_quote::quote;
/// let i = vec![1, 2, 3];
/// let tokens = quote!{
/// 	#(
/// 		#(if i > &2) {
/// 			+ #i
/// 		}
/// 		#(else) {
/// 			- #i
/// 		}
/// 	)*
/// };
/// assert_eq!("- 1i32 - 2i32 + 3i32", tokens.to_string());
/// ```
///
/// ```
/// # use template_quote::quote;
/// let i = vec![1, 2, 3, 4, 5];
/// let tokens = quote!{
/// 	#(
/// 		#(if i % &2 == 0) {
/// 			+ #i
/// 		}
/// 		#(else if i % &3 == 0) {
/// 			- #i
/// 		}
/// 		#(else) {
/// 			#i
/// 		}
/// 	)*
/// };
/// assert_eq!("1i32 + 2i32 - 3i32 + 4i32 5i32", tokens.to_string());
/// ```
///
/// ## For syntax
///
/// For syntax iterates around the variable (like interporation), but it
/// specifies which variable to iterate.
///
/// ```
/// # use template_quote::quote;
/// let v1 = vec![1, 2];
/// let v2 = vec!['a', 'b'];
/// let tokens = quote!{
/// 	#(for i1 in &v1) {
/// 		#(for i2 in &v2) {
/// 			#i1 -> #i2
/// 		}
/// 	}
/// };
/// assert_eq!("1i32 -> 'a' 1i32 -> 'b' 2i32 -> 'a' 2i32 -> 'b'", tokens.to_string());
/// ```
///
/// Internal loop can be replaced with interporation:
///
/// ```
/// # use template_quote::quote;
/// let v1 = vec![1, 2];
/// let v2 = vec!['a', 'b'];
/// let tokens = quote!{
/// 	#(for i1 in &v1) {
/// 		#(
/// 			#i1 -> #v2
/// 		)*
/// 	}
/// };
/// assert_eq!("1i32 -> 'a' 1i32 -> 'b' 2i32 -> 'a' 2i32 -> 'b'", tokens.to_string());
/// ```
///
/// You can also specify separator with for statement.
///
/// ```
/// # use template_quote::quote;
/// let v = vec![1, 2];
/// let tokens = quote!{
/// 	#(for i in v) | { #i }
/// };
/// assert_eq!("1i32 | 2i32", tokens.to_string());
/// ```
///
/// Interporation is not usable with variables binded in for syntax. For
/// example,
///
/// ```compile_fail
/// # use template_quote::quote;
/// let v = vec![vec![1, 2], vec![3]];
/// let tokens = quote!{
/// 	#(
/// 		#(for i in v) { #i }
/// 	),*
/// };
/// assert_eq!("1i32 2i32 , 3i32", tokens.to_string());
/// ```
///
/// will fail into error because no variables is available in the interporation
/// syntax.
///
/// ```text
/// error: proc macro panicked
///   --> ***
///    |
/// 6  |   let tokens = quote!{
///    |  ______________^
/// 7  | |     #(
/// 8  | |         #(for i in v) { #i }
/// 9  | |     )*
/// 10 | | };
///    | |_^
///    |
///    = help: message: Iterative vals not found
/// ```
///
/// In this case, you can use `#(for i in #v)` syntax to specify which variable
/// to iterate with interporation:
///
/// ```
/// # use template_quote::quote;
/// let v = vec![vec![1, 2], vec![3]];
/// let tokens = quote!{
/// 	#(
/// 		#(for i in #v) { #i }
/// 	),*
/// };
/// assert_eq!("1i32 2i32 , 3i32", tokens.to_string());
/// ```
///
/// ## While syntax
///
/// ```
/// # use template_quote::quote;
/// let mut v = vec![1, 2].into_iter();
/// let tokens = quote!{
/// 	#(while v.next().is_some()) { hello }
/// };
/// assert_eq!("hello hello", tokens.to_string());
/// ```
///
/// ## While-Let syntax
///
/// ```
/// # use template_quote::quote;
/// let mut v = vec![1, 2].into_iter();
/// let tokens = quote!{
/// 	#(while let Some(i) = v.next()) { #i }
/// };
/// assert_eq!("1i32 2i32", tokens.to_string());
/// ```
///
/// Same as 'for' syntax, the binded valiables in 'while' is not iteratable with
/// interporation syntax. For example,
///
/// ```compile_fail
/// # use template_quote::quote;
/// let mut v = vec![1, 2].into_iter();
/// quote!{
/// 	#(
/// 		#(while let Some(i) = v.next()) { #i }
/// 	)*
/// };
/// ```
///
/// will fail.
///
/// ## Let syntax
///
/// Let syntax bind new variables usable inside the block.
///
/// ```
/// # use template_quote::quote;
/// let v = vec![(1, 'a'), (2, 'b')];
/// let tokens = quote!{
/// 	#(for i in v), {
/// 		#(let (n, c) = i) {
/// 			#n -> #c
/// 		}
/// 	}
/// };
/// assert_eq!("1i32 -> 'a' , 2i32 -> 'b'", tokens.to_string());
/// ```
///
/// Here, `#n` and `#c` is not iteratable with interporation syntax.
///
/// ## Inline expression
///
/// You can place inline expression in `quote!` macro.
///
/// ```
/// # use template_quote::quote;
/// let v = vec![1, 2];
/// let tokens = quote!{
/// 	#(for i in v){
/// 		#i -> #{ i.to_string() }
/// 	}
/// };
/// assert_eq!("1i32 -> \"1\" 2i32 -> \"2\"", tokens.to_string());
/// ```
///
/// The following example will fail to compile because it does not understand
/// which variable to be interpolated:
///
/// ```compile_fail
/// # use template_quote::quote;
/// let v = vec![1, 2];
/// let tokens = quote!{
/// 	#(
/// 		#{ v.to_string() }
/// 	)*
/// };
/// assert_eq!("\"1\" \"2\"", tokens.to_string());
/// ```
///
/// In this case, you can use `#i` syntax in inline expression to specify which
/// variable to iterate with interporation syntax.
///
/// ```
/// # use template_quote::quote;
/// let v = vec![1, 2];
/// let tokens = quote!{
/// 	#(
/// 		#{ #v.to_string() }
/// 	)*
/// };
/// assert_eq!("\"1\" \"2\"", tokens.to_string());
/// ```
///
/// ## Inline statement
///
/// You can place arbitrary statement inside this macro. For example,
///
/// ```
/// # use template_quote::quote;
/// let v = vec![1, 2, 3];
/// let tokens = quote!{
/// 	#(
/// 		#v
/// 		#{ eprintln!("debug: {}", &v); }
/// 	)*
/// };
/// assert_eq!("1i32 2i32 3i32", tokens.to_string());
/// ```
///
/// will print:
///
/// ```text
/// debug: 1
/// debug: 2
/// debug: 3
/// ```
///
/// To be distinguishable, all statements have to end with ';'. For example,
/// 'if' statement in inline statement syntax should placed with extra ';'.
///
/// ```
/// # use template_quote::quote;
/// let v = vec![1, 2, 3];
/// quote!{
/// 	#(
/// 		#v
/// 		#{ if v >= &2 { eprintln!("debug: {}", &v); } ; }
/// 	)*
/// };
/// ```
///
/// ## Break, Continue
///
/// You can put control statement like `break` or `continue` in inline
/// statement, but it is a bit danger.
///
/// If you use `break;` inside block (like `{ ... }` or `( ... )`), `break` will
/// suddenly give up emitting whole group, and nothing will be emitted. For
/// example, the following code does not emit any group:
///
/// ```
/// # use template_quote::quote;
/// let v = vec![1, 2, 3];
/// let tokens = quote!{
/// 	#(for i in v) {
/// 		#i // this is emitted once
/// 		// The block is not emitted
/// 		{
/// 			#i
/// 			#{ break; }
/// 		}
/// 	}
/// };
/// assert_eq!("1i32", tokens.to_string());
/// ```
///
/// `break` also affects on interporation syntax like:
///
/// ```
/// # use template_quote::quote;
/// let v = vec![1, 2, 3];
/// let tokens = quote!{
/// 	#(
/// 		#v
/// 		#{ break; }
/// 	),*
/// };
/// assert_eq!("1i32", tokens.to_string());
/// ```
///
/// Unfortunately, `break` will leak outside of `quote!` macro. This is example
/// which the internal `break` affects on 'for' loop, which is placed outer of
/// the `quote!` macro.
///
/// ```
/// # use template_quote::quote;
/// let mut v = Vec::new();
/// for _ in 0..3 {
/// 	let tokens = quote!{
/// 		#{ break; }
/// 	};
/// 	v.push(tokens);
/// }
/// assert_eq!(v.len(), 0);
/// ```
pub use template_quote_impl::quote;

/// [`quote_configured!`] macro is configurable version of [`quote!`].
///
/// ```ignore
/// # use template_quote::quote_configured;
/// quote_configured! {
/// 	{
/// 		proc_macro2: ::proc_macro2,
/// 		quote: ::quote,
/// 		core: ::core,	// core crate in std
/// 		quote: ::quote,
/// 		span: ::some_span,
/// 	} =>
/// 	...
/// };
/// ```
pub use template_quote_impl::quote_configured;

/// [`quote_spanned!`] macro emit `TokenTree` with specified
/// `Span`.
///
/// ```ignore
/// use syn::Span;
/// let span = Span::call_site();
/// let tokens = quote_spanned! {span => ... };
/// ```
pub use template_quote_impl::quote_spanned;

pub use imp::Repeat;
pub use quote::ToTokens;

mod imp {
	use quote::ToTokens;
	use std::borrow::Borrow;
	use std::slice;

	// This trait is from `proc-quote` crate.
	pub unsafe trait Repeat<T: Iterator> {
		#[allow(non_snake_case)]
		#[doc(hidden)]
		fn __template_quote__as_repeat(self) -> T;

		#[allow(non_snake_case)]
		#[doc(hidden)]
		fn __template_quote_is_iterable(&self) -> bool;
	}

	unsafe impl<T, I: Iterator<Item = T>> Repeat<I> for I {
		fn __template_quote__as_repeat(self) -> I {
			self
		}

		fn __template_quote_is_iterable(&self) -> bool {
			true
		}
	}

	unsafe impl<'a, T: 'a, S: Borrow<[T]>> Repeat<slice::Iter<'a, T>> for &'a S {
		fn __template_quote__as_repeat(self) -> slice::Iter<'a, T> {
			(*self).borrow().iter()
		}

		fn __template_quote_is_iterable(&self) -> bool {
			true
		}
	}

	unsafe impl<'a, T: ToTokens + 'a> Repeat<ToTokensRepeat<'a, T>> for &'a T {
		fn __template_quote__as_repeat(self) -> ToTokensRepeat<'a, T> {
			ToTokensRepeat(self)
		}

		fn __template_quote_is_iterable(&self) -> bool {
			false
		}
	}

	pub struct ToTokensRepeat<'a, T: ToTokens + 'a>(&'a T);
	impl<'a, T: ToTokens + 'a> Iterator for ToTokensRepeat<'a, T> {
		type Item = &'a T;
		fn next(&mut self) -> Option<Self::Item> {
			Some(self.0)
		}
	}
}
