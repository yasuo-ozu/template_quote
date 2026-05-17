#![doc = include_str!("../README.md")]

/// See module-level doc.
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
