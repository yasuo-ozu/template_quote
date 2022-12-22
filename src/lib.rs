extern crate proc_quote;
extern crate template_quote_impl;

pub use template_quote_impl::quote;
pub use template_quote_impl::quote_configured;
pub use template_quote_impl::quote_spanned;

pub use proc_quote::Repeat;
pub use proc_quote::ToTokens;
