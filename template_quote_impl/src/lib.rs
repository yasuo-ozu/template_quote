extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::{Delimiter, Literal, Punct, Spacing, Span, TokenTree};
use proc_macro_error::ResultExt;
use quote::{quote as qquote, TokenStreamExt};
use std::collections::{HashSet, VecDeque};
use syn::{parse_quote, Expr, Ident, Path, Token};

struct ParseEnvironment {
	span: Expr,
	path_proc_macro2: Path,
	path_quote: Path,
	path_core: Path,
}

impl syn::parse::Parse for ParseEnvironment {
	fn parse(input: syn::parse::ParseStream<'_>) -> syn::parse::Result<Self> {
		// Read comma-separated `key = value` fields
		let mut fields: Vec<(Ident, Expr)> = Vec::new();
		while !input.is_empty() {
			let ident = input.parse()?;
			input.parse::<Token![:]>()?;
			let expr = input.parse()?;
			fields.push((ident, expr));
			if !input.is_empty() {
				input.parse::<Token![,]>()?;
			}
		}
		let mut this: ParseEnvironment = Default::default();
		for (id, expr) in fields {
			match (id.to_string().as_str(), expr) {
				("proc_macro2", Expr::Path(pat)) => {
					this.path_proc_macro2 = pat.path;
				}
				("quote", Expr::Path(pat)) => {
					this.path_quote = pat.path;
				}
				("core", Expr::Path(pat)) => {
					this.path_core = pat.path;
				}
				("span", expr) => {
					this.span = expr;
				}
				(key, _) => {
					return Err(input.error(&format!("Bad config name: {}", key)));
				}
			}
		}
		Ok(this)
	}
}

impl core::default::Default for ParseEnvironment {
	fn default() -> Self {
		Self {
			span: parse_quote! { ::proc_macro2::Span::call_site() },
			path_proc_macro2: parse_quote! { ::proc_macro2 },
			path_quote: parse_quote! { ::template_quote },
			path_core: parse_quote! { ::core },
		}
	}
}

impl ParseEnvironment {
	fn emit_ident(&self, ident: &Ident, stream_id: &Ident) -> TokenStream2 {
		let Self {
			span,
			path_proc_macro2,
			path_quote,
			..
		} = self;
		let s = ident.to_string();
		if s.starts_with("r#") {
			let s = &s[2..];
			qquote! {
				<_ as #path_quote::ToTokens>::to_tokens(&#path_proc_macro2::Ident::new_raw(#s, (#span)), &mut #stream_id);
			}
		} else {
			qquote! {
				<_ as #path_quote::ToTokens>::to_tokens(&#path_proc_macro2::Ident::new(#s, (#span)), &mut #stream_id);
			}
		}
	}

	fn emit_literal(&self, lit: &Literal, stream_id: &Ident) -> TokenStream2 {
		let Self {
			span,
			path_proc_macro2,
			..
		} = self;
		let s = lit.to_string();
		qquote! {
			{
				let ts: #path_proc_macro2::TokenStream = #s.parse().expect("Invalid literal str");
				#stream_id.extend(ts.into_iter().map(|t| {
					t.set_span(#span);
					t
				}));
			}
		}
	}

	fn emit_punct(&self, punct: &Punct, stream_id: &Ident) -> TokenStream2 {
		let Self {
			span,
			path_proc_macro2,
			path_quote,
			..
		} = self;
		let p = punct.as_char();
		let spacing = match punct.spacing() {
			Spacing::Alone => qquote! {#path_proc_macro2::Spacing::Alone},
			Spacing::Joint => qquote! {#path_proc_macro2::Spacing::Joint},
		};
		qquote! {
			<_ as #path_quote::ToTokens>::to_tokens(&{
				let mut p = #path_proc_macro2::Punct::new(#p, #spacing);
				p.set_span(#span);
				p
			}, &mut #stream_id);
		}
	}

	fn emit_group(
		&self,
		delim: &Delimiter,
		inner: TokenStream2,
		stream_id: &Ident,
	) -> TokenStream2 {
		let Self {
			span,
			path_proc_macro2,
			path_core,
			..
		} = self;
		let delim = match delim {
			Delimiter::Parenthesis => qquote! { #path_proc_macro2::Delimiter::Parenthesis },
			Delimiter::Brace => qquote! { #path_proc_macro2::Delimiter::Brace },
			Delimiter::Bracket => qquote! { #path_proc_macro2::Delimiter::Bracket },
			Delimiter::None => qquote! { #path_proc_macro2::Delimiter::None },
		};
		qquote! {
			#stream_id.extend(
				#path_core::option::Option::Some(
					#path_proc_macro2::TokenTree::Group(
						{
							let mut g = #path_proc_macro2::Group::new(#delim, {
								let mut #stream_id = #path_proc_macro2::TokenStream::new();
								{ #inner }
								#stream_id
							});
							g.set_span(#span);
							g
						}
					)
				)
			);
		}
	}

	fn parse_conditional(
		&self,
		cond: TokenStream2,
		input: VecDeque<TokenTree>,
		vals: &mut HashSet<Ident>,
		sep: Option<Punct>,
		stream_id: &Ident,
	) -> TokenStream2 {
		let inner = self.parse_inner(input, vals, stream_id);
		let cond = cond.into_iter().collect::<Vec<_>>();
		let is_if_or_else = match cond.get(0) {
			Some(TokenTree::Ident(id)) if &id.to_string() == "if" || &id.to_string() == "else" => {
				if sep.is_some() {
					panic!("Separator should not be specified in if conditional")
				} else {
					true
				}
			}
			_ => false,
		};
		if is_if_or_else {
			qquote! {
				#(#cond)* { #inner }
			}
		} else {
			let code_sep = sep
				.into_iter()
				.map(|sep| self.emit_punct(&sep, stream_id))
				.collect::<Vec<_>>();
			let val_nam = code_sep.iter().map(|_| qquote! { __i }).collect::<Vec<_>>();
			qquote! {
				{
					#( let mut #val_nam = false; )*
					#(#cond)* {
						#(
							if #val_nam { #code_sep }
							#val_nam = true;
						)*
						#inner
					}
				}
			}
		}
	}

	fn parse_iteration(
		&self,
		input: VecDeque<TokenTree>,
		vals: &mut HashSet<Ident>,
		sep: Option<Punct>,
		stream_id: &Ident,
	) -> TokenStream2 {
		let path_quote = &self.path_quote;
		let mut inner_vals = HashSet::new();
		let inner_output = self.parse_inner(input, &mut inner_vals, stream_id);
		let code_sep =
			sep.map(|sep| self.emit_punct(&Punct::new(sep.as_char(), Spacing::Alone), stream_id));
		let val_nam = code_sep
			.as_ref()
			.map(|_| qquote! { __i })
			.into_iter()
			.collect::<Vec<_>>();
		vals.extend(inner_vals.iter().cloned());
		let mut iter = inner_vals.iter();
		let first = iter.next().expect("Iterative vals not found");
		let idents_in_tuple = iter.clone().cloned().fold(qquote! {#first}, |prev, next| {
			qquote! {
				(#prev, #next)
			}
		});
		let zip_iterators = iter
			.map(|ident| {
				qquote! {
					.zip(#ident .__proc_quote__as_repeat())
				}
			})
			.collect::<Vec<_>>();
		qquote! {
			{
				#(let mut #val_nam = false;)*
				use #path_quote::Repeat as __Repeat;
				for #idents_in_tuple in #first .__proc_quote__as_repeat() #(#zip_iterators)* {
					#(
						if #val_nam { #code_sep }
						#val_nam = true;
					)*
					#inner_output
				}
			}
		}
	}

	fn parse(&self, input: TokenStream2) -> TokenStream2 {
		let Self {
			path_proc_macro2, ..
		} = self;
		let mut hs = HashSet::new();
		let stream_id = Ident::new("__stream", Span::call_site());
		let result = self.parse_inner(input.into_iter().collect(), &mut hs, &stream_id);
		qquote! {
			{
				let mut #stream_id = #path_proc_macro2::TokenStream::new();
				{ #result }
				#stream_id
			}
		}
	}

	fn parse_inner(
		&self,
		mut input: VecDeque<TokenTree>,
		vals: &mut HashSet<Ident>,
		stream_id: &Ident,
	) -> TokenStream2 {
		let Self { path_quote, .. } = self;
		let mut output = TokenStream2::new();
		while let Some(token) = input.pop_front() {
			match token {
				TokenTree::Group(group) => {
					let inner = group.stream().into_iter().collect();
					let result = self.parse_inner(inner, vals, stream_id);
					let result = self.emit_group(&group.delimiter(), result, stream_id);
					output.append_all(result);
				}
				TokenTree::Punct(punct) => match (punct.as_char(), input.pop_front()) {
					// # val
					('#', Some(TokenTree::Ident(ident))) => {
						vals.insert(ident.clone());
						output.append_all(
							qquote! { <_ as #path_quote::ToTokens>::to_tokens(&#ident, &mut #stream_id); },
						);
					}
					// # { ... }
					('#', Some(TokenTree::Group(group)))
						if group.delimiter() == Delimiter::Brace =>
					{
						let inner = group.stream().into_iter().collect::<Vec<_>>();
						// Check if the inner stream ends with ';'
						let is_expr = match inner.last() {
							Some(TokenTree::Punct(p)) if p.as_char() == ';' => false,
							_ => true,
						};
						if is_expr {
							output.append_all(qquote! {
								#[allow(unused_braces)]
								<_ as #path_quote::ToTokens>::to_tokens(#group, &mut #stream_id);
							});
						} else {
							output.append(group);
						}
					}
					('#', Some(TokenTree::Group(group)))
						if group.delimiter() == Delimiter::Parenthesis =>
					{
						match input.pop_front() {
							// # ( ... ) { ... }
							Some(TokenTree::Group(group2))
								if group2.delimiter() == Delimiter::Brace =>
							{
								output.append_all(self.parse_conditional(
									group.stream().into(),
									group2.stream().into_iter().collect(),
									vals,
									None,
									stream_id,
								));
							}
							// # ( ... ) *
							Some(TokenTree::Punct(punct)) if punct.as_char() == '*' => output
								.append_all(self.parse_iteration(
									group.stream().into_iter().collect(),
									vals,
									None,
									stream_id,
								)),
							Some(TokenTree::Punct(punct0)) => match input.pop_front() {
								// # ( ... ) [SEP] *
								Some(TokenTree::Punct(punct1)) if punct1.as_char() == '*' => output
									.append_all(self.parse_iteration(
										group.stream().into_iter().collect(),
										vals,
										Some(punct0),
										stream_id,
									)),
								// # ( ... ) [SEP] { ... }
								Some(TokenTree::Group(group2))
									if group2.delimiter() == Delimiter::Brace =>
								{
									output.append_all(self.parse_conditional(
										group.stream().into(),
										group2.stream().into_iter().collect(),
										vals,
										Some(punct0),
										stream_id,
									));
								}
								o => {
									if let Some(o) = o {
										input.push_front(o);
									}
									input.push_front(TokenTree::Punct(punct0));
									input.push_front(TokenTree::Group(group));
									output.append_all(self.emit_punct(&punct, stream_id));
								}
							},
							o => {
								if let Some(o) = o {
									input.push_front(o)
								}
								input.push_front(TokenTree::Group(group));
								output.append_all(self.emit_punct(&punct, stream_id));
							}
						}
					}
					(_, o) => {
						if let Some(o) = o {
							input.push_front(o);
						}
						output.append_all(self.emit_punct(&punct, stream_id));
					}
				},
				TokenTree::Ident(o) => output.append_all(self.emit_ident(&o, stream_id)),
				TokenTree::Literal(o) => output.append_all(self.emit_literal(&o, stream_id)),
			}
		}
		output
	}
}

#[proc_macro]
pub fn quote(input: TokenStream) -> TokenStream {
	let env: ParseEnvironment = Default::default();
	env.parse(input.into()).into()
}

#[proc_macro]
pub fn quote_configured(input: TokenStream) -> TokenStream {
	let input0: TokenStream2 = input.into();
	let mut input = VecDeque::new();
	input.extend(input0.into_iter());
	let env: ParseEnvironment = match input.pop_front() {
		Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Brace => {
			syn::parse2(g.stream()).expect_or_abort("Bad config format")
		}
		_ => panic!("Bad config format"),
	};
	match input.pop_front() {
		Some(TokenTree::Punct(p)) if p.as_char() == '=' && p.spacing() == Spacing::Joint => (),
		_ => panic!("Bad config format"),
	}
	match input.pop_front() {
		Some(TokenTree::Punct(p)) if p.as_char() == '>' && p.spacing() == Spacing::Alone => (),
		_ => panic!("Bad config format"),
	}
	let mut stream = TokenStream2::new();
	stream.extend(input);
	env.parse(stream).into()
}

#[proc_macro]
pub fn quote_spanned(input: TokenStream) -> TokenStream {
	let input0: TokenStream2 = input.into();
	let mut input = VecDeque::new();
	input.extend(input0.into_iter());
	let mut span = TokenStream2::new(); // Tokens before '=>'
	loop {
		match input.pop_front() {
			Some(TokenTree::Punct(p)) if p.as_char() == '=' && p.spacing() == Spacing::Joint => {
				match input.pop_front() {
					Some(TokenTree::Punct(p))
						if p.as_char() == '>' && p.spacing() == Spacing::Alone =>
					{
						// Found '=>'.
						break;
					}
					Some(o) => {
						span.extend(Some(TokenTree::Punct(p)));
						input.push_front(o);
					}
					None => {
						span.extend(Some(TokenTree::Punct(p)));
					}
				}
			}
			Some(o) => span.extend(Some(o)),
			None => panic!("wrong quote_spanned format"),
		}
	}
	let mut env: ParseEnvironment = Default::default();
	env.span = syn::parse2(span).expect_or_abort("Span must be expr");
	let mut stream = TokenStream2::new();
	stream.extend(input);
	env.parse(stream).into()
}
