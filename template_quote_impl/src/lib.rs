extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::{Delimiter, Literal, Punct, Spacing, Span, TokenTree};
use proc_macro_error::ResultExt;
use quote::{quote as qquote, quote_spanned as qquote_spanned, TokenStreamExt};
use std::collections::{HashSet, VecDeque};
use syn::{parse_quote, Expr, Ident, Path, Token};

struct ParseEnvironment {
	span: Expr,
	path_proc_macro2: Path,
	path_quote: Path,
	path_core: Path,
	id_stream: Ident,
	id_repeat: Ident,
	id_counter: Ident,
	id_inline_expr: String,
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
			id_stream: parse_quote! { __stream },
			id_repeat: parse_quote! { __Repeat },
			id_inline_expr: "__inline_".to_owned(),
			id_counter: parse_quote! { __i },
		}
	}
}

impl ParseEnvironment {
	fn emit_ident(&self, ident: &Ident) -> TokenStream2 {
		let Self {
			span,
			path_proc_macro2,
			path_quote,
			id_stream,
			..
		} = self;
		let s = ident.to_string();
		if s.starts_with("r#") {
			let s = &s[2..];
			qquote! {
				<_ as #path_quote::ToTokens>::to_tokens(&#path_proc_macro2::Ident::new_raw(#s, (#span)), &mut #id_stream);
			}
		} else {
			qquote! {
				<_ as #path_quote::ToTokens>::to_tokens(&#path_proc_macro2::Ident::new(#s, (#span)), &mut #id_stream);
			}
		}
	}

	fn emit_literal(&self, lit: &Literal) -> TokenStream2 {
		let Self {
			span,
			path_proc_macro2,
			id_stream,
			..
		} = self;
		let s = lit.to_string();
		qquote! {
			{
				let ts: #path_proc_macro2::TokenStream = #s.parse().expect("Invalid literal str");
				#id_stream.extend(ts.into_iter().map(|mut t| {
					t.set_span(#span);
					t
				}));
			}
		}
	}

	fn emit_punct(&self, punct: &Punct) -> TokenStream2 {
		let Self {
			span,
			path_proc_macro2,
			path_quote,
			id_stream,
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
			}, &mut #id_stream);
		}
	}

	fn emit_group(&self, delim: &Delimiter, inner: TokenStream2) -> TokenStream2 {
		let Self {
			span,
			path_proc_macro2,
			path_core,
			id_stream,
			..
		} = self;
		let delim = match delim {
			Delimiter::Parenthesis => qquote! { #path_proc_macro2::Delimiter::Parenthesis },
			Delimiter::Brace => qquote! { #path_proc_macro2::Delimiter::Brace },
			Delimiter::Bracket => qquote! { #path_proc_macro2::Delimiter::Bracket },
			Delimiter::None => qquote! { #path_proc_macro2::Delimiter::None },
		};
		qquote! {
			#id_stream.extend(
				#path_core::option::Option::Some(
					#path_proc_macro2::TokenTree::Group(
						{
							let mut g = #path_proc_macro2::Group::new(#delim, {
								let mut #id_stream = #path_proc_macro2::TokenStream::new();
								{ #inner }
								#id_stream
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
		conditional: TokenStream2,
		input: VecDeque<TokenTree>,
		vals: &mut HashSet<Ident>,
		sep: Option<Punct>,
		inline_expr_dict: &mut Vec<(Ident, TokenStream2, Span)>,
	) -> TokenStream2 {
		fn eat_terminator(input: &mut VecDeque<TokenTree>) -> bool {
			match (input.pop_front(), input.pop_front(), input.pop_front()) {
				(Some(TokenTree::Punct(p1)), Some(TokenTree::Punct(p2)), None)
					if p1.as_char() == '.'
						&& p2.as_char() == '.' && p1.spacing() == Spacing::Joint
						&& p2.spacing() == Spacing::Alone =>
				{
					return true;
				}
				(Some(tt1), Some(tt2), Some(tt3)) => {
					input.push_front(tt3);
					input.push_front(tt2);
					input.push_front(tt1);
				}
				(Some(tt1), Some(tt2), None) => {
					input.push_front(tt2);
					input.push_front(tt1);
				}
				(Some(tt1), None, None) => {
					input.push_front(tt1);
				}
				_ => (),
			}
			false
		}
		fn eat_comma(input: &mut VecDeque<TokenTree>) -> bool {
			match input.pop_front() {
				Some(TokenTree::Punct(p))
					if p.as_char() == ',' && p.spacing() == Spacing::Alone =>
				{
					true
				}
				Some(tt) => {
					input.push_front(tt);
					false
				}
				_ => false,
			}
		}
		fn collect_ident(input: &mut VecDeque<TokenTree>) -> Result<Vec<Ident>, ()> {
			fn collect_paren_stream(
				mut input: VecDeque<TokenTree>,
				ret: &mut Vec<Ident>,
			) -> Result<(), ()> {
				while input.len() > 0 && !eat_terminator(&mut input) {
					ret.extend(collect_ident(&mut input)?);
					if input.len() > 0 && !eat_comma(&mut input) {
						return Err(());
					}
				}
				Ok(())
			}
			let mut ret = Vec::new();
			match (input.pop_front(), input.pop_front()) {
				// Parse `let (..) = ..`
				(Some(TokenTree::Group(g)), opt) if g.delimiter() == Delimiter::Parenthesis => {
					collect_paren_stream(g.stream().into_iter().collect(), &mut ret)?;
					if let Some(tt1) = opt {
						input.push_front(tt1);
					}
					Ok(ret)
				}
				// Parse `let Ident ( .. ) = ..`
				(Some(TokenTree::Ident(_)), Some(TokenTree::Group(g)))
					if g.delimiter() == Delimiter::Parenthesis =>
				{
					collect_paren_stream(g.stream().into_iter().collect(), &mut ret)?;
					Ok(ret)
				}
				// Parse `let Ident { key: value, value2, ... } = ..`
				(Some(TokenTree::Ident(_)), Some(TokenTree::Group(g)))
					if g.delimiter() == Delimiter::Brace =>
				{
					let mut inner: VecDeque<TokenTree> = g.stream().into_iter().collect();
					while inner.len() > 0 && !eat_terminator(&mut inner) {
						match inner.pop_front().unwrap() {
							TokenTree::Ident(key) => match (inner.pop_front(), inner.pop_front()) {
								(Some(TokenTree::Punct(colon)), Some(TokenTree::Ident(value)))
									if colon.as_char() == ':'
										&& colon.spacing() == Spacing::Alone =>
								{
									ret.push(value);
								}
								(item1, item2) => {
									if let Some(tt2) = item2 {
										inner.push_front(tt2);
									}
									if let Some(tt1) = item1 {
										inner.push_front(tt1);
									}
									ret.push(key)
								}
							},
							_ => return Err(()),
						}
						if inner.len() > 0 && !eat_comma(&mut inner) {
							return Err(());
						}
					}
					Ok(ret)
				}
				// Parse `let ident = ..`
				(Some(TokenTree::Ident(id)), opt) => {
					ret.push(id);
					if let Some(tt1) = opt {
						input.push_front(tt1);
					}
					Ok(ret)
				}
				_ => Err(()),
			}
		}
		fn collect_ident_eq(input: &mut VecDeque<TokenTree>) -> Result<Vec<Ident>, ()> {
			let v = collect_ident(input)?;
			match input.pop_front() {
				Some(TokenTree::Punct(eq))
					if eq.as_char() == '=' && eq.spacing() == Spacing::Alone =>
				{
					Ok(v)
				}
				_ => Err(()),
			}
		}
		let mut cond: VecDeque<TokenTree> = conditional.clone().into_iter().collect();
		let (ids, cond) = match (cond.pop_front(), sep.is_some()) {
			(Some(TokenTree::Ident(id)), false) if &id.to_string() == "if" => {
				match cond.pop_front() {
					Some(TokenTree::Ident(id_let)) if &id_let.to_string() == "let" => (
						collect_ident_eq(&mut cond).expect("Bad format in if-let conditional"),
						conditional,
					),
					_ => (vec![], conditional),
				}
			}
			(Some(TokenTree::Ident(id)), false) if &id.to_string() == "else" => {
				if cond.len() == 0 {
					(vec![], conditional) // no more idents after `else`
				} else {
					panic!("Bad format in else conditional")
				}
			}
			(Some(TokenTree::Ident(id)), false) if id.to_string() == "let" => (
				collect_ident_eq(&mut cond).expect("Bad format in let binding"),
				qquote! {#conditional ;},
			),
			(Some(TokenTree::Ident(id)), _) if id.to_string() == "while" => {
				match cond.pop_front() {
					Some(TokenTree::Ident(id_let)) if &id_let.to_string() == "let" => (
						collect_ident_eq(&mut cond).expect("Bad format in while-let loop"),
						conditional,
					),
					_ => (vec![], conditional),
				}
			}
			(Some(TokenTree::Ident(id)), _) if id.to_string() == "for" => {
				match (collect_ident(&mut cond), cond.pop_front()) {
					(Ok(v), Some(TokenTree::Ident(id_in))) if &id_in.to_string() == "in" => {
						(v, conditional)
					}
					_ => panic!("Bad format in for loop"),
				}
			}
			_ => panic!("Bad format in conditional"),
		};
		let inner = self.parse_inner(input, vals, inline_expr_dict);
		for id in ids {
			vals.remove(&id);
		}
		if let Some(sep) = sep {
			let code_sep = self.emit_punct(&sep);
			let id_counter = &self.id_counter;
			qquote! {
				{
					let mut #id_counter = false;
					#cond {
						if #id_counter { #code_sep }
						#id_counter = true;
						#inner
					}
				}
			}
		} else {
			qquote! {
				#cond {
					#inner
				}
			}
		}
	}

	fn parse_iteration(
		&self,
		input: VecDeque<TokenTree>,
		vals: &mut HashSet<Ident>,
		sep: Option<Punct>,
		inline_expr_dict: &mut Vec<(Ident, TokenStream2, Span)>,
	) -> TokenStream2 {
		let Self {
			path_quote,
			id_repeat,
			..
		} = self;
		let mut inner_vals = HashSet::new();
		let inner_output = self.parse_inner(input, &mut inner_vals, inline_expr_dict);
		let code_sep = sep.map(|sep| self.emit_punct(&Punct::new(sep.as_char(), Spacing::Alone)));
		let val_nam = code_sep
			.as_ref()
			.map(|_| self.id_counter.clone())
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
				use #path_quote::Repeat as #id_repeat;
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
			path_proc_macro2,
			id_stream,
			..
		} = self;
		let mut hs = HashSet::new();
		let mut dict = Vec::new();
		let result = self.parse_inner(input.into_iter().collect(), &mut hs, &mut dict);
		let inline_vals_code =
			dict.into_iter()
				.fold(TokenStream2::new(), |acc, (id, inner, span)| {
					qquote_spanned! { span =>
						#acc
						let #id = { #inner };
					}
				});
		qquote! {
			{
				let mut #id_stream= #path_proc_macro2::TokenStream::new();
				#inline_vals_code
				{ #result }
				#id_stream
			}
		}
	}

	fn parse_inner(
		&self,
		mut input: VecDeque<TokenTree>,
		vals: &mut HashSet<Ident>,
		inline_expr_dict: &mut Vec<(Ident, TokenStream2, Span)>,
	) -> TokenStream2 {
		let Self {
			path_quote,
			id_stream,
			..
		} = self;
		let mut output = TokenStream2::new();
		while let Some(token) = input.pop_front() {
			match token {
				TokenTree::Group(group) => {
					let inner = group.stream().into_iter().collect();
					let result = self.parse_inner(inner, vals, inline_expr_dict);
					let result = self.emit_group(&group.delimiter(), result);
					output.append_all(result);
				}
				TokenTree::Punct(punct) => match (punct.as_char(), input.pop_front()) {
					// # val
					('#', Some(TokenTree::Ident(ident))) => {
						vals.insert(ident.clone());
						output.append_all(
							qquote! { <_ as #path_quote::ToTokens>::to_tokens(&#ident, &mut #id_stream); },
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
							let inline_expr_name =
								format!("{}{}", self.id_inline_expr, inline_expr_dict.len());
							let inline_expr_id = Ident::new(&inline_expr_name, group.span());
							output.append_all(qquote! {
								<_ as #path_quote::ToTokens>::to_tokens(&#inline_expr_id, &mut #id_stream);
							});
							vals.insert(inline_expr_id.clone());
							inline_expr_dict.push((
								inline_expr_id,
								group.stream().into_iter().collect(),
								group.span(),
							));
						} else {
							let stream = group.stream();
							output.append_all(qquote! {
								{ #stream }
							});
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
									inline_expr_dict,
								));
							}
							// # ( ... ) *
							Some(TokenTree::Punct(punct)) if punct.as_char() == '*' => output
								.append_all(self.parse_iteration(
									group.stream().into_iter().collect(),
									vals,
									None,
									inline_expr_dict,
								)),
							Some(TokenTree::Punct(punct0)) => match input.pop_front() {
								// # ( ... ) [SEP] *
								Some(TokenTree::Punct(punct1)) if punct1.as_char() == '*' => output
									.append_all(self.parse_iteration(
										group.stream().into_iter().collect(),
										vals,
										Some(punct0),
										inline_expr_dict,
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
										inline_expr_dict,
									));
								}
								o => {
									if let Some(o) = o {
										input.push_front(o);
									}
									input.push_front(TokenTree::Punct(punct0));
									input.push_front(TokenTree::Group(group));
									output.append_all(self.emit_punct(&punct));
								}
							},
							o => {
								if let Some(o) = o {
									input.push_front(o)
								}
								input.push_front(TokenTree::Group(group));
								output.append_all(self.emit_punct(&punct));
							}
						}
					}
					(_, o) => {
						if let Some(o) = o {
							input.push_front(o);
						}
						output.append_all(self.emit_punct(&punct));
					}
				},
				TokenTree::Ident(o) => output.append_all(self.emit_ident(&o)),
				TokenTree::Literal(o) => output.append_all(self.emit_literal(&o)),
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
