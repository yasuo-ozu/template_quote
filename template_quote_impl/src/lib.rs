extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::{Delimiter, Group, Literal, Punct, Spacing, Span, TokenTree};
use proc_macro_error::{abort, proc_macro_error};
use quote::{quote as qquote, quote_spanned as qquote_spanned, TokenStreamExt};
use std::collections::{HashSet, VecDeque};
use syn::{parse_quote, Expr, Ident, Path, Token};

/// The parser for `quote!` macro.
///
/// this struct has some global configs to be initialized using
/// `Default::default()` or `syn::parse::Parse::parse()`. User can specify
/// configs using `quote_configured!` macro.
struct ParseEnvironment {
	span: Expr,
	path_proc_macro2: Path,
	path_quote: Path,
	path_core: Path,
	id_stream: Ident,
	id_counter: Ident,
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
					return Err(input.error(format!("Bad config name: {}", key)));
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
			id_stream: Ident::new("__template_quote_stream", Span::mixed_site()),
			id_counter: Ident::new("__template_quote_idcnt", Span::mixed_site()),
		}
	}
}

fn eat_terminator(input: &mut VecDeque<TokenTree>) -> bool {
	// Recognize a `..` rest pattern (two joint `.` puncts) wherever it appears
	// in a tuple/slice/struct pattern, not only when it is the final element.
	match (input.pop_front(), input.pop_front()) {
		(Some(TokenTree::Punct(p1)), Some(TokenTree::Punct(p2)))
			if p1.as_char() == '.' && p2.as_char() == '.' && p1.spacing() == Spacing::Joint =>
		{
			true
		}
		(a, b) => {
			if let Some(b) = b {
				input.push_front(b);
			}
			if let Some(a) = a {
				input.push_front(a);
			}
			false
		}
	}
}
fn eat_comma(input: &mut VecDeque<TokenTree>) -> bool {
	match input.pop_front() {
		Some(TokenTree::Punct(p)) if p.as_char() == ',' && p.spacing() == Spacing::Alone => true,
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
		while !input.is_empty() {
			// A `..` rest pattern binds nothing; consume a following separator
			// so remaining elements (e.g. `[a, .., b]`) are still parsed.
			if eat_terminator(&mut input) {
				if !input.is_empty() && !eat_comma(&mut input) {
					return Err(());
				}
				continue;
			}
			ret.extend(collect_ident(&mut input)?);
			if !input.is_empty() && !eat_comma(&mut input) {
				return Err(());
			}
		}
		Ok(())
	}
	let mut ret = Vec::new();
	// Skip an optional leading `::` (absolute path in e.g. `::core::..::Some(x)`).
	if matches!(
		(input.front(), input.get(1)),
		(Some(TokenTree::Punct(p1)), Some(TokenTree::Punct(p2)))
			if p1.as_char() == ':' && p2.as_char() == ':' && p1.spacing() == Spacing::Joint
	) {
		input.pop_front();
		input.pop_front();
	}
	// Skip binding modifiers: `ref`, `mut`, `ref mut`.
	while matches!(input.front(), Some(TokenTree::Ident(id)) if id == "ref" || id == "mut") {
		input.pop_front();
	}
	match input.pop_front() {
		// Parse `let (..) = ..`
		Some(TokenTree::Group(g))
			if (g.delimiter() == Delimiter::Parenthesis || g.delimiter() == Delimiter::Bracket) =>
		{
			collect_paren_stream(g.stream().into_iter().collect(), &mut ret)?;
			Ok(ret)
		}
		Some(TokenTree::Ident(id)) => {
			loop {
				match input.pop_front() {
					Some(TokenTree::Punct(colon))
						if colon.as_char() == ':' && colon.spacing() == Spacing::Joint =>
					{
						if matches!(
						input.pop_front(),
						Some(TokenTree::Punct(colon)) if colon.as_char() == ':' && colon.spacing() == Spacing::Alone
						) && matches!(input.pop_front(), Some(TokenTree::Ident(_)))
						{
							continue;
						} else {
							return Err(());
						}
					}
					Some(o) => input.push_front(o),
					None => (),
				}
				break;
			}
			match input.pop_front() {
				// Parse `let Ident ( .. ) = ..`
				Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Parenthesis => {
					collect_paren_stream(g.stream().into_iter().collect(), &mut ret)?;
					Ok(ret)
				}
				// Parse `let Ident { key: value, value2, ... } = ..`
				Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Brace => {
					let mut inner: VecDeque<TokenTree> = g.stream().into_iter().collect();
					while !inner.is_empty() && !eat_terminator(&mut inner) {
						// Skip `ref`/`mut` modifiers on shorthand fields.
						while matches!(inner.front(), Some(TokenTree::Ident(id)) if id == "ref" || id == "mut")
						{
							inner.pop_front();
						}
						match inner.pop_front() {
							Some(TokenTree::Ident(key)) => {
								// `key: <pattern>` (recurse into the sub-pattern, so
								// non-ident values like `a: (x, y)` work) or the
								// field-shorthand `key`.
								if matches!(inner.front(), Some(TokenTree::Punct(colon)) if colon.as_char() == ':')
								{
									inner.pop_front(); // the field separator `:`
									ret.extend(collect_ident(&mut inner)?);
								} else {
									ret.push(key);
								}
							}
							_ => return Err(()),
						}
						if !inner.is_empty() && !eat_comma(&mut inner) {
							return Err(());
						}
					}
					Ok(ret)
				}
				// Parse `let ident = ..`
				Some(o) => {
					input.push_front(o);
					ret.push(id);
					Ok(ret)
				}
				None => {
					ret.push(id);
					Ok(ret)
				}
			}
		}
		_ => Err(()),
	}
}

fn collect_ident_eq(input: &mut VecDeque<TokenTree>) -> Result<Vec<Ident>, ()> {
	let v = collect_ident(input)?;
	match input.pop_front() {
		Some(TokenTree::Punct(eq)) if eq.as_char() == '=' && eq.spacing() == Spacing::Alone => {
			Ok(v)
		}
		_ => Err(()),
	}
}

fn collect_punct_in_expr(mut stream: VecDeque<TokenTree>) -> (Vec<Ident>, VecDeque<TokenTree>) {
	let mut output = VecDeque::new();
	let mut ids = Vec::new();
	while let Some(tt) = stream.pop_front() {
		match tt {
			TokenTree::Punct(punct) if punct.as_char() == '#' => match stream.pop_front() {
				Some(TokenTree::Ident(id)) => {
					ids.push(id.clone());
					output.push_back(TokenTree::Ident(id));
				}
				Some(o) => {
					output.push_back(TokenTree::Punct(punct));
					output.push_back(o);
				}
				None => {
					output.push_back(TokenTree::Punct(punct));
				}
			},
			// Recurse into groups so that `#var` nested inside a group (e.g.
			// `(#v).iter()` or `matches!(#v, _)`) is interpolated just like at
			// the top level, instead of being left as a literal `# v`.
			TokenTree::Group(group) => {
				let (inner_ids, inner) =
					collect_punct_in_expr(group.stream().into_iter().collect());
				ids.extend(inner_ids);
				let mut new_group = Group::new(group.delimiter(), inner.into_iter().collect());
				new_group.set_span(group.span());
				output.push_back(TokenTree::Group(new_group));
			}
			o => output.push_back(o),
		}
	}
	(ids, output)
}

fn hash_will_be_processed(next: Option<&TokenTree>, rest: &VecDeque<TokenTree>) -> bool {
	match next {
		Some(TokenTree::Ident(_)) => true,
		Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Brace => true,
		Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Parenthesis => {
			match (rest.front(), rest.get(1)) {
				(Some(TokenTree::Group(group2)), _) if group2.delimiter() == Delimiter::Brace => {
					true
				}
				(Some(TokenTree::Punct(punct)), _) if punct.as_char() == '*' => true,
				(Some(TokenTree::Punct(_)), Some(TokenTree::Punct(punct2)))
					if punct2.as_char() == '*' =>
				{
					true
				}
				(Some(TokenTree::Punct(_)), Some(TokenTree::Group(group2)))
					if group2.delimiter() == Delimiter::Brace =>
				{
					true
				}
				_ => false,
			}
		}
		_ => false,
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
		if let Some(s) = s.strip_prefix("r#") {
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

	fn emit_punct_with_spacing(&self, punct: &Punct, spacing: Spacing) -> TokenStream2 {
		let mut p = Punct::new(punct.as_char(), spacing);
		p.set_span(punct.span());
		self.emit_punct(&p)
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
		fn parse_if_inner(
			mut cond: VecDeque<TokenTree>,
		) -> (Vec<Ident>, Vec<Ident>, Vec<TokenTree>) {
			let span_ts: TokenStream2 = cond.iter().cloned().collect();
			let mut bak: Vec<_> = cond.iter().cloned().collect();
			match cond.pop_front() {
				Some(TokenTree::Ident(id_let)) if id_let == "let" => {
					let removing_ids = collect_ident_eq(&mut cond).unwrap_or_else(|_| {
						abort!(span_ts, "malformed `if let` conditional in template")
					});
					bak.truncate(bak.len() - cond.len());
					let (appending_ids, rem) = collect_punct_in_expr(cond);
					bak.extend(rem);
					(removing_ids, appending_ids, bak)
				}
				Some(o) => {
					cond.push_front(o);
					bak.truncate(bak.len() - cond.len());
					let (appending_ids, rem) = collect_punct_in_expr(cond);
					bak.extend(rem);
					(vec![], appending_ids, bak)
				}
				None => abort!(span_ts, "empty `if` conditional in template"),
			}
		}
		let mut cond: VecDeque<TokenTree> = conditional.clone().into_iter().collect();
		let cond_len = cond.len();
		let (removing_ids, appending_ids, cond) = match (cond.pop_front(), sep.is_some()) {
			(Some(TokenTree::Ident(id)), false) if id == "if" => {
				let (removing_ids, appending_ids, rem) = parse_if_inner(cond);
				(
					removing_ids,
					appending_ids,
					Some(TokenTree::Ident(id)).into_iter().chain(rem).collect(),
				)
			}
			(Some(TokenTree::Ident(id)), false) if id == "else" => {
				match cond.pop_front() {
					// else if
					Some(TokenTree::Ident(id_if)) if id_if == "if" => {
						let (removing_ids, appending_ids, rem) = parse_if_inner(cond);
						(
							removing_ids,
							appending_ids,
							vec![TokenTree::Ident(id), TokenTree::Ident(id_if)]
								.into_iter()
								.chain(rem)
								.collect(),
						)
					}
					Some(_) => {
						abort!(
							conditional.clone(),
							"malformed `else` in template (expected `else` or `else if`)"
						)
					}
					None => (vec![], vec![], conditional),
				}
			}
			(Some(TokenTree::Ident(id)), false) if id == "let" => {
				let removing_ids = collect_ident_eq(&mut cond).unwrap_or_else(|_| {
					abort!(conditional.clone(), "malformed `let` binding in template")
				});
				let n = cond_len - cond.len();
				let (appending_ids, rem) = collect_punct_in_expr(cond);
				let conditional: TokenStream2 =
					conditional.clone().into_iter().take(n).chain(rem).collect();
				(removing_ids, appending_ids, qquote! {#conditional ;})
			}
			(Some(TokenTree::Ident(id)), _) if id == "while" => match cond.pop_front() {
				Some(TokenTree::Ident(id_let)) if id_let == "let" => {
					let removing_ids = collect_ident_eq(&mut cond).unwrap_or_else(|_| {
						abort!(
							conditional.clone(),
							"malformed `while let` loop in template"
						)
					});
					let n = cond_len - cond.len();
					let (appending_ids, rem) = collect_punct_in_expr(cond);
					let conditional: TokenStream2 =
						conditional.clone().into_iter().take(n).chain(rem).collect();
					(removing_ids, appending_ids, conditional)
				}
				Some(o) => {
					cond.push_front(o);
					let n = cond_len - cond.len();
					let (appending_ids, rem) = collect_punct_in_expr(cond);
					let conditional: TokenStream2 =
						conditional.clone().into_iter().take(n).chain(rem).collect();
					(vec![], appending_ids, conditional)
				}
				None => abort!(conditional.clone(), "empty `while` conditional in template"),
			},
			(Some(TokenTree::Ident(id)), _) if id == "for" => {
				match (collect_ident(&mut cond), cond.pop_front()) {
					(Ok(v), Some(TokenTree::Ident(id_in))) if id_in == "in" => {
						let n = cond_len - cond.len();
						let (appending_ids, rem) = collect_punct_in_expr(cond);
						let conditional: TokenStream2 =
							conditional.clone().into_iter().take(n).chain(rem).collect();
						(v, appending_ids, conditional)
					}
					_ => abort!(
						conditional.clone(),
						"malformed `for` loop in template (expected `for <pat> in <expr>`)"
					),
				}
			}
			_ => abort!(
				conditional.clone(),
				"malformed conditional in template (expected `if`, `else`, `let`, `while` or `for`)"
			),
		};
		let inner = self.parse_inner(input, vals, inline_expr_dict);
		for id in removing_ids {
			vals.remove(&id);
		}
		for id in appending_ids {
			vals.insert(id);
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
			path_core,
			..
		} = self;
		let debug_str = input
			.iter()
			.take(6)
			.map(|item| match item {
				TokenTree::Group(g) => match g.delimiter() {
					Delimiter::Parenthesis => "( .. )".to_owned(),
					Delimiter::Brace => "{ .. }".to_owned(),
					Delimiter::Bracket => "[ .. ]".to_owned(),
					Delimiter::None => "..".to_owned(),
				},
				_ => format!("{}", item),
			})
			.chain(if input.len() > 6 {
				Some("..".to_owned())
			} else {
				None
			})
			.collect::<Vec<_>>()
			.join(" ");
		// Snapshot the repetition-group tokens before `input` is consumed below,
		// so a diagnostic can point directly at the offending group.
		let group_tokens: TokenStream2 = input.iter().cloned().collect();
		let mut inner_vals = HashSet::new();
		let inner_output = self.parse_inner(input, &mut inner_vals, inline_expr_dict);
		let code_sep = sep.map(|sep| self.emit_punct(&Punct::new(sep.as_char(), Spacing::Alone)));
		let val_nam = code_sep
			.as_ref()
			.map(|_| self.id_counter.clone())
			.into_iter()
			.collect::<Vec<_>>();
		vals.extend(inner_vals.iter().cloned());
		let vars: Vec<Ident> = inner_vals.iter().cloned().collect();
		if vars.is_empty() {
			abort!(
				group_tokens,
				"Repetition group `#( {} )*` contains no interpolation variable (`#var`)", debug_str;
				note = "a repetition needs at least one `#var` inside it to drive the iteration"
			);
		}
		// Fresh call-site helper idents per distinct interpolation var: the bound
		// iterator and its runtime "is iterable" flag.
		let it_idents: Vec<Ident> = (0..vars.len())
			.map(|i| Ident::new(&format!("__template_quote_it_{}", i), Span::mixed_site()))
			.collect();
		let m_idents: Vec<Ident> = (0..vars.len())
			.map(|i| Ident::new(&format!("__template_quote_m_{}", i), Span::mixed_site()))
			.collect();
		let bindings = vars
			.iter()
			.zip(&it_idents)
			.zip(&m_idents)
			.map(|((var, it), m)| {
				qquote! {
					#[allow(unused_mut)]
					let (mut #it, #m) = (#var).__template_quote_into_iter();
				}
			});
		// Panic at runtime (preserving historical behavior) if no var is iterable.
		let checker = m_idents
			.iter()
			.fold(qquote! {false}, |acc, m| qquote! {#acc || #m});
		// Left-nested tuple pattern of the user idents matching the left-nested
		// `zip` chain of the bound iterators.
		let (first_var, rest_vars) = vars.split_first().unwrap();
		let (first_it, rest_it) = it_idents.split_first().unwrap();
		let idents_in_tuple = rest_vars
			.iter()
			.fold(qquote! {#first_var}, |prev, next| qquote! {(#prev, #next)});
		let zipped = rest_it
			.iter()
			.fold(qquote! {#first_it}, |prev, next| qquote! {#prev.zip(#next)});
		qquote! {
			{
				#(let mut #val_nam = false;)*
				#[allow(unused_imports)]
				use #path_quote::__private::ext::{
					RepAsIteratorExt as _, RepIteratorExt as _, RepToTokensExt as _,
				};
				#(#bindings)*
				if !(#checker) {
					#path_core::panic!("Cannot iterate the group: #( {} )*", #debug_str);
				}
				for #idents_in_tuple in #zipped {
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
						let inner = group.stream().into_iter().collect::<VecDeque<_>>();
						if inner.is_empty() {
							// `#{}` expands to nothing.
						} else {
							// The inner block is a statement block when it ends with
							// `;`, otherwise it is an expression to interpolate.
							let is_expr = !matches!(
								inner.back(),
								Some(TokenTree::Punct(p)) if p.as_char() == ';'
							);
							let (appending_ids, inner) = collect_punct_in_expr(inner);
							for id in appending_ids {
								vals.insert(id);
							}
							let stream: TokenStream2 = inner.into_iter().collect();
							if is_expr {
								output.append_all(qquote! {
									<_ as #path_quote::ToTokens>::to_tokens(&{
										#stream
									}, &mut #id_stream);
								});
							} else {
								output.append_all(qquote! {
									{ #stream }
								});
							}
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
									group.stream(),
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
										group.stream(),
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
						// A punct written `Joint` immediately before an interpolation
						// that will be processed must be downgraded to `Alone`: the
						// macro cannot know what the interpolation expands to, and a
						// `Joint` punct would silently fuse with a leading punct in
						// the expansion (e.g. `-` + `>` -> `->`). This matches the
						// spacing behaviour of the upstream `quote!` macro.
						let adjust_joint_before_hash = punct.spacing() == Spacing::Joint
							&& matches!(o, Some(TokenTree::Punct(ref p)) if p.as_char() == '#')
							&& hash_will_be_processed(input.front(), &input);
						if let Some(o) = o {
							input.push_front(o);
						}
						if adjust_joint_before_hash {
							output.append_all(self.emit_punct_with_spacing(&punct, Spacing::Alone));
						} else {
							output.append_all(self.emit_punct(&punct));
						}
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
#[proc_macro_error]
pub fn quote(input: TokenStream) -> TokenStream {
	let env: ParseEnvironment = Default::default();
	env.parse(input.into()).into()
}

#[proc_macro]
#[proc_macro_error]
pub fn quote_configured(input: TokenStream) -> TokenStream {
	let input0: TokenStream2 = input.into();
	let mut input = VecDeque::new();
	input.extend(input0.into_iter());
	let env: ParseEnvironment = match input.pop_front() {
		Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Brace => syn::parse2(g.stream())
			.unwrap_or_else(|e| abort!(g, "malformed `quote_configured!` config block: {}", e)),
		Some(other) => abort!(
			other,
			"`quote_configured!` expects a `{{ key: path, ... }}` config block first"
		),
		None => abort!(
			Span::call_site(),
			"`quote_configured!` expects a `{{ key: path, ... }} => ...` invocation"
		),
	};
	match input.pop_front() {
		Some(TokenTree::Punct(p)) if p.as_char() == '=' && p.spacing() == Spacing::Joint => (),
		Some(other) => abort!(
			other,
			"`quote_configured!` expects `=>` after the config block"
		),
		None => abort!(
			Span::call_site(),
			"`quote_configured!` expects `=>` after the config block"
		),
	}
	match input.pop_front() {
		Some(TokenTree::Punct(p)) if p.as_char() == '>' && p.spacing() == Spacing::Alone => (),
		Some(other) => abort!(
			other,
			"`quote_configured!` expects `=>` after the config block"
		),
		None => abort!(
			Span::call_site(),
			"`quote_configured!` expects `=>` after the config block"
		),
	}
	let mut stream = TokenStream2::new();
	stream.extend(input);
	env.parse(stream).into()
}

#[proc_macro]
#[proc_macro_error]
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
			None => abort!(
				Span::call_site(),
				"`quote_spanned!` expects `<span-expr> => <tokens>` (no `=>` found)"
			),
		}
	}
	let mut env: ParseEnvironment = Default::default();
	let span_tokens = span.clone();
	env.span = syn::parse2(span).unwrap_or_else(|e| {
		abort!(
			span_tokens,
			"the `<span>` before `=>` in `quote_spanned!` must be an expression: {}",
			e
		)
	});
	let mut stream = TokenStream2::new();
	stream.extend(input);
	env.parse(stream).into()
}
