use template_quote::quote;

fn main() {
    // An empty repetition group has no interpolation variable either.
    let _ = quote! { #()* };
}
