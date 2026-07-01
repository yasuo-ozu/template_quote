use template_quote::quote;

fn main() {
    // A repetition group with no interpolation variable cannot be driven.
    let _ = quote! { #(hello)* };
}
