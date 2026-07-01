use template_quote::quote;

fn main() {
    // Malformed `for` header (missing `in <expr>`).
    let _ = quote! { #(for bad) { x } };
}
