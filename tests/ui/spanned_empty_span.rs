use template_quote::quote_spanned;

fn main() {
    // `quote_spanned!` requires a span expression before `=>`.
    let _ = quote_spanned! { => x };
}
