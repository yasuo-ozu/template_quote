use template_quote::quote_spanned;

fn main() {
    // `quote_spanned!` requires `<span-expr> => <tokens>`; there is no `=>`.
    let _ = quote_spanned! { no arrow here };
}
