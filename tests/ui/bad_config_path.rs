use template_quote::quote_configured;

fn main() {
    // Path-valued config keys only accept a path.
    let _ = quote_configured! { { quote: 1 } => x };
}
