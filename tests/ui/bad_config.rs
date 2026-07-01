use template_quote::quote_configured;

fn main() {
    // The config block is not a valid `{ key: path, ... }` sequence.
    let _ = quote_configured! { { bogus } => x };
}
