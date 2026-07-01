//! Compile-fail (UI) tests for the macros' diagnostics. The expected output
//! lives in the sibling `tests/ui/*.stderr` files; regenerate them after an
//! intentional message change with `TRYBUILD=overwrite cargo test --test ui`.
//!
//! Note: `.stderr` rendering is rustc-version-sensitive, so these are gated to
//! run only under the toolchain they were captured on (set `RUN_UI_TESTS=1`).
#[test]
fn ui() {
	if std::env::var_os("RUN_UI_TESTS").is_none() {
		eprintln!("skipping UI tests; set RUN_UI_TESTS=1 to run (rustc-version sensitive)");
		return;
	}
	let t = trybuild::TestCases::new();
	t.compile_fail("tests/ui/*.rs");
}
