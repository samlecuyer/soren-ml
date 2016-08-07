open OUnit2

let suite = "All Tests" >::: [
	Parsing_tests.suite
]

let _ = run_test_tt_main suite