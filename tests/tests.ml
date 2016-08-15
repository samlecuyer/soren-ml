open OUnit2

let suite = "All Tests" >::: [
	Reader_tests.suite;
    Eval_tests.suite;
]

let _ = run_test_tt_main suite