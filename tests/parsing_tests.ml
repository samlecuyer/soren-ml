open OUnit2
module T = Types.Types

let test_parse_numbers _ =
	assert_equal ~msg:"should parse zero" 
		(T.Number (Types.Int 0)) (Reader.read_str "0");
	assert_equal ~msg:"should parse zero." 
		(T.Number (Types.Float 0.)) (Reader.read_str "0.0");
	assert_equal ~msg:"should parse ints" 
		(T.Number (Types.Int 12)) (Reader.read_str "12");
	assert_equal ~msg:"should parse negative numbers" 
		(T.Number (Types.Int (-12))) (Reader.read_str "-12");
	assert_equal ~msg:"should parse exponent numbers" 
		(T.Number (Types.Float 130000.)) (Reader.read_str "1.3e5");
		assert_equal ~msg:"should parse exponent numbers" 
		(T.Number (Types.Float (-0.000013))) (Reader.read_str "-1.3e-5")

let test_parse_lists _ =
	assert_equal ~msg:"an empty list" 
		(T.List []) (Reader.read_str "()");
	assert_raises ~msg:"unbalanced parens" 
		(Types.SyntaxError "could not parse") (Reader.read_str "(5 6)");
	assert_equal ~msg:"a list containing a list" 
		(T.List [T.List []; T.List []; T.List []]) (Reader.read_str "(()()())");
	assert_equal ~msg:"a list containing atoms" 
		(T.List [T.Number (Types.Int 5); T.Number (Types.Int 6)]) (Reader.read_str "(5 6)")

let test_parse_vectors _ =
	assert_equal ~msg:"an empty vector" 
		(T.Vector []) (Reader.read_str "[]");
	assert_equal ~msg:"a list containing a list" 
		(T.Vector [T.List []; T.List []; T.List []]) (Reader.read_str "[()()()]");
	assert_equal ~msg:"a list containing atoms" 
		(T.Vector [T.Number (Types.Int 5); T.Number (Types.Int 6)]) (Reader.read_str "[5 6]")

let suite = "Parsing Literals" >:::[
	"test parsing numbers" >:: test_parse_numbers;
	"test parsing lists" >:: test_parse_lists;
	"test parsing vectors" >:: test_parse_vectors;
]