open OUnit2
module T = Types.Types
module N = Numeric

let printer o = Printer.pr_str o false

let test_parse_numbers _ =
	assert_equal ~printer:printer
		~msg:"should parse zero"
		(T.Number (N.Int 0)) (Reader.read_str "0");
	assert_equal ~printer:printer
		~msg:"should parse zero."
		(T.Number (N.Float 0.)) (Reader.read_str "0.0");
	assert_equal ~printer:printer
		~msg:"should parse ints" 
		(T.Number (N.Int 12)) (Reader.read_str "12");
	assert_equal ~printer:printer
		~msg:"should parse negative numbers" 
		(T.Number (N.Int (-12))) (Reader.read_str "-12");
	assert_equal ~printer:printer
		~msg:"should parse exponent numbers" 
		(T.Number (N.Float 130000.)) (Reader.read_str "1.3e5");
		assert_equal ~msg:"should parse exponent numbers" 
		(T.Number (N.Float (-0.000013))) (Reader.read_str "-1.3e-5")

let test_parse_lists _ =
	assert_equal ~printer:printer
		~msg:"an empty list"
		(T.List []) (Reader.read_str "()");
	assert_raises 
		~msg:"unbalanced parens" 
		(Types.SyntaxError "unexpected EOF") (fun _ -> Reader.read_str "(5 6");
	assert_equal ~printer:printer
		~msg:"a list containing a list" 
		(T.List [T.List []; T.List []; T.List []]) (Reader.read_str "(()()())");
	assert_equal ~printer:printer
		~msg:"a list containing atoms" 
		(T.List [T.Number (N.Int 5); T.Number (N.Int 6)]) (Reader.read_str "(5 6)")

let test_parse_vectors _ =
	assert_equal ~printer:printer
		~msg:"an empty vector" 
		(T.Vector []) (Reader.read_str "[]");
	assert_equal ~printer:printer
		~msg:"a vector containing a list" 
		(T.Vector [T.List []; T.List []; T.List []]) (Reader.read_str "[()()()]");
	assert_equal ~printer:printer
		~msg:"a vector containing atoms" 
		(T.Vector [T.Number (N.Int 5); T.Number (N.Int 6)]) (Reader.read_str "[5 6]")

let test_parse_maps _ =
	assert_equal ~printer:printer
		~msg:"an empty map" 
		(Types.map Types.SnMap.empty) (Reader.read_str "{}");
	assert_equal ~printer:printer
		~msg:"a map of keywords" 
		(Types.map Types.SnMap.(empty
			|> add (Types.keyword ":a") (T.Number (N.Int 1))
			|> add (Types.keyword ":b") (T.Number (N.Int 2))
		)) 
		(Reader.read_str "{:a 1 :b 2}");
	assert_equal ~printer:printer
		~msg:"a map of just about anything" 
		(Types.map Types.SnMap.(empty
			|> add (T.Bool true) (Types.symbol "keyword")
			|> add (T.Number (N.Float 5.)) (T.Vector [])
		)) 
		(Reader.read_str "{true keyword 5.0 []}");
	assert_raises
		~msg:"odd number of elements" 
		(Types.SyntaxError "maps need an even number of keys")
		(fun _ -> Reader.read_str "{:a 1 :b}")

let test_parse_forms _ =
	assert_equal ~printer:printer
		~msg:"should parse keywords"
		(Types.keyword ":atom") (Reader.read_str ":atom");
	assert_equal ~printer:printer
		~msg:"should parse symbols with ?"
		(Types.symbol "symbol?") (Reader.read_str "symbol?");
	assert_equal ~printer:printer
		~msg:"should parse booleans: true"
		(T.Bool true) (Reader.read_str " true ");
	assert_equal ~printer:printer
		~msg:"should parse booleans: false"
		(T.Bool false) (Reader.read_str " false ");
	assert_equal ~printer:printer
		~msg:"should parse nil"
		(T.Nil) (Reader.read_str " nil ")

let suite = "Reader Literals" >:::[
	"test parsing numbers" >:: test_parse_numbers;
	"test parsing lists" >:: test_parse_lists;
	"test parsing vectors" >:: test_parse_vectors;
	"test parsing maps" >:: test_parse_maps;
	"test parsing forms" >:: test_parse_forms;
]