module T = Types.Types

module Core = Map.Make(String)

(* TODO: support the full numeric stack *)
let rec num_fun f = (Types.fn
    (function
    | [T.Number (Numeric.Int a); T.Number (Numeric.Int b)] -> T.Number (Numeric.Int (f a b))
    | [] -> (num_fun f)
    | _ -> raise (Invalid_argument "use ints")))

let raise_num = function
    | T.Number n -> n
    | _ -> Numeric.NaN

let raised_num_fun f = (Types.fn
    (function
    | hd::rest ->
        T.Number (List.fold_left
            (fun acc x -> f acc (raise_num x))
            (raise_num hd)
            rest)
    | [] -> raise (Invalid_argument "numeric args must take a parameter")))


let num_bool_fun f = (Types.fn
    (function
    | [T.Number (Numeric.Int a); T.Number (Numeric.Int b)] -> T.Bool (f a b)
    | _ -> raise (Invalid_argument "use ints")))

let sn_unary f = (Types.fn
    (function
    | hd::_ -> f hd
    | [] -> T.Nil))

let sn_unary_pred f = (Types.fn
    (function
    | hd::_ -> T.Bool (f hd)
    | _ -> T.Bool false))

let sn_atom a = T.Atom (ref a)

let sn_print str =
	print_endline (Printer.pr_str str true);
    T.Nil

let sn_read_str = function
    | T.String s -> Reader.read_str s
    | _ -> T.Nil

let sn_slurp = (Types.fn
    (function
    | (T.String s)::_ -> 
        (try
            let file = open_in s in
            let size = in_channel_length file in
            let buf = Buffer.create size in
            Buffer.add_channel buf file size;
            T.String (Buffer.contents buf)
        with e -> raise e)
    | _ -> T.Nil))

let sn_str = (Types.fn
	(fun args ->
		let prr_str ast = Printer.pr_str ast false in
	 	T.String (String.concat " " (List.map prr_str args))))

let sn_list = Types.fn (fun l -> T.List l)

let sn_rest = Types.fn 
    (function
    | (T.List (_::rest))::_ -> Types.list rest
    | (T.List [])::_ -> Types.list []
    | _ -> raise (Invalid_argument "should take a list"))

let sn_first = Types.fn 
    (function
    | (T.List (hd::_))::_ -> hd
    | (T.List [])::_ -> T.Nil
    | _ -> raise (Invalid_argument "should take a list and number"))

let sn_nth = Types.fn 
    (function
    | [T.List lst; T.Number (Numeric.Int n)] -> 
        try List.nth lst n with _ -> raise (Invalid_argument "out of range")
    | _ -> raise (Invalid_argument "should take a list and number"))

let sn_empty = Types.fn
    (function
    | (T.List [])::_ -> (T.Bool true)
    | (T.Vector [])::_ -> (T.Bool true)
    | (T.Map m)::_ -> (T.Bool (Types.SnMap.is_empty m))
    | _ -> (T.Bool false))

let sn_bindings = Types.fn
    (function
    | (T.Map m)::_ -> Types.list (List.map (fun (k, v) -> k) (Types.SnMap.bindings m))
    | _ -> T.Nil)

let sn_map = Types.fn
    (function
    | T.Fn {value = f}::arg::_ ->
        (match arg with
        | T.List l -> Types.list (List.map (fun x -> f [x]) l)
        | T.Vector l -> Types.vector (List.map (fun x -> f [x]) l)
        | T.Map m -> Types.map (Types.SnMap.map (fun x -> f [x]) m)
        | _ -> raise (Invalid_argument "unimplementd"))
    | _ -> raise (Invalid_argument "map takes a function and a list"))

(* let sn_apply = Types.fn
    (function
    | T.Fn {value = f}::arg ->
        (match arg with
        | T.List l -> Types.list (List.map (fun x -> f [x]) l)
        | T.Vector l -> Types.vector (List.map (fun x -> f [x]) l)
        | T.Map m -> Types.map (Types.SnMap.map (fun x -> f [x]) m)
        | _ -> raise (Invalid_argument "unimplementd"))
    | _ -> raise (Invalid_argument "map takes a function and a list"))
 *)
let sn_count = (Types.fn
    (function
    | (T.List l)::_ -> (T.Number (Numeric.Int (List.length l)))
    | (T.Vector l)::_ -> (T.Number (Numeric.Int (List.length l)))
    | _ -> (T.Number (Numeric.Int 0))))

let sn_cons = (Types.fn Types.cons)
let sn_concat = (Types.fn Types.concat)

let sn_equal = Types.fn
    (function
    | [a; b] -> T.Bool (Types.is_equal a b)
    | _ -> raise (Invalid_argument "use ints"))

let ns = Core.(empty
	(* simple math functions *)
	|> add "+" (raised_num_fun Numeric.add)
	|> add "-" (num_fun ( - ))
	|> add "*" (num_fun ( * ))
	|> add "/" (num_fun ( / ))
	(* comparisons *)
	|> add ">"  (num_bool_fun ( > ))
	|> add ">=" (num_bool_fun ( >= ))
	|> add "<"  (num_bool_fun ( < ))
	|> add "<=" (num_bool_fun ( <= ))
    (* type predicates *)
    |> add "list?"    (sn_unary_pred Types.is_list)
    |> add "vector?"  (sn_unary_pred Types.is_vector)
    |> add "map?"     (sn_unary_pred Types.is_map)
    |> add "string?"  (sn_unary_pred Types.is_string)
    |> add "number?"  (sn_unary_pred Types.is_number)
    |> add "symbol?"  (sn_unary_pred Types.is_symbol)
    |> add "keyword?" (sn_unary_pred Types.is_keyword)
    (* |> add "fn?"      (sn_unary_pred Types.is_fn) *)
    |> add "atom?"    (sn_unary_pred Types.is_atom)
    |> add "nil?"     (sn_unary_pred Types.is_nil)
	(* other core functions *)
	|> add "prn"      (sn_unary sn_print)
	|> add "str"      sn_str
	|> add "list"     sn_list
	
	|> add "empty?"   sn_empty
	|> add "count"    sn_count
	|> add "="        sn_equal
    (* read-string *)
    |> add "read-string" (sn_unary sn_read_str)
    |> add "slurp"    sn_slurp
    (* atoms *)
    |> add "atom"     (sn_unary sn_atom)
    |> add "cons"     sn_cons
    |> add "concat"   sn_concat
    |> add "first"   sn_first
    |> add "rest"   sn_rest
    |> add "nth" sn_nth
    |> add "map" sn_map
    |> add "keys" sn_bindings
)