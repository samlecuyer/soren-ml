module T = Types.Types

module Core = Map.Make(String)

(* TODO: support the full numeric stack *)
let num_fun f = (T.Fn
    (function
    | [T.Number (Numeric.Int a); T.Number (Numeric.Int b)] -> T.Number (Numeric.Int (f a b))
    | _ -> raise (Invalid_argument "use ints")))

let num_bool_fun f = (T.Fn
    (function
    | [T.Number (Numeric.Int a); T.Number (Numeric.Int b)] -> T.Bool (f a b)
    | _ -> raise (Invalid_argument "use ints")))

let sn_unary f = (T.Fn
    (function
    | hd::_ -> f hd
    | [] -> T.Nil))

let sn_unary_pred f = (T.Fn
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

let sn_slurp = (T.Fn
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

let sn_str = (T.Fn
	(fun args ->
		let prr_str ast = Printer.pr_str ast false in
	 	T.String (String.concat " " (List.map prr_str args))))

let sn_list = T.Fn (fun l -> T.List l)

let sn_empty = T.Fn
    (function
    | (T.List [])::_ -> (T.Bool true)
    | (T.Vector [])::_ -> (T.Bool true)
    | _ -> (T.Bool false))

let sn_count = (T.Fn
    (function
    | (T.List l)::_ -> (T.Number (Numeric.Int (List.length l)))
    | (T.Vector l)::_ -> (T.Number (Numeric.Int (List.length l)))
    | _ -> (T.Number (Numeric.Int 0))))

let sn_cons = (T.Fn Types.cons)

let sn_concat = (T.Fn Types.concat)


let sn_equal = T.Fn
    (function
    | [a; b] -> T.Bool (Types.is_equal a b)
    | _ -> raise (Invalid_argument "use ints"))

let ns = Core.(empty
	(* simple math functions *)
	|> add "+" (num_fun ( + ))
	|> add "-" (num_fun ( - ))
	|> add "*" (num_fun ( * ))
	|> add "/" (num_fun ( / ))
	(* comparisons *)
	|> add ">"  (num_bool_fun ( > ))
	|> add ">=" (num_bool_fun ( >= ))
	|> add "<"  (num_bool_fun ( < ))
	|> add "<=" (num_bool_fun ( <= ))
	(* other core functions *)
	|> add "prn"      (sn_unary sn_print)
	|> add "str"      sn_str
	|> add "list"     sn_list
	|> add "list?"    (sn_unary_pred Types.is_list)
	|> add "empty?"   sn_empty
	|> add "count"    sn_count
	|> add "="        sn_equal
    (* read-string *)
    |> add "read-string" (sn_unary sn_read_str)
    |> add "slurp"    sn_slurp
    (* atoms *)
    |> add "atom"     (sn_unary sn_atom)
    |> add "atom?"    (sn_unary_pred Types.is_atom)
    |> add "cons"     sn_cons
    |> add "concat"   sn_concat
)