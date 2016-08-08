module T = Types.Types

module Core = Map.Make(String)

(* TODO: support the full numeric stack *)
let num_fun f = (T.Fn
    (function
    | [T.Number (Types.Int a); T.Number (Types.Int b)] -> T.Number (Types.Int (f a b))
    | _ -> raise (Invalid_argument "use ints")))

let num_bool_fun f = (T.Fn
    (function
    | [T.Number (Types.Int a); T.Number (Types.Int b)] -> T.Bool (f a b)
    | _ -> raise (Invalid_argument "use ints")))

let sn_print = (T.Fn
	(function
	| any::_ -> print_endline (Printer.pr_str any true); T.Nil
	| [] -> T.Nil))

let sn_list = T.Fn (fun l -> T.List l)

let sn_is_list = T.Fn
    (function
    | (T.List l)::_ -> (T.Bool true)
    | _ -> (T.Bool false))

let sn_empty = T.Fn
    (function
    | (T.List [])::_ -> (T.Bool true)
    | (T.Vector [])::_ -> (T.Bool true)
    | _ -> (T.Bool false))

let sn_count = (T.Fn
    (function
    | (T.List l)::_ -> (T.Number (Types.Int (List.length l)))
    | (T.Vector l)::_ -> (T.Number (Types.Int (List.length l)))
    | _ -> (T.Number (Types.Int 0))))

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
	|> add "prn"      sn_print
	|> add "list"     sn_list
	|> add "is_list?" sn_is_list
	|> add "empty?"   sn_empty
	|> add "count"    sn_count
	|> add "="        sn_equal
)