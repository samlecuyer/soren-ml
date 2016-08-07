module T = Types.Types

module Core = Map.Make(String)

let num_fun f = (T.Fn
    (function
    | [T.Number (Types.Int a); T.Number (Types.Int b)] -> T.Number (Types.Int (f a b))
    | _ -> raise (Invalid_argument "use ints")))

let print_fun = (T.Fn
	(function
	| any::_ -> print_endline (Printer.pr_str any); T.Nil
	| [] -> T.Nil))

let to_list = T.Fn (fun l -> T.List l)

let is_list = T.Fn
    (function
    | (T.List l)::_ -> (T.Bool true)
    | _ -> (T.Bool false))

let ist_empty = T.Fn
    (function
    | (T.List [])::_ -> (T.Bool true)
    | (T.Vector [])::_ -> (T.Bool true)
    | _ -> (T.Bool false))

let count = (T.Fn
    (function
    | (T.List l)::_ -> (T.Number (Types.Int (List.length l)))
    | (T.Vector l)::_ -> (T.Number (Types.Int (List.length l)))
    | _ -> (T.Number (Types.Int 0))))

let is_eq = T.Fn
    (function
    | [a; b] -> T.Bool (Types.is_equal a b)
    | _ -> raise (Invalid_argument "use ints"))


let ns = Core.(empty
	|> add "+" (num_fun ( + ))
	|> add "-" (num_fun ( - ))
	|> add "*" (num_fun ( * ))
	|> add "/" (num_fun ( / ))
	|> add "prn" print_fun
	|> add "list" to_list
	|> add "is_list?" is_list
	|> add "empty?" ist_empty
	|> add "count" count
	|> add "=" is_eq
)