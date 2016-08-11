open Types
open Numeric

let rec pr_num = function
    | Int i -> string_of_int i
    | Float f -> string_of_float f
    | Ratio (n, d) -> Printf.sprintf "%d/%d" n d
    | Complex (r, i) -> Printf.sprintf "%s+%si" (pr_num r) (pr_num i)

let rec pr_map k d accum readably = (pr_str k readably)::(pr_str d readably)::accum

and pr_str_ast str print_readably =
	(* if print_readably then
		str |> (Str.global_replace (Str.regexp "\"") "\\\"")
	else *)
		str

and pr_str ast print_readably =
	let prr_str = (fun ast -> pr_str ast print_readably) in
	let prr_map = (fun k d acc -> pr_map k d acc print_readably) in
	match ast with
    | Types.List l   -> Printf.sprintf "(%s)" (String.concat " " (List.map prr_str l))
    | Types.Vector l   -> Printf.sprintf "[%s]" (String.concat " " (List.map prr_str l))
    | Types.Map m   -> Printf.sprintf "{%s}" (String.concat " " (SMap.fold prr_map m []))
    | Types.String s -> Printf.sprintf  "%s"(pr_str_ast s print_readably)
    | Types.Number n -> pr_num n
    | Types.Symbol s -> s
    | Types.Keyword s -> s
    | Types.Bool b   -> string_of_bool b
    | Types.Nil      -> "nil"
    | Types.Fn _     -> "#<function>"