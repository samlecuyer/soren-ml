open Types

let rec pr_num = function
    | Int i -> string_of_int i
    | Float f -> string_of_float f

let rec pr_map k d accum = (pr_str k)::(pr_str d)::accum

and pr_str = function
    | Types.List l   -> Printf.sprintf "(%s)" (String.concat " " (List.map pr_str l))
    | Types.Vector l   -> Printf.sprintf "[%s]" (String.concat " " (List.map pr_str l))
    | Types.Map m   -> Printf.sprintf "{%s}" (String.concat " " (SMap.fold pr_map m []))
    | Types.String s -> Printf.sprintf "<str>\"%s\"" s
    | Types.Number n -> pr_num n
    | Types.Symbol s -> "<s>"^s
    | Types.Keyword s -> "<k>"^s
    | Types.Bool b   -> string_of_bool b
    | Types.Nil      -> "nil"
    | Types.Fn _     -> "<native>"