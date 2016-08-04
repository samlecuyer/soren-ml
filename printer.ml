open Types

let rec pr_num = function
    | Int i -> string_of_int i
    | Float f -> string_of_float f

let rec pr_str = function
    | List l   -> Printf.sprintf "(%s)" (String.concat " " (List.map pr_str l))
    | String s -> Printf.sprintf "<str>\"%s\"" s
    | Number n -> pr_num n
    | Symbol s -> Printf.sprintf "<sym>%s" s
    | Bool b   -> string_of_bool b
    | Nil      -> "nil"