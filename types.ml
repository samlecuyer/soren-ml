
type number_tower =
    | Int of int
    | Float of float

type t =
    | List   of t list
    | String of string
    | Number of number_tower
    | Symbol of string
    | Bool   of bool
    | Nil

exception SyntaxError of string
