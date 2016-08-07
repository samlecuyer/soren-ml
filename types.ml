
exception SyntaxError of string

type number_tower =
    | Int of int
    | Float of float

module rec Types : sig
type t =
    | List   of t list
    | Vector of t list
    | Map    of t SMap.t
    | String of string
    | Number of number_tower
    | Symbol of string
    | Keyword of string
    | Bool   of bool
    | Fn     of (t list -> t)
    | Nil
end = Types 
and Value : sig
	type t = Types.t
    val compare : t -> t -> int
end = struct
	type t = Types.t
	let compare = Pervasives.compare
end
and SMap : Map.S with type key = Value.t = Map.Make(Value)

type t = Value.t

let to_bool = function
    | Types.Nil | Types.Bool false -> Types.Bool false
    | _ -> Types.Bool true

let rec is_equal a b =
	match (a, b) with
	| (Types.List a, Types.List b) -> is_list_equal a b
	| _ -> a = b

and is_list_equal a b =
	List.length a = List.length b && List.for_all2 is_equal a b