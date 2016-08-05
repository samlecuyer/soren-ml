
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

exception SyntaxError of string
