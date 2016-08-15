
exception SyntaxError of string
exception RuntimeError of string

module rec Types : sig
    type 'a with_meta = {value : 'a; meta: t}   
    and t =
    | List   of t list
    | Vector of t list
    | Map    of t SnMap.t
    | String of string
    | Number of Numeric.t
    | Symbol of string
    | Keyword of string
    | Bool   of bool
    | Fn     of (t list -> t) with_meta
    | Atom   of t ref
    | Nil
end = Types 

and Value : sig
	type t = Types.t
    val compare : t -> t -> int
end = struct
	type t = Types.t
	let compare = Pervasives.compare
end

and SnMap : Map.S with type key = Value.t = Map.Make(Value)

type t = Value.t

let list    a = Types.List a
let vector  a = Types.Vector a
let map     a = Types.Map a
let symbol  a = Types.Symbol a
let keyword a = Types.Keyword a
let fn      f = Types.Fn { value = f; meta = Types.Nil }

let to_bool = function
    | Types.Nil | Types.Bool false -> false
    | _ -> true

let rec is_equal a b =
	match (a, b) with
	| (Types.List a, Types.List b) -> is_list_equal a b
	| (Types.Map a, Types.Map b) -> SnMap.equal is_equal a b
	| _ -> a = b

and is_list_equal a b =
	List.length a = List.length b && List.for_all2 is_equal a b

(* predicates *)
let is_list = function
    | Types.List _ -> true
    | _ -> false

(* predicates *)
let is_pair = function
    | Types.List [] -> false
    | Types.List _ -> true
    | _ -> false

let is_vector = function
    | Types.Vector _ -> true
    | _ -> false

let is_map = function
    | Types.Map _ -> true
    | _ -> false

let is_string = function
    | Types.String _ -> true
    | _ -> false

let is_number = function
    | Types.Number _ -> true
    | _ -> false

let is_symbol = function
    | Types.Symbol _ -> true
    | _ -> false

let is_string = function
    | Types.String _ -> true
    | _ -> false

let is_keyword = function
    | Types.Keyword _ -> true
    | _ -> false

let is_atom = function
    | Types.Atom _ -> true
    | _ -> false

let is_nil = function
    | Types.Nil -> true
    | _ -> false

let concat l = 
    let lists = List.map (function 
    | Types.List ls -> ls
    | Types.Vector ls -> ls
    | any -> [any]) l in
    Types.List (List.concat lists)

let cons = function
    | v::(Types.List l)::_ -> Types.List (v::l)
    | v::(Types.Vector l)::_ -> Types.Vector (v::l)
    | _ -> Types.Nil
