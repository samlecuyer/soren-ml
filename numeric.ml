
type t =
    | NaN
    | Int of int
    | Float of float
    | Ratio of int * int
    | Complex of t * t

let rec add a b =
	match (a, b) with
	(* NaN is contagious in that any number added to it is not a number *)
	| (NaN, _) | (_, NaN) -> NaN
	| (Int a, Int b) -> Int (a + b)
	| (Float a, Float b) -> Float (a +. b)
	| (Int a, Float b) -> Float ((float_of_int a) +. b)
	| (Float _, Int _) -> (add b a)
	| _ -> raise (Invalid_argument "only add ints")

let div a b =
	match (a, b) with
	| (NaN, _) | (_, NaN) -> NaN
	| (Int a, Int b) -> Ratio (a, b)
	| (Float a, Float b) -> Float (a /. b)
	| (Int a, Float b) -> Float ((float_of_int a) +. b)
	| (Float _, Int _) -> (add b a)
	| _ -> raise (Invalid_argument "only add ints")