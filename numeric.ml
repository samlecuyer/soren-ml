
type t =
    | Int of int
    | Float of float
    | Ratio of int * int
    | Complex of t * t