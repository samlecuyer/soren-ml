
type t =
    | NaN
    | Int of int
    | Float of float
    | Ratio of int * int
    | Complex of t * t

