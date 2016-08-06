module T = Types.Types
module Env = Map.Make(String)

type t = {
    outer: t option;
    data: Types.t Env.t ref
}

let rec make outer binds exprs = 
    let env = {outer = outer; data = ref Env.empty} in
    List.iter2 (fun k v -> set env k v) binds exprs;
    env

and set env k v =
    match k with
    | T.Symbol s -> env.data := Env.add s v !(env.data)
    | _ -> raise (Invalid_argument "must take a symbol key")

let rec find k env =
    match k with
    | T.Symbol s -> 
        (if Env.mem s !(env.data) then
            Some env
        else
            match env.outer with
            | None -> None
            | Some outer -> find k outer)
    | _ -> raise (Invalid_argument "must take a symbol key")

let get env k =
    match k with
    | T.Symbol s -> 
        (match find k env with
        | None -> raise Not_found
        | Some e -> Env.find s !(e.data))
    | _ -> raise (Invalid_argument "must take a symbol key")