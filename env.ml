module T = Types.Types
module Env = Map.Make(String)

type t = {
    outer: t option;
    data: Types.t Env.t ref
}

let rec make_variadic outer binds exprs = 
    let env = {outer = outer; data = ref Env.empty} in
    let rec bind_variadic binds exprs =
        match (binds, exprs) with
        (* if there's a &, the next symbol binds rest *)
        | ((T.Symbol "&")::last::_, rest) -> set env last (T.List rest);
        (* but there needs to be a next symbol *)
        | ((T.Symbol "&")::[], _) -> raise (Types.SyntaxError "& must be a tail position");
        (* otherwise, just keep binding *)
        | (k::bindings, v::exprs) -> 
            set env k v;
            bind_variadic bindings exprs;
        (* but if we're out of values, bind nil *)
        | (k::bindings, []) -> 
            set env k T.Nil;
            bind_variadic bindings exprs;
        (* and if we're out of bindings, just skip the rest *)
        | ([], _) -> ();
    in
    bind_variadic binds exprs;
    env

and make outer binds exprs = 
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