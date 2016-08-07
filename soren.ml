module T = Types.Types
module C = Core.Core

let repl_env = Env.make None [] [];;
C.iter (fun k v -> Env.set repl_env (T.Symbol k) v) Core.ns;;

let read str = Reader.read_str str
let print expr = Printer.pr_str expr

let rec eval_ast ast env =
    match ast with
    | T.List l -> T.List (List.map (fun el -> eval el env) l)
    | T.Vector l -> T.Vector (List.map (fun el -> eval el env) l)
    | T.Map l -> T.Map (Types.SMap.map (fun v -> eval v env) l)
    | T.Symbol s -> (try Env.get env ast with Not_found -> T.Nil)
    | _ -> ast

and eval ast env =
    match ast with
    (* empty list, just return it *)
    | T.List [] -> ast
    (* special forms *)
    | T.List ((T.Symbol "do")::expr::rest) ->
        List.fold_left (fun _ x -> eval_ast x env) (eval_ast expr env) rest
    | T.List [T.Symbol "if"; antecedent; consequent] ->
        (match Types.to_bool (eval antecedent env) with
        | T.Bool true -> eval consequent env
        | _ -> T.Nil)
    | T.List [T.Symbol "if"; antecedent; consequent; alt] ->
        (match Types.to_bool (eval antecedent env) with
        | T.Bool true -> eval consequent env
        | _ -> eval alt env)
    | T.List [T.Symbol "def!"; key; expr] ->
        let v = eval expr env in
        Env.set env key v;
        v
    | T.List [T.Symbol "fn*"; T.Vector bindings; expr] ->
        T.Fn (fun args ->
            let scope = Env.make (Some env) bindings args in
            eval expr scope)
    | T.List [T.Symbol "let*"; T.List bindings; expr] ->
        let rec bind_syms syms env =
            (match syms with
            | k :: expr :: rest ->
                (Env.set env k (eval_ast expr env); bind_syms rest env)
            | _ :: [] -> raise (Invalid_argument "bindings must be even")
            | [] -> ())
        in
        let env = Env.make (Some env) [] [] in
        bind_syms bindings env;
        eval expr env
    (* otherwise, evaluate the list *)
    (* and call the first one with the others *)
    | T.List l ->
        (match eval_ast ast env with
        | T.List ((T.Fn f) :: args) -> (f args)
        | _ -> raise (Invalid_argument "expected a function"))
    (* otherwise, just  *)
    | _ -> eval_ast ast env

let rep str = print (eval (read str) repl_env)

let () =
    try
        while true do
        	try
                print_string "user> ";
	            print_endline (rep (read_line ()));
	        with Types.SyntaxError e -> Printf.printf "SyntaxError: %s\n" e;
        done
    with End_of_file -> ()