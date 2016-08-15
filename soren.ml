module T = Types.Types
module C = Core_sn.Core

let repl_env = Env.make None [] [];;
C.iter (fun k v -> Env.set repl_env (T.Symbol k) v) Core_sn.ns;;

let read str = Reader.read_str str

let print expr = Printer.pr_str expr false

let rec quasiquote = function
    | T.List [T.Symbol "unquote"; ast] -> ast
    | T.List ((T.List [T.Symbol "splice-unquote"; ast])::rest) ->
        T.List [T.Symbol "concat"; ast; quasiquote (T.List rest)]
    | T.List (ast::rest) ->
        T.List [T.Symbol "cons"; quasiquote ast; quasiquote (T.List rest)]
    | ast -> T.List [T.Symbol "quote"; ast]

let is_macro = T.Keyword "macro"

let is_macro_call ast env =
    match ast with
    | T.List (s::rest) ->
        (match (try Env.get env s with _ -> T.Nil) with
        | T.Fn { T.meta = T.Map m } ->
            Types.SnMap.mem is_macro m && Types.to_bool (Types.SnMap.find is_macro m)
        | _ -> false)
    | _ -> false

let rec macroexpand ast env =
    match ast with
    | T.List (hd::rest) when (is_macro_call ast env) ->
        (match (try Env.get env hd with _ -> T.Nil) with
           | T.Fn { T.value = f } -> macroexpand (f rest) env
           | _ -> ast)
    | _ -> ast

and eval_ast ast env =
    match ast with
    | T.List l -> T.List (List.map (fun el -> eval el env) l)
    | T.Vector l -> T.Vector (List.map (fun el -> eval el env) l)
    | T.Map l -> T.Map (Types.SnMap.map (fun v -> eval v env) l)
    | T.Symbol s -> (try Env.get env ast with Not_found -> T.Nil)
    | _ -> ast

and eval ast env =
    match macroexpand ast env with
    (* empty list, just return it *)
    | T.List [] -> ast
    (* special forms *)
    | T.List [T.Symbol "quote"; form] -> form
    | T.List [T.Symbol "quasiquote"; form] -> eval (quasiquote form) env
    | T.List ((T.Symbol "do")::stmts) ->
        List.fold_left (fun _ x -> eval x env) T.Nil stmts
    | T.List [T.Symbol "if"; antecedent; consequent] ->
        (match Types.to_bool (eval antecedent env) with
        | true -> eval consequent env
        | _ -> T.Nil)
    | T.List [T.Symbol "if"; antecedent; consequent; alt] ->
        (match Types.to_bool (eval antecedent env) with
        | true -> eval consequent env
        | _ -> eval alt env)
    | T.List [T.Symbol "def!"; key; expr] ->
        let v = eval expr env in
        Env.set env key v;
        v
    | T.List [T.Symbol "defmacro!"; key; expr] ->
        (match eval expr env with
        | T.Fn {value = f; meta = m} ->
            let meta = match m with
            | T.Map m -> Types.SnMap.add is_macro (T.Bool true) m
            | _ -> Types.SnMap.(empty |> add is_macro (T.Bool true))
            in
            let fn = T.Fn {value = f; meta = Types.map meta} in
            Env.set env key fn; fn
        | _ -> raise (Invalid_argument "macros must be a function"))
    | T.List [T.Symbol "macroexpand"; expr] -> macroexpand expr env
    | T.List [T.Symbol "fn*"; T.Vector bindings; expr]
    | T.List [T.Symbol "fn*"; T.List bindings; expr] ->
        Types.fn (fun args ->
            let scope = Env.make_variadic (Some env) bindings args in
            eval expr scope)
    | T.List [T.Symbol "let*"; T.List bindings; expr]
    | T.List [T.Symbol "let*"; T.Vector bindings; expr] ->
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
        | T.List ((T.Fn { T.value = f }) :: args) -> (f args)
        | T.List ((T.Map m) :: key :: _) -> (try Types.SnMap.find key m with _ -> T.Nil)
        | _ -> raise (Invalid_argument ("could not apply: " ^ (print ast))))
    (* otherwise, just  *)
    | _ -> eval_ast ast env

let rep str = print (eval (read str) repl_env);;

Env.set repl_env (T.Symbol "eval") (Types.fn
    (function
    | arg::_ -> eval arg repl_env
    | _ -> T.Nil));;

let rec do_repl () = 
    try
        print_string "user> ";
        print_endline (rep (read_line ()));
    with 
    | Invalid_argument e
    | Types.SyntaxError e
    | Types.RuntimeError e -> Printf.printf "Error: %s\n" e;
    do_repl ()

let () =
    try
        ignore (rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))");
        ignore (rep "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))");
        ignore (rep "(def! inc (fn* [x] (+ 1 x)))");
        let args = Array.map (fun s -> T.String s) Sys.argv in 
        Env.set repl_env (T.Symbol "*argv*") (T.List (Array.to_list args));
        do_repl ()
    with _ -> ()