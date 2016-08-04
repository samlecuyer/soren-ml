
let read str = Reader.read_str str

let eval ast env = ast

let print expr = Printer.pr_str expr

let rep str = print (eval (read str) "")

let () =
    try
        while true do
            print_string "user> ";
            print_endline (rep (read_line ()));
        done
    with End_of_file -> ()