
open Types
open Sedlexing

(* tokens for parsing *)

type token =
  | EOF | Unset

  (* special characters *)
  | TildeAt         (* ~@ *)
  | LBrace | RBrace (* {} *)
  | LBrack | RBrack (* [] *)
  | LParen | RParen (* () *)
  | Quote | Backtick | Tilde | Caret | At

  (* double quoted strings *)
  | String of string

  (* non-special characters *)
  | True | False | Nil
  | Symbol of string
  | IntNum of int
  | FloatNum of float

let keyword_of_str str = match str with
  | "true"       -> Some(True)
  | _            -> None


let to_str = function
  | IntNum n   -> "<num>"^string_of_int n
  | FloatNum n -> "<num>"^string_of_float n
  | LBrace     -> "{"
  | RBrace     -> "}"
  | LBrack     -> "["
  | RBrack     -> "]"
  | LParen     -> "("
  | RParen     -> ")"
  | EOF        -> "<eof>"
  | _          -> "none"


(* lexing bits *)

(* regexes for matching a decimal literal *)
let dec_digit       = [%sedlex.regexp? '0'..'9']
let dec_digits      = [%sedlex.regexp? Star dec_digit]
let dec_digit_nz    = [%sedlex.regexp? '1'..'9']
let dec_int_lit     = [%sedlex.regexp? Opt '-', ('0' | (dec_digit_nz, Opt dec_digits))]
let exponent_part   = [%sedlex.regexp? ('e' | 'E'), Opt ('-'|'+'), dec_digits]
let decimal_literal1 = [%sedlex.regexp? dec_int_lit, '.', Opt dec_digits, Opt exponent_part]
let decimal_literal2 = [%sedlex.regexp? '.', Plus dec_digit, Opt exponent_part]
let decimal_literal3 = [%sedlex.regexp? dec_int_lit, Opt exponent_part]
let decimal_literal = [%sedlex.regexp?  Opt '-', (decimal_literal1|decimal_literal2|decimal_literal3)]
(* regexes for binary integer literal *)
let binary_literal = [%sedlex.regexp? ("0b"|"0B"), Star ('0'|'1')]
(* regexes for hex integer literal *)
let hex_digit = [%sedlex.regexp? '0'..'9' | 'a'..'f' | 'A'..'F']
let hex_literal = [%sedlex.regexp? ("0x"|"0X"), Star hex_digit]
(* regexes for octal integer literal *)
let octal_literal = [%sedlex.regexp? ("0o"|"0O"), Star ('0'..'7')]
let symbol_literal = [%sedlex.regexp? Compl(white_space| Chars "[]{}('\"`,;)")]

let rec token buf =
  match%sedlex buf with
  | white_space -> token buf
  | ';', Star any, (eof | '\n') -> token buf
  | '"' -> read_string (Buffer.create 0) buf
  | dec_int_lit -> IntNum (int_of_string @@ Utf8.lexeme @@ buf)
  | decimal_literal -> FloatNum (float_of_string @@ Utf8.lexeme @@ buf)
  | binary_literal | hex_literal | octal_literal -> IntNum (int_of_string @@ Utf8.lexeme @@ buf)
  | "{"    -> LBrace
  | "}"    -> RBrace
  | "["    -> LBrack
  | "]"    -> RBrack
  | "("    -> LParen
  | ")"    -> RParen

  | "true"  -> True
  | "false" -> False
  | "nil"   -> Nil

  | eof    -> EOF
  | Star symbol_literal -> Symbol (Utf8.lexeme buf)
  (* | any    -> raise (SyntaxError ("lexeing error: " ^ (Sedlexing.Utf8.lexeme buf))) *)
  | _      -> failwith "unreachable"
  (* | _ -> Symbol (Utf8.lexeme buf) *)

and read_comment buf =
  match%sedlex buf with
  | "*)" -> token buf
  | any -> read_comment buf
  | eof -> raise (SyntaxError "Close your comment before the file ends")
  | _ -> raise (SyntaxError "Unknown character")

and read_string b buf =
  match%sedlex buf with
  | '\\', '\\' -> Buffer.add_char b '\\'; read_string b buf
  | '\\', '"' -> Buffer.add_char b '"'; read_string b buf
  | '\\', 'n' -> Buffer.add_char b '\n'; read_string b buf
  | '"' -> String (Buffer.contents b)
  | any -> Buffer.add_string b (Utf8.lexeme buf); read_string b buf
  | _ -> failwith "unreachable"

(* reading bits *)

type t = {
    mutable curr : token;
    mutable last : token;
    lexbuf : Sedlexing.lexbuf
};;

let create lexbuf = 
    {last = Unset; curr = Unset; lexbuf = lexbuf}

let rec next p =
    p.last <- p.curr;
    p.curr <- token p.lexbuf;
    p.curr

let skip p = ignore(next p)

let rec read_str str =
    let lexer = Utf8.from_string str in
    let parser = create lexer in
    skip parser;
    read_form parser

and read_form parser =
    match parser.curr with
    | LParen -> read_list parser
    | LBrack -> read_vector parser
    | LBrace -> read_map parser
    | _ -> read_atom parser

and read_list parser =
    let rec handle_elems accum p =
        match p.curr with
        | RParen -> skip p; List.rev accum
        | EOF -> raise (SyntaxError "unexpected EOF")
        | _ -> handle_elems ((read_form p)::accum) p
    in
    skip parser;
    Types.List (handle_elems [] parser)

and read_vector parser =
    let rec handle_elems accum p =
        match p.curr with
        | RBrack -> skip p; List.rev accum
        | EOF -> raise (SyntaxError "unexpected EOF")
        | _ -> handle_elems ((read_form p)::accum) p
    in
    skip parser;
    Types.Vector (handle_elems [] parser)

and read_map parser  =
    let rec map_of_list l m =
      match l with
      | k :: v :: rest -> map_of_list rest (SMap.add k v m)
      | [] -> m
      | _ :: [] -> raise (SyntaxError "maps need an even number of keys")
    and handle_elems accum p =
        match p.curr with
        | RBrace -> skip p; map_of_list (List.rev accum) SMap.empty
        | EOF -> raise (SyntaxError "unexpected EOF")
        | _ -> handle_elems ((read_form p)::accum) p
    in
    skip parser;
    Types.Map (handle_elems [] parser)

and read_atom p =
    match p.curr with
    | String s -> skip p; Types.String s
    | IntNum n -> skip p; Types.Number (Int n)
    | FloatNum n -> skip p; Types.Number (Float n)
    | Symbol s when s.[0] = ':' -> skip p; Types.Keyword s
    | Symbol s -> skip p; Types.Symbol s
    | True     -> skip p; Types.Bool true
    | False    -> skip p; Types.Bool false
    | Nil      -> skip p; Types.Nil
    | _ -> raise (SyntaxError "unexpected token")

