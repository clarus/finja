{
  open Lexing ;;
  open Parser ;;

  exception Error of string ;;

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p
    in lexbuf.lex_curr_p <- {
      pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum
    } ;;
}

let alpha = ['a'-'z' 'A'-'Z']
let num = ['0'-'9']
let ident = alpha (alpha | num | '_' | '\'')*

rule token = parse
  [' ' '\t'] { token lexbuf }
| '\n'       { newline lexbuf; token lexbuf }
| "noprop"   { Lnoprop }
| "prime"    { Lprime }
| ","        { Lcomma }
| ";"        { Lsemicolon }
| ":="       { Lassign }
(* | "="        { Lequal } *)
(* | "^"        { Lcirc } *)
(* | "-"        { Lminus } *)
(* | "+"        { Lplus } *)
(* | "*"        { Lstar } *)
(* | "!"        { Lbang } *)
| "1"        { Lone }
| "0"        { Lzero }
| "{"        { Lobrace }
| "}"        { Lcbrace }
(* | "("        { Loparen } *)
(* | ")"        { Lcparen } *)
(* | "["        { Lobracket } *)
(* | "]"        { Lcbracket } *)
| "%"        { Lpercent }
(* | "mod"      { Lmod } *)
(* | "if"       { Lif } *)
(* | "then"     { Lthen } *)
(* | "else"     { Lelse } *)
(* | "end"      { Lend } *)
| "return"   { Lreturn }
(* | "@"        { Lat } *)
(* | "/"        { Lslash } *)
(* | "\\"       { Lbackslash } *)
| ident as s { Lident (s) }
| _ as c     { raise (Error (String.make 1 c)) }
| eof        { Leof }
