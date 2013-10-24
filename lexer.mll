{
  open Lexing ;;
  open Parser ;;

  exception Error of char ;;

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p
    in lexbuf.lex_curr_p <- {
      pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum
    } ;;
}

let alpha = [ 'a'-'z' 'A'-'Z' ]
let num = [ '0'-'9' ]
let ident = '_' | '@' | ( alpha ( alpha | num | '_' | '@' | '\'' )* )

rule token = parse
| "--"         { comment lexbuf }
| [' ' '\t']   { token lexbuf }
| '\n'         { newline lexbuf; token lexbuf }
| "noprop"     { Lnoprop }
| "prime"      { Lprime }
| ","          { Lcomma }
| ";"          { Lsemicolon }
| ":="         { Lassign }
| "!="         { Lnoteq }
| "="          { Lequal }
| "^"          { Lcirc }
| "-"          { Lminus }
| "+"          { Lplus }
| "*"          { Lstar }
| "1"          { Lone }
| "0"          { Lzero }
| "{"          { Lobrace }
| "}"          { Lcbrace }
| "("          { Loparen }
| ")"          { Lcparen }
| "["          { Lobracket }
| "]"          { Lcbracket }
| "%"          { Lpercent }
| "mod"        { Lmod }
| "if"         { Lif }
| "abort with" { Labortwith }
| "return"     { Lreturn }
| "/\\"        { Land }
| "\\/"        { Lor }
| ident as s   { Lident (s) }
| _ as c       { raise (Error c) }
| eof          { Leof }

and comment = parse
| '\n'         { newline lexbuf; token lexbuf }
| _            { comment lexbuf }
