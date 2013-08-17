open Batteries ;;

open Lexing ;;
open Ast ;;

let () =
  let f = open_in "tests/foo.fia" in
  let buf = Lexing.from_channel f in
  let ast = Parser.desc Lexer.token  buf
  in begin
    close_in f;
    ()
  end
