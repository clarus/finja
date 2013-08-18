open Batteries ;;

let fia_input = ref "" ;;
let html_output = ref "" ;;
let lint_only = ref false ;;

let set_fia_input filename =
  fia_input := filename;
  if !html_output = "" then html_output := filename ^ ".html"
;;

let usage = "finja [options] filename" ;;
let options =
  [ "-o", Arg.Set_string html_output, "HTML report (output file)"
  ; "-l", Arg.Set lint_only, "Only check syntax"
  ] ;;


let location startp endp =
  let l = startp.Lexing.pos_lnum
  in let sc = startp.Lexing.pos_cnum - startp.Lexing.pos_bol
  in let ec = endp.Lexing.pos_cnum - endp.Lexing.pos_bol
  in Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n" !fia_input l sc ec
;;


let () =
  Arg.parse options set_fia_input usage;

  if !fia_input = "" then begin
    Printf.eprintf "No input file specified.\n";
    exit 1
  end;
  
  let fia = File.open_in !fia_input in
  let buf = Lexing.from_channel fia in
  try
    let desc = Parser.desc Lexer.token buf
    in begin
      IO.close_in fia;
      
      if !lint_only then exit 0;
      
      File.with_file_out !html_output
        (fun report ->
          Html.print_header report !fia_input;
          Html.print_term report (fst desc);
          Html.close_html report);
      ()
    end
  with
  | Lexer.Error c ->
    location (Lexing.lexeme_start_p buf) (Lexing.lexeme_end_p buf);
    Printf.eprintf "Unexpected char '%c'.\n" c
  | Parser.Error ->
    location (Lexing.lexeme_start_p buf) (Lexing.lexeme_end_p buf);
    Printf.eprintf "Syntax error.\n"
