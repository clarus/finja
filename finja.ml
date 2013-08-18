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

let () =
  Arg.parse options set_fia_input usage;

  if !fia_input = "" then begin
    Printf.eprintf "No input file specified.\n";
    exit 1
  end;
  
  let fia = File.open_in !fia_input in
  let buf = Lexing.from_channel fia in
  let desc = Parser.desc Lexer.token buf
  in begin
    IO.close_in fia;

    if !lint_only then exit 0;
    
    let report = File.open_out !html_output in
    Html.print_header report !fia_input;
    Html.print_term report (fst desc);
    Html.close_html report;
    IO.close_out report;
    ()
  end
