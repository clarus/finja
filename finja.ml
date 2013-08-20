open Batteries ;;

let fia_input = ref "" ;;
let html_output = ref "" ;;
let lint_only = ref false ;;
let zeroing = ref false ;;
let transcient = ref false ;;

let set_fia_input filename =
  fia_input := filename;
  if !html_output = "" then html_output := filename ^ ".html"
;;

let usage = "finja [options] filename" ;;
let options =
  [ "-o", Arg.Set_string html_output, "HTML report (output file)"
  ; "-l", Arg.Set lint_only, "Only check syntax"
  ; "-z", Arg.Set zeroing, "Inject zeroing faults"
  ; "-r", Arg.Clear zeroing, "Inject randomizing faults"
  ; "-t", Arg.Set transcient, "Enable transcient faults"
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

      let term = Sanity.check_term (fst desc) in
      let cond = (snd desc) in

      if !lint_only then exit 0;

      let report = File.open_out !html_output in
      Html.start_header report !fia_input;
      Html.print_options report !transcient !zeroing;
      Html.print_term report "Computation" term;
      Html.print_attack_success_condition report cond;
      Html.print_term report "Reduced computation" Computation.Zero;
      Html.end_header report;

      let attempt = FaultInjection.inject_fault term !transcient !zeroing in
      let rec loop i =
        try
          let t' = attempt i in
          if t' <> term then begin
            Html.print_attempt report i t' Computation.Zero false;
            loop (i + 1)
          end
        with FaultInjection.Non_faultable ->
          loop (i + 1)
      in loop 1;

      Html.close_html report;
      IO.close_out report;
      ()
    end
  with
  | Lexer.Error c ->
    location (Lexing.lexeme_start_p buf) (Lexing.lexeme_end_p buf);
    Printf.eprintf "Unexpected char '%c'.\n" c
  | Parser.Error ->
    location (Lexing.lexeme_start_p buf) (Lexing.lexeme_end_p buf);
    Printf.eprintf "Syntax error.\n"
  | Sanity.Error (m, s, e) ->
    location s e;
    Printf.eprintf "%s\n" m
