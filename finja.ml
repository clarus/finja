open Batteries ;;
open Computation ;;

let fia_input = ref "" ;;
let html_output = ref "" ;;
let lint_only = ref false ;;
let transcient = ref false ;;
let fault_type = ref Randomizing ;;

let set_fia_input filename =
  fia_input := filename;
  if !html_output = "" then html_output := filename ^ ".html"
;;

let usage = "finja [options] <input-file>" ;;
let options =
  [ "-o", Arg.Set_string html_output, "<output-file> HTML report (defaults to input-file.html)"
  ; "-l", Arg.Set lint_only, "Only check syntax"
  ; "-t", Arg.Set transcient, "Enable transcient faults (default is only permanent fault)"
  ; "-r", Arg.Unit (fun () -> fault_type := Randomizing), "Inject randomizing faults (default)"
  ; "-z", Arg.Unit (fun () -> fault_type := Zeroing), "Inject zeroing faults"
  ; "-b", Arg.Unit (fun () -> fault_type := Both), "Inject both types of faults"
  ] ;;


let location startp endp =
  let l = startp.Lexing.pos_lnum in
  let sc = startp.Lexing.pos_cnum - startp.Lexing.pos_bol in
  let ec = endp.Lexing.pos_cnum - endp.Lexing.pos_bol in
  Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n"
    !fia_input l sc ec
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
      Html.print_options report !transcient !fault_type;
      Html.print_term report "Computation" term;
      Html.print_attack_success_condition report cond;
      Html.print_term report "Reduced computation" Computation.Zero;
      Html.end_header report;

      let attempt = FaultInjection.inject_fault term !transcient in
      let rec loop i prev =
        try
          let ftype = match !fault_type with
            | Both -> if i = prev then Zeroing else Randomizing
            | _ -> !fault_type
          in
          let faulted_term = attempt ftype i in
          if faulted_term <> term then begin
            let reduced_fterm = Zero in
            let result = false in
            Html.print_attempt report faulted_term reduced_fterm result;
            loop (if !fault_type = Both && i != prev then i else i + 1) i
          end
        with FaultInjection.Non_faultable ->
          loop (i + 1) i
      in loop 1 0;

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
