open Batteries ;;
open Computation ;;

let fia_input = ref "" ;;
let html_output = ref "" ;;
let lint_only = ref false ;;
let reduce_only = ref false ;;
let transient = ref false ;;
let fault_types = ref [] ;;
let count = ref 1 ;;

let set_fia_input filename =
  fia_input := filename;
  if !html_output = "" then html_output := filename ^ ".html"
;;

let add_fault_type ftype =
  if List.length !fault_types >= !count
  then raise (Arg.Bad "Too many fault type options")
  else fault_types := ftype :: !fault_types

let usage = "finja [options] <input-file>" ;;
let options =
  [ "-o", Arg.Set_string html_output,
    "<output-file> HTML report (defaults to input-file.html)"
  ; "-l", Arg.Set lint_only,
    "Only check syntax"
  ; "-s", Arg.Set reduce_only,
    "Only simplify the input term (no attacks)"
  ; "-t", Arg.Set transient,
    "Enable transient faults (default is only permanent fault)"
  ; "-r", Arg.Unit (fun () -> add_fault_type Randomizing),
    "Inject randomizing fault (default)"
  ; "-z", Arg.Unit (fun () -> add_fault_type Zeroing),
    "Inject zeroing fault"
  ; "-n", Arg.Set_int count,
    "Specify the number of faults (default is 1). If specified, you can use \
     the -r and -z option for each fault (last one is repeated)."
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

  if !count < 1 then begin
    Printf.eprintf "Number of faults must be > 0.\n";
    exit 1
  end;

  let fia = File.open_in !fia_input in
  let buf = Lexing.from_channel fia in begin
    try
      let desc = Parser.desc Lexer.token buf
      in begin
        IO.close_in fia;

        let term, cond = Sanity.check desc in
        if !lint_only then exit 0;

        let fault_types = List.rev !fault_types
          @ List.make (!count - (List.length !fault_types))
            (if !fault_types = [] then Randomizing else List.hd !fault_types)
        in

        let reduced_term = Reduction.reduce term in
        let env = !Reduction.env_at_return in

        let tmp, tmpname = File.open_temporary_out () in
        let attacks_count =
          if !reduce_only then 0
          else Analysis.analyse tmp env term cond !transient fault_types !count
        in
        IO.close_out tmp;

        File.with_file_out !html_output (fun report ->
          Html.start_header report !fia_input;
          Html.print_options report !transient fault_types !count;
          Html.print_summary report attacks_count;
          Html.print_term report "Computation" term;
          Html.print_attack_success_condition report cond;
          Html.print_term report "Reduced computation" reduced_term;
          Html.end_header report;
          Enum.iter (Printf.fprintf report "%s\n") (File.lines_of tmpname);
          Html.close_html report)
      end;
      exit 0
    with
    | Lexer.Error (c) ->
      location (Lexing.lexeme_start_p buf) (Lexing.lexeme_end_p buf);
      Printf.eprintf "Unexpected char '%c'.\n" c
    | Parser.Error ->
      location (Lexing.lexeme_start_p buf) (Lexing.lexeme_end_p buf);
      Printf.eprintf "Syntax error.\n"
    | Sanity.Error (m, s, e) ->
      location s e;
      Printf.eprintf "%s\n" m
  end;
  exit 1
;;
