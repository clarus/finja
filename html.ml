open Batteries ;;
open Computation ;;

let attempt = ref 0 ;;

let html_of_term t =
  let rec noprop = function
    | Let (v, NoProp (v'), t) when v = v' ->
      ", <var>" ^ v ^ "</var>" ^ (noprop t)
    | _ as t -> " ;\n" ^ (hot t)
  and prime = function
    | Let (v, Prime (v'), t) when v = v' ->
      ", <var>" ^ v ^ "</var>" ^ (prime t)
    | _ as t -> " ;\n" ^ (hot t)
  and opp = function
    | Opp (t) -> " - " ^ (hot t)
    | _ as t  -> " + " ^ (hot t)
  and hot = function
    | Let (v, NoProp (v'), t) when v = v' ->
      "<b>noprop</b> <var>" ^ v ^ "</var>" ^ (noprop t)
    | Let (v, Prime (v'), t) when v = v' ->
      "<b>prime</b> <var>" ^ v ^ "</var>" ^ (prime t)
    | Let (v, e, t) ->
      "<var>" ^ v ^ "</var> := " ^ (hot e) ^ " ;\n" ^ (hot t)
    | Var (v) | NoProp (v) | Prime (v) ->
      "<var>" ^ v ^ "</var>"
    | Protected (t) ->
      "<i>{</i> " ^ (hot t) ^ " <i>}</i>"
    | If (c, t, e) ->
      "<b>if</b> " ^ (hot c) ^ " <b>abort with</b> " ^ (hot t) ^ " ;\n"
      ^ (hot e)
    | Sum (l) -> List.fold_left (fun h t -> h ^ (opp t))
        (hot (List.hd l)) (List.tl l)
    | Opp (t) -> "-" ^ (hot t)
    | Prod (l) -> List.fold_left (fun h t -> h ^ " * " ^ (hot t))
        (hot (List.hd l)) (List.tl l)
    | Inv (t)            -> "(" ^ (hot t) ^ ")<sup>-1</sup>"
    | Exp (a, b)         -> "(" ^ (hot a) ^ ")<sup>" ^ (hot b) ^ "</sup>"
    | Mod (a, b)         -> (hot a) ^ " mod " ^ (hot b)
    | Zero               -> "0"
    | One                -> "1"
    | Eq (a, b)          -> "(" ^ (hot a) ^ " = " ^ (hot b) ^ ")"
    | NotEq (a, b)       -> "(" ^ (hot a) ^ " &ne; " ^ (hot b) ^ ")"
    | EqMod (a, b, m)    -> "(" ^ (hot a) ^ " &equiv; " ^ (hot b)
      ^ " (mod " ^ (hot m) ^ "))"
    | NotEqMod (a, b, m) -> "(" ^ (hot a) ^ " &#8802; " ^ (hot b)
      ^ " (mod " ^ (hot m) ^ "))"
    | And (a, b)         -> "(" ^ (hot a) ^ " &and; " ^ (hot b) ^ ")"
    | Or (a, b)          -> "(" ^ (hot a) ^ " &or; " ^ (hot b) ^ ")"
    | Return (t)         -> "<b>return</b> " ^ (hot t) ^ " ;\n"
    | RandomFault (_)    -> "<strong>Random</strong>"
    | ZeroFault (_)      -> "<strong>Zero</strong>"
  in hot t
;;

let start_header html fia =
  Printf.fprintf html "<!DOCTYPE html>\
<html>\
<head>\
  <title>finja report for %s</title>\
  <style type=\"text/css\">\
  * { margin:0; padding:0; }\
  body { font-size:1em; color:#222; padding:0.5em; font-size:0.8em; }\
  p { margin:0 0 0.5em 0; }\
  h1 { font-size:1.3em; margin:0 0.39em; }\
  h2 { font-size:0.8em; margin:1em 0.63em 0 0.63em; padding:0.63em 0 0 0;\
       border-top:1px solid #aaa; font-weight:normal; }\
  .success { color: #d22; }\
  .failure { color: #8b2; }\
  pre { font-size:1.2em; padding:0.5em; background-color:#ddd;\
        line-height:150%%; }\
  pre strong { color:#48f; }\
  dl.attempt { display: none; }\
  dt { font-weight:bold; margin:0.5em 0.5em 0 0.5em; }\
  dd { margin:0 0.5em 0.5em 0.5em; }\
  a { float:right; font-size:1em; color:#aaa; }\
  </style>\
  <script type=\"text/javascript\">\
  function ec (target) {\
    var dl = document.getElementById(target.substr(target.indexOf('#') + 1));\
    dl.style.display = (dl.style.display == 'block') ? 'none' : 'block';
    return false;\
   }\
   </script>\
 </head>\
<body>\
  <h1>finja report for &quot;%s&quot;</h1>\
" fia fia
;;

let print_options html transcient fault_type =
  Printf.fprintf html "<dl><dt>Options:</dt><dd><p><i>transcient faults:</i>\
  %s.<br /><i>fault type:</i> %s.</p></dd>"
    (if transcient then "enabled" else "disabled")
    (match fault_type with
    | Randomizing -> "randomizing"
    | Zeroing -> "zeroing"
    | Both -> "both randomizing and zeroing")
;;

let print_attack_success_condition html cond =
  Printf.fprintf html "<dt>Attack success condition:</dt><dd><p>%s</p></dd>"
    (html_of_term cond)
;;

let end_header html =
  Printf.fprintf html "</dl>"
;;

let print_summary html successful_attacks_count =
  Printf.fprintf html "<dt>Summary</dt><dd><p>\
  <i>Total number of different fault injection:</i> %d.<br />\
  <i>Total number of successful attack:</i> %d \
  <strong class=\"%s\">%s</strong>.</p></dd>"
    !attempt successful_attacks_count
    (if successful_attacks_count = 0 then "failure" else "success")
    (if successful_attacks_count = 0 then "PROTECTED" else "BROKEN")

let print_term html title term =
  Printf.fprintf html "<dt>%s</dt><dd><pre>%s</pre></dd>"
    title (html_of_term term)
;;

let print_attempt html term reduced_term result =
  Printf.fprintf html "<h2 class=\"%s\">Attempt %d <a href=\"#attempt%d\" \
  onclick=\"return ec(this.href);\">expand/collapse</a></h2>\
  <dl id=\"attempt%d\" class=\"attempt\">"
    (if result then "success" else "failure") !attempt !attempt !attempt;
  attempt := !attempt + 1;
  print_term html "Faulted computation" term;
  print_term html "Reduced version" reduced_term;
  Printf.fprintf html "<dt>Result</dt><dd><p>%s</p></dd></dl>"
    (if result then "Attack successful." else "Harmless fault injection.");
;;

let close_html html =
  Printf.fprintf html "</body></html>"
;;
