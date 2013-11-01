open Batteries ;;
open Computation ;;

let html_of_term t =
  let rec noprop = function
    | Let (v, NoProp (v'), t) when v = v' ->
      ", <var>" ^ v ^ "</var>" ^ (noprop t)
    | t -> " ;\n" ^ (hot 0 t)
  and prime = function
    | Let (v, Prime (v'), t) when v = v' ->
      ", <var>" ^ v ^ "</var>" ^ (prime t)
    | t -> " ;\n" ^ (hot 0 t)
  and opp = function
    | Opp (t) -> " &minus; " ^ (hot 2 t)
    | t -> " + " ^ (hot 1 t)
  and hot p = function
    (* p is for parentheses: 0 none, 1 might, 2 may *)
    | Let (v, NoProp (v'), t) when v = v' ->
      "<b>noprop</b> <var>" ^ v ^ "</var>" ^ (noprop t)
    | Let (v, Prime (v'), t) when v = v' ->
      "<b>prime</b> <var>" ^ v ^ "</var>" ^ (prime t)
    | Let (v, e, t) ->
      "<var>" ^ v ^ "</var> := " ^ (hot 0 e) ^ " ;\n" ^ (hot 0 t)
    | Var (v) | NoProp (v) | Prime (v) ->
      "<var>" ^ v ^ "</var>"
    | Protected (t) ->
      "<i>{</i> " ^ (hot 0 t) ^ " <i>}</i>"
    | If (c, t, e) ->
      "<b>if</b> " ^ (hot 0 c) ^ " <b>abort with</b> "
      ^ (hot 0 t) ^ " ;\n" ^ (hot 0 e)
    | Sum (l) ->
      (if p > 1 && (List.length l) > 1 then "(" else "")
      ^ (List.fold_left (fun h t -> h ^ (opp t))
           (hot 0 (List.hd l)) (List.tl l))
      ^ (if p > 1 && (List.length l) > 1 then ")" else "")
    | Opp (t) -> "-" ^ (hot 2 t)
    | Prod (l) ->
      (if p > 1 && (List.length l) > 1 then "(" else "")
      ^ (List.fold_left (fun h t -> h ^ " &times; " ^ (hot 2 t))
           (hot 0 (List.hd l)) (List.tl l))
      ^ (if p > 1 && (List.length l) > 1 then ")" else "")
    | Inv (t)            -> (hot 2 t) ^ "<sup>&minus;1</sup>"
    | Exp (a, b)         -> (hot 2 a) ^ "<sup>" ^ (hot 0 b) ^ "</sup>"
    | Mod (a, b)         ->
      (if p > 0 then "(" else "")
      ^ (hot 1 a) ^ " mod " ^ (hot 2 b)
      ^ (if p > 0 then ")" else "")
    | Zero               -> "0"
    | One                -> "1"
    | Eq (a, b)          ->
      (if p > 1 then "(" else "")
      ^ (hot 0 a) ^ " = " ^ (hot 0 b)
      ^ (if p > 1 then ")" else "")
    | NotEq (a, b)       ->
      (if p > 1 then "(" else "")
      ^ (hot 0 a) ^ " &ne; " ^ (hot 0 b)
      ^ (if p > 1 then ")" else "")
    | EqMod (a, b, m)    ->
      (if p > 1 then "(" else "")
      ^ (hot 0 a) ^ " &equiv; " ^ (hot 0 b) ^ " [mod " ^ (hot 0 m) ^ "]"
      ^ (if p > 1 then ")" else "")
    | NotEqMod (a, b, m) ->
      (if p > 1 then "(" else "")
      ^ (hot 0 a) ^ " &#8802; " ^ (hot 0 b) ^ " [mod " ^ (hot 0 m) ^ "]"
      ^ (if p > 1 then ")" else "")
    | And (a, b)         ->
      (if p > 0 then "(" else "")
      ^ (hot 1 a) ^ " &and; " ^ (hot 1 b)
      ^ (if p > 0 then ")" else "")
    | Or (a, b)          ->
      (if p > 0 then "(" else "")
      ^ (hot 1 a) ^ " &or; " ^ (hot 1 b)
      ^ (if p > 0 then ")" else "")
    | Return (t)         -> "<b>return</b> " ^ (hot 0 t) ^ " ;\n"
    | RandomFault (_)    -> "<strong>Random</strong>"
    | ZeroFault (_)      -> "<strong>Zero</strong>"
    | Nil                -> "<small>nil</small>"
  in hot 0 t
;;

let start_header html fia =
  Printf.fprintf html "<!DOCTYPE html>\n\
<html>\
<head>\
  <title>finja report for %s</title>\
  <style type=\"text/css\">\
  * { margin:0; padding:0; }\
  body { font-size:1em; color:#222; padding:0.5em; font-size:0.8em; }\
  p { margin:0 0 0.5em 0; }\
  h1 { font-size:1.3em; margin:0 0.39em; }\
  h2 { font-size:0.8em; }\
  h2 a { display:block; margin:0 0.63em; padding:0.63em 0 1em 0;\
         border-top:1px solid #aaa; font-weight:normal; color:inherit;\
         text-decoration:none; }\
  h2 code { margin: 0 0 0 1em; font-size:1.3em; color:#333; }\
  h2 a span { float:right; font-size:1em; color:#aaa;\
              text-decoration:underline; }\
  .success, .s { color: #d22; }\
  .failure, .f { color: #8b2; }\
  pre { font-size:1.2em; padding:0.5em; background-color:#ddd;\
        line-height:150%%; }\
  pre strong { color:#48f; }\
  dl.attempt { display: none; }\
  dt { font-weight:bold; margin:0.5em 0.5em 0 0.5em; }\
  dd { margin:0 0.5em 0.5em 0.5em; }\
  </style>\
  <script type=\"text/javascript\">\
  function ec (target) {\
    var dl = document.getElementById(target.substr(target.indexOf('#') + 1));\
    dl.style.display = (dl.style.display == 'block') ? 'none' : 'block';
    return false;\
   }\
   function hide_failures (l) {\
     var attempts = document.getElementsByClassName('attempt');\
     for (i = 0; i < attempts.length; i++) attempts[i].style.display = 'none';\
     var failures = document.getElementsByClassName('failure');\
     for (i = 0; i < failures.length; i++) failures[i].style.display = 'none';\
     l.innerHTML = 'show all';\
     l.onclick = function(){ return show_all(l); };\
     return false;\
   }\
   function show_all (l) {\
     var attempts = document.getElementsByClassName('attempt');\
     for (i = 0; i < attempts.length; i++) attempts[i].style.display = 'none';\
     var fail = document.getElementsByClassName('failure');\
     for (i = 0; i < fail.length; i++) fail[i].style.display = 'block';\
     l.innerHTML = 'hide other';\
     l.onclick = function(){ return hide_failures(l); };\
     return false;\
   }\
   </script>\
 </head>\
<body>\
  <h1>finja report for &quot;%s&quot;</h1>"
    fia fia
;;

let print_options html transient fault_types count =
  Printf.fprintf html "<dl><dt>Options:</dt><dd><p><i>transient faults:</i> \
  %s.<br /><i>fault types:</i> %s.<br /><i>Maximum number of faults:</i> \
  %d.</p></dd>"
    (if transient then "enabled" else "disabled")
    (List.fold_left (fun a -> function
    | Randomizing -> a ^ " randomizing"
    | Zeroing -> a ^ " zeroing") "" fault_types)
    count
;;

let print_attack_success_condition html cond =
  Printf.fprintf html "<dt>Attack success condition:</dt><dd><p>%s</p></dd>"
    (html_of_term cond)
;;

let end_header html =
  Printf.fprintf html "</dl>"
;;

let print_summary html attempts_count attacks_count =
  Printf.fprintf html "<dt>Summary <strong class=\"%s\">%s</strong></dt><dd>\
  <p><i>Total number of different fault injections:</i> %d.<br />\
  <i>Total number of successful attack:</i> %d (<a href=\"#\" \
  onclick=\"return hide_failures(this);\">hide others</a>).</p></dd>"
    (if attacks_count = 0 then "f" else "s")
    (if attacks_count = 0 then "PROTECTED" else "BROKEN")
    attempts_count attacks_count
;;

let print_term html title term =
  Printf.fprintf html "<dt>%s</dt><dd><pre>%s</pre></dd>"
    title (html_of_term term)
;;

let print_attempt html term reduced_term faulted_subterms result attempt_num =
  Printf.fprintf html "<h2 class=\"%s\"><a href=\"#attempt%d\" \
  onclick=\"return ec(this.href);\">Attempt %d <code>%s</code>\
  <span>expand/collapse</span></a></h2><dl id=\"attempt%d\" class=\"attempt\">"
    (if result then "success" else "failure") attempt_num attempt_num
    (List.fold_left (fun a t -> (html_of_term t) ^ " | " ^ a)
       (html_of_term (List.hd faulted_subterms)) (List.tl faulted_subterms))
    attempt_num;
  print_term html "Faulted computation" term;
  print_term html "Result" reduced_term;
  Printf.fprintf html "<dd><p>%s</p></dd></dl>"
    (if result then "Attack successful." else "Harmless fault injection.");
;;

let close_html html =
  Printf.fprintf html "</body></html>"
;;
