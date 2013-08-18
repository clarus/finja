open Batteries ;;
open Computation ;;

let html_of_term t =
  let rec noprop = function
    | Let (v, NoProp (v'), t) when v = v' -> ", <var>" ^ v ^ "</var>"
      ^ (noprop t)
    | _ as t -> " ;\n" ^ (hot t)
  and prime = function
    | Let (v, Prime (v'), t) when v = v' -> ", <var>" ^ v ^ "</var>"
      ^ (prime t)
    | _ as t -> " ;\n" ^ (hot t)
  and opp = function
    | Opp (t) -> " - " ^ (hot t)
    | _ as t -> " + " ^ (hot t)
  and hot = function
    | Let (v, NoProp (v'), t) when v = v' -> "<b>noprop</b> <var>" ^ v
      ^ "</var>" ^ (noprop t)
    | Let (v, Prime (v'), t) when v = v' -> "<b>prime</b> <var>" ^ v
      ^ "</var>" ^ (prime t)
    | Let (v, e, t) -> "<var>" ^ v ^ "</var> := " ^ (hot e) ^ " ;\n"
      ^ (hot t)
    | Var (v) | NoProp (v) | Prime (v) -> "<var>" ^ v ^ "</var>"
    | Protected (t) -> "<i>{</i> " ^ (hot t) ^ " <i>}</i>"
    | If (c, t, e) -> "<b>if</b> " ^ (hot c) ^ " <b>abort with</b> "
      ^ (hot t) ^ " ;\n" ^ (hot e)
    | Sum (l) -> List.fold_left (fun h t -> h ^ (opp t))
      (hot (List.hd l)) (List.tl l)
    | Opp (t) -> "-" ^ (hot t)
    | Prod (l) -> List.fold_left (fun h t -> h ^ " * " ^ (hot t))
      (hot (List.hd l)) (List.tl l)
    | Inv (t) -> "(" ^ (hot t) ^ ")<sup>-1</sup>"
    | Exp (a, b) -> "(" ^ (hot a) ^ ")<sup>" ^ (hot b) ^ "</sup>"
    | Mod (a, b) -> (hot a) ^ " mod " ^ (hot b)
    | Zero -> "0"
    | One -> "1"
    | RandomFault (_) -> "<strong>Random</strong>"
    | ZeroFault (_) -> "<strong>Zero</strong>"
    | Return (t) -> "<b>return</b> " ^ (hot t) ^ " ;\n"
    | Eq (a, b) -> (hot a) ^ " = " ^ (hot b)
    | NotEq (a, b) -> (hot a) ^ " &ne; " ^ (hot b)
    | EqMod (a, b, m) -> (hot a) ^ " &equiv; " ^ (hot b)
      ^ " (mod " ^ (hot m) ^ ")"
    | NotEqMod (a, b, m) -> (hot a) ^ " &#8802; " ^ (hot b)
      ^ " (mod " ^ (hot m) ^ ")"
  in hot t
;;

let print_header html fia =
  Printf.fprintf html "<!DOCTYPE html>\
<html>\
  <head>\
    <title>finja report for %s</title>\
  <style type=\"text/css\">\
  * { margin:0; padding:0; font-size:1em; }\
  body { font-size:1em; color:#222; padding:0.5em; }\
  h1 { font-size:1.2em; padding:0.5em; }\
  h2 { font-size:1em; padding:0.5em; font-weight:bold; }\
  pre { margin:0.5em; padding:0.5em; background-color:#ddd; line-height:150%%; }\
  strong { color:red; }\
  </style>\
  </head>\
  <body>\
    <h1>finja report for &quot;%s&quot;</h1>\
" fia fia
;;

let print_title html n =
  Printf.fprintf html "<h2>Attempt %d</h2>\n" n
;;

let print_p html p =
  Printf.fprintf html "<p>%s</p>\n" p
;;

let print_term html term =
  Printf.fprintf html "<pre>%s</pre>\n" (html_of_term term)
;;

let close_html html =
  Printf.fprintf html "</body>\n</html>"
;;
