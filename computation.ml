open Batteries ;;

type term =
| Let of string * term * term
| Var of string
| NoProp of string
| Prime of string
| Protected of term
| If of term * term * term
| Sum of term list
| Opp of term
| Prod of term list
| Inv of term
| Exp of term * term
| Mod of term * term
| Zero
| One
| RandomFault of int
| ZeroFault of int

type attack_condition =
| False

type description = term * attack_condition

let html_of_term t =
  let rec hot l t =
    (String.make (l * 2) ' ')
    ^ (match t with
    | Let (v, NoProp (v'), t) when v = v' -> "<b>noprop</i> <var>" ^ v
      ^ "</var> ;\n" ^ (hot l t)
    | Let (v, Prime (v'), t) when v = v' -> "<b>prime</b> <var>" ^ v
      ^ "</var> ;\n" ^ (hot l t)
    | Let (v, e, t) -> "<var>" ^ v ^ "</var> := " ^ (hot 0 e) ^ " ;\n" ^ (hot l t)
    | Var (v) | NoProp (v) | Prime (v) -> "<var>" ^ v ^ "</var>"
    | Protected (t) -> "<i>{</i>" ^ (hot 0 t) ^ "<i>}</i>"
    | If (c, t, e) -> "<b>if</b> " ^ (hot 0 c) ^ "<b>then</b>\n"
      ^ (hot (l + 1) t) ^ "\n"
      ^ (String.make (l * 2) ' ') ^ "<b>else</b>\n" ^ (hot l e) ^ "\n<b>end</b>"
    | Sum (l) -> List.fold_left (fun h t -> h ^ " + " ^ (hot 0 t))
      (hot 0 (List.hd l)) (List.tl l)
    | Opp (t) -> "-" ^ (hot 0 t)
    | Prod (l) -> List.fold_left (fun h t -> h ^ " * " ^ (hot 0 t))
      (hot 0 (List.hd l)) (List.tl l)
    | Inv (t) -> "(" ^ (hot 0 t) ^ ")<sup>-1</sup>"
    | Exp (a, b) -> "(" ^ (hot 0 a) ^ ")<sup>" ^ (hot 0 b) ^ "</sup>"
    | Mod (a, b) -> (hot 0 a) ^ " <b>mod</b> " ^ (hot 0 b)
    | Zero -> "0"
    | One -> "1"
    | RandomFault (_) -> "<strong>Fault</strong>"
    | ZeroFault (_) -> "<em>Fault</em>")
  in hot 0 t
;;
