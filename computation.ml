open Batteries ;;

type term =
| Let of string * term * term
| Var of string
| NoProp of string
| Prime of string
| Protected of term
| If of cond * term * term
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
| Return of term

and cond =
| Eq of term * term
| NotEq of term * term
| EqMod of term * term * term
| NotEqMod of term * term * term

type attack_condition =
| False

type description = term * attack_condition

let html_of_term t =
  let rec html_of_cond = function
    | Eq (a, b) -> (hot a) ^ " = " ^ (hot b)
    | NotEq (a, b) -> (hot a) ^ " &ne; " ^ (hot b)
    | EqMod (a, b, m) -> (hot a) ^ " &equiv; " ^ (hot b)
      ^ " (mod " ^ (hot m) ^ ")"
    | NotEqMod (a, b, m) -> (hot a) ^ " &#8802; " ^ (hot b)
      ^ " (mod " ^ (hot m) ^ ")"
  and noprop = function
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
    | If (c, t, e) -> "<b>if</b> " ^ (html_of_cond c) ^ " <b>abort with</b> "
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
    | RandomFault (_) -> "<strong>Fault</strong>"
    | ZeroFault (_) -> "<em>Fault</em>"
    | Return (t) -> "<b>return</b> " ^ (hot t) ^ " ;\n"
  in hot t
;;
