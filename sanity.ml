open Batteries ;;
open Computation ;;

exception Error of string * Lexing.position * Lexing.position ;;

module StrSet = Set.Make(String) ;;

let t pterm = pterm.term ;;
let s pterm = pterm.spos ;;
let e pterm = pterm.epos ;;

let check desc =

  let cond_env = ref (StrSet.add "_" (StrSet.add "@" StrSet.empty)) in

  let rec flatten_sum = function
    | { term = PSum (s) ; spos = _ ; epos = _ } :: tl ->
      (flatten_sum s) @ (flatten_sum tl)
    | t :: tl -> t :: (flatten_sum tl)
    | []      -> []

  and flatten_prod = function
    | { term = PProd (s) ; spos = _ ; epos = _ } :: tl ->
      (flatten_prod s) @ (flatten_prod tl)
    | t :: tl -> t :: (flatten_prod tl)
    | []      -> []

  and chk env pt =
    match t pt with
    | PLet (v, x, t) ->
      if StrSet.mem v env then
        raise (Error ("Variable " ^ v ^ " is defined.", s pt, e pt));
      cond_env := StrSet.add v !cond_env;
      Let (v, chk env x, chk (StrSet.add v env) t)
    | PVar (v) ->
      if not (StrSet.mem v env) then
        raise (Error ("Variable " ^ v ^ " does not exists.", s pt, e pt));
      Var (v)
    | PNoProp (v)         -> NoProp (v)
    | PPrime (v)          -> Prime (v)
    | PProtected (t)      -> Protected (chk env t)
    | PIf (c, h, l)       -> If (chk env c, chk env h, chk env l)
    | PSum (l)            -> Sum (List.map (chk env) (flatten_sum l))
    | POpp (t)            -> Opp (chk env t)
    | PProd (l)           -> Prod (List.map (chk env) (flatten_prod l))
    | PInv (t)            -> Inv (chk env t)
    | PExp (a, b)         -> Exp (chk env a, chk env b)
    | PMod (a, b)         -> Mod (chk env a, chk env b)
    | PZero               -> Zero
    | POne                -> One
    | PEq (a, b)          -> Eq (chk env a, chk env b)
    | PNotEq (a, b)       -> NotEq (chk env a, chk env b)
    | PEqMod (a, b, m)    -> EqMod (chk env a, chk env b, chk env m)
    | PNotEqMod (a, b, m) -> NotEqMod (chk env a, chk env b, chk env m)
    | PAnd (a, b)         -> And (chk env a, chk env b)
    | POr (a, b)          -> Or (chk env a, chk env b)
    | PReturn (t)         -> Return (chk env t)
  in
  let term = chk StrSet.empty (fst desc) in
  let cond = chk !cond_env (snd desc) in
  term, cond
;;
