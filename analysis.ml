open Batteries ;;
open Computation ;;

exception Should_not_happen ;;

let order term =
  let addr = ref (-1) in
  let rec p inc t =
    if inc then incr addr;
    let i = !addr in
    match t with
    | Let (v, e, t)      -> i, ALet (v, p true e, (-1, snd (p false t)))
    | Var (v)            -> i, AVar (v)
    | NoProp (v)         -> i, ANoProp (v)
    | Prime (v)          -> i, APrime (v)
    | Protected (t)      -> i, AProtected (p true t)
    | If (c, t, e)       -> i, AIf (p true c, p true t, (-1, snd (p false e)))
    | Sum (l)            -> i, ASum (List.map (p true) l)
    | Opp (t)            -> i, AOpp (p true t)
    | Prod (l)           -> i, AProd (List.map (p true) l)
    | Inv (t)            -> i, AInv (p true t)
    | Exp (a, b)         -> i, AExp (p true a, p true b)
    | Mod (a, b)         -> i, AMod (p true a, p true b)
    | Zero               -> i, AZero
    | One                -> i, AOne
    | Eq (a, b)          -> i, AEq (p true a, p true  b)
    | NotEq (a, b)       -> i, ANotEq (p true a, p true  b)
    | EqMod (a, b, m)    -> i, AEqMod (p true a, p true b, p true m)
    | NotEqMod (a, b, m) -> i, ANotEqMod (p true a, p true b, p true m)
    | And (a, b)         -> i, AAnd (p true a, p true b)
    | Or (a, b)          -> i, AOr (p true a, p true b)
    | Return (t)         -> i, AReturn (p true t)
    | RandomFault (f)    -> raise Should_not_happen
    | ZeroFault (f)      -> raise Should_not_happen
    | Nil                -> raise Should_not_happen
  in
  let aterm = p false term in
  aterm, !addr
;;

let rec extract aterm =
  match snd aterm with
  | ALet (v, e, t)      -> Let (v, extract e, extract t)
  | AVar (v)            -> Var (v)
  | ANoProp (v)         -> NoProp (v)
  | APrime (v)          -> Prime (v)
  | AProtected (t)      -> Protected (extract t)
  | AIf (c, t, e)       -> If (extract c, extract t, extract e)
  | ASum (l)            -> Sum (List.map extract l)
  | AOpp (t)            -> Opp (extract t)
  | AProd (l)           -> Prod (List.map extract l)
  | AInv (t)            -> Inv (extract t)
  | AExp (a, b)         -> Exp (extract a, extract b)
  | AMod (a, b)         -> Mod (extract a, extract b)
  | AZero               -> Zero
  | AOne                -> One
  | AEq (a, b)          -> Eq (extract a, extract  b)
  | ANotEq (a, b)       -> NotEq (extract a, extract  b)
  | AEqMod (a, b, m)    -> EqMod (extract a, extract b, extract m)
  | ANotEqMod (a, b, m) -> NotEqMod (extract a, extract b, extract m)
  | AAnd (a, b)         -> And (extract a, extract b)
  | AOr (a, b)          -> Or (extract a, extract b)
  | AReturn (t)         -> Return (extract t)
  | ARandomFault (f)    -> RandomFault (f)
  | AZeroFault (f)      -> ZeroFault (f)
  | ANil                -> Nil
;;

let check_attack env cond faulted_term =
  Reduction.reduce_cond
    (Reduction.Env.add "@" (Reduction.reduce faulted_term) env)
    cond
;;

let analyse out env term cond transient fault_type count =
  let aterm, max_location = order term in
  let next = Fault.iterator max_location in
  let inject = Fault.inject aterm transient in
  let fault_types = List.make count fault_type in
  let rec loop locations success =
    match inject fault_types locations with
    | None -> begin
      match next locations with
      | []        -> success
      | locations -> loop locations success
      end
    | Some (faulted_term, faulted_subterms) ->
      let fterm = extract faulted_term in
      let result = check_attack env cond fterm in
      Html.print_attempt out fterm (List.map extract faulted_subterms)
        result;
      let success = success + (if result then 1 else 0) in
      match next locations with
      | []        -> success
      | locations -> loop locations success
  in loop (List.init count identity) 0
;;
