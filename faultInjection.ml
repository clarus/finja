open Batteries ;;
open Computation ;;

exception Non_faultable ;;
exception Error ;;

let inject_fault term transcient fault_type tentative =
  let counter = ref (-1) in
  let rec inj inc t =
    if inc then counter := !counter + 1;
    if inc && !counter = tentative then
      match t with
      | Protected _               -> raise Non_faultable
      | Var _ when not transcient -> raise Non_faultable
      | Zero when not transcient  -> raise Non_faultable
      | One when not transcient   -> raise Non_faultable
      | _ -> begin
        match fault_type with
        | Randomizing -> RandomFault (tentative)
        | Zeroing     -> ZeroFault (tentative)
        | Both        -> raise Error
      end
    else
      match t with
      | Let (v, e, t)      -> Let (v, inj true e, inj false t)
      | Var (v)            -> Var (v)
      | NoProp (v)         -> NoProp (v)
      | Prime (v)          -> Prime (v)
      | Protected (t)      -> Protected (t)
      | If (c, t, e)       -> If (inj true c, inj true t, inj false e)
      | Sum (l)            -> Sum (List.map (inj true) l)
      | Opp (t)            -> Opp (inj true t)
      | Prod (l)           -> Prod (List.map (inj true) l)
      | Inv (t)            -> Inv (inj true t)
      | Exp (a, b)         -> Exp (inj true a, inj true b)
      | Mod (a, b)         -> Mod (inj true a, inj true b)
      | Zero               -> Zero
      | One                -> One
      | Eq (a, b)          -> Eq (inj true a, inj true  b)
      | NotEq (a, b)       -> NotEq (inj true a, inj true  b)
      | EqMod (a, b, m)    -> EqMod (inj true a, inj true b, inj true m)
      | NotEqMod (a, b, m) -> NotEqMod (inj true a, inj true b, inj true m)
      | And (a, b)         -> And (inj true a, inj true b)
      | Or (a, b)          -> Or (inj true a, inj true b)
      | Return (t)         -> Return (inj true t)
      | RandomFault (f)    -> RandomFault (f)
      | ZeroFault (f)      -> ZeroFault (f)
  in inj true term
;;


(* TODO permanent faulting actually needs state? *)
