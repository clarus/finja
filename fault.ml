open Batteries ;;
open Computation ;;

exception Non_faultable ;;
exception End ;;
exception Should_not_happen ;;

let iterator max locations =
  let rec next = function
    | []                        -> []
    | [ l ] when l >= max       -> raise End
    | [ l ]                     -> [ l + 1 ]
    | a :: b :: tl when b = max ->
      List.init (2 + (List.length tl)) (fun i -> a + 1 + i)
    | hd :: tl                  -> hd :: (next tl)
  in try next locations
    with End -> []
;;

let inject_fault term transient fault_type location =
  let faulted_subterm = ref (-1, ANil) in
  let rec inj protected term =
    if fst term = location then
      if protected then raise Non_faultable
      else match snd term with
      | AProtected (_)              -> raise Non_faultable
      | AVar (_) when not transient -> raise Non_faultable
      | AZero when not transient    -> raise Non_faultable
      | AOne when not transient     -> raise Non_faultable
      | ARandomFault (f)            -> faulted_subterm := term;
        fst term, ARandomFault (-location)
      | AZeroFault (f)              -> faulted_subterm := term;
        fst term, AZeroFault (-location)
      | _ -> begin
        match fault_type with
        | Randomizing -> faulted_subterm := term;
          fst term, ARandomFault (location)
        | Zeroing     -> faulted_subterm := term;
          fst term, AZeroFault (location)
      end
    else
      let inj' = inj protected in
      match snd term with
      | ALet (v, e, t)      -> fst term, ALet (v, inj' e, inj' t)
      | AVar (v)            -> fst term, AVar (v)
      | ANoProp (v)         -> fst term, ANoProp (v)
      | APrime (v)          -> fst term, APrime (v)
      | AProtected (t)      -> fst term, AProtected (inj true t)
      | AIf (c, t, e)       -> fst term, AIf (inj' c, inj' t, inj' e)
      | ASum (l)            -> fst term, ASum (List.map (inj') l)
      | AOpp (t)            -> fst term, AOpp (inj' t)
      | AProd (l)           -> fst term, AProd (List.map (inj') l)
      | AInv (t)            -> fst term, AInv (inj' t)
      | AExp (a, b)         -> fst term, AExp (inj' a, inj' b)
      | AMod (a, b)         -> fst term, AMod (inj' a, inj' b)
      | AZero               -> fst term, AZero
      | AOne                -> fst term, AOne
      | AEq (a, b)          -> fst term, AEq (inj' a, inj'  b)
      | ANotEq (a, b)       -> fst term, ANotEq (inj' a, inj'  b)
      | AEqMod (a, b, m)    -> fst term, AEqMod (inj' a, inj' b, inj' m)
      | ANotEqMod (a, b, m) -> fst term, ANotEqMod (inj' a, inj' b, inj' m)
      | AAnd (a, b)         -> fst term, AAnd (inj' a, inj' b)
      | AOr (a, b)          -> fst term, AOr (inj' a, inj' b)
      | AReturn (t)         -> fst term, AReturn (inj' t)
      | ARandomFault (f)    -> fst term, ARandomFault (f)
      | AZeroFault (f)      -> fst term, AZeroFault (f)
      | ANil                -> fst term, ANil
  in
  let faulted_term = inj false term in
  faulted_term, !faulted_subterm
;;

let inject term transient fault_types locations =
  let rec inj fault_types locations term fsubterms =
    match locations, fault_types with
    | l_h :: l_t, t_h :: t_t ->
      let fterm, fsubterm = inject_fault term transient t_h l_h in
      inj t_t l_t fterm (fsubterm :: fsubterms)
    | [], [] -> term, fsubterms
    | [], _  -> raise Should_not_happen
    | _, []  -> raise Should_not_happen
  in try Some (inj fault_types (List.rev locations) term [])
    with Non_faultable -> None
;;
