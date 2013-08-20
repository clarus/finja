open Batteries ;;
open Computation ;;

exception Faulted of term ;;
exception Not_prime of term ;;
exception Should_not_happen ;;

module StrMap = Map.Make(String) ;;

let rec flatten_sum = function
  | Sum (l) :: tl -> (flatten_sum l) @ (flatten_sum tl)
  | t :: tl       -> t :: (flatten_sum tl)
  | []            -> []
;;

let rec flatten_prod = function
  | Prod (l) :: tl -> (flatten_prod l) @ (flatten_prod tl)
  | t :: tl        -> t :: (flatten_prod tl)
  | []             -> []
;;

let rec quotient a b =
  match a, b with
  | Prod (pa), Prod (pb) ->
    if (List.fold_left (fun acc e -> acc && (List.mem e pa)) true pb)
    then Prod (pb)
    else Prod (pa)
  | a', (Prod (_) as b) -> quotient (Prod ([ a' ])) b
  | (Prod (_) as a), b' -> quotient a (Prod ([ b' ]))
  | a', b'              -> quotient (Prod ([ a' ])) (Prod ([ b' ]))
;;

let phi t =
  let rec phi_ = function
    | Prime (_) as p -> Sum([ Opp(One) ; p ])
    | Exp (Prime (_) as p, k) ->
      Prod ([ Sum([ Opp (One) ; p ]) ; Exp (p, Sum ([ Opp (One) ; k ])) ])
    | Prod (l) -> Prod (List.map (fun p -> phi_ p) l)
    | RandomFault (_) as t -> raise (Faulted t)
    | ZeroFault (_) as t -> raise (Faulted t)
    | _ as t -> raise (Not_prime t)
  in
  try phi_ t with Faulted (t) -> t
;;

let rec reduce_sum env l =
  let rec red_s e before after =
    match after with
    | hd :: tl ->
      if red env (Opp (hd)) = e then before @ tl
      else red_s e (before @ [ hd ]) tl
    | [] -> e :: before
  in match l with
  | Zero :: tl          -> reduce_sum env tl
  | ZeroFault (_) :: tl -> reduce_sum env tl
  | hd :: tl            -> red_s hd [] (reduce_sum env tl)
  | []                  -> []

and reduce_prod env l =
  let rec red_p e before after =
    match after with
    | hd :: tl ->
      begin
        match hd with
        | Zero          -> [ Zero ]
        | ZeroFault (_) -> [ Zero ]
        | _             ->
          if red env (Inv (hd)) = e then before @ tl
          else red_p e (before @ [ hd ]) tl
      end
    | [] -> e :: before
  in match l with
  | Zero :: _          -> [ Zero ]
  | ZeroFault (_) :: _ -> [ Zero ]
  | One :: tl          -> reduce_prod env tl
  | hd :: tl           -> red_p hd [] (reduce_prod env tl)
  | []                 -> []

and reduce_mod env t m =
  if t = m then Zero
  else t (* match t with
  | Let (v, e, t)      ->
  | Var (v)            ->
  | NoProp (v)         ->
  | Prime (v)          ->
  | Protected (t)      ->
  | If (c, t, e)       ->
  | Sum (l)            ->
  | Opp (t)            ->
  | Prod (l)           ->
  | Inv (t)            ->
  | Exp (a, b)         ->
  | Mod (a, b)         ->
  | Zero               ->
  | One                ->
  | Eq (a, b)          ->
  | NotEq (a, b)       ->
  | EqMod (a, b, m)    ->
  | NotEqMod (a, b, m) ->
  | And (a, b)         ->
  | Or (a, b)          ->
  | Return (t)         ->
  | RandomFault (_)    ->
  | ZeroFault (_)      -> *)

and reduce_cond env = function
  | Eq (a, b)          -> red env a = red env b
  | NotEq (a, b)       -> red env a <> red env b
  | EqMod (a, b, m)    -> reduce_mod env a m = reduce_mod env b m
  | NotEqMod (a, b, m) -> reduce_mod env a m = reduce_mod env b m
  | And (a, b)         -> reduce_cond env a && reduce_cond env b
  | Or (a, b)          -> reduce_cond env a || reduce_cond env b
  | _ as t             -> red env t <> Zero

and red env term =
  match term with
  | Let (v, e, t)      -> red (StrMap.add v (red env e) env) t
  | Var (v)            -> red env (StrMap.find v env)
  | NoProp (v)         -> NoProp (v)
  | Prime (v)          -> Prime (v)
  | Protected (t)      -> Protected (red env t)
  | If (c, t, e)       -> if reduce_cond env c
    then red env t else red env e
  | Sum (l)            ->
    let l' = List.stable_sort compare
      (reduce_sum env
         (List.map (red env)
            (List.stable_sort compare (flatten_sum l)))) in
    begin
      match l' with
      | []    -> Zero
      | [ t ] -> t (* TODO see if reduction needed *)
      | _     -> Sum (l')
    end
  | Opp (t)            ->
    let t' = red env t in
    begin
      match t' with
      | Opp (t)       -> t
      | Zero          -> Zero
      | ZeroFault (_) -> Zero
      | _             -> Opp (t')
    end
  | Prod (l)           ->
    let l' = List.stable_sort compare
      (reduce_prod env
         (List.map (red env)
            (List.stable_sort compare (flatten_prod l)))) in
    begin
      match l' with
      | []    -> One
      | [ t ] -> t
      | _     -> Prod (l')
    end
  | Inv (t)            ->
    let t' = red env t in
    begin
      match t' with
      | Inv (t) -> t
      | One     -> One
      | _       -> Inv (t')
    end
  | Exp (a, b)         ->
    let a' = red env a in
    let b' = red env b in
    begin
      match b' with
      | Zero          -> One
      | ZeroFault (_) -> One
      | Opp (One)     -> Inv (a')
      | _ -> begin
        match a' with
        | Exp (a, b) -> red env (Exp (a, Prod ([ b ; b' ])))
        | _          -> Exp (a', b')
      end
    end
  | Mod (a, b)         ->
    let b' = red env b in
    if b' = One then Zero else
      begin
        match reduce_mod env b' a with
        | Zero          -> Zero
        | ZeroFault (f) -> Zero
        | Mod (a, b)    ->
          let b_ = quotient b b' in
          if b_ = b then Mod (red env (Mod (a, b)), b')
          else red env (Mod (a, b_))
        | _ as a'       -> Mod (a', b')
      end
  | Zero               -> Zero
  | One                -> One
  | Eq (a, b)          -> raise Should_not_happen
  | NotEq (a, b)       -> raise Should_not_happen
  | EqMod (a, b, m)    -> raise Should_not_happen
  | NotEqMod (a, b, m) -> raise Should_not_happen
  | And (a, b)         -> raise Should_not_happen
  | Or (a, b)          -> raise Should_not_happen
  | Return (t)         -> red env t
  | RandomFault (f)    -> RandomFault (f)
  | ZeroFault (f)      -> Zero
;;

let reduce term =
  red StrMap.empty term
;;
