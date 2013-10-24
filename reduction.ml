open Batteries ;;
open Computation ;;

exception Faulted of term ;;
exception Not_prime ;;
exception Should_not_happen ;;

module Env = Map.Make(String) ;;

let env_at_return = ref Env.empty ;;

let rec flatten_sum = function
  | Opp (Sum (l)) :: tl ->
    (flatten_sum (List.map (fun e -> Opp (e)) l)) @ (flatten_sum tl)
  | Sum (l) :: tl       -> (flatten_sum l) @ (flatten_sum tl)
  | t :: tl             -> t :: (flatten_sum tl)
  | []                  -> []
;;

let rec flatten_prod = function
  | Inv (Prod (l)) :: tl ->
    (flatten_prod (List.map (fun e -> Inv (e)) l)) @ (flatten_prod tl)
  | Prod (l) :: tl       -> (flatten_prod l) @ (flatten_prod tl)
  | t :: tl              -> t :: (flatten_prod tl)
  | []                   -> []
;;

let coprimes l =
  let rec cop l acc =
    match l with
    | Prime (_) as p :: tl -> p :: cop tl acc
    | _ as hd :: tl        -> cop tl (hd :: acc)
    | []                   -> if List.is_empty acc then [] else [ Prod (acc) ]
  in cop (flatten_prod l) []
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
    | _ -> raise Not_prime
  in
  try phi_ t with
  | Faulted (t)   -> t
  | Not_prime -> t
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
    | hd :: tl -> begin
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

and reduce_mod env m t =
  let rec red_mod env m t =
    if t = m then Zero
    else match t with
    | Let (v, e, t)      ->
      reduce_mod (Env.add v (reduce_mod env m e) env) m e
    | Var (v)            -> reduce_mod env m (Env.find v env)
    | NoProp (v)         -> NoProp (v)
    | Prime (v)          -> Prime (v)
    | Protected (t)      -> reduce_mod env m t
    | If (c, t, e)       -> raise Should_not_happen
    | Sum (l)            -> Sum (List.map (reduce_mod env m) l)
    | Opp (t)            -> Opp (reduce_mod env m t)
    | Prod (l)           -> Prod (List.map (reduce_mod env m) l)
    | Inv (t)            -> Inv (reduce_mod env m t)
    | Exp (a, b)         ->
      let a' = reduce_mod env m a in
      let b' = red env b in
      begin
        match m, a' with
        | Prod ([ r ; r' ]), Sum ([ One ; r'' ]) when r = r' && r = r'' ->
          Sum ([ One ; Prod ([ r ; reduce_mod env m b' ]) ])
        | _, _ -> begin
          match b' with
          | Opp (One)    -> Inv (reduce_mod env m a)
          | Sum ([ Prime (_) as p ; Opp (One) ])
              when p = m -> One
          | _            -> Exp (a', Mod (b', phi m))
        end
      end
    | Mod (a, b)         ->
      let m' = red env (quotient (red env b) m) in
      if m' = m then reduce_mod env m a
      else red env (Mod (a, m'))
    | Zero               -> Zero
    | One                -> if m = One then Zero else One
    | Eq (a, b)          -> raise Should_not_happen
    | NotEq (a, b)       -> raise Should_not_happen
    | EqMod (a, b, m)    -> raise Should_not_happen
    | NotEqMod (a, b, m) -> raise Should_not_happen
    | And (a, b)         -> raise Should_not_happen
    | Or (a, b)          -> raise Should_not_happen
    | Return (t)         ->
      let r = reduce_mod env m t in
      env_at_return := Env.add "_" r env;
      r
    | RandomFault (_)    -> if m = t then Zero else t
    | ZeroFault (_)      -> Zero
    | Nil                -> Nil
  in red env (red_mod env (red env m) (red env t))

and reduce_cond env = function
  | Eq (a, b)          -> red env a = red env b
  | NotEq (a, b)       -> red env a <> red env b
  | EqMod (a, b, m)    -> begin
    match red env m with
    | Prod (l) ->
      List.fold_left
        (fun acc e -> acc && reduce_mod env e a = reduce_mod env e b)
        true (coprimes l)
    | _        -> reduce_mod env m a = reduce_mod env m b
  end
  | NotEqMod (a, b, m) -> begin
    match red env m with
    | Prod (l) ->
      List.fold_left
        (fun acc e -> acc || reduce_mod env e a <> reduce_mod env e b)
        false (coprimes l)
    | _        -> reduce_mod env m a <> reduce_mod env m b
  end
  | And (a, b)         -> reduce_cond env a && reduce_cond env b
  | Or (a, b)          -> reduce_cond env a || reduce_cond env b
  | _ as t             -> red env t <> Zero

and red env term =
  match term with
  | Let (v, e, t)      -> red (Env.add v (red env e) env) t
  | Var (v)            -> red env (Env.find v env)
  | NoProp (v)         -> NoProp (v)
  | Prime (v)          -> Prime (v)
  | Protected (t)      -> red env t
  | If (c, t, e)       ->
    if reduce_cond env c
    then let r = red env t in env_at_return := Env.add "_" r env; r
    else red env e
  | Sum (l)            ->
    let l' = List.stable_sort compare
      (reduce_sum env
         (List.map (red env)
            (List.stable_sort compare (flatten_sum l)))) in
    begin
      match l' with
      | []    -> Zero
      | [ t ] -> t
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
    if b' = One then Zero else begin
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
  | Return (t)         ->
    let r = red env t in
    env_at_return := Env.add "_" r env;
    r
  | RandomFault (f)    -> RandomFault (f)
  | ZeroFault (f)      -> Zero
  | Nil                -> Nil
;;

let reduce term =
  red Env.empty term
;;
