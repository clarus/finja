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
  let group l =
    List.map (function | [t] -> t | l -> Prod (l))
      (List.group_consecutive (=) (List.stable_sort compare l))
  in
  let rec cop l acc =
    match l with
    | Prime (_) as p :: tl       -> p :: cop tl acc
    | RandomFault (_) as r :: tl -> r :: cop tl acc
    | NoProp (_) as n :: tl      -> n :: cop tl acc
    | _ as hd :: tl              -> cop tl (hd :: acc)
    | [] -> if List.is_empty acc then [] else [ Prod (acc) ]
  in group (cop (flatten_prod l) [])
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
    | Prime (_) as p          -> Sum([ Opp(One) ; p ])
    | Exp (Prime (_) as p, k) -> Prod ([ p ; Sum([ Opp (One) ])
                                       ; Exp (p, Sum ([ k ; Opp (One) ])) ])
    | Prod (l)                -> Prod (List.map (fun p -> phi_ p) l)
    | RandomFault (_) as t    -> raise (Faulted t)
    | ZeroFault (_) as t      -> raise (Faulted t)
    | _ -> raise Not_prime
  in
  try phi_ t with
  | Faulted (t) -> t
;;

let crt l m =
  let recomposition a b p q =
    Mod (Sum ([ Prod ([ a ; q ; Mod (Inv (q), p) ])
              ; Prod ([ b ; p ; Mod (Inv (p), q) ]) ]),
         Prod ([ p ; q ]))
  in
  let rec loop = function
    | [t], [p]             -> t
    | t :: m_tl, p :: c_tl ->
      recomposition t (loop (m_tl, c_tl)) p (Prod (c_tl))
    | _, _                 -> raise Should_not_happen
  in loop (l, m)
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
    | Let (v, e, t)      -> raise Should_not_happen
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
        | Prod ([ r ; r' ]), Sum ([ One ; r_ ]) when r = r' && r = r_ ->
          Sum ([ One ; Prod ([ r ; reduce_mod env m b' ]) ])
        | _, _ -> begin
          match b' with
          | Opp (One)    -> Inv (reduce_mod env m a)
          | Sum ([ Prime (_) as p ; Opp (One) ])
              when p = m -> One
          | _            -> Exp (a', try Mod (b', phi m) with Not_prime -> b')
        end
      end
    | Mod (a, b)         ->
      let m' = red env (quotient (red env b) m) in
      if m' = m then reduce_mod env m a
      else red env (Mod (a, m'))
    | Zero               -> Zero
    | One                -> One
    | Eq (a, b)          -> raise Should_not_happen
    | NotEq (a, b)       -> raise Should_not_happen
    | EqMod (a, b, m)    -> raise Should_not_happen
    | NotEqMod (a, b, m) -> raise Should_not_happen
    | And (a, b)         -> raise Should_not_happen
    | Or (a, b)          -> raise Should_not_happen
    | Return (t)         -> raise Should_not_happen
    | RandomFault (_)    -> t
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
    | _ as m   -> reduce_mod env m a = reduce_mod env m b
  end
  | NotEqMod (a, b, m) -> begin
    match red env m with
    | Prod (l) ->
      List.fold_left
        (fun acc e -> acc || reduce_mod env e a <> reduce_mod env e b)
        false (coprimes l)
    | _ as m   -> reduce_mod env m a <> reduce_mod env m b
  end
  | And (a, b)         -> reduce_cond env a && reduce_cond env b
  | Or (a, b)          -> reduce_cond env a || reduce_cond env b
  | _ as t             -> red env t <> Zero

and red env term =
  match term with
  | Let (v, e, t)      -> red (Env.add v e env) t
  | Var (v)            -> red env (Env.find v env)
  | NoProp (v)         -> NoProp (v)
  | Prime (v)          -> Prime (v)
  | Protected (t)      -> red env t
  | If (c, t, e)       -> env_at_return := env;
    red env (if reduce_cond env c then t else e)
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
  | Opp (t)            -> begin
    match red env t with
    | Opp (t)       -> t
    | Zero          -> Zero
    | ZeroFault (_) -> Zero
    | _ as t        -> Opp (t)
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
  | Inv (t)            -> begin
    match red env t with
    | Inv (t) -> t
    | One     -> One
    | _ as t  -> Inv (t)
  end
  | Exp (a, b)         -> begin
    match red env b with
    | Zero          -> One
    | ZeroFault (_) -> One
    | Opp (One)     -> Inv (red env a)
    | _  as b'      -> begin
      match red env a with
      | Exp (a, b) -> red env (Exp (a, Prod ([ b ; b' ])))
      | _ as a'    -> Exp (a', b')
    end
  end
  | Mod (a, b)         -> begin
    match red env b with
    | One            -> Zero
    | _ as b'        -> begin
      match reduce_mod env b' a with
      | Zero          -> Zero
      | ZeroFault (f) -> Zero
      | Mod (a, b)    ->
        let b_ = quotient b b' in
        if b_ = b then Mod (red env (Mod (a, b)), b')
        else red env (Mod (a, b_))
      | _ as a'       -> Mod (a', b')
    end
  end
  | Zero               -> Zero
  | One                -> One
  | Eq (a, b)          -> raise Should_not_happen
  | NotEq (a, b)       -> raise Should_not_happen
  | EqMod (a, b, m)    -> raise Should_not_happen
  | NotEqMod (a, b, m) -> raise Should_not_happen
  | And (a, b)         -> raise Should_not_happen
  | Or (a, b)          -> raise Should_not_happen
  | Return (t)         -> env_at_return := env; red env t
  | RandomFault (f)    -> RandomFault (f)
  | ZeroFault (f)      -> Zero
  | Nil                -> Nil
;;

let reduce term =
  let result =
    match red Env.empty term with
    | Mod (a, Prod (l)) ->
      let c = coprimes l in
      red !env_at_return
        (crt (List.map (fun e -> reduce_mod !env_at_return e a) c) c)
    | _ as t -> t
  in env_at_return := Env.add "_" result !env_at_return;
  result
;;
