open Batteries ;;
open Computation ;;

let check_attack env cond faulted_term =
  let check = Let ("@", faulted_term, If (cond, One, Zero)) in
  Reduction.red env check = One
;;
