open Batteries ;;
open Computation ;;

let check_attack cond term faulted_term =
  let test = Let ("_", term, Let ("@", faulted_term, If (cond, One, Zero))) in
  Reduction.reduce test = Zero
;;
