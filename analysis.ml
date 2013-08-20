open Batteries ;;
open Computation ;;

let check cond term faulted_term =
  term = faulted_term
;;
