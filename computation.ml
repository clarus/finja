TYPE_CONV_PATH "Computation"

open Sexplib.Std

type term_p = {
  term: pterm ;
  spos: Lexing.position;
  epos: Lexing.position;
}
and pterm =
| PLet       of string * term_p * term_p
| PVar       of string
| PNoProp    of string
| PPrime     of string
| PProtected of term_p
| PIf        of term_p * term_p * term_p
| PSum       of term_p list
| POpp       of term_p
| PProd      of term_p list
| PInv       of term_p
| PExp       of term_p * term_p
| PMod       of term_p * term_p
| PZero
| POne
| PEq        of term_p * term_p
| PNotEq     of term_p * term_p
| PEqMod     of term_p * term_p * term_p
| PNotEqMod  of term_p * term_p * term_p
| PAnd       of term_p * term_p
| POr        of term_p * term_p
| PReturn    of term_p
;;

type term_a = int * aterm
and aterm =
| ALet         of string * term_a * term_a
| AVar         of string
| ANoProp      of string
| APrime       of string
| AProtected   of term_a
| AIf          of term_a * term_a * term_a
| ASum         of term_a list
| AOpp         of term_a
| AProd        of term_a list
| AInv         of term_a
| AExp         of term_a * term_a
| AMod         of term_a * term_a
| AZero
| AOne
| AEq          of term_a * term_a
| ANotEq       of term_a * term_a
| AEqMod       of term_a * term_a * term_a
| ANotEqMod    of term_a * term_a * term_a
| AAnd         of term_a * term_a
| AOr          of term_a * term_a
| AReturn      of term_a
| ARandomFault of int
| AZeroFault   of int
| ANil
;;

type term =
| Let         of string * term * term
| Var         of string
| NoProp      of string
| Prime       of string
| Protected   of term
| If          of term * term * term
| Sum         of term list
| Opp         of term
| Prod        of term list
| Inv         of term
| Exp         of term * term
| Mod         of term * term
| Zero
| One
| Eq          of term * term
| NotEq       of term * term
| EqMod       of term * term * term
| NotEqMod    of term * term * term
| And         of term * term
| Or          of term * term
| Return      of term
| RandomFault of int
| ZeroFault   of int
| Nil
with sexp_of ;;

type description = term_p * term_p ;;

type fault_type =
| Randomizing
| Zeroing
;;

let print_term t =
  print_endline (Sexplib.Sexp.to_string (sexp_of_term t))
;;
