type term_p = {
  term: pterm ;
  spos: Lexing.position;
  epos: Lexing.position;
}
and pterm =
| PLet of string * term_p * term_p
| PVar of string
| PNoProp of string
| PPrime of string
| PProtected of term_p
| PIf of term_p * term_p * term_p
| PSum of term_p list
| POpp of term_p
| PProd of term_p list
| PInv of term_p
| PExp of term_p * term_p
| PMod of term_p * term_p
| PZero
| POne
| PReturn of term_p
| PEq of term_p * term_p
| PNotEq of term_p * term_p
| PEqMod of term_p * term_p * term_p
| PNotEqMod of term_p * term_p * term_p

type term =
| Let of string * term * term
| Var of string
| NoProp of string
| Prime of string
| Protected of term
| If of term * term * term
| Sum of term list
| Opp of term
| Prod of term list
| Inv of term
| Exp of term * term
| Mod of term * term
| Zero
| One
| RandomFault of int
| ZeroFault of int
| Return of term
| Eq of term * term
| NotEq of term * term
| EqMod of term * term * term
| NotEqMod of term * term * term

type attack_condition =
| False
| True
| Faulted
| Result
| Equal of attack_condition * attack_condition
| NotEqual of attack_condition * attack_condition
| EqualMod of attack_condition * attack_condition * string
| NotEqualMod of attack_condition * attack_condition * string
| And of attack_condition * attack_condition
| Or of attack_condition * attack_condition

type description = term_p * attack_condition

type fault_type =
| Randomizing
| Zeroing
| Both
