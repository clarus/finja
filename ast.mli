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

type attack_condition =
| False

type fia = term * attack_condition
