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

type description = term_p * term_p

type fault_type =
| Randomizing
| Zeroing
| Both
