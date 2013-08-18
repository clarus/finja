%{
  (* open Batteries ;; *)
  open Computation ;;

  let t pt s e = { term = pt ; spos = s ; epos = e } ;;
  
%}

%token <string> Lident
%token Lnoprop Lprime
%token Lcomma Lsemicolon
%token Lassign
%token Lequal Lnoteq
%token Lone Lzero
%token Lplus Lminus Lstar Lcirc Lmod
%token Lobrace Lcbrace
%token Loparen Lcparen
%token Lif Labortwith
%token Lobracket Lcbracket
%token Lpercent
(* %token Lslash Lbackslash *)
(* %token Lat *)
%token Lreturn
%token Leof

%left Lplus Lminus
%left Lcirc Lmod
%left Lstar
%nonassoc uminus

%start desc

%type <Computation.description> desc

%%

desc:
 | pt = term; Lpercent; a = attack; Leof {
   (pt, a)
 }
;

term:
| Lnoprop; l = separated_nonempty_list(Lcomma, Lident); Lsemicolon; cont = term {
  List.fold_right
  (fun i acc -> t (PLet (i, t (PNoProp (i)) $startpos(l) $endpos(l), acc))
    $startpos $endpos)
  l cont
}
| Lprime; l = separated_nonempty_list(Lcomma, Lident); Lsemicolon; cont = term {
  List.fold_right
  (fun i acc -> t (PLet (i, t (PPrime (i)) $startpos(l) $endpos(l), acc))
    $startpos $endpos)
  l cont
}
| i = Lident; Lassign; e = mp_expr; Lsemicolon; cont = term {
  t (PLet (i, e, cont)) $startpos $endpos
}
| Lif; c = cond; Labortwith; e = mp_expr; Lsemicolon; cont = term {
  t (PIf (c, e, cont)) $startpos $endpos
}
| Lreturn; e = mp_expr; Lsemicolon {
  t (PReturn (e)) $startpos $endpos
}
;

mp_expr:
| Lobrace; e = expr; Lcbrace {
  t (PProtected (e)) $startpos $endpos
}
| e = expr { e }
;

expr:
| Loparen; e = mp_expr; Lcparen { e }
| i = Lident { t (PVar (i)) $startpos $endpos }
| Lone { t (POne) $startpos $endpos }
| Lzero { t (PZero) $startpos $endpos }
| a = mp_expr; Lminus; b = mp_expr {
  t (PSum ([ a ; t (POpp(b)) $startpos(b) $endpos(b) ]))
  $startpos $endpos
}
| a = mp_expr; Lplus; b = mp_expr {
  t (PSum ([ a ; b ])) $startpos $endpos
}
| a = mp_expr; Lstar; b = mp_expr  {
  t (PProd ([ a ; b ])) $startpos $endpos
}
| a = mp_expr; Lcirc; b = mp_expr {
  t (PExp (a, b)) $startpos $endpos
}
| a = mp_expr; Lmod; b = mp_expr {
  t (PMod (a, b)) $startpos $endpos
}
| Lminus; e = mp_expr; %prec uminus { t (POpp (e)) $startpos $endpos }
;

cond:
| a = mp_expr; Lequal; b = mp_expr {
  t (PEq (a, b)) $startpos $endpos
}
| a = mp_expr; Lnoteq; b = mp_expr {
  t (PNotEq (a, b)) $startpos $endpos
}
| a = mp_expr; Lequal; Lobracket; m = expr; Lcbracket; b = mp_expr {
  t (PEqMod (a, b, m)) $startpos $endpos
}
| a = mp_expr; Lnoteq; Lobracket; m = expr; Lcbracket; b = mp_expr {
  t (PNotEqMod (a, b, m)) $startpos $endpos
}
| e = mp_expr {
  e
}
;

attack:
| Lzero { False }
;
