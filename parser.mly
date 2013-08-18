%{
  (* open Batteries ;; *)
  open Computation ;;
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
 | t = term; Lpercent; a = attack; Leof {
   (t, a)
 }
;

term:
| Lnoprop; l = separated_nonempty_list(Lcomma, Lident); Lsemicolon; cont = term {
  List.fold_right (fun i acc -> Let(i, NoProp(i), acc)) l cont
}
| Lprime; l = separated_nonempty_list(Lcomma, Lident); Lsemicolon; cont = term {
  List.fold_right (fun i acc -> Let(i, Prime(i), acc)) l cont
}
| i = Lident; Lassign; e = mp_expr; Lsemicolon; cont = term {
  Let(i, e, cont)
}
| Lif; c = cond; Labortwith; e = mp_expr; Lsemicolon; cont = term {
  If(c, e, cont)
}
| Lreturn; e = mp_expr; Lsemicolon {
  Return(e)
}
;

mp_expr:
| Lobrace; e = expr; Lcbrace {
  Protected(e)
}
| e = expr { e }
;

expr:
| Loparen; e = mp_expr; Lcparen { e }
| i = Lident { Var(i) }
| Lone { One }
| Lzero { Zero }
| a = mp_expr; Lminus; b = mp_expr { Sum([ a ; Opp(b) ]) }
| a = mp_expr; Lplus; b = mp_expr { Sum([ a ; b ]) }
| a = mp_expr; Lstar; b = mp_expr { Prod([ a ; b ]) }
| a = mp_expr; Lcirc; b = mp_expr { Exp(a, b) }
| a = mp_expr; Lmod; b = mp_expr { Mod(a, b) }
| Lminus; e = mp_expr; %prec uminus { Opp(e) }
;

cond:
| a = mp_expr; Lequal; b = mp_expr {
  Eq(a, b)
}
| a = mp_expr; Lnoteq; b = mp_expr {
  NotEq(a, b)
}
| a = mp_expr; Lequal; Lobracket; m = expr; Lcbracket; b = mp_expr {
  EqMod(a, b, m)
}
| a = mp_expr; Lnoteq; Lobracket; m = expr; Lcbracket; b = mp_expr {
  NotEqMod(a, b, m)
}
;

attack:
| Lzero { False }
;
