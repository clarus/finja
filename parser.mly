%{
  (* open Batteries ;; *)
  open Ast ;;
%}

%token <string> Lident
%token Lnoprop Lprime
%token Lcomma Lsemicolon
%token Lassign
(* %token Lequal Lbang Lmod *)
(* %token Lcirc Lminus Lplus Lstar *)
%token Lone Lzero
%token Lobrace Lcbrace
(* %token Loparen Lcparen *)
(* %token Lobracket Lcbracket *)
%token Lpercent
(* %token Lif Lthen Lelse Lend Lreturn *)
(* %token Lslash Lbackslash *)
(* %token Lat *)
%token Lreturn
%token Leof

%start desc

%type <Ast.fia> desc

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
| i = Lident; Lassign; e = maybe_protected_expr; Lsemicolon; cont = term {
  Let(i, e, cont)
}
| Lreturn; i = Lident; Lsemicolon {
  Var(i)
}
| e = expr {
  e
}
;

maybe_protected_expr:
| Lobrace; e = expr; Lcbrace {
  Protected(e)
}
| e = expr { e }
;

expr:
| i = Lident { Var(i) }
| Lone { One }
| Lzero { Zero }
;

attack:
| Lzero { False }
;
