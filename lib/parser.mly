%token <string> VAR
%token ABSTRACTION
%token DOT
%token APPLICATION
%token EOF

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;


expr:
| s = VAR { Var s }
| ABSTRACTION; st = str ; DOT; e2 = expr { Abstraction (st, e2) }
| e1 = expr; APPLICATION; e2 = expr { Application (e1, e2) }
;

str:
| st = VAR { st }
; 