%token EOF

%start <Ast.expr> prog

%%

prog:
  | EOF { () }
  ;
