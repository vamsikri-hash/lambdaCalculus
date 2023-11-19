{
open Parser
}

let string = ['a'-'z']


rule read = parse
| "(" { LPAREN }
| ")" { RPAREN }
| " " { APPLICATION }
| "." { DOT }
| "\\" { ABSTRACTION }
| string { VAR (Lexing.lexeme lexbuf) }
| eof { EOF  }