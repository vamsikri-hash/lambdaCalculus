{
open Parser
}

let var = ['a'-'z']['0'-'9' 'a'-'z']*


rule read = parse
| "(" { LPAREN }
| ")" { RPAREN }
| " " { APPLICATION }
| "." { DOT }
| "\\" { ABSTRACTION }
| var { VAR (Lexing.lexeme lexbuf) }
| eof { EOF  }