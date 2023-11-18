{
open Parser
}

let string = ['a'-'z']


rule read = parse
| " " { APPLICATION }
| "." { DOT }
| "\\" { ABSTRACTION }
| string { VAR (Lexing.lexeme lexbuf) }
| eof { EOF  }