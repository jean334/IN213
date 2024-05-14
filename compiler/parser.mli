type token =
  | EOF
  | INT of (
# 6 "parser.mly"
        int
# 7 "parser.mli"
)
  | IDENT of (
# 7 "parser.mly"
        string
# 12 "parser.mli"
)
  | STRING of (
# 8 "parser.mly"
        string
# 17 "parser.mli"
)
  | TRUE
  | FALSE
  | RECT
  | CIRCLE
  | TRIANGLE
  | LINE
  | FORCE
  | WIN
  | COLONEQUAL
  | WHILE
  | DO
  | DONE
  | BEGIN
  | END
  | RETURN
  | VAR
  | PLUS
  | MINUS
  | MULT
  | DIV
  | EQUALEQUAL
  | GREATER
  | SMALLER
  | GREATEREQUAL
  | SMALLEREQUAL
  | LPAR
  | RPAR
  | SEMICOLON
  | COMMA
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | IF
  | THEN
  | ELSE
  | ENDIF
  | PRINT

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
