type token =
  | EOF
  | INT of (
# 6 "impparse.mly"
        int
# 7 "impparse.mli"
)
  | IDENT of (
# 7 "impparse.mly"
        string
# 12 "impparse.mli"
)
  | TRUE
  | FALSE
  | STRING of (
# 9 "impparse.mly"
        string
# 19 "impparse.mli"
)
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
  | IF
  | THEN
  | ELSE
  | ENDIF
  | PRINT

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Impast.program
