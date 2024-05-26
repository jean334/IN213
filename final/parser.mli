type token =
  | EOF
  | INT of (
# 6 "parser.mly"
        int
# 7 "parser.mli"
)
  | FLOAT of (
# 7 "parser.mly"
        float
# 12 "parser.mli"
)
  | SIN
  | COS
  | IDENT of (
# 9 "parser.mly"
        string
# 19 "parser.mli"
)
  | STRING of (
# 10 "parser.mly"
        string
# 24 "parser.mli"
)
  | TRUE
  | FALSE
  | RECT
  | URECT
  | CIRCLE
  | UCIRCLE
  | TRIANGLE
  | LINE
  | FORCE
  | WIN
  | FPS
  | FUNC
  | BACKGROUND
  | COLONEQUAL
  | DOT_X
  | DOT_Y
  | DOT_WIDTH
  | DOT_HEIGHT
  | DOT_COLOR
  | DOT_RADIUS
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
  | DOT
  | IF
  | THEN
  | ELSE
  | ENDIF
  | PRINT

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
