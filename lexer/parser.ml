type token =
  | EOF
  | INT of (
# 6 "parser.mly"
        int
# 7 "parser.ml"
)
  | IDENT of (
# 7 "parser.mly"
        string
# 12 "parser.ml"
)
  | STRING of (
# 8 "parser.mly"
        string
# 17 "parser.ml"
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

open Parsing
let _ = parse_error;;
# 2 "parser.mly"
open Ast ;;
# 62 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  260 (* TRUE *);
  261 (* FALSE *);
  262 (* RECT *);
  263 (* CIRCLE *);
  264 (* TRIANGLE *);
  265 (* LINE *);
  266 (* FORCE *);
  267 (* WIN *);
  268 (* COLONEQUAL *);
  269 (* WHILE *);
  270 (* DO *);
  271 (* DONE *);
  272 (* BEGIN *);
  273 (* END *);
  274 (* RETURN *);
  275 (* VAR *);
  276 (* PLUS *);
  277 (* MINUS *);
  278 (* MULT *);
  279 (* DIV *);
  280 (* EQUALEQUAL *);
  281 (* GREATER *);
  282 (* SMALLER *);
  283 (* GREATEREQUAL *);
  284 (* SMALLEREQUAL *);
  285 (* LPAR *);
  286 (* RPAR *);
  287 (* SEMICOLON *);
  288 (* COMMA *);
  289 (* LBRACKET *);
  290 (* RBRACKET *);
  291 (* LBRACE *);
  292 (* RBRACE *);
  293 (* IF *);
  294 (* THEN *);
  295 (* ELSE *);
  296 (* ENDIF *);
  297 (* PRINT *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* IDENT *);
  259 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\003\000\003\000\004\000\005\000\006\000\010\000\010\000\
\012\000\012\000\013\000\013\000\007\000\014\000\014\000\015\000\
\015\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\016\000\016\000\017\000\017\000\011\000\011\000\
\018\000\018\000\018\000\018\000\018\000\000\000"

let yylen = "\002\000\
\001\000\002\000\001\000\001\000\001\000\001\000\001\000\001\000\
\002\000\003\000\006\000\006\000\006\000\006\000\000\000\002\000\
\000\000\002\000\001\000\002\000\008\000\000\000\002\000\000\000\
\003\000\007\000\005\000\004\000\007\000\003\000\001\000\001\000\
\001\000\003\000\005\000\005\000\004\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000\003\000\
\001\000\004\000\000\000\001\000\000\000\002\000\000\000\003\000\
\001\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\001\000\057\000\000\000\060\000\058\000\059\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\062\000\000\000\003\000\004\000\005\000\
\006\000\007\000\008\000\000\000\049\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\031\000\032\000\
\033\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\009\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\020\000\030\000\034\000\010\000\000\000\
\048\000\000\000\000\000\000\000\000\000\045\000\046\000\000\000\
\000\000\000\000\000\000\000\000\028\000\000\000\023\000\000\000\
\054\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\035\000\000\000\016\000\000\000\000\000\
\000\000\037\000\050\000\027\000\000\000\000\000\000\000\000\000\
\036\000\025\000\056\000\000\000\000\000\000\000\012\000\013\000\
\014\000\011\000\000\000\018\000\000\000\029\000\026\000\021\000"

let yydgoto = "\002\000\
\020\000\021\000\022\000\039\000\040\000\041\000\026\000\042\000\
\064\000\102\000\097\000\133\000\043\000\065\000\095\000\045\000\
\066\000\029\000"

let yysindex = "\027\000\
\001\000\000\000\000\000\000\000\253\254\000\000\000\000\000\000\
\032\255\047\255\058\255\003\255\071\255\003\255\083\255\003\255\
\003\255\003\255\063\255\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\134\000\000\000\003\255\043\255\003\255\
\065\255\067\255\069\255\053\255\051\000\007\255\000\000\000\000\
\000\000\071\255\080\255\216\000\070\255\006\255\068\255\194\000\
\227\255\003\255\000\000\003\255\003\255\003\255\003\255\003\255\
\003\255\003\255\003\255\003\255\000\000\146\000\245\254\121\000\
\072\255\075\255\061\000\003\255\003\255\003\255\003\255\003\255\
\071\255\003\255\003\255\000\000\000\000\000\000\000\000\003\255\
\000\000\071\255\077\255\068\255\068\255\000\000\000\000\100\255\
\100\255\100\255\100\255\100\255\000\000\098\255\000\000\003\255\
\000\000\074\255\086\255\099\255\121\000\084\255\088\255\094\255\
\111\255\076\000\127\255\114\255\091\000\106\000\106\255\117\255\
\131\255\121\000\130\255\000\000\003\255\000\000\128\255\143\255\
\147\255\000\000\000\000\000\000\086\255\099\255\158\255\071\255\
\000\000\000\000\000\000\130\255\071\255\158\000\000\000\000\000\
\000\000\000\000\152\255\000\000\155\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\170\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\162\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\174\255\000\000\
\000\000\000\000\000\000\105\255\000\000\000\000\000\000\000\000\
\000\000\026\255\000\000\175\255\000\000\000\000\126\255\000\000\
\000\000\177\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\205\000\178\255\
\000\000\000\000\000\000\189\255\189\255\189\255\177\255\000\000\
\000\000\177\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\141\255\156\255\000\000\000\000\171\255\
\186\255\201\255\009\000\030\000\000\000\000\000\000\000\000\000\
\000\000\000\000\225\000\182\000\178\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\191\255\178\255\097\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\097\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\201\000\000\000\151\255\002\000\049\000\060\000\000\000\062\000\
\255\255\006\000\167\255\098\000\216\255\000\000\110\000\000\000\
\220\255\000\000"

let yytablesize = 509
let yytable = "\028\000\
\003\000\076\000\023\000\004\000\036\000\006\000\007\000\008\000\
\030\000\132\000\037\000\118\000\044\000\083\000\047\000\048\000\
\049\000\071\000\030\000\028\000\094\000\072\000\023\000\016\000\
\131\000\031\000\132\000\001\000\062\000\032\000\067\000\017\000\
\107\000\033\000\105\000\074\000\079\000\108\000\080\000\075\000\
\019\000\111\000\019\000\004\000\063\000\006\000\007\000\008\000\
\034\000\024\000\084\000\085\000\086\000\087\000\088\000\089\000\
\090\000\091\000\092\000\035\000\025\000\019\000\027\000\016\000\
\019\000\019\000\101\000\101\000\101\000\024\000\106\000\017\000\
\038\000\109\000\103\000\104\000\009\000\010\000\110\000\011\000\
\025\000\071\000\027\000\012\000\046\000\072\000\013\000\139\000\
\014\000\054\000\055\000\050\000\141\000\068\000\114\000\069\000\
\077\000\070\000\017\000\113\000\078\000\098\000\017\000\017\000\
\099\000\017\000\112\000\018\000\115\000\017\000\117\000\019\000\
\017\000\119\000\017\000\134\000\116\000\120\000\061\000\052\000\
\053\000\054\000\055\000\121\000\061\000\061\000\061\000\061\000\
\061\000\061\000\061\000\061\000\061\000\017\000\061\000\061\000\
\061\000\017\000\061\000\047\000\122\000\124\000\061\000\125\000\
\128\000\047\000\047\000\129\000\015\000\047\000\047\000\047\000\
\047\000\047\000\043\000\047\000\047\000\047\000\135\000\047\000\
\043\000\043\000\094\000\047\000\043\000\043\000\043\000\043\000\
\043\000\044\000\043\000\043\000\043\000\136\000\043\000\044\000\
\044\000\137\000\043\000\044\000\044\000\044\000\044\000\044\000\
\038\000\044\000\044\000\044\000\138\000\044\000\144\000\143\000\
\051\000\044\000\038\000\038\000\038\000\038\000\038\000\039\000\
\038\000\038\000\038\000\022\000\038\000\052\000\053\000\055\000\
\038\000\039\000\039\000\039\000\039\000\039\000\041\000\039\000\
\039\000\039\000\015\000\039\000\024\000\051\000\130\000\039\000\
\041\000\041\000\041\000\041\000\041\000\140\000\041\000\041\000\
\041\000\000\000\041\000\000\000\000\000\000\000\041\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\052\000\053\000\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\000\000\
\000\000\004\000\005\000\006\000\007\000\008\000\009\000\010\000\
\082\000\011\000\000\000\000\000\000\000\012\000\000\000\000\000\
\013\000\000\000\014\000\015\000\000\000\016\000\040\000\000\000\
\000\000\000\000\000\000\000\000\000\000\017\000\000\000\000\000\
\040\000\040\000\040\000\040\000\040\000\018\000\040\000\040\000\
\040\000\019\000\040\000\042\000\000\000\000\000\040\000\000\000\
\000\000\000\000\000\000\000\000\000\000\042\000\042\000\042\000\
\042\000\042\000\000\000\042\000\042\000\042\000\000\000\042\000\
\073\000\000\000\000\000\042\000\000\000\000\000\052\000\053\000\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\000\000\
\052\000\053\000\054\000\055\000\056\000\057\000\058\000\059\000\
\060\000\000\000\000\000\000\000\000\000\000\000\100\000\052\000\
\053\000\054\000\055\000\056\000\057\000\058\000\059\000\060\000\
\000\000\000\000\000\000\000\000\000\000\123\000\052\000\053\000\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\000\000\
\000\000\000\000\000\000\000\000\126\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\000\000\000\000\
\000\000\000\000\000\000\127\000\052\000\053\000\054\000\055\000\
\056\000\057\000\058\000\059\000\060\000\000\000\000\000\000\000\
\096\000\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
\059\000\060\000\000\000\000\000\061\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\000\000\000\000\
\093\000\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
\059\000\060\000\000\000\000\000\142\000\061\000\061\000\061\000\
\061\000\061\000\061\000\061\000\061\000\061\000\000\000\000\000\
\061\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
\050\000\050\000\000\000\000\000\050\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\000\000\081\000\
\061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
\061\000\000\000\024\000\052\000\053\000\054\000\055\000\056\000\
\057\000\058\000\059\000\060\000\037\000\037\000\037\000\037\000\
\037\000\037\000\037\000\037\000\037\000"

let yycheck = "\001\000\
\000\000\042\000\001\000\001\001\002\001\003\001\004\001\005\001\
\012\001\115\000\012\000\101\000\014\000\050\000\016\000\017\000\
\018\000\029\001\012\001\021\000\032\001\033\001\021\000\021\001\
\114\000\029\001\132\000\001\000\030\000\033\001\032\000\029\001\
\073\000\002\001\071\000\029\001\031\001\074\000\033\001\033\001\
\015\001\082\000\017\001\001\001\002\001\003\001\004\001\005\001\
\002\001\001\000\052\000\053\000\054\000\055\000\056\000\057\000\
\058\000\059\000\060\000\002\001\001\000\036\001\001\000\021\001\
\039\001\040\001\068\000\069\000\070\000\021\000\072\000\029\001\
\002\001\075\000\069\000\070\000\006\001\007\001\080\000\009\001\
\021\000\029\001\021\000\013\001\002\001\033\001\016\001\128\000\
\018\001\022\001\023\001\029\001\133\000\029\001\096\000\029\001\
\017\001\029\001\002\001\002\001\031\001\030\001\006\001\007\001\
\030\001\009\001\030\001\037\001\035\001\013\001\012\001\041\001\
\016\001\030\001\018\001\117\000\031\001\030\001\014\001\020\001\
\021\001\022\001\023\001\030\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\037\001\030\001\031\001\
\032\001\041\001\034\001\014\001\030\001\015\001\038\001\030\001\
\039\001\020\001\021\001\031\001\019\001\024\001\025\001\026\001\
\027\001\028\001\014\001\030\001\031\001\032\001\031\001\034\001\
\020\001\021\001\032\001\038\001\024\001\025\001\026\001\027\001\
\028\001\014\001\030\001\031\001\032\001\031\001\034\001\020\001\
\021\001\031\001\038\001\024\001\025\001\026\001\027\001\028\001\
\014\001\030\001\031\001\032\001\031\001\034\001\036\001\040\001\
\031\001\038\001\024\001\025\001\026\001\027\001\028\001\014\001\
\030\001\031\001\032\001\030\001\034\001\031\001\030\001\030\001\
\038\001\024\001\025\001\026\001\027\001\028\001\014\001\030\001\
\031\001\032\001\030\001\034\001\030\001\021\000\113\000\038\001\
\024\001\025\001\026\001\027\001\028\001\132\000\030\001\031\001\
\032\001\255\255\034\001\255\255\255\255\255\255\038\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\038\001\009\001\255\255\255\255\255\255\013\001\255\255\255\255\
\016\001\255\255\018\001\019\001\255\255\021\001\014\001\255\255\
\255\255\255\255\255\255\255\255\255\255\029\001\255\255\255\255\
\024\001\025\001\026\001\027\001\028\001\037\001\030\001\031\001\
\032\001\041\001\034\001\014\001\255\255\255\255\038\001\255\255\
\255\255\255\255\255\255\255\255\255\255\024\001\025\001\026\001\
\027\001\028\001\255\255\030\001\031\001\032\001\255\255\034\001\
\014\001\255\255\255\255\038\001\255\255\255\255\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\255\255\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\255\255\255\255\255\255\255\255\255\255\034\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\255\255\255\255\255\255\255\255\255\255\034\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\255\255\
\255\255\255\255\255\255\255\255\034\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\255\255\255\255\
\255\255\255\255\255\255\034\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\255\255\255\255\255\255\
\032\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\255\255\255\255\031\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\255\255\255\255\
\031\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\255\255\255\255\031\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\255\255\255\255\
\031\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\255\255\255\255\031\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\255\255\030\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\255\255\030\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001"

let yynames_const = "\
  EOF\000\
  TRUE\000\
  FALSE\000\
  RECT\000\
  CIRCLE\000\
  TRIANGLE\000\
  LINE\000\
  FORCE\000\
  WIN\000\
  COLONEQUAL\000\
  WHILE\000\
  DO\000\
  DONE\000\
  BEGIN\000\
  END\000\
  RETURN\000\
  VAR\000\
  PLUS\000\
  MINUS\000\
  MULT\000\
  DIV\000\
  EQUALEQUAL\000\
  GREATER\000\
  SMALLER\000\
  GREATEREQUAL\000\
  SMALLEREQUAL\000\
  LPAR\000\
  RPAR\000\
  SEMICOLON\000\
  COMMA\000\
  LBRACKET\000\
  RBRACKET\000\
  LBRACE\000\
  RBRACE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  ENDIF\000\
  PRINT\000\
  "

let yynames_block = "\
  INT\000\
  IDENT\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 32 "parser.mly"
      ( [] )
# 388 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'toplevel) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.program) in
    Obj.repr(
# 33 "parser.mly"
                   ( _1 :: _2 )
# 396 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var_decl) in
    Obj.repr(
# 37 "parser.mly"
           ( Vardecl _1 )
# 403 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_decl) in
    Obj.repr(
# 38 "parser.mly"
            ( Rect _1 )
# 410 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'circle_decl) in
    Obj.repr(
# 39 "parser.mly"
              ( Circle _1 )
# 417 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'line_decl) in
    Obj.repr(
# 40 "parser.mly"
            ( Line _1 )
# 424 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fun_def) in
    Obj.repr(
# 41 "parser.mly"
          ( Fundef _1 )
# 431 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 42 "parser.mly"
        ( Instr _1 )
# 438 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 43 "parser.mly"
                 ( Expr _1 )
# 445 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 47 "parser.mly"
                      ( (_2, Scalar) )
# 452 "parser.ml"
               : 'var_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 48 "parser.mly"
                                             ( (_2, (Array _4)) )
# 460 "parser.ml"
               : 'var_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exprs_list) in
    Obj.repr(
# 53 "parser.mly"
    ( { r_name = _2 ; r_params = _4 } )
# 468 "parser.ml"
               : 'rect_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exprs_list) in
    Obj.repr(
# 57 "parser.mly"
    ( { c_name = _2 ; c_params = _4 } )
# 476 "parser.ml"
               : 'circle_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exprs_list) in
    Obj.repr(
# 61 "parser.mly"
    ( { l_name = _2 ; l_params = _4 } )
# 484 "parser.ml"
               : 'line_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
  ( [] )
# 490 "parser.ml"
               : 'exprs_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 65 "parser.mly"
             ( _1 :: _2 )
# 498 "parser.ml"
               : 'exprs_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
  ( [] )
# 504 "parser.ml"
               : 'opt_var_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt_var_decls) in
    Obj.repr(
# 69 "parser.mly"
                         ( _1 :: _2 )
# 512 "parser.ml"
               : 'opt_var_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 73 "parser.mly"
        ( _1 )
# 519 "parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'instrs) in
    Obj.repr(
# 74 "parser.mly"
               ( Seq (_1, _2) )
# 527 "parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'opt_params) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'opt_var_decls) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'instrs) in
    Obj.repr(
# 78 "parser.mly"
    ( { f_name = _1 ; params = _3 ; vars = _6 ; body = _7 } )
# 537 "parser.ml"
               : 'fun_def))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
  ( [] )
# 543 "parser.ml"
               : 'opt_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'params) in
    Obj.repr(
# 85 "parser.mly"
               ( _1 :: _2 )
# 551 "parser.ml"
               : 'opt_params))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
  ( [] )
# 557 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'params) in
    Obj.repr(
# 90 "parser.mly"
                     ( _2 :: _3 )
# 565 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'instrs) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'instrs) in
    Obj.repr(
# 96 "parser.mly"
    ( If (_2, _4, _6) )
# 574 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'instrs) in
    Obj.repr(
# 98 "parser.mly"
    ( While (_2, _4) )
# 582 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
    ( Assign (_1, _3) )
# 590 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
    ( ArrayWrite (_1, _3, _6) )
# 599 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'instrs) in
    Obj.repr(
# 104 "parser.mly"
    ( _2 )
# 606 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_decl) in
    Obj.repr(
# 106 "parser.mly"
    ( Rect _1 )
# 613 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'circle_decl) in
    Obj.repr(
# 108 "parser.mly"
    ( Circle _1 )
# 620 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'line_decl) in
    Obj.repr(
# 110 "parser.mly"
    ( Line _1 )
# 627 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'opt_expr) in
    Obj.repr(
# 112 "parser.mly"
    ( Return _2 )
# 634 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'opt_exprs) in
    Obj.repr(
# 114 "parser.mly"
    ( Iapp (_1, _3) )
# 642 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'opt_exprs) in
    Obj.repr(
# 116 "parser.mly"
    ( Print _3 )
# 649 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'opt_exprs) in
    Obj.repr(
# 123 "parser.mly"
                                 ( App (_1, _3) )
# 657 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                                 ( Binop ("==", _1, _3) )
# 665 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                                 ( Binop (">", _1, _3) )
# 673 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                                 ( Binop (">=", _1, _3) )
# 681 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                                 ( Binop ("<", _1, _3) )
# 689 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                                 ( Binop ("<=", _1, _3) )
# 697 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                                 ( Binop ("+", _1, _3) )
# 705 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                                 ( Binop ("-", _1, _3) )
# 713 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                                 ( Binop ("*", _1, _3) )
# 721 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                                 ( Binop ("/", _1, _3) )
# 729 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                                 ( Monop ("-", _2) )
# 736 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                                 ( _2 )
# 743 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 135 "parser.mly"
                                 ( _1 )
# 750 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                                 ( ArrayRead (_1, _3) )
# 758 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "parser.mly"
  ( None )
# 764 "parser.ml"
               : 'opt_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                     ( Some _1 )
# 771 "parser.ml"
               : 'opt_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 146 "parser.mly"
  ( [] )
# 777 "parser.ml"
               : 'opt_exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 147 "parser.mly"
                           ( _1 :: _2 )
# 785 "parser.ml"
               : 'opt_exprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 151 "parser.mly"
  ( [] )
# 791 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 152 "parser.mly"
                           ( _2 :: _3 )
# 799 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 156 "parser.mly"
                 ( Int (_1) )
# 806 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 157 "parser.mly"
                 ( Bool (true) )
# 812 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 158 "parser.mly"
                 ( Bool (false) )
# 818 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 159 "parser.mly"
                 ( String (_1) )
# 825 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 160 "parser.mly"
                 ( Ident (_1) )
# 832 "parser.ml"
               : 'atom))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
