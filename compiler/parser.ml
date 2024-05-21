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
  | DOT_X
  | DOT_Y
  | DOT_WIDTH
  | DOT_HEIGHT
  | DOT_COLOR
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

open Parsing
let _ = parse_error;;
# 2 "parser.mly"
open Ast ;;
# 68 "parser.ml"
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
  269 (* DOT_X *);
  270 (* DOT_Y *);
  271 (* DOT_WIDTH *);
  272 (* DOT_HEIGHT *);
  273 (* DOT_COLOR *);
  274 (* WHILE *);
  275 (* DO *);
  276 (* DONE *);
  277 (* BEGIN *);
  278 (* END *);
  279 (* RETURN *);
  280 (* VAR *);
  281 (* PLUS *);
  282 (* MINUS *);
  283 (* MULT *);
  284 (* DIV *);
  285 (* EQUALEQUAL *);
  286 (* GREATER *);
  287 (* SMALLER *);
  288 (* GREATEREQUAL *);
  289 (* SMALLEREQUAL *);
  290 (* LPAR *);
  291 (* RPAR *);
  292 (* SEMICOLON *);
  293 (* COMMA *);
  294 (* LBRACKET *);
  295 (* RBRACKET *);
  296 (* LBRACE *);
  297 (* RBRACE *);
  298 (* DOT *);
  299 (* IF *);
  300 (* THEN *);
  301 (* ELSE *);
  302 (* ENDIF *);
  303 (* PRINT *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* IDENT *);
  259 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\003\000\003\000\005\000\004\000\006\000\007\000\008\000\009\000\
\010\000\011\000\012\000\013\000\017\000\017\000\019\000\019\000\
\020\000\020\000\014\000\021\000\021\000\022\000\022\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\016\000\016\000\016\000\016\000\016\000\016\000\
\016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
\023\000\023\000\024\000\024\000\018\000\018\000\025\000\025\000\
\025\000\025\000\025\000\000\000"

let yylen = "\002\000\
\001\000\002\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000\
\003\000\006\000\006\000\006\000\005\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\000\000\002\000\000\000\002\000\
\001\000\002\000\008\000\000\000\002\000\000\000\003\000\007\000\
\005\000\004\000\007\000\003\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\003\000\
\005\000\005\000\004\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000\003\000\001\000\004\000\
\000\000\001\000\000\000\002\000\000\000\003\000\001\000\001\000\
\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\001\000\079\000\000\000\082\000\080\000\081\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\084\000\000\000\003\000\004\000\
\005\000\006\000\007\000\008\000\009\000\010\000\011\000\012\000\
\013\000\014\000\015\000\000\000\071\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\045\000\046\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\034\000\044\000\056\000\017\000\000\000\070\000\000\000\000\000\
\000\000\000\000\000\000\067\000\068\000\000\000\000\000\000\000\
\000\000\000\000\042\000\000\000\000\000\000\000\000\000\000\000\
\000\000\037\000\000\000\030\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\076\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\021\000\000\000\057\000\
\000\000\030\000\000\000\000\000\000\000\000\000\059\000\072\000\
\041\000\000\000\000\000\000\000\000\000\058\000\022\000\023\000\
\024\000\025\000\026\000\039\000\078\000\000\000\000\000\000\000\
\019\000\027\000\028\000\020\000\018\000\000\000\032\000\000\000\
\043\000\040\000\035\000"

let yydgoto = "\002\000\
\021\000\022\000\053\000\054\000\055\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\034\000\064\000\137\000\
\092\000\132\000\183\000\065\000\093\000\130\000\067\000\094\000\
\037\000"

let yysindex = "\025\000\
\001\000\000\000\000\000\000\000\112\255\000\000\000\000\000\000\
\000\255\037\255\051\255\056\255\011\255\023\255\011\255\063\255\
\011\255\011\255\011\255\035\255\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\210\000\000\000\011\255\038\255\061\255\
\064\255\068\255\069\255\114\255\011\255\070\255\078\255\087\255\
\103\255\229\254\128\000\118\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\255\
\075\255\091\001\102\255\025\255\237\254\014\001\108\000\011\255\
\000\000\011\255\011\255\011\255\011\255\011\255\011\255\011\255\
\011\255\011\255\000\000\222\000\011\255\011\255\011\255\011\255\
\011\255\073\255\197\000\116\255\120\255\122\255\137\000\011\255\
\011\255\011\255\011\255\011\255\011\255\023\255\011\255\011\255\
\000\000\000\000\000\000\000\000\011\255\000\000\023\255\197\000\
\124\255\237\254\237\254\000\000\000\000\117\255\117\255\117\255\
\117\255\117\255\000\000\025\001\036\001\047\001\058\001\069\001\
\145\255\000\000\011\255\000\000\113\255\121\255\127\255\141\255\
\197\000\132\255\142\255\149\255\150\255\151\255\152\000\134\255\
\153\255\167\000\182\000\146\255\000\000\156\255\162\255\166\255\
\169\255\170\255\171\255\172\255\197\000\000\000\188\255\000\000\
\011\255\000\000\177\255\183\255\187\255\190\255\000\000\000\000\
\000\000\127\255\141\255\191\255\023\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\188\255\023\255\234\000\
\000\000\000\000\000\000\000\000\000\000\182\255\000\000\192\255\
\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\246\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\194\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\199\255\000\000\000\000\000\000\000\000\
\000\000\143\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\119\255\
\000\000\204\255\000\000\000\000\164\255\000\000\000\000\209\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\080\001\216\255\000\000\000\000\000\000\000\000\199\255\
\199\255\199\255\199\255\209\255\000\000\000\000\199\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\216\255\
\000\000\185\255\206\255\000\000\000\000\010\000\031\000\050\000\
\069\000\088\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\100\001\002\001\
\216\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\217\255\216\255\000\000\053\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\053\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\231\000\000\000\255\255\009\000\016\000\018\000\034\000\051\000\
\053\000\056\000\066\000\067\000\098\000\000\000\100\000\005\000\
\150\000\147\255\072\000\196\255\000\000\099\000\000\000\189\255\
\000\000"

let yytablesize = 645
let yytable = "\023\000\
\003\000\046\000\149\000\105\000\113\000\036\000\100\000\076\000\
\077\000\024\000\101\000\004\000\050\000\006\000\007\000\008\000\
\025\000\051\000\026\000\066\000\023\000\069\000\070\000\071\000\
\052\000\001\000\036\000\162\000\009\000\010\000\024\000\011\000\
\142\000\012\000\027\000\145\000\017\000\025\000\047\000\026\000\
\013\000\144\000\084\000\014\000\018\000\015\000\016\000\181\000\
\091\000\095\000\148\000\028\000\048\000\029\000\031\000\027\000\
\030\000\049\000\031\000\031\000\108\000\031\000\109\000\031\000\
\068\000\019\000\031\000\032\000\072\000\020\000\031\000\085\000\
\028\000\031\000\029\000\031\000\112\000\030\000\114\000\115\000\
\116\000\117\000\118\000\119\000\120\000\121\000\122\000\031\000\
\032\000\124\000\125\000\126\000\127\000\128\000\086\000\031\000\
\106\000\087\000\033\000\031\000\035\000\088\000\089\000\096\000\
\112\000\143\000\100\000\091\000\146\000\129\000\101\000\097\000\
\190\000\147\000\004\000\090\000\006\000\007\000\008\000\033\000\
\098\000\035\000\192\000\038\000\039\000\040\000\041\000\042\000\
\043\000\038\000\039\000\040\000\041\000\042\000\043\000\157\000\
\099\000\107\000\033\000\017\000\033\000\074\000\075\000\076\000\
\077\000\044\000\156\000\018\000\158\000\045\000\133\000\103\000\
\161\000\169\000\134\000\104\000\135\000\182\000\150\000\033\000\
\159\000\083\000\160\000\033\000\033\000\184\000\163\000\083\000\
\083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
\164\000\083\000\083\000\083\000\182\000\083\000\069\000\165\000\
\166\000\167\000\083\000\170\000\069\000\069\000\173\000\174\000\
\069\000\069\000\069\000\069\000\069\000\175\000\069\000\069\000\
\069\000\176\000\069\000\065\000\177\000\178\000\179\000\069\000\
\129\000\065\000\065\000\016\000\185\000\065\000\065\000\065\000\
\065\000\065\000\186\000\065\000\065\000\065\000\187\000\065\000\
\066\000\188\000\189\000\194\000\065\000\073\000\066\000\066\000\
\195\000\029\000\066\000\066\000\066\000\066\000\066\000\074\000\
\066\000\066\000\066\000\075\000\066\000\138\000\139\000\140\000\
\141\000\066\000\077\000\038\000\073\000\191\000\180\000\000\000\
\000\000\004\000\005\000\006\000\007\000\008\000\009\000\010\000\
\000\000\011\000\000\000\012\000\000\000\000\000\000\000\000\000\
\000\000\000\000\013\000\000\000\000\000\014\000\000\000\015\000\
\016\000\000\000\017\000\000\000\060\000\000\000\000\000\000\000\
\000\000\000\000\018\000\000\000\000\000\000\000\060\000\060\000\
\060\000\060\000\060\000\019\000\060\000\060\000\060\000\020\000\
\060\000\061\000\000\000\000\000\000\000\060\000\000\000\000\000\
\000\000\000\000\000\000\061\000\061\000\061\000\061\000\061\000\
\000\000\061\000\061\000\061\000\063\000\061\000\000\000\000\000\
\000\000\000\000\061\000\000\000\000\000\000\000\063\000\063\000\
\063\000\063\000\063\000\000\000\063\000\063\000\063\000\062\000\
\063\000\000\000\000\000\000\000\000\000\063\000\000\000\000\000\
\000\000\062\000\062\000\062\000\062\000\062\000\000\000\062\000\
\062\000\062\000\064\000\062\000\000\000\000\000\000\000\000\000\
\062\000\000\000\000\000\000\000\064\000\064\000\064\000\064\000\
\064\000\000\000\064\000\064\000\064\000\000\000\064\000\000\000\
\000\000\000\000\000\000\064\000\074\000\075\000\076\000\077\000\
\078\000\079\000\080\000\081\000\082\000\000\000\000\000\000\000\
\000\000\000\000\102\000\000\000\000\000\000\000\000\000\111\000\
\074\000\075\000\076\000\077\000\078\000\079\000\080\000\081\000\
\082\000\074\000\075\000\076\000\077\000\078\000\079\000\080\000\
\081\000\082\000\000\000\000\000\000\000\000\000\000\000\136\000\
\074\000\075\000\076\000\077\000\078\000\079\000\080\000\081\000\
\082\000\000\000\000\000\000\000\000\000\000\000\168\000\074\000\
\075\000\076\000\077\000\078\000\079\000\080\000\081\000\082\000\
\000\000\000\000\000\000\000\000\000\000\171\000\074\000\075\000\
\076\000\077\000\078\000\079\000\080\000\081\000\082\000\000\000\
\000\000\000\000\000\000\000\000\172\000\074\000\075\000\076\000\
\077\000\078\000\079\000\080\000\081\000\082\000\000\000\000\000\
\000\000\131\000\074\000\075\000\076\000\077\000\078\000\079\000\
\080\000\081\000\082\000\000\000\000\000\083\000\074\000\075\000\
\076\000\077\000\078\000\079\000\080\000\081\000\082\000\000\000\
\000\000\123\000\074\000\075\000\076\000\077\000\078\000\079\000\
\080\000\081\000\082\000\000\000\000\000\193\000\083\000\083\000\
\083\000\083\000\083\000\083\000\083\000\083\000\083\000\000\000\
\000\000\083\000\072\000\072\000\072\000\072\000\072\000\072\000\
\072\000\072\000\072\000\000\000\000\000\072\000\074\000\075\000\
\076\000\077\000\078\000\079\000\080\000\081\000\082\000\000\000\
\110\000\074\000\075\000\076\000\077\000\078\000\079\000\080\000\
\081\000\082\000\000\000\151\000\074\000\075\000\076\000\077\000\
\078\000\079\000\080\000\081\000\082\000\000\000\152\000\074\000\
\075\000\076\000\077\000\078\000\079\000\080\000\081\000\082\000\
\000\000\153\000\074\000\075\000\076\000\077\000\078\000\079\000\
\080\000\081\000\082\000\000\000\154\000\074\000\075\000\076\000\
\077\000\078\000\079\000\080\000\081\000\082\000\000\000\155\000\
\083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
\083\000\000\000\038\000\074\000\075\000\076\000\077\000\078\000\
\079\000\080\000\081\000\082\000\059\000\059\000\059\000\059\000\
\059\000\059\000\059\000\059\000\059\000"

let yycheck = "\001\000\
\000\000\002\001\112\000\064\000\072\000\001\000\034\001\027\001\
\028\001\001\000\038\001\001\001\002\001\003\001\004\001\005\001\
\001\000\013\000\001\000\015\000\022\000\017\000\018\000\019\000\
\002\001\001\000\022\000\137\000\006\001\007\001\022\000\009\001\
\100\000\011\001\001\000\103\000\026\001\022\000\002\001\022\000\
\018\001\102\000\038\000\021\001\034\001\023\001\024\001\157\000\
\044\000\045\000\111\000\001\000\002\001\001\000\002\001\022\000\
\001\000\002\001\006\001\007\001\036\001\009\001\038\001\011\001\
\002\001\043\001\001\000\001\000\034\001\047\001\018\001\034\001\
\022\000\021\001\022\000\023\001\072\000\022\000\074\000\075\000\
\076\000\077\000\078\000\079\000\080\000\081\000\082\000\022\000\
\022\000\085\000\086\000\087\000\088\000\089\000\034\001\043\001\
\022\001\034\001\001\000\047\001\001\000\034\001\034\001\034\001\
\100\000\101\000\034\001\103\000\104\000\037\001\038\001\034\001\
\173\000\109\000\001\001\002\001\003\001\004\001\005\001\022\000\
\034\001\022\000\183\000\012\001\013\001\014\001\015\001\016\001\
\017\001\012\001\013\001\014\001\015\001\016\001\017\001\131\000\
\034\001\036\001\020\001\026\001\022\001\025\001\026\001\027\001\
\028\001\034\001\002\001\034\001\036\001\038\001\035\001\034\001\
\012\001\020\001\035\001\038\001\035\001\159\000\035\001\041\001\
\040\001\019\001\036\001\045\001\046\001\161\000\035\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\035\001\035\001\036\001\037\001\182\000\039\001\019\001\035\001\
\035\001\035\001\044\001\035\001\025\001\026\001\045\001\036\001\
\029\001\030\001\031\001\032\001\033\001\036\001\035\001\036\001\
\037\001\036\001\039\001\019\001\036\001\036\001\036\001\044\001\
\037\001\025\001\026\001\024\001\036\001\029\001\030\001\031\001\
\032\001\033\001\036\001\035\001\036\001\037\001\036\001\039\001\
\019\001\036\001\036\001\046\001\044\001\036\001\025\001\026\001\
\041\001\035\001\029\001\030\001\031\001\032\001\033\001\036\001\
\035\001\036\001\037\001\035\001\039\001\096\000\097\000\098\000\
\099\000\044\001\035\001\035\001\022\000\182\000\156\000\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\255\255\009\001\255\255\011\001\255\255\255\255\255\255\255\255\
\255\255\255\255\018\001\255\255\255\255\021\001\255\255\023\001\
\024\001\255\255\026\001\255\255\019\001\255\255\255\255\255\255\
\255\255\255\255\034\001\255\255\255\255\255\255\029\001\030\001\
\031\001\032\001\033\001\043\001\035\001\036\001\037\001\047\001\
\039\001\019\001\255\255\255\255\255\255\044\001\255\255\255\255\
\255\255\255\255\255\255\029\001\030\001\031\001\032\001\033\001\
\255\255\035\001\036\001\037\001\019\001\039\001\255\255\255\255\
\255\255\255\255\044\001\255\255\255\255\255\255\029\001\030\001\
\031\001\032\001\033\001\255\255\035\001\036\001\037\001\019\001\
\039\001\255\255\255\255\255\255\255\255\044\001\255\255\255\255\
\255\255\029\001\030\001\031\001\032\001\033\001\255\255\035\001\
\036\001\037\001\019\001\039\001\255\255\255\255\255\255\255\255\
\044\001\255\255\255\255\255\255\029\001\030\001\031\001\032\001\
\033\001\255\255\035\001\036\001\037\001\255\255\039\001\255\255\
\255\255\255\255\255\255\044\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\255\255\255\255\255\255\
\255\255\255\255\019\001\255\255\255\255\255\255\255\255\044\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\255\255\255\255\255\255\255\255\255\255\039\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\255\255\255\255\255\255\255\255\255\255\039\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\255\255\255\255\255\255\255\255\255\255\039\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\255\255\
\255\255\255\255\255\255\255\255\039\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\255\255\255\255\
\255\255\037\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\255\255\255\255\036\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\255\255\
\255\255\036\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\255\255\255\255\036\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\255\255\
\255\255\036\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\255\255\255\255\036\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\255\255\
\035\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\255\255\035\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\255\255\035\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\255\255\035\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\255\255\035\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\255\255\035\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\255\255\035\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001"

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
  DOT_X\000\
  DOT_Y\000\
  DOT_WIDTH\000\
  DOT_HEIGHT\000\
  DOT_COLOR\000\
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
  DOT\000\
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
# 33 "parser.mly"
      ( [] )
# 469 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'toplevel) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.program) in
    Obj.repr(
# 34 "parser.mly"
                   ( _1 :: _2 )
# 477 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var_decl) in
    Obj.repr(
# 38 "parser.mly"
           ( Vardecl _1 )
# 484 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'win_decl) in
    Obj.repr(
# 39 "parser.mly"
           (Win _1)
# 491 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_decl) in
    Obj.repr(
# 40 "parser.mly"
            ( Rect _1 )
# 498 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_move) in
    Obj.repr(
# 41 "parser.mly"
            ( RectMove _1 )
# 505 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_change_x) in
    Obj.repr(
# 42 "parser.mly"
                ( RectChangeX _1 )
# 512 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_change_y) in
    Obj.repr(
# 43 "parser.mly"
                ( RectChangeY _1 )
# 519 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_change_width) in
    Obj.repr(
# 44 "parser.mly"
                    ( RectChangeWidth _1 )
# 526 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_change_height) in
    Obj.repr(
# 45 "parser.mly"
                     ( RectChangeHeight _1 )
# 533 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_change_color) in
    Obj.repr(
# 46 "parser.mly"
                    ( RectChangeColor _1 )
# 540 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'circle_decl) in
    Obj.repr(
# 47 "parser.mly"
              ( Circle _1 )
# 547 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'line_decl) in
    Obj.repr(
# 48 "parser.mly"
            ( Line _1 )
# 554 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fun_def) in
    Obj.repr(
# 49 "parser.mly"
          ( Fundef _1 )
# 561 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 50 "parser.mly"
        ( Instr _1 )
# 568 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 51 "parser.mly"
                 ( Expr _1 )
# 575 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 55 "parser.mly"
                      ( (_2, Scalar) )
# 582 "parser.ml"
               : 'var_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 56 "parser.mly"
                                             ( (_2, (Array _4)) )
# 590 "parser.ml"
               : 'var_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exprs_list) in
    Obj.repr(
# 59 "parser.mly"
    ( { r_name = _2 ; r_params = _4 } )
# 598 "parser.ml"
               : 'rect_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exprs_list) in
    Obj.repr(
# 63 "parser.mly"
    ( { w_name = _2 ; w_params = _4 } )
# 606 "parser.ml"
               : 'win_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'exprs_list) in
    Obj.repr(
# 67 "parser.mly"
    ( { r_name = _1 ; r_params = _3 } )
# 614 "parser.ml"
               : 'rect_move))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
    ( { r_name = _1 ; r_params = _4 } )
# 622 "parser.ml"
               : 'rect_change_x))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
    ( { r_name = _1 ; r_params = _4 } )
# 630 "parser.ml"
               : 'rect_change_y))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
    ( { r_name = _1 ; r_params = _4 } )
# 638 "parser.ml"
               : 'rect_change_width))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
    ( { r_name = _1 ; r_params = _4 } )
# 646 "parser.ml"
               : 'rect_change_height))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
    ( { r_name = _1 ; r_params = _4 } )
# 654 "parser.ml"
               : 'rect_change_color))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exprs_list) in
    Obj.repr(
# 91 "parser.mly"
    ( { c_name = _2 ; c_params = _4 } )
# 662 "parser.ml"
               : 'circle_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exprs_list) in
    Obj.repr(
# 95 "parser.mly"
    ( { l_name = _2 ; l_params = _4 } )
# 670 "parser.ml"
               : 'line_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
  ( [] )
# 676 "parser.ml"
               : 'exprs_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 99 "parser.mly"
             ( _1 :: _2 )
# 684 "parser.ml"
               : 'exprs_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
  ( [] )
# 690 "parser.ml"
               : 'opt_var_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt_var_decls) in
    Obj.repr(
# 103 "parser.mly"
                         ( _1 :: _2 )
# 698 "parser.ml"
               : 'opt_var_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 107 "parser.mly"
        ( _1 )
# 705 "parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'instrs) in
    Obj.repr(
# 108 "parser.mly"
               ( Seq (_1, _2) )
# 713 "parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'opt_params) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'opt_var_decls) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'instrs) in
    Obj.repr(
# 112 "parser.mly"
    ( { f_name = _1 ; params = _3 ; vars = _6 ; body = _7 } )
# 723 "parser.ml"
               : 'fun_def))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
  ( [] )
# 729 "parser.ml"
               : 'opt_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'params) in
    Obj.repr(
# 119 "parser.mly"
               ( _1 :: _2 )
# 737 "parser.ml"
               : 'opt_params))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "parser.mly"
  ( [] )
# 743 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'params) in
    Obj.repr(
# 124 "parser.mly"
                     ( _2 :: _3 )
# 751 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'instrs) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'instrs) in
    Obj.repr(
# 130 "parser.mly"
    ( If (_2, _4, _6) )
# 760 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'instrs) in
    Obj.repr(
# 132 "parser.mly"
    ( While (_2, _4) )
# 768 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
    ( Assign (_1, _3) )
# 776 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
    ( ArrayWrite (_1, _3, _6) )
# 785 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'instrs) in
    Obj.repr(
# 138 "parser.mly"
    ( _2 )
# 792 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var_decl) in
    Obj.repr(
# 141 "parser.mly"
    ( Vardecl _1 )
# 799 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'win_decl) in
    Obj.repr(
# 143 "parser.mly"
    ( Win _1 )
# 806 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_decl) in
    Obj.repr(
# 145 "parser.mly"
    ( Rect _1 )
# 813 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_move) in
    Obj.repr(
# 147 "parser.mly"
    ( RectMove _1 )
# 820 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_change_x) in
    Obj.repr(
# 149 "parser.mly"
    ( RectChangeX _1 )
# 827 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_change_y) in
    Obj.repr(
# 151 "parser.mly"
    ( RectChangeY _1 )
# 834 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_change_width) in
    Obj.repr(
# 153 "parser.mly"
    ( RectChangeWidth _1 )
# 841 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_change_height) in
    Obj.repr(
# 155 "parser.mly"
    ( RectChangeHeight _1 )
# 848 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_change_color) in
    Obj.repr(
# 157 "parser.mly"
    ( RectChangeColor _1 )
# 855 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'circle_decl) in
    Obj.repr(
# 159 "parser.mly"
    ( Circle _1 )
# 862 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'line_decl) in
    Obj.repr(
# 161 "parser.mly"
    ( Line _1 )
# 869 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'opt_expr) in
    Obj.repr(
# 163 "parser.mly"
    ( Return _2 )
# 876 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'opt_exprs) in
    Obj.repr(
# 165 "parser.mly"
    ( Iapp (_1, _3) )
# 884 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'opt_exprs) in
    Obj.repr(
# 167 "parser.mly"
    ( Print _3 )
# 891 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'opt_exprs) in
    Obj.repr(
# 172 "parser.mly"
                                 ( App (_1, _3) )
# 899 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 173 "parser.mly"
                                 ( Binop ("==", _1, _3) )
# 907 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 174 "parser.mly"
                                 ( Binop (">", _1, _3) )
# 915 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 175 "parser.mly"
                                 ( Binop (">=", _1, _3) )
# 923 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 176 "parser.mly"
                                 ( Binop ("<", _1, _3) )
# 931 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 177 "parser.mly"
                                 ( Binop ("<=", _1, _3) )
# 939 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 178 "parser.mly"
                                 ( Binop ("+", _1, _3) )
# 947 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 179 "parser.mly"
                                 ( Binop ("-", _1, _3) )
# 955 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 180 "parser.mly"
                                 ( Binop ("*", _1, _3) )
# 963 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 181 "parser.mly"
                                 ( Binop ("/", _1, _3) )
# 971 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 182 "parser.mly"
                                 ( Monop ("-", _2) )
# 978 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 183 "parser.mly"
                                 ( _2 )
# 985 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 184 "parser.mly"
                                 ( _1 )
# 992 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 185 "parser.mly"
                                 ( ArrayRead (_1, _3) )
# 1000 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 190 "parser.mly"
  ( None )
# 1006 "parser.ml"
               : 'opt_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 191 "parser.mly"
                     ( Some _1 )
# 1013 "parser.ml"
               : 'opt_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 195 "parser.mly"
  ( [] )
# 1019 "parser.ml"
               : 'opt_exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 196 "parser.mly"
                           ( _1 :: _2 )
# 1027 "parser.ml"
               : 'opt_exprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 200 "parser.mly"
  ( [] )
# 1033 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 201 "parser.mly"
                           ( _2 :: _3 )
# 1041 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 205 "parser.mly"
                 ( Int (_1) )
# 1048 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 206 "parser.mly"
                 ( Bool (true) )
# 1054 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 207 "parser.mly"
                 ( Bool (false) )
# 1060 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 208 "parser.mly"
                 ( String (_1) )
# 1067 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 209 "parser.mly"
                 ( Ident (_1) )
# 1074 "parser.ml"
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
