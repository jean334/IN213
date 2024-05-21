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
  | X
  | Y
  | WIDTH
  | HEIGHT
  | COLOR
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
  269 (* X *);
  270 (* Y *);
  271 (* WIDTH *);
  272 (* HEIGHT *);
  273 (* COLOR *);
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
\003\000\006\000\006\000\006\000\005\000\007\000\007\000\007\000\
\007\000\007\000\006\000\006\000\000\000\002\000\000\000\002\000\
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
\045\000\046\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\034\000\044\000\056\000\017\000\
\000\000\070\000\000\000\000\000\000\000\000\000\000\000\067\000\
\068\000\000\000\000\000\000\000\000\000\000\000\042\000\000\000\
\037\000\000\000\030\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\076\000\000\000\000\000\000\000\021\000\000\000\057\000\000\000\
\000\000\000\000\000\000\000\000\000\000\030\000\000\000\000\000\
\000\000\000\000\059\000\072\000\041\000\000\000\000\000\000\000\
\000\000\058\000\039\000\078\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\019\000\027\000\028\000\020\000\
\018\000\000\000\032\000\000\000\043\000\022\000\023\000\024\000\
\025\000\026\000\040\000\035\000"

let yydgoto = "\002\000\
\021\000\022\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\034\000\060\000\133\000\
\083\000\123\000\174\000\061\000\084\000\121\000\063\000\085\000\
\037\000"

let yysindex = "\012\000\
\001\000\000\000\000\000\000\000\058\255\000\000\000\000\000\000\
\039\255\044\255\050\255\052\255\033\255\008\255\033\255\054\255\
\033\255\033\255\033\255\029\255\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\191\000\000\000\033\255\083\255\033\255\
\017\000\032\255\061\255\063\255\065\255\234\254\109\000\081\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\255\084\255\072\001\077\255\223\254\
\062\255\251\000\221\255\033\255\000\000\033\255\033\255\033\255\
\033\255\033\255\033\255\033\255\033\255\033\255\000\000\203\000\
\027\255\178\000\079\255\087\255\091\255\118\000\096\255\097\255\
\104\255\106\255\107\255\033\255\033\255\033\255\033\255\033\255\
\033\255\008\255\033\255\033\255\000\000\000\000\000\000\000\000\
\033\255\000\000\008\255\178\000\110\255\062\255\062\255\000\000\
\000\000\212\255\212\255\212\255\212\255\212\255\000\000\130\255\
\000\000\033\255\000\000\111\255\108\255\114\255\139\255\033\255\
\033\255\033\255\033\255\033\255\178\000\117\255\119\255\120\255\
\121\255\123\255\133\000\148\255\140\255\148\000\163\000\131\255\
\000\000\141\255\142\255\178\000\000\000\158\255\000\000\033\255\
\006\001\017\001\028\001\039\001\050\001\000\000\147\255\153\255\
\157\255\160\255\000\000\000\000\000\000\114\255\139\255\161\255\
\008\255\000\000\000\000\000\000\158\255\008\255\215\000\162\255\
\164\255\167\255\168\255\174\255\000\000\000\000\000\000\000\000\
\000\000\171\255\000\000\173\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\227\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\182\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\184\255\000\000\
\000\000\000\000\000\000\000\000\000\000\134\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\098\255\000\000\185\255\000\000\000\000\
\155\255\000\000\000\000\189\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\061\001\190\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\184\255\184\255\184\255\184\255\189\255\
\000\000\000\000\184\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\190\255\000\000\176\255\197\255\000\000\
\000\000\010\000\031\000\050\000\069\000\088\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\081\001\239\000\000\000\
\000\000\000\000\000\000\000\000\190\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\196\255\190\255\000\000\051\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\051\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\213\000\000\000\255\255\003\000\006\000\008\000\046\000\049\000\
\090\000\102\000\106\000\107\000\120\000\000\000\124\000\005\000\
\150\000\156\255\082\000\198\255\000\000\110\000\000\000\199\255\
\000\000"

let yytablesize = 626
let yytable = "\023\000\
\003\000\101\000\104\000\024\000\105\000\036\000\025\000\145\000\
\026\000\048\000\109\000\096\000\001\000\009\000\010\000\097\000\
\011\000\047\000\012\000\062\000\023\000\065\000\066\000\067\000\
\024\000\013\000\036\000\025\000\014\000\026\000\015\000\016\000\
\158\000\004\000\046\000\006\000\007\000\008\000\138\000\140\000\
\042\000\141\000\080\000\082\000\086\000\043\000\027\000\172\000\
\144\000\028\000\019\000\044\000\031\000\045\000\020\000\064\000\
\031\000\031\000\017\000\031\000\096\000\031\000\068\000\120\000\
\097\000\092\000\018\000\027\000\031\000\038\000\028\000\031\000\
\108\000\031\000\110\000\111\000\112\000\113\000\114\000\115\000\
\116\000\117\000\118\000\004\000\081\000\006\000\007\000\008\000\
\072\000\073\000\029\000\039\000\038\000\031\000\093\000\040\000\
\094\000\031\000\095\000\041\000\108\000\139\000\030\000\082\000\
\142\000\102\000\031\000\032\000\017\000\143\000\186\000\029\000\
\103\000\124\000\099\000\188\000\018\000\033\000\100\000\033\000\
\033\000\125\000\041\000\030\000\035\000\126\000\148\000\031\000\
\032\000\128\000\129\000\147\000\153\000\154\000\155\000\156\000\
\157\000\130\000\033\000\131\000\132\000\033\000\033\000\033\000\
\146\000\035\000\149\000\150\000\173\000\151\000\152\000\159\000\
\083\000\160\000\161\000\162\000\175\000\163\000\083\000\083\000\
\083\000\083\000\083\000\083\000\083\000\083\000\083\000\165\000\
\083\000\083\000\083\000\173\000\083\000\069\000\166\000\169\000\
\170\000\083\000\120\000\069\000\069\000\016\000\181\000\069\000\
\069\000\069\000\069\000\069\000\182\000\069\000\069\000\069\000\
\183\000\069\000\065\000\184\000\185\000\190\000\069\000\191\000\
\065\000\065\000\192\000\193\000\065\000\065\000\065\000\065\000\
\065\000\194\000\065\000\065\000\065\000\196\000\065\000\066\000\
\195\000\073\000\029\000\065\000\074\000\066\000\066\000\075\000\
\077\000\066\000\066\000\066\000\066\000\066\000\038\000\066\000\
\066\000\066\000\069\000\066\000\070\000\071\000\072\000\073\000\
\066\000\134\000\135\000\136\000\137\000\070\000\071\000\072\000\
\073\000\074\000\075\000\076\000\077\000\078\000\187\000\000\000\
\171\000\004\000\005\000\006\000\007\000\008\000\009\000\010\000\
\107\000\011\000\000\000\012\000\000\000\000\000\000\000\000\000\
\000\000\000\000\013\000\000\000\000\000\014\000\000\000\015\000\
\016\000\000\000\017\000\000\000\060\000\087\000\088\000\089\000\
\090\000\091\000\018\000\000\000\000\000\000\000\060\000\060\000\
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
\064\000\000\000\064\000\064\000\064\000\000\000\064\000\098\000\
\000\000\000\000\000\000\064\000\000\000\070\000\071\000\072\000\
\073\000\074\000\075\000\076\000\077\000\078\000\070\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\000\000\
\000\000\000\000\000\000\000\000\127\000\070\000\071\000\072\000\
\073\000\074\000\075\000\076\000\077\000\078\000\000\000\000\000\
\000\000\000\000\000\000\164\000\070\000\071\000\072\000\073\000\
\074\000\075\000\076\000\077\000\078\000\000\000\000\000\000\000\
\000\000\000\000\167\000\070\000\071\000\072\000\073\000\074\000\
\075\000\076\000\077\000\078\000\000\000\000\000\000\000\000\000\
\000\000\168\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\000\000\000\000\000\000\122\000\070\000\
\071\000\072\000\073\000\074\000\075\000\076\000\077\000\078\000\
\000\000\000\000\079\000\070\000\071\000\072\000\073\000\074\000\
\075\000\076\000\077\000\078\000\000\000\000\000\119\000\070\000\
\071\000\072\000\073\000\074\000\075\000\076\000\077\000\078\000\
\000\000\000\000\189\000\083\000\083\000\083\000\083\000\083\000\
\083\000\083\000\083\000\083\000\000\000\000\000\083\000\072\000\
\072\000\072\000\072\000\072\000\072\000\072\000\072\000\072\000\
\000\000\000\000\072\000\070\000\071\000\072\000\073\000\074\000\
\075\000\076\000\077\000\078\000\000\000\106\000\070\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\000\000\
\176\000\070\000\071\000\072\000\073\000\074\000\075\000\076\000\
\077\000\078\000\000\000\177\000\070\000\071\000\072\000\073\000\
\074\000\075\000\076\000\077\000\078\000\000\000\178\000\070\000\
\071\000\072\000\073\000\074\000\075\000\076\000\077\000\078\000\
\000\000\179\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\000\000\180\000\083\000\083\000\083\000\
\083\000\083\000\083\000\083\000\083\000\083\000\000\000\038\000\
\070\000\071\000\072\000\073\000\074\000\075\000\076\000\077\000\
\078\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
\059\000\059\000"

let yycheck = "\001\000\
\000\000\060\000\036\001\001\000\038\001\001\000\001\000\108\000\
\001\000\002\001\068\000\034\001\001\000\006\001\007\001\038\001\
\009\001\013\000\011\001\015\000\022\000\017\000\018\000\019\000\
\022\000\018\001\022\000\022\000\021\001\022\000\023\001\024\001\
\133\000\001\001\002\001\003\001\004\001\005\001\096\000\098\000\
\002\001\099\000\038\000\039\000\040\000\002\001\001\000\148\000\
\107\000\001\000\043\001\002\001\002\001\002\001\047\001\002\001\
\006\001\007\001\026\001\009\001\034\001\011\001\034\001\037\001\
\038\001\034\001\034\001\022\000\018\001\012\001\022\000\021\001\
\068\000\023\001\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\001\001\002\001\003\001\004\001\005\001\
\027\001\028\001\001\000\034\001\012\001\043\001\034\001\038\001\
\034\001\047\001\034\001\042\001\096\000\097\000\001\000\099\000\
\100\000\022\001\001\000\001\000\026\001\105\000\169\000\022\000\
\036\001\035\001\034\001\174\000\034\001\020\001\038\001\022\001\
\001\000\035\001\042\001\022\000\001\000\035\001\122\000\022\000\
\022\000\034\001\034\001\002\001\128\000\129\000\130\000\131\000\
\132\000\034\001\041\001\034\001\034\001\022\000\045\001\046\001\
\035\001\022\000\036\001\040\001\150\000\036\001\012\001\035\001\
\019\001\035\001\035\001\035\001\152\000\035\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\020\001\
\035\001\036\001\037\001\173\000\039\001\019\001\035\001\045\001\
\036\001\044\001\037\001\025\001\026\001\024\001\036\001\029\001\
\030\001\031\001\032\001\033\001\036\001\035\001\036\001\037\001\
\036\001\039\001\019\001\036\001\036\001\036\001\044\001\036\001\
\025\001\026\001\036\001\036\001\029\001\030\001\031\001\032\001\
\033\001\036\001\035\001\036\001\037\001\041\001\039\001\019\001\
\046\001\036\001\035\001\044\001\036\001\025\001\026\001\035\001\
\035\001\029\001\030\001\031\001\032\001\033\001\035\001\035\001\
\036\001\037\001\022\000\039\001\025\001\026\001\027\001\028\001\
\044\001\092\000\093\000\094\000\095\000\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\173\000\255\255\
\147\000\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\044\001\009\001\255\255\011\001\255\255\255\255\255\255\255\255\
\255\255\255\255\018\001\255\255\255\255\021\001\255\255\023\001\
\024\001\255\255\026\001\255\255\019\001\013\001\014\001\015\001\
\016\001\017\001\034\001\255\255\255\255\255\255\029\001\030\001\
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
\033\001\255\255\035\001\036\001\037\001\255\255\039\001\019\001\
\255\255\255\255\255\255\044\001\255\255\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\255\255\
\255\255\255\255\255\255\255\255\039\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\255\255\255\255\
\255\255\255\255\255\255\039\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\255\255\255\255\255\255\
\255\255\255\255\039\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\255\255\255\255\255\255\255\255\
\255\255\039\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\255\255\255\255\255\255\037\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\255\255\255\255\036\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\255\255\255\255\036\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\255\255\255\255\036\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\255\255\255\255\036\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\255\255\255\255\036\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\255\255\035\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\255\255\
\035\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\255\255\035\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\255\255\035\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\255\255\035\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\255\255\035\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\255\255\035\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001"

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
  X\000\
  Y\000\
  WIDTH\000\
  HEIGHT\000\
  COLOR\000\
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
# 465 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'toplevel) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.program) in
    Obj.repr(
# 34 "parser.mly"
                   ( _1 :: _2 )
# 473 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var_decl) in
    Obj.repr(
# 38 "parser.mly"
           ( Vardecl _1 )
# 480 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'win_decl) in
    Obj.repr(
# 39 "parser.mly"
           (Win _1)
# 487 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_decl) in
    Obj.repr(
# 40 "parser.mly"
            ( Rect _1 )
# 494 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_move) in
    Obj.repr(
# 41 "parser.mly"
            ( RectMove _1 )
# 501 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_change_x) in
    Obj.repr(
# 42 "parser.mly"
                ( RectChangeX _1 )
# 508 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_change_y) in
    Obj.repr(
# 43 "parser.mly"
                ( RectChangeY _1 )
# 515 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_change_width) in
    Obj.repr(
# 44 "parser.mly"
                    ( RectChangeWidth _1 )
# 522 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_change_height) in
    Obj.repr(
# 45 "parser.mly"
                     ( RectChangeHeight _1 )
# 529 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_change_color) in
    Obj.repr(
# 46 "parser.mly"
                    ( RectChangeColor _1 )
# 536 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'circle_decl) in
    Obj.repr(
# 47 "parser.mly"
              ( Circle _1 )
# 543 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'line_decl) in
    Obj.repr(
# 48 "parser.mly"
            ( Line _1 )
# 550 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fun_def) in
    Obj.repr(
# 49 "parser.mly"
          ( Fundef _1 )
# 557 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 50 "parser.mly"
        ( Instr _1 )
# 564 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 51 "parser.mly"
                 ( Expr _1 )
# 571 "parser.ml"
               : 'toplevel))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 55 "parser.mly"
                      ( (_2, Scalar) )
# 578 "parser.ml"
               : 'var_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 56 "parser.mly"
                                             ( (_2, (Array _4)) )
# 586 "parser.ml"
               : 'var_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exprs_list) in
    Obj.repr(
# 59 "parser.mly"
    ( { r_name = _2 ; r_params = _4 } )
# 594 "parser.ml"
               : 'rect_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exprs_list) in
    Obj.repr(
# 63 "parser.mly"
    ( { w_name = _2 ; w_params = _4 } )
# 602 "parser.ml"
               : 'win_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'exprs_list) in
    Obj.repr(
# 67 "parser.mly"
    ( { r_name = _1 ; r_params = _3 } )
# 610 "parser.ml"
               : 'rect_move))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
    ( { r_name = _1 ; r_params = _5 } )
# 618 "parser.ml"
               : 'rect_change_x))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
    ( { r_name = _1 ; r_params = _5 } )
# 626 "parser.ml"
               : 'rect_change_y))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
    ( { r_name = _1 ; r_params = _5 } )
# 634 "parser.ml"
               : 'rect_change_width))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
    ( { r_name = _1 ; r_params = _5 } )
# 642 "parser.ml"
               : 'rect_change_height))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
    ( { r_name = _1 ; r_params = _5 } )
# 650 "parser.ml"
               : 'rect_change_color))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exprs_list) in
    Obj.repr(
# 91 "parser.mly"
    ( { c_name = _2 ; c_params = _4 } )
# 658 "parser.ml"
               : 'circle_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exprs_list) in
    Obj.repr(
# 95 "parser.mly"
    ( { l_name = _2 ; l_params = _4 } )
# 666 "parser.ml"
               : 'line_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
  ( [] )
# 672 "parser.ml"
               : 'exprs_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 99 "parser.mly"
             ( _1 :: _2 )
# 680 "parser.ml"
               : 'exprs_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
  ( [] )
# 686 "parser.ml"
               : 'opt_var_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt_var_decls) in
    Obj.repr(
# 103 "parser.mly"
                         ( _1 :: _2 )
# 694 "parser.ml"
               : 'opt_var_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 107 "parser.mly"
        ( _1 )
# 701 "parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'instrs) in
    Obj.repr(
# 108 "parser.mly"
               ( Seq (_1, _2) )
# 709 "parser.ml"
               : 'instrs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'opt_params) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'opt_var_decls) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'instrs) in
    Obj.repr(
# 112 "parser.mly"
    ( { f_name = _1 ; params = _3 ; vars = _6 ; body = _7 } )
# 719 "parser.ml"
               : 'fun_def))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
  ( [] )
# 725 "parser.ml"
               : 'opt_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'params) in
    Obj.repr(
# 119 "parser.mly"
               ( _1 :: _2 )
# 733 "parser.ml"
               : 'opt_params))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "parser.mly"
  ( [] )
# 739 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'params) in
    Obj.repr(
# 124 "parser.mly"
                     ( _2 :: _3 )
# 747 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'instrs) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'instrs) in
    Obj.repr(
# 130 "parser.mly"
    ( If (_2, _4, _6) )
# 756 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'instrs) in
    Obj.repr(
# 132 "parser.mly"
    ( While (_2, _4) )
# 764 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
    ( Assign (_1, _3) )
# 772 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
    ( ArrayWrite (_1, _3, _6) )
# 781 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'instrs) in
    Obj.repr(
# 138 "parser.mly"
    ( _2 )
# 788 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var_decl) in
    Obj.repr(
# 141 "parser.mly"
    ( Vardecl _1 )
# 795 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'win_decl) in
    Obj.repr(
# 143 "parser.mly"
    ( Win _1 )
# 802 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_decl) in
    Obj.repr(
# 145 "parser.mly"
    ( Rect _1 )
# 809 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_move) in
    Obj.repr(
# 147 "parser.mly"
    ( RectMove _1 )
# 816 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_change_x) in
    Obj.repr(
# 149 "parser.mly"
    ( RectChangeX _1 )
# 823 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_change_y) in
    Obj.repr(
# 151 "parser.mly"
    ( RectChangeY _1 )
# 830 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_change_width) in
    Obj.repr(
# 153 "parser.mly"
    ( RectChangeWidth _1 )
# 837 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_change_height) in
    Obj.repr(
# 155 "parser.mly"
    ( RectChangeHeight _1 )
# 844 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rect_change_color) in
    Obj.repr(
# 157 "parser.mly"
    ( RectChangeColor _1 )
# 851 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'circle_decl) in
    Obj.repr(
# 159 "parser.mly"
    ( Circle _1 )
# 858 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'line_decl) in
    Obj.repr(
# 161 "parser.mly"
    ( Line _1 )
# 865 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'opt_expr) in
    Obj.repr(
# 163 "parser.mly"
    ( Return _2 )
# 872 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'opt_exprs) in
    Obj.repr(
# 165 "parser.mly"
    ( Iapp (_1, _3) )
# 880 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'opt_exprs) in
    Obj.repr(
# 167 "parser.mly"
    ( Print _3 )
# 887 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'opt_exprs) in
    Obj.repr(
# 174 "parser.mly"
                                 ( App (_1, _3) )
# 895 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 175 "parser.mly"
                                 ( Binop ("==", _1, _3) )
# 903 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 176 "parser.mly"
                                 ( Binop (">", _1, _3) )
# 911 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 177 "parser.mly"
                                 ( Binop (">=", _1, _3) )
# 919 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 178 "parser.mly"
                                 ( Binop ("<", _1, _3) )
# 927 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 179 "parser.mly"
                                 ( Binop ("<=", _1, _3) )
# 935 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 180 "parser.mly"
                                 ( Binop ("+", _1, _3) )
# 943 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 181 "parser.mly"
                                 ( Binop ("-", _1, _3) )
# 951 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 182 "parser.mly"
                                 ( Binop ("*", _1, _3) )
# 959 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 183 "parser.mly"
                                 ( Binop ("/", _1, _3) )
# 967 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 184 "parser.mly"
                                 ( Monop ("-", _2) )
# 974 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 185 "parser.mly"
                                 ( _2 )
# 981 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 186 "parser.mly"
                                 ( _1 )
# 988 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 187 "parser.mly"
                                 ( ArrayRead (_1, _3) )
# 996 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 192 "parser.mly"
  ( None )
# 1002 "parser.ml"
               : 'opt_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 193 "parser.mly"
                     ( Some _1 )
# 1009 "parser.ml"
               : 'opt_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 197 "parser.mly"
  ( [] )
# 1015 "parser.ml"
               : 'opt_exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 198 "parser.mly"
                           ( _1 :: _2 )
# 1023 "parser.ml"
               : 'opt_exprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 202 "parser.mly"
  ( [] )
# 1029 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 203 "parser.mly"
                           ( _2 :: _3 )
# 1037 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 207 "parser.mly"
                 ( Int (_1) )
# 1044 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 208 "parser.mly"
                 ( Bool (true) )
# 1050 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 209 "parser.mly"
                 ( Bool (false) )
# 1056 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 210 "parser.mly"
                 ( String (_1) )
# 1063 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 211 "parser.mly"
                 ( Ident (_1) )
# 1070 "parser.ml"
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
