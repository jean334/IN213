%{
open Ast ;;
%}

%token EOF
%token <int> INT
%token <string> IDENT
%token <string> STRING
%token TRUE FALSE
%token RECT 
%token CIRCLE
%token TRIANGLE
%token LINE
%token FORCE
%token WIN
%token COLONEQUAL
%token DOT_X DOT_Y DOT_WIDTH DOT_HEIGHT DOT_COLOR
%token WHILE DO DONE BEGIN END RETURN VAR
%token PLUS MINUS MULT DIV EQUALEQUAL GREATER SMALLER GREATEREQUAL SMALLEREQUAL
%token LPAR RPAR SEMICOLON COMMA LBRACKET RBRACKET LBRACE RBRACE DOT
%token IF THEN ELSE ENDIF PRINT

%left EQUALEQUAL GREATER SMALLER GREATEREQUAL SMALLEREQUAL
%left PLUS MINUS
%left MULT DIV

%start program
%type <Ast.program> program

%%

program:
| EOF { [] }
| toplevel program { $1 :: $2 }
;

toplevel:
| var_decl { Vardecl $1 }
| win_decl {Win $1}
| rect_decl { Rect $1 }
| rect_move { RectMove $1 }
| rect_change_x { RectChangeX $1 }
| rect_change_y { RectChangeY $1 }
| rect_change_width { RectChangeWidth $1 }
| rect_change_height { RectChangeHeight $1 }
| rect_change_color { RectChangeColor $1 }
| circle_decl { Circle $1 }
| line_decl { Line $1 }
| fun_def { Fundef $1 }
| instr { Instr $1 }
| expr SEMICOLON { Expr $1 }
;

var_decl:
| VAR IDENT SEMICOLON { ($2, Scalar) }
| VAR IDENT LBRACKET expr RBRACKET SEMICOLON { ($2, (Array $4)) }
rect_decl:
| RECT IDENT LPAR exprs_list RPAR SEMICOLON
    { { r_name = $2 ; r_params = $4 } }

win_decl:
| WIN IDENT LPAR exprs_list RPAR SEMICOLON
    { { w_name = $2 ; w_params = $4 } }
    
rect_move:
| IDENT LPAR exprs_list RPAR SEMICOLON
    { { r_name = $1 ; r_params = $3 } }

rect_change_x:
    IDENT DOT_X LPAR expr RPAR SEMICOLON
    { { r_name = $1 ; r_params = $4 } }

rect_change_y:
    IDENT DOT_Y LPAR expr RPAR SEMICOLON
    { { r_name = $1 ; r_params = $4 } }

rect_change_width:
    IDENT DOT_WIDTH LPAR expr RPAR SEMICOLON
    { { r_name = $1 ; r_params = $4 } }

rect_change_height:
    IDENT DOT_HEIGHT LPAR expr RPAR SEMICOLON
    { { r_name = $1 ; r_params = $4 } }

rect_change_color:
    IDENT DOT_COLOR LPAR expr RPAR SEMICOLON
    { { r_name = $1 ; r_params = $4 } }

circle_decl:
| CIRCLE IDENT LPAR exprs_list RPAR SEMICOLON
    { { c_name = $2 ; c_params = $4 } }

line_decl:
| LINE IDENT LPAR exprs_list RPAR SEMICOLON
    { { l_name = $2 ; l_params = $4 } }

exprs_list:
| { [] }
| expr exprs { $1 :: $2 }

opt_var_decls:
| { [] }
| var_decl opt_var_decls { $1 :: $2 }
;

instrs:
| instr { $1 }
| instr instrs { Seq ($1, $2) }
;
fun_def:
| IDENT LPAR opt_params RPAR LBRACE opt_var_decls instrs RBRACE
    { { f_name = $1 ; params = $3 ; vars = $6 ; body = $7 } }
;



opt_params:
| { [] }
| IDENT params { $1 :: $2 }
;

params:
| { [] }
| COMMA IDENT params { $2 :: $3 }
;


instr:
| IF expr THEN instrs ELSE instrs ENDIF
    { If ($2, $4, $6) }
| WHILE expr DO instrs DONE
    { While ($2, $4) }
| IDENT COLONEQUAL expr SEMICOLON
    { Assign ($1, $3) }
| IDENT LBRACKET expr RBRACKET COLONEQUAL expr SEMICOLON
    { ArrayWrite ($1, $3, $6) }
| BEGIN instrs END
    { $2 }

| var_decl
    { Vardecl $1 }
| win_decl
    { Win $1 }
| rect_decl
    { Rect $1 }
| rect_move
    { RectMove $1 }
| rect_change_x
    { RectChangeX $1 }
| rect_change_y
    { RectChangeY $1 }
| rect_change_width
    { RectChangeWidth $1 }
| rect_change_height
    { RectChangeHeight $1 }
| rect_change_color
    { RectChangeColor $1 }
| circle_decl
    { Circle $1 }
| line_decl
    { Line $1 }
| RETURN opt_expr SEMICOLON
    { Return $2 }
| IDENT LPAR opt_exprs RPAR SEMICOLON
    { Iapp ($1, $3) }
| PRINT LPAR opt_exprs RPAR SEMICOLON
    { Print $3 }

;

expr:
| IDENT LPAR opt_exprs RPAR      { App ($1, $3) }
| expr EQUALEQUAL expr           { Binop ("==", $1, $3) }
| expr GREATER expr              { Binop (">", $1, $3) }
| expr GREATEREQUAL expr         { Binop (">=", $1, $3) }
| expr SMALLER expr              { Binop ("<", $1, $3) }
| expr SMALLEREQUAL expr         { Binop ("<=", $1, $3) }
| expr PLUS expr                 { Binop ("+", $1, $3) }
| expr MINUS expr                { Binop ("-", $1, $3) }
| expr MULT expr                 { Binop ("*", $1, $3) }
| expr DIV expr                  { Binop ("/", $1, $3) }
| MINUS expr                     { Monop ("-", $2) }
| LPAR expr RPAR                 { $2 }
| atom                           { $1 }
| IDENT LBRACKET expr RBRACKET   { ArrayRead ($1, $3) }

;     

 opt_expr:
| { None }
| expr               { Some $1 }
;

opt_exprs:
| { [] }
| expr exprs               { $1 :: $2 }
;

exprs:
| { [] }
| COMMA expr exprs         { $2 :: $3 }
;

atom:
| INT            { Int ($1) }
| TRUE           { Bool (true) }
| FALSE          { Bool (false) }
| STRING         { String ($1) }
| IDENT          { Ident ($1) }
;
