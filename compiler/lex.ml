# 1 "lex.mll"
 
  open Parser ;;
  exception Eoi ;;

  let pc c = Printf.eprintf "Lu '%c'\n%!" c;;
  let ps c = Printf.eprintf "Lu '%s'\n%!" c;;

  (* Emprunt� de l'analyseur lexical du compilateur OCaml *)
  (* To buffer string literals *)

  let initial_string_buffer = Bytes.create 256;;
  let string_buff = ref initial_string_buffer;;
  let string_index = ref 0;;

  let reset_string_buffer () =
    string_buff := initial_string_buffer;
    string_index := 0;;

  let store_string_char c =
    if !string_index >= Bytes.length (!string_buff) then begin
      let new_buff = Bytes.create (Bytes.length (!string_buff) * 2) in
      Bytes.blit (!string_buff) 0 new_buff 0 (Bytes.length (!string_buff));
      string_buff := new_buff
    end;
    Bytes.unsafe_set (!string_buff) (!string_index) c;
    incr string_index;;

  let get_stored_string () =
    let s = Bytes.to_string (Bytes.sub (!string_buff) 0 (!string_index)) in
    string_buff := initial_string_buffer;
    s;;

  (* To translate escape sequences *)

  let char_for_backslash c = match c with
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

  let char_for_decimal_code lexbuf i =
    let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
             10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                  (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
    if (c < 0 || c > 255)
    then raise (Failure ("Illegal_escape: " ^ (Lexing.lexeme lexbuf)))
    else Char.chr c;;

  let char_for_hexadecimal_code lexbuf i =
    let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
    let val1 = if d1 >= 97 then d1 - 87
               else if d1 >= 65 then d1 - 55
               else d1 - 48
    in
    let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
    let val2 = if d2 >= 97 then d2 - 87
               else if d2 >= 65 then d2 - 55
               else d2 - 48
    in
    Char.chr (val1 * 16 + val2);;
  
  (*exception LexError of (Lexing.position * Lexing.position) ;;
  voir pcflex si on veut ajouter les exceptions*)


# 69 "lex.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\230\255\231\255\234\255\235\255\236\255\237\255\238\255\
    \239\255\240\255\002\000\242\255\243\255\082\000\245\255\246\255\
    \247\255\003\000\031\000\033\000\082\000\157\000\001\000\255\255\
    \252\255\249\255\248\255\232\255\233\255\241\255\004\000\248\255\
    \249\255\002\000\250\255\183\000\255\255\251\255\217\000\193\000\
    \254\255\000\001\253\255\016\001\252\255\005\000\253\255\254\255\
    \255\255\098\000\253\255\254\255\048\000\255\255\006\000\254\255\
    \008\000\255\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\025\000\255\255\255\255\011\000\255\255\255\255\
    \255\255\005\000\004\000\025\000\002\000\001\000\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\005\000\255\255\007\000\255\255\255\255\004\000\004\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\001\000\255\255\255\255\255\255\
    \000\000\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\255\255\000\000\000\000\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\031\000\000\000\
    \000\000\255\255\000\000\037\000\000\000\000\000\255\255\255\255\
    \000\000\255\255\000\000\255\255\000\000\047\000\000\000\000\000\
    \000\000\051\000\000\000\000\000\255\255\000\000\055\000\000\000\
    \255\255\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\023\000\023\000\023\000\034\000\022\000\034\000\048\000\
    \057\000\033\000\057\000\056\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \023\000\000\000\003\000\000\000\000\000\000\000\036\000\000\000\
    \009\000\008\000\014\000\016\000\011\000\015\000\020\000\013\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\010\000\012\000\017\000\019\000\018\000\029\000\
    \026\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\007\000\025\000\006\000\024\000\053\000\
    \035\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\005\000\027\000\004\000\000\000\000\000\
    \000\000\028\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\052\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\000\000\000\000\000\000\
    \000\000\020\000\000\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\040\000\
    \000\000\040\000\000\000\000\000\000\000\000\000\040\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\000\000\000\000\000\000\032\000\046\000\255\255\000\000\
    \000\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\040\000\000\000\000\000\000\000\000\000\
    \000\000\040\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \000\000\000\000\000\000\000\000\000\000\040\000\000\000\000\000\
    \000\000\040\000\000\000\040\000\000\000\000\000\000\000\038\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\044\000\044\000\044\000\044\000\044\000\044\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\050\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\044\000\044\000\044\000\044\000\044\000\044\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\022\000\033\000\000\000\030\000\045\000\
    \054\000\030\000\056\000\054\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\255\255\255\255\255\255\030\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\010\000\
    \017\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\018\000\000\000\019\000\052\000\
    \030\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\013\000\000\000\255\255\255\255\
    \255\255\013\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\049\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\255\255\255\255\255\255\
    \255\255\020\000\255\255\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\035\000\
    \255\255\035\000\255\255\255\255\255\255\255\255\035\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\030\000\045\000\054\000\255\255\
    \255\255\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\035\000\255\255\255\255\255\255\255\255\
    \255\255\035\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \255\255\255\255\255\255\255\255\255\255\035\000\255\255\255\255\
    \255\255\035\000\255\255\035\000\255\255\255\255\255\255\035\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\043\000\043\000\043\000\043\000\043\000\043\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\049\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\043\000\043\000\043\000\043\000\043\000\043\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\035\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec lex lexbuf =
   __ocaml_lex_lex_rec lexbuf 0
and __ocaml_lex_lex_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 72 "lex.mll"
      ( lex lexbuf )
# 255 "lex.ml"

  | 1 ->
let
# 73 "lex.mll"
                  lxm
# 261 "lex.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 74 "lex.mll"
      ( INT(int_of_string lxm) )
# 265 "lex.ml"

  | 2 ->
let
# 75 "lex.mll"
                                                             lxm
# 271 "lex.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 76 "lex.mll"
      ( match lxm with
        | ".x" -> DOT_X
        | ".y" -> DOT_Y
        | ".width" -> DOT_WIDTH
        | ".height" -> DOT_HEIGHT
        | ".color" -> DOT_COLOR
        | "if" -> IF
        | "then" -> THEN
        | "else" -> ELSE
        | "endif" -> ENDIF
        | "var" -> VAR
        | "begin" -> BEGIN
        | "end" -> END
        | "do" -> DO
        | "done" -> DONE
        | "while" -> WHILE
        | "return" -> RETURN
        | "print" -> PRINT
        | "true" -> TRUE
        | "false" -> FALSE
        | "win" -> WIN
        | "force" -> FORCE
        | "rect" -> RECT
        | "circle" -> CIRCLE
        | "triangle" -> TRIANGLE
        | "line" -> LINE
        | _ -> IDENT(lxm) )
# 301 "lex.ml"

  | 3 ->
# 104 "lex.mll"
           ( EQUALEQUAL )
# 306 "lex.ml"

  | 4 ->
# 105 "lex.mll"
          ( GREATER)
# 311 "lex.ml"

  | 5 ->
# 105 "lex.mll"
                            ( SMALLER )
# 316 "lex.ml"

  | 6 ->
# 106 "lex.mll"
          ( GREATEREQUAL)
# 321 "lex.ml"

  | 7 ->
# 106 "lex.mll"
                                  ( SMALLEREQUAL )
# 326 "lex.ml"

  | 8 ->
# 107 "lex.mll"
          ( PLUS )
# 331 "lex.ml"

  | 9 ->
# 107 "lex.mll"
                           ( MINUS )
# 336 "lex.ml"

  | 10 ->
# 107 "lex.mll"
                                           ( MULT )
# 341 "lex.ml"

  | 11 ->
# 107 "lex.mll"
                                                          ( DIV )
# 346 "lex.ml"

  | 12 ->
# 108 "lex.mll"
          ( SEMICOLON )
# 351 "lex.ml"

  | 13 ->
# 109 "lex.mll"
          ( COMMA )
# 356 "lex.ml"

  | 14 ->
# 110 "lex.mll"
          ( COLONEQUAL )
# 361 "lex.ml"

  | 15 ->
# 111 "lex.mll"
          ( LPAR )
# 366 "lex.ml"

  | 16 ->
# 112 "lex.mll"
          ( RPAR )
# 371 "lex.ml"

  | 17 ->
# 113 "lex.mll"
          ( LBRACKET )
# 376 "lex.ml"

  | 18 ->
# 114 "lex.mll"
          ( RBRACKET )
# 381 "lex.ml"

  | 19 ->
# 115 "lex.mll"
          ( LBRACE )
# 386 "lex.ml"

  | 20 ->
# 116 "lex.mll"
          ( RBRACE )
# 391 "lex.ml"

  | 21 ->
# 117 "lex.mll"
          ( reset_string_buffer();
            in_string lexbuf;
            STRING (get_stored_string()) )
# 398 "lex.ml"

  | 22 ->
# 120 "lex.mll"
          ( in_cpp_comment lexbuf )
# 403 "lex.ml"

  | 23 ->
# 121 "lex.mll"
          ( in_c_comment lexbuf )
# 408 "lex.ml"

  | 24 ->
# 122 "lex.mll"
          ( EOF )
# 413 "lex.ml"

  | 25 ->
let
# 123 "lex.mll"
          c
# 419 "lex.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 123 "lex.mll"
            ( Printf.eprintf "Invalid char `%c'\n%!" c ; lex lexbuf )
# 423 "lex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_lex_rec lexbuf __ocaml_lex_state

and in_string lexbuf =
   __ocaml_lex_in_string_rec lexbuf 30
and __ocaml_lex_in_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 127 "lex.mll"
      ( () )
# 435 "lex.ml"

  | 1 ->
# 129 "lex.mll"
      ( store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        in_string lexbuf )
# 441 "lex.ml"

  | 2 ->
# 132 "lex.mll"
      ( store_string_char(char_for_decimal_code lexbuf 1);
        in_string lexbuf )
# 447 "lex.ml"

  | 3 ->
# 135 "lex.mll"
      ( store_string_char(char_for_hexadecimal_code lexbuf 2);
         in_string lexbuf )
# 453 "lex.ml"

  | 4 ->
let
# 137 "lex.mll"
              chars
# 459 "lex.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 138 "lex.mll"
      ( skip_to_eol lexbuf; raise (Failure("Illegal escape: " ^ chars)) )
# 463 "lex.ml"

  | 5 ->
let
# 139 "lex.mll"
               s
# 469 "lex.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 140 "lex.mll"
      ( for i = 0 to String.length s - 1 do
          store_string_char s.[i];
        done;
        in_string lexbuf
      )
# 477 "lex.ml"

  | 6 ->
# 146 "lex.mll"
      ( raise Eoi )
# 482 "lex.ml"

  | 7 ->
let
# 147 "lex.mll"
         c
# 488 "lex.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 148 "lex.mll"
      ( store_string_char c; in_string lexbuf )
# 492 "lex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_in_string_rec lexbuf __ocaml_lex_state

and in_cpp_comment lexbuf =
   __ocaml_lex_in_cpp_comment_rec lexbuf 45
and __ocaml_lex_in_cpp_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 151 "lex.mll"
         ( lex lexbuf )
# 504 "lex.ml"

  | 1 ->
# 152 "lex.mll"
         ( in_cpp_comment lexbuf )
# 509 "lex.ml"

  | 2 ->
# 153 "lex.mll"
         ( raise Eoi )
# 514 "lex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_in_cpp_comment_rec lexbuf __ocaml_lex_state

and in_c_comment lexbuf =
   __ocaml_lex_in_c_comment_rec lexbuf 49
and __ocaml_lex_in_c_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 156 "lex.mll"
         ( lex lexbuf )
# 526 "lex.ml"

  | 1 ->
# 157 "lex.mll"
         ( in_c_comment lexbuf )
# 531 "lex.ml"

  | 2 ->
# 158 "lex.mll"
         ( raise Eoi )
# 536 "lex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_in_c_comment_rec lexbuf __ocaml_lex_state

and skip_to_eol lexbuf =
   __ocaml_lex_skip_to_eol_rec lexbuf 54
and __ocaml_lex_skip_to_eol_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 161 "lex.mll"
            ( () )
# 548 "lex.ml"

  | 1 ->
# 162 "lex.mll"
            ( skip_to_eol lexbuf )
# 553 "lex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_skip_to_eol_rec lexbuf __ocaml_lex_state

;;

