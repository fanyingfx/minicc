(** Lexer tests *)

open Minicc

let%test "leading whitespace" = Lexer.lex "   return" = [ KWReturn ]
let%test "trailing whitespace" = Lexer.lex "0;\t\n" = [ Constant 0; Semicolon ]

let%test "a full program" =
  Lexer.lex "int main(void){return 0;}"
  = [ KWInt
    ; Identifier "main"
    ; LParen
    ; KWVoid
    ; RParen
    ; LBrace
    ; KWReturn
    ; Constant 0
    ; Semicolon
    ; RBrace
    ]
;;

let%test "two hyphens" = Lexer.lex "- -" = [ Hyphen; Hyphen ]
let%test "double hyphen" = Lexer.lex "a--" = [ Identifier "a"; DoubleHyphen ]
let%test "two tildes" = Lexer.lex "~~" = [ Tilde; Tilde ]

let%test "arithmetic" = Lexer.lex "+-*/" = [Plus;Hyphen;Star;Slash]
