(** Lexer tests *)

open Minicc

let%test "leading whitespace" = Lexer.lex "   return" = [ KWReturn ]
let%test "trailing whitespace" = Lexer.lex "0;\t\n" = [ Constant 0; Semicolon ]

let%test "a full program" =
  Lexer.lex "int main(void){return 0;}"
  = [
      KWInt;
      Identifier "main";
      LParen;
      KWVoid;
      RParen;
      LBrace;
      KWReturn;
      Constant 0;
      Semicolon;
      RBrace;
    ]
