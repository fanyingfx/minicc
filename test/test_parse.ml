(** Parser tests *)

open Minicc

let%test "expression" =
  Parser.Private.parse_exp (Token_stream.of_list [ Token_type.Constant 100 ])
  = Ast.Constant 100
;;

let%test "group" =
  Parser.Private.parse_exp
    (Token_stream.of_list Token_type.[ LParen; Constant 100; RParen ])
  = Ast.Constant 100
;;

let%test "negate" =
  Parser.Private.parse_exp (Token_stream.of_list Token_type.[ Hyphen; Constant 100 ])
  = Ast.Unary (Ast.Negate, Ast.Constant 100)
;;

let%test "complement" =
  Parser.Private.parse_exp (Token_stream.of_list Token_type.[ Tilde; Constant 100 ])
  = Ast.Unary (Ast.Complement, Ast.Constant 100)
;;
let%test "statement" =
  Parser.Private.parse_statement
    (Token_stream.of_list
       [ Token_type.KWReturn; Token_type.Constant 4; Token_type.Semicolon ])
  = Ast.Return (Ast.Constant 4)
;;

let%test "error" =
  match Parser.parse [ Token_type.KWInt ] with
  | exception Parser.ParseError _ -> true
  | _ -> false
;;

let%test "empty" =
  match Parser.parse [] with
  | exception Parser.ParseError _ -> true
  | _ -> false
;;
