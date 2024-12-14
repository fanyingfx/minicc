(** Parser tests *)

open Minicc

module Test = struct
  let parse_exp exp_str =
    exp_str |> Lexer.lex |> Token_stream.of_list |> Parser.Private.parse_exp 0

  let parse_block_item stmt =
    stmt |> Lexer.lex |> Token_stream.of_list |> Parser.Private.parse_block_item

  let parse_stmt stmt =
    stmt |> Lexer.lex |> Token_stream.of_list |> Parser.Private.parse_statement
end

let%test "expression" = Test.parse_exp "100;" = Ast.Constant 100
let%test "group" = Test.parse_exp "(100);" = Ast.Constant 100

let%test "negate " =
  Test.parse_exp "-100;" = Ast.Unary (Ast.Negate, Ast.Constant 100)

let%test "complement" =
  Test.parse_exp "~100;" = Ast.Unary (Ast.Complement, Ast.Constant 100)

let%test "statement" = Test.parse_stmt "return 4;" = Ast.Return (Ast.Constant 4)

let%test "assignment" =
  Test.parse_block_item "int a=3;"
  = Ast.D (Ast.Declaration { name = "a"; init = Some (Ast.Constant 3) })

let%test "assignment right assoc" =
  Test.parse_block_item "int a=b=3;"
  = Ast.(
      D
        (Declaration
           { name = "a"; init = Some (Assignment (Var "b", Constant 3)) }))

let%test "error" =
  match Parser.parse [ Token_type.KWInt ] with
  | exception Parser.ParseError _ -> true
  | _ -> false

let%test "empty" =
  match Parser.parse [] with
  | exception Parser.ParseError _ -> true
  | _ -> false
