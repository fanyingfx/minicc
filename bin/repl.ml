open Minicc

let read_file filepath = In_channel.with_open_text filepath In_channel.input_all

let str = read_file "./examples/unop_add.c"
let tokens = Lexer.lex str
let ast = Parser.parse tokens
