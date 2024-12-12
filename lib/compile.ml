let lex src = src |> Lexer.lex
let parse src = src |> lex |> Parser.parse
let gen src = src |> parse |> Codegen.gen

let emit src src_file =
  let asm_ast = gen src in
  let asm_filename = Filename.chop_extension src_file ^ ".s" in
  ignore (Emit.emit asm_filename asm_ast)
;;

let compile stage src_file =
  let source = In_channel.with_open_text src_file In_channel.input_all in
  match stage with
  | Settings.Lex -> ignore (lex source)
  | Settings.Parse -> ignore (parse source)
  | Settings.Codegen -> ignore (gen source)
  | _ -> emit source src_file
;;
