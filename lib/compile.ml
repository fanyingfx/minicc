let lex content = content |> Lexer.lex
let parse content = content |> lex |> Parser.parse

let validate_ast ast =
  let labeled_ast = ast |> Resolve.resolve |> Label_loops.label_loops in
  labeled_ast |> Typecheck.typecheck;
  labeled_ast

let tacky_gen content src_file =
  let tacky = Tacky_gen.gen (parse content |> validate_ast) in
  Tacky_print.debug_print_tacky src_file tacky;
  tacky

let gen content src_file =
  let asm_ast = tacky_gen content src_file |> Codegen.gen in
  (if !Settings.debug then
     let prealloc_filename =
       Filename.chop_extension src_file ^ ".prealloc.debug.s"
     in
     Emit.emit prealloc_filename asm_ast);
  let asm_ast1= Replace_pseudos.replace_pseudos asm_ast in
  let asm_ast2 = Instruction_fixup.fixup_program asm_ast1 in
  asm_ast2

let emit src src_file =
  let asm_ast = gen src src_file in
  let asm_filename = Filename.chop_extension src_file ^ ".s" in
  Emit.emit asm_filename asm_ast

let compile stage src_file =
  let content = In_channel.with_open_text src_file In_channel.input_all in
  match stage with
  | Settings.Lex -> ignore (lex content)
  | Settings.Parse -> ignore (parse content)
  | Settings.Validate -> ignore (parse content |> validate_ast)
  | Settings.Tacky -> ignore (tacky_gen content src_file)
  | Settings.Codegen -> ignore (gen content src_file)
  | Settings.Assembly | Settings.Obj | Settings.Executable ->
      emit content src_file
