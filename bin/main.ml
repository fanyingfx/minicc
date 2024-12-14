open Cmdliner
open Minicc

(* let current_platform = Settings.Linux;; *)
let validate_extension filename =
  let ext = Filename.extension filename in
  if ext = ".c" || ext = ".h" then ()
  else failwith "Expected C source file with .c or .h extension"

let replace_extension filename new_extension =
  let base = Filename.chop_extension filename in
  base ^ new_extension

let run_command cmd args =
  if Sys.command (Filename.quote_command cmd args) <> 0 then
    failwith ("Command failed: " ^ cmd)

let preprocess src =
  let _ = validate_extension src in
  let output = replace_extension src ".i" in
  let _ = run_command "gcc" [ "-E"; "-P"; src; "-o"; output ] in
  output

let compile stage preprocessed_src =
  let _ = Compile.compile stage preprocessed_src in
  (* remove preprocessed src *)
  run_command "rm" [ preprocessed_src ];
  replace_extension preprocessed_src ".s"

let assemble_and_link ?(cleanup = true) src =
  let assembly_file = replace_extension src ".s" in
  let output_file = Filename.chop_extension src in
  let _ = run_command "gcc" [ assembly_file; "-o"; output_file ] in
  if cleanup then run_command "rm" [ assembly_file ]

let driver debug stage src =
  let preprocessed_name = preprocess src in
  let assembly_name = compile stage preprocessed_name in
  if stage = Settings.Executable then
    assemble_and_link ~cleanup:(not debug) assembly_name
  else ()

let stage =
  let lex = (Settings.Lex, Arg.info [ "lex" ] ~doc:"Run the lexer")
  and parse =
    (Settings.Parse, Arg.info [ "parse" ] ~doc:"Run the lexer and parser")
  and validate =
    ( Settings.Validate,
      Arg.info [ "validate" ] ~doc:"Run the lexer and parser and validatation"
    )
  and tacky =
    ( Settings.Tacky,
      Arg.info [ "tacky" ] ~doc:"Run the lexer and parser and tacky generator"
    )
  and codegen =
    let doc = "Run through code generation but stop before emitting assembly" in
    (Settings.Codegen, Arg.info [ "codegen" ] ~doc)
  and assembly =
    let doc = "Stop before assembling (keep .s file)" in
    (Settings.Assembly, Arg.info [ "s"; "S" ] ~doc)
  in
  Arg.(
    value
    & vflag Settings.Executable
        [ lex; parse; validate; tacky; codegen; assembly ])

let debug =
  let doc =
    "Write out pre- and post-register-allocation assembly and DOT files of \
     interference graphs."
  in
  Arg.(value & flag & info [ "d" ] ~doc)

let src_file =
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"files")

let cmd =
  let doc = "A not-quite-C compiler" in
  let info = Cmd.info "minicc" ~doc in
  Cmd.v info Term.(const driver $ debug $ stage $ src_file)

let main () = exit (Cmd.eval cmd)
let () = main ()
