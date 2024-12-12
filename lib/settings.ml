type target = OS_X | Linux
let platform = ref Linux (* default to OS X *)
type stage =
  | Lex
  | Parse
  | Codegen
  | Assembly
  | Executable
