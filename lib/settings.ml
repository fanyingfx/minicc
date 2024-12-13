type target = OS_X | Linux

let platform = ref Linux (* default to OS X *)
let debug = ref true

type stage = Lex | Parse | Tacky | Codegen | Assembly | Executable
