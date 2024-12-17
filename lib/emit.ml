open Assembly

let show_reg = function
  | AX -> "%eax"
  | CX -> "%ecx"
  | DX -> "%edx"
  | DI -> "%edi"
  | SI -> "%esi"
  | R8 -> "%r8d"
  | R9 -> "%r9d"
  | R10 -> "%r10d"
  | R11 -> "%r11d"

let show_operand = function
  | Reg r -> show_reg r
  | Imm i -> Printf.sprintf "$%d" i
  | Stack i -> Printf.sprintf "%d(%%rbp)" i
  | Pseudo name -> Printf.sprintf "%%%s" name

let show_byte_reg = function
  | AX -> "%al"
  | CX -> "%cl"
  | DX -> "%dl"
  | DI -> "%dil"
  | SI -> "%sil"
  | R8 -> "%r8b"
  | R9 -> "%r9b"
  | R10 -> "%r10b"
  | R11 -> "%r11b"

let show_byte_operand = function
  | Reg r -> show_byte_reg r
  | other -> show_operand other

let show_quadword_reg = function
  | AX -> "%rax"
  | CX -> "%rcx"
  | DX -> "%rdx"
  | DI -> "%rdi"
  | SI -> "%rsi"
  | R8 -> "%r8"
  | R9 -> "%r9"
  | R10 -> "%r10"
  | R11 -> "%r11"

let show_quadword_operand = function
  | Reg r -> show_quadword_reg r
  | other -> show_operand other

let show_unary_instruction = function Neg -> "negl" | Not -> "notl"

let show_binary_instruciton = function
  | Add -> "addl"
  | Sub -> "subl"
  | Mult -> "imull"

let show_local_label label = ".L" ^ label
let show_fun_name f = if Symbols.is_defined f then f else f ^ "@PLT"

let convert_cond = function
  | E -> "e"
  | NE -> "ne"
  | L -> "l"
  | LE -> "le"
  | G -> "g"
  | GE -> "ge"

let emit_instruction chan = function
  | Mov (src, dst) ->
      Printf.fprintf chan "\tmovl %s, %s\n" (show_operand src)
        (show_operand dst)
  | Unary (operator, dst) ->
      Printf.fprintf chan "\t%s %s\n"
        (show_unary_instruction operator)
        (show_operand dst)
  | Cmp (op1, op2) ->
      Printf.fprintf chan "\tcmpl %s, %s\n" (show_operand op1)
        (show_operand op2)
  | Jmp label -> Printf.fprintf chan "\tjmp %s\n" (show_local_label label)
  | JmpCC (cond, label) ->
      Printf.fprintf chan "\tj%s %s\n" (convert_cond cond)
        (show_local_label label)
  | SetCC (cond, operand) ->
      Printf.fprintf chan "\tset%s %s\n" (convert_cond cond)
        (show_byte_operand operand)
  | Label label -> Printf.fprintf chan "%s:\n" (show_local_label label)
  | Binary { op = operator; src; dst } ->
      Printf.fprintf chan "\t%s %s, %s\n"
        (show_binary_instruciton operator)
        (show_operand src) (show_operand dst)
  | Idiv operand ->
      Printf.fprintf chan "\t%s %s\n" "idivl" (show_operand operand)
  | Cdq -> Printf.fprintf chan "cdq\n"
  | AllocateStack i -> Printf.fprintf chan "subq $%d, %%rsp\n" i
  | DeallocateStack i -> Printf.fprintf chan "\taddq $%d, %%rsp\n" i
  | Push op -> Printf.fprintf chan "\tpushq %s\n" (show_quadword_operand op)
  | Call f -> Printf.fprintf chan "\tcall %s\n" (show_fun_name f)
  | Ret -> Printf.fprintf chan {|
  movq %%rbp, %%rsp
  popq %%rbp
  ret
|}

let emit_function chan (Function { name; instructions }) =
  Printf.fprintf chan {|
  .global %s
%s:
  pushq %%rbp
  movq %%rsp, %%rbp
  |}
    name name;
  List.iter (emit_instruction chan) instructions

let _emit_intel_syntax chan = Printf.fprintf chan ".intel_syntax noprefix;\n"

let emit_stack_note chan =
  Printf.fprintf chan "\t.section .note.GUN-stack,\"\",@progbits\n"

let emit assembly_file (Program function_defs) =
  let output_channel = open_out assembly_file in
  (* emit_intel_syntax output_channel; *)
  List.iter (emit_function output_channel) function_defs;
  emit_stack_note output_channel;
  close_out output_channel
