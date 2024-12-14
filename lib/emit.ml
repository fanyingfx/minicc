open Assembly
let show_operand = function
  | Reg AX -> "%eax"
  | Reg R10 -> "%r10d"
  | Reg DX -> "%edx"
  | Reg R11 -> "%r11d"
  | Imm i -> Printf.sprintf "$%d" i
  | Stack i -> Printf.sprintf "%d(%%rbp)" i
  | Pseudo name -> Printf.sprintf "%%%s" name 
let show_byte_operand = function
  | Reg AX -> "%al"
  | Reg R10 -> "%r10b"
  | Reg DX -> "%dl"
  | Reg R11 -> "%r11b"
  | other ->  
    let rs = show_operand other in 
    rs


let show_unary_instruction = function Neg -> "negl" | Not -> "notl"

let show_binary_instruciton = function
  | Add -> "addl"
  | Sub -> "subl"
  | Mult -> "imull"

let show_local_label label = ".L" ^ label

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
  | Cdq -> Printf.fprintf chan "cdq"
  | AllocateStack i -> Printf.fprintf chan "subq $%d, %%rsp\n" i
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

let emit assembly_file (Program function_def) =
  let output_channel = open_out assembly_file in
  (* emit_intel_syntax output_channel; *)
  emit_function output_channel function_def;
  emit_stack_note output_channel;
  close_out output_channel

  