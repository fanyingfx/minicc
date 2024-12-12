open Assembly

let show_operand = function
  | Reg AX -> "%eax"
  | Reg R10 -> "%r10d"
  | Imm i -> Printf.sprintf "$%d" i
  | Stack i -> Printf.sprintf "%d(%%rbp)" i
  | Pseudo name -> Printf.sprintf "%%%s" name [@coverage off]
;;

let show_unary_instruction = function
  | Neg -> "negl"
  | Not -> "notl"
;;

let emit_instruction chan = function
  | Mov (src, dst) ->
    Printf.fprintf chan "\tmovl %s, %s\n" (show_operand src) (show_operand dst)
  | Unary (operator,dst)->
    Printf.fprintf chan "\t%s %s\n" (show_unary_instruction operator) (show_operand dst)
  | AllocateStack i -> Printf.fprintf chan "\tsubq $%d, %%rsp\n" i
  | Ret -> Printf.fprintf chan {|
    movq %%rbp, %%rsp
    popq %%rbp
    ret
  |}
;;

let emit_function chan (Function { name; instructions }) =
  Printf.fprintf
    chan
    {|
  .global %s
%s:
  pushq %%rbp
  movq %%rsp, %%rbp
  |}
    name
    name;
  List.iter (emit_instruction chan) instructions
;;

let _emit_intel_syntax chan = Printf.fprintf chan ".intel_syntax noprefix;\n"

let emit_stack_note chan =
  Printf.fprintf chan "\t.section .note.GUN-stack,\"\",@progbits\n"
;;

let emit assembly_file (Program function_def) =
  let output_channel = open_out assembly_file in
  (* emit_intel_syntax output_channel; *)
  emit_function output_channel function_def;
  emit_stack_note output_channel;
  close_out output_channel
;;
