open Assembly

let show_operand = function
  | Register -> "%eax"
  | Imm i -> Printf.sprintf "$%d" i
;;

let emit_instruction chan = function
  | Mov (src, dst) ->
    Printf.fprintf chan "\tmovl %s, %s\n" (show_operand src) (show_operand dst)
  | Ret -> Printf.fprintf chan "\tret\n"
;;

let emit_function chan (Function { name; instructions }) =
  Printf.fprintf
    chan
    {|
  .global %s
%s:
  |}
    name
    name;
  List.iter (emit_instruction chan) instructions
;;

let emit_stack_note chan =
  Printf.fprintf chan "\t.section .note.GUN-stack,\"\",@progbits\n"
;;

let emit assembly_file (Program function_def) =
  let output_channel = open_out assembly_file in
  emit_function output_channel function_def;
  emit_stack_note output_channel;
  close_out output_channel
;;
