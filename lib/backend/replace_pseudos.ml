open Assembly
module StringMap = Map.Make (String)

type replacement_state = {
  current_offset : int; (* last used stack slot *)
  offset_map : int StringMap.t (* map from pseudoregister to stack slots *);
}

let replace_operand state = function
  | Pseudo s -> (
      match StringMap.find_opt s state.offset_map with
      | Some offset -> (state, Stack offset)
      | None ->
          let new_offset = state.current_offset - 4 in
          let new_state =
            {
              current_offset = new_offset;
              offset_map = StringMap.add s new_offset state.offset_map;
            }
          in
          (new_state, Stack new_offset))
  | other -> (state, other)

let replace_pseudos_in_instruction state = function
  | Mov (src, dst) ->
      let state1, new_src = replace_operand state src in
      let state2, new_dst = replace_operand state1 dst in
      let new_mov = Mov (new_src, new_dst) in
      (state2, new_mov)
  | Unary (op, dst) ->
      let state1, new_dst = replace_operand state dst in
      let new_unary = Unary (op, new_dst) in
      (state1, new_unary)
  | Binary { op; src; dst } ->
      let state1, new_src = replace_operand state src in
      let state2, new_dst = replace_operand state1 dst in
      let new_binary = Binary { op; src = new_src; dst = new_dst } in
      (state2, new_binary)
  | Idiv operand ->
      let state1, new_operand = replace_operand state operand in
      (state1, Idiv new_operand)
  | Cmp (op1, op2) ->
      let state1, new_op1 = replace_operand state op1 in
      let state2, new_op2 = replace_operand state1 op2 in
      let new_cmp = Cmp (new_op1, new_op2) in
      (state2, new_cmp)
  | SetCC (cond, op) ->
      let state1, new_op = replace_operand state op in
      let new_setcc = SetCC (cond, new_op) in
      (state1, new_setcc)
  | JmpCC (cond ,op) ->
      (state, JmpCC (cond,op))
  | (Ret | Cdq | Label _ | Jmp _) as other -> (state, other)
  | AllocateStack _ ->
      failwith "Internal error: AllocateStack should't be present at this point"
      [@coverage off]

let replace_pseudos_in_function (Function { name; instructions }) =
  let init_state = { current_offset = 0; offset_map = StringMap.empty } in
  let final_state, fixed_instructions =
    List.fold_left_map replace_pseudos_in_instruction init_state instructions
  in
  ( Function { name; instructions = fixed_instructions },
    final_state.current_offset )

let replace_pseudos (Program fn_def) =
  let fixed_def, last_stack_slot = replace_pseudos_in_function fn_def in
  (Program fixed_def, last_stack_slot)
