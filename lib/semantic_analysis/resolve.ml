open Ast
module StringMap = Map.Make (String)

type var_entry = { unique_name : string; from_current_block : bool }

let copy_variable_map =
  StringMap.map (fun entry -> { entry with from_current_block = false })

let rec resolve_exp var_map = function
  | Assignment (left, right) ->
      let _ =
        match left with
        | Var _ -> ()
        | _ ->
            failwith
              (Format.asprintf
                 "Expected expression on left-hand side of assignment \
                  statement, found %a"
                 pp_exp left)
      in
      Assignment (resolve_exp var_map left, resolve_exp var_map right)
  | Var v -> (
      match StringMap.find_opt v var_map with
      | Some item -> Var item.unique_name
      | None -> failwith (Printf.sprintf "Undeclared variable %s" v))
  | Unary (op, e) -> Unary (op, resolve_exp var_map e)
  | Binary (op, e1, e2) ->
      Binary (op, resolve_exp var_map e1, resolve_exp var_map e2)
  | Conditional { condition; then_result; else_result } ->
      Conditional
        {
          condition = resolve_exp var_map condition;
          then_result = resolve_exp var_map then_result;
          else_result = resolve_exp var_map else_result;
        }
  | Constant _ as c -> c

let resolve_declaration var_map (Declaration { name; init }) =
  match StringMap.find_opt name var_map with
  | Some { from_current_block = true; _ } ->
      failwith "Duplicate variable declaration"
  | _ ->
      let unique_name = Unique_ids.make_name_temporary name in
      let new_map =
        StringMap.add name { unique_name; from_current_block = true } var_map
      in
      let resolved_init = Option.map (resolve_exp new_map) init in
      (new_map, Declaration { name = unique_name; init = resolved_init })

let rec resolve_statement var_map = function
  | Return e -> Return (resolve_exp var_map e)
  | Expression e -> Expression (resolve_exp var_map e)
  | If { condition; then_clause; else_clause } ->
      If
        {
          condition = resolve_exp var_map condition;
          then_clause = resolve_statement var_map then_clause;
          else_clause = Option.map (resolve_statement var_map) else_clause;
        }
  | Compound (Block block) ->
      let new_var_map = copy_variable_map var_map in
      let new_block = resolve_block new_var_map block in
      Compound (Block new_block)
  | Null -> Null

and resolve_block var_map block =
  let _var_map, new_block =
    List.fold_left_map resolve_block_item var_map block
  in
  new_block

and resolve_block_item var_map = function
  | S s ->
      let resolved_s = resolve_statement var_map s in
      (var_map, S resolved_s)
  | D d ->
      let new_map, resolved_d = resolve_declaration var_map d in
      (new_map, D resolved_d)

let resolve_function_def (Function { name; body = Block body }) =
  let var_map = StringMap.empty in
  let resolved_body = resolve_block var_map body in
  Function { name; body = Block resolved_body }

let resolve (Program fn_def) = Program (resolve_function_def fn_def)
