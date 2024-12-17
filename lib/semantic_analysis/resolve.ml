open Ast
module StringMap = Map.Make (String)

type id_entry = {
  unique_name : string;
  from_current_scope : bool;
  has_linkage : bool;
}

let copy_identifier_map =
  StringMap.map (fun entry -> { entry with from_current_scope = false })

let rec resolve_exp id_map = function
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
      Assignment (resolve_exp id_map left, resolve_exp id_map right)
  | Var v -> (
      match StringMap.find_opt v id_map with
      | Some item -> Var item.unique_name
      | None -> failwith (Printf.sprintf "Undeclared variable %s" v))
  | Unary (op, e) -> Unary (op, resolve_exp id_map e)
  | Binary (op, e1, e2) ->
      Binary (op, resolve_exp id_map e1, resolve_exp id_map e2)
  | Conditional { condition; then_result; else_result } ->
      Conditional
        {
          condition = resolve_exp id_map condition;
          then_result = resolve_exp id_map then_result;
          else_result = resolve_exp id_map else_result;
        }
  | Constant _ as c -> c
  | FunCall { name; args } ->
      if StringMap.mem name id_map then
        let new_name = (StringMap.find name id_map).unique_name in
        let new_args = List.map (resolve_exp id_map) args in
        FunCall { name = new_name; args = new_args }
      else failwith "Undeclared function!"

let resolve_local_var_helper id_map name =
  match StringMap.find_opt name id_map with
  | Some { from_current_scope = true; _ } ->
      failwith "Duplicate variable declaration"
  | _ ->
      let unique_name = Unique_ids.make_name_temporary name in
      let new_map =
        StringMap.add name
          { unique_name; from_current_scope = true; has_linkage = false }
          id_map
      in
      (new_map, unique_name)

let resolve_local_var_declaration id_map { name; init } =
  let new_map, unique_name = resolve_local_var_helper id_map name in
  let resolved_init = Option.map (resolve_exp new_map) init in
  (new_map, { name = unique_name; init = resolved_init })

let rec resolve_statement id_map = function
  | Return e -> Return (resolve_exp id_map e)
  | Expression e -> Expression (resolve_exp id_map e)
  | If { condition; then_clause; else_clause } ->
      If
        {
          condition = resolve_exp id_map condition;
          then_clause = resolve_statement id_map then_clause;
          else_clause = Option.map (resolve_statement id_map) else_clause;
        }
  | Compound block ->
      let new_var_map = copy_identifier_map id_map in
      let new_block = resolve_block new_var_map block in
      Compound new_block
  | For { init; condition; post; body; id } ->
      let id_map = copy_identifier_map id_map in
      let new_var_map, new_init = resolve_for_init id_map init in
      let new_condition = resolve_optional_exp new_var_map condition in
      let new_post = resolve_optional_exp new_var_map post in
      let new_body = resolve_statement new_var_map body in
      For
        {
          init = new_init;
          condition = new_condition;
          post = new_post;
          body = new_body;
          id;
        }
  | While { condition; body; id } ->
      let new_condition = resolve_exp id_map condition in
      let new_body = resolve_statement id_map body in
      While { condition = new_condition; body = new_body; id }
  | DoWhile { condition; body; id } ->
      let new_condition = resolve_exp id_map condition in
      let new_body = resolve_statement id_map body in
      DoWhile { condition = new_condition; body = new_body; id }
  | Break id -> Break id
  | Continue id -> Continue id
  | Null -> Null

and resolve_for_init id_map = function
  | InitExp e -> (id_map, InitExp (resolve_optional_exp id_map e))
  | InitDecl d ->
      let new_map, new_d = resolve_local_var_declaration id_map d in
      (new_map, InitDecl new_d)

and resolve_optional_exp id_map = Option.map (resolve_exp id_map)

and resolve_block id_map (Block block) =
  let _var_map, new_block =
    List.fold_left_map resolve_block_item id_map block
  in
  Block new_block

and resolve_block_item id_map = function
  | S s ->
      let resolved_s = resolve_statement id_map s in
      (id_map, S resolved_s)
  | D d ->
      let new_map, resolved_d = resolve_local_declaration id_map d in
      (new_map, D resolved_d)

and resolve_local_declaration id_map = function
  | VarDecl d ->
      let new_map, resolved_vd = resolve_local_var_declaration id_map d in
      (new_map, VarDecl resolved_vd)
  | FunDecl { body = Some _; _ } ->
      failwith "nested function definitions are not allowed"
  | FunDecl fd ->
      let new_map, resolved_fd = resolve_function_declaration id_map fd in
      (new_map, FunDecl resolved_fd)

and resolve_function_declaration id_map fn =
  match StringMap.find_opt fn.name id_map with
  | Some { from_current_scope = true; has_linkage = false; _ } ->
      failwith "Duplicate declaration"
  | _ ->
      let new_entry =
        { unique_name = fn.name; from_current_scope = true; has_linkage = true }
      in
      let new_map = StringMap.add fn.name new_entry id_map in
      let inner_map = copy_identifier_map new_map in
      let inner_map1, resolved_params = resolve_params inner_map fn.params in
      let resolved_body = Option.map (resolve_block inner_map1) fn.body in
      (new_map, { fn with params = resolved_params; body = resolved_body })

and resolve_params id_map = List.fold_left_map resolve_local_var_helper id_map

let resolve (Program fn_decls) =
  let _, resolved_decls =
    List.fold_left_map resolve_function_declaration StringMap.empty fn_decls
  in
  Program resolved_decls
