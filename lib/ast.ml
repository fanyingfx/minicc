type exp = Constant of int [@@deriving show]
type statement = Return of exp [@@deriving show]

type function_definition =
  | Function of
      { name : string
      ; body : statement
      }
[@@deriving show]

type t = Program of function_definition [@@deriving show]

let pretty_print_exp = function
  | Constant i -> "Constant(" ^ string_of_int i ^ ")"
;;

let pretty_print_statement = function
  | Return exp -> "Return(\n \t\t\t" ^ pretty_print_exp exp ^ ")"
;;

let pretty_print_function_definition = function
  | Function { name; body } ->
    "Function {\n\t\tname = \""
    ^ name
    ^ "\",\n\t\tbody="
    ^ pretty_print_statement body
    ^ " }"
;;

let pretty_print_program = function
  | Program func ->
    print_string ("Program(\n\t" ^ pretty_print_function_definition func ^ ")")
;;
