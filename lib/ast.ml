type exp =
  | Constant of int
  | Unary of unary_operator * exp
[@@deriving show]

and unary_operator =
  | Complement
  | Negate

type statement = Return of exp [@@deriving show]

type function_definition =
  | Function of
      { name : string
      ; body : statement
      }
[@@deriving show]

type t = Program of function_definition [@@deriving show]

let print_unary_operator = function
| Complement -> "~"
| Negate -> "-"

let rec pretty_print_exp = function
  | Constant i -> "Constant(" ^ string_of_int i ^ ")"
  | Unary (op, exp) -> "Unary(" ^ print_unary_operator op ^"," ^ pretty_print_exp exp ^ ")"
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
