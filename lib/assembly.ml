type operand =
  | Imm of int
  | Reg of reg
  | Pseudo of string
  | Stack of int
[@@deriving show]

and reg =
  | AX
  | DX
  | R10
  | R11
[@@deriving show]

type unary_operator =
  | Neg
  | Not
[@@deriving show]
type binary_operator = 
| Add| Sub | Mult
[@@deriving show]

type instruction =
  | Mov of operand * operand
  | Unary of unary_operator * operand
  | Binary of {op:binary_operator ;src:operand ;dst:operand}
  | Idiv of operand
  | Cdq
  | AllocateStack of int
  | Ret
[@@deriving show]

type function_definition =
  | Function of
      { name : string
      ; instructions : instruction list
      }
[@@deriving show]

type t = Program of function_definition [@@deriving show]
