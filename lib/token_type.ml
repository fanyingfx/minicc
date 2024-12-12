type t = 
| Identifier of string
| Constant of int
| KWInt
| KWVoid
| KWReturn
| LParen
| RParen
| LBrace
| RBrace
| Semicolon
| EOF
[@@deriving show, eq]