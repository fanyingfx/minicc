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
| Hyphen
| DoubleHyphen
| Tilde
| Semicolon
| EOF
[@@deriving show, eq]