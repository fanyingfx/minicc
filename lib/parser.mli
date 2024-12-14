(** Functions to parse individual symbols. These are not accessed by the rest of
    the compiler and are exported only for testing. *)
module Private : sig
  val parse_id : Token_stream.t -> string
  val parse_int : Token_stream.t -> Ast.exp
  val parse_exp : int -> Token_stream.t -> Ast.exp
  val parse_statement : Token_stream.t -> Ast.statement
  val parse_block_item: Token_stream.t -> Ast.block_item
  val parse_function_definition : Token_stream.t -> Ast.function_definition
  val parse_program : Token_stream.t -> Ast.t
end

exception ParseError of string
(** Raised when we hit an unexpected token or end of file *)

val parse : Token_type.t list -> Ast.t
(** Convert a list of tokens to an AST
    @raise ParseError if the program is syntactically invalid. *)
