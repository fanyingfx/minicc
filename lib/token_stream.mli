type t

exception End_of_stream

val take_token : t -> Token_type.t
val peek : t -> Token_type.t
val is_empty : t -> bool
val of_list : Token_type.t list -> t
