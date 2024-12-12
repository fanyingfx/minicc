type t = Token_type.t Stream.t

exception End_of_stream

let take_token tokens =
  try Stream.next tokens with
  | Stream.Failure -> raise End_of_stream
;;

let peek tokens =
  match Stream.peek tokens with
  | Some t -> t
  | None -> raise End_of_stream
;;

let is_empty tokens =
  try
    Stream.empty tokens;
    true
  with
  | Stream.Failure -> false
;;

let of_list = Stream.of_list
