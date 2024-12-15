type t = Token_type.t list ref

exception End_of_stream

let take_token tokens =
  match !tokens with
  | [] -> raise End_of_stream
  | hd :: tail ->
      tokens := tail;
      hd

(* try Stream.next tokens with Stream.Failure -> raise End_of_stream *)

(* let peek tokens = match Stream.peek tokens with Some t -> t | None -> raise
   End_of_stream *)
let peek tokens = match !tokens with [] -> raise End_of_stream | hd :: _ -> hd

(* let is_empty tokens = try Stream.empty tokens; true with Stream.Failure ->
   false *)
let is_empty tokens = match !tokens with [] -> true | _ -> false
let of_list list = ref list
