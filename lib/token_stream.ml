type t = Token_type.t Seq.t ref

exception End_of_stream

let take_token tokens =
  match Seq.uncons !tokens with
  | None -> raise End_of_stream
  | Some (hd, tail) ->
      tokens := tail;
      hd

let peek tokens =
  match Seq.uncons !tokens with
  | None -> raise End_of_stream
  | Some (hd, _) -> hd

let is_empty tokens = Seq.is_empty !tokens
let of_list list = ref (List.to_seq list)
