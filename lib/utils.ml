module ListUtil = struct
  let max cmp l = List.(hd (rev (sort cmp l)))

  let rec take_drop n = function
    | h :: t when n > 0 ->
        let l1, l2 = take_drop (n - 1) t in
        (h :: l1, l2)
    | l -> ([], l)
end

module StringUtil = struct
  let drop n s = String.sub s n (String.length s - n)
end
(* We're assuming that n > 0 *)
let round_away_from_zero n = function
  | x when x mod n = 0 -> x
  | x when x < 0 ->
      (* when x is negative and n is positive, x mod n will be negative *)
      x - n - (x mod n)
  | x -> x + n - (x mod n)


let read_file file = In_channel.with_open_bin file In_channel.input_all

let print_option_char msg ch =
  match ch with
  | Some c -> Printf.printf "%s <%c>\n" msg c
  | None -> Printf.printf "%s None" msg
