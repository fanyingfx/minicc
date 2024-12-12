let read_file file = In_channel.with_open_bin file In_channel.input_all

let print_option_char msg ch = 
  match ch with
  | Some c -> Printf.printf "%s <%c>\n" msg c
  | None -> Printf.printf "%s None" msg