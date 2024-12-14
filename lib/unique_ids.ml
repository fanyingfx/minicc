let counter = ref 0

let make_temporary () =
  let n = !counter in
  counter := n + 1;
  "tmp." ^ Int.to_string n

let make_label prefix =
  let n = !counter in
  counter := n + 1;
  prefix ^ "." ^ Int.to_string n

let make_name_temporary = make_label
