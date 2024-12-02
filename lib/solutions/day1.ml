(** [tuple_of_line] is the tuple (x * y) parsed from line "x   y" *)
let tuple_of_line s =
  let parts = String.split_on_char ' ' s in
  let nonempty = List.filter (fun x -> x <> "") parts in
  match nonempty with
  | [ x; y ] -> (int_of_string x, int_of_string y)
  | _ -> failwith "Invalid input"

(** [parsed_columns] is the pair of space-delimited columns parsed from [data] *)
let parsed_columns data =
  let lines = data |> Util.String.nonempty_lines in
  let pairs = List.map tuple_of_line lines in
  List.split pairs

let part1 data =
  let xs, ys = parsed_columns data in
  let sorted_xs = List.sort compare xs in
  let sorted_ys = List.sort compare ys in
  let distances = List.map2 (fun x y -> abs (x - y)) sorted_xs sorted_ys in
  Util.List.sum distances

let part2 data =
  let xs, ys = parsed_columns data in
  let similarities = List.map (fun x -> Util.List.count_in_list x ys * x) xs in
  Util.List.sum similarities
