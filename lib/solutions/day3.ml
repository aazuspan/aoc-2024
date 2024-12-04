(** [find_all_matches_idx p s] returns all substrings and their index matching pattern [p] in [s] *)
let find_all_matches_idx p s =
  let rec aux acc i =
    try
      let start = Str.search_forward p s i in
      let substr = Str.matched_string s in
      aux ((start, substr) :: acc) (start + String.length substr)
    with Not_found -> acc
  in
  aux [] 0

(** [parse_mul s] parses the operands x and y from [s] in the form  op(x, y) *)
let parse_mul s =
  List.map int_of_string @@ Util.Regex.find_all_matches (Str.regexp "[0-9]+") s

(** [op_is_enabled start data] checks for "do" and "don't" preceding [start] in [data] *)
let op_is_enabled start data =
  let prev_do =
    try Str.search_backward (Str.regexp "do") data start with Not_found -> -1
  in

  let prev_dont =
    try Str.search_backward (Str.regexp "don't") data start
    with Not_found -> -1
  in

  match (prev_do, prev_dont) with
  (* "do" occured last or "don't" hasn't occured *)
  | x, y when x > y || y = -1 -> true
  | _ -> false

let part1 data =
  let regex = Str.regexp "mul(\\(-?[0-9]+\\),\\(-?[0-9]+\\))" in
  let ops = Util.Regex.find_all_matches regex data in
  let nums = List.map parse_mul ops in
  let prods = List.map (List.fold_left ( * ) 1) nums in
  Util.List.sum prods

let part2 data =
  let regex = Str.regexp "mul(\\(-?[0-9]+\\),\\(-?[0-9]+\\))" in
  let ops = Util.Regex.find_all_matches_idx regex data in
  let enabled_ops = List.filter (fun (i, _) -> op_is_enabled i data) ops in
  let nums = List.map (fun (_, op) -> parse_mul op) enabled_ops in
  let prods = List.map (List.fold_left ( * ) 1) nums in
  Util.List.sum prods
