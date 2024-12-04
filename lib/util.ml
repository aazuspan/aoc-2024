module IO = struct
  (** [string_of_file path] is the string contents of file at [path]. *)
  let string_of_file path = In_channel.with_open_bin path In_channel.input_all

  (** [lines_of_file path] is the list of string lines of file at [path]. *)
  let lines_of_file path = string_of_file path |> String.split_on_char '\n'
end

module List = struct
  include Stdlib.List

  (** [count_in_list x l] is the number of occurences of [x] in [l] *)
  let count_in_list x l = List.filter (fun y -> y = x) l |> List.length

  let sum l = List.fold_left ( + ) 0 l

  (** [pairs l] is all neighboring element pairs in [l] *)
  let rec pairs l =
    match l with
    | [] | [ _ ] -> []
    | h1 :: h2 :: t -> (h1, h2) :: pairs (h2 :: t)
end

module String = struct
  include Stdlib.String

  let split_lines s = String.split_on_char '\n' s

  (** [nonempty_lines s] is the list of all lines in [s] that aren't blank *)
  let nonempty_lines s = split_lines s |> List.filter (fun x -> x <> "")
end

module Sign = struct
  type sign = Positive | Negative | Zero

  let sign_of_int = function
    | x when x < 0 -> Negative
    | 0 -> Zero
    | _ -> Positive

  let sign_of_float = function
    | x when x < 0. -> Negative
    | 0. -> Zero
    | _ -> Positive
end

module Regex = struct
  (** [find_all_matches p s] returns all substrings matching pattern [p] in [s] *)
  let find_all_matches p s =
    let rec aux acc i =
      try
        let start = Str.search_forward p s i in
        let substr = Str.matched_string s in
        aux (substr :: acc) (start + String.length substr)
      with Not_found -> acc
    in
    aux [] 0

  (** [find_all_matches_idx p s] returns (idx, substring) for all substrings matching pattern [p] in [s] *)
  let find_all_matches_idx p s =
    let rec aux acc i =
      try
        let start = Str.search_forward p s i in
        let substr = Str.matched_string s in
        aux ((start, substr) :: acc) (start + String.length substr)
      with Not_found -> acc
    in
    aux [] 0
end
