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

  let range ?(start=0) end_excl = 
    let len = end_excl - start in
    List.init len (fun x -> x + start)

  (** [product l1 l2] is the Cartesian product of [l1] and [l2] *)
  let product l1 l2 =
    l1 |> List.map (fun e1 -> l2 |> List.map (fun e2 -> (e1, e2))) |> List.concat

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

module Matrix = struct
  type 'a t = 'a array array [@@deriving show]

  let height (t : 'a t) = Array.length t
  let width (t : 'a t) = Array.length t.(0)

  let columns t =
    let col_nums = List.range (width t) |> Array.of_list in
    Array.map (fun col -> Array.map (fun row -> row.(col)) t) col_nums

  (** [diagonal t offset] is the array of values on the main diagonal with x offset [t]*)
  let diagonal t offset =
    let h = height t in
    let w = width t in

    let max_dim = max h w in
    let idxs =
      List.range max_dim
      |> List.map (fun x -> (x, x + offset))
      |> List.filter (fun (x, y) ->
             match (x, y) with
             | y, _ when y < 0 || y >= h -> false
             | _, x when x < 0 || x >= w -> false
             | _ -> true)
    in

    Array.map (fun (y, x) -> t.(y).(x)) (Array.of_list idxs)

  let flip_ud t =
    List.range (height t)
    |> Array.of_list
    |> Array.map (fun i -> t.(height t - 1 - i))

  let flip_lr t =
    let col_nums = List.range (width t) |> Array.of_list in
    Array.map
      (fun row -> Array.map (fun col -> row.(width t - 1 - col)) col_nums)
      t

  (** [slice t x y w h] is the [w]x[h] chunk of matrix [t] starting at ([x], [y]) *)
  let slice t x y ~w ~h =
    let placeholder = t.(0).(0) in
    let block = Array.make_matrix h w placeholder in

    for row = 0 to h - 1 do
      for col = 0 to w - 1 do
        block.(row).(col) <- t.(row + y).(col + x)
      done
    done;
    block

end
