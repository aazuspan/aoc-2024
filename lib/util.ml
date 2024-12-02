module IO = struct
  (** [string_of_file] is the string contents of file at [path]. *)
  let string_of_file path = In_channel.with_open_bin path In_channel.input_all

  (** [lines_of_file] is the list of string lines of file at [path]. *)
  let lines_of_file path = string_of_file path |> String.split_on_char '\n'
end

module List = struct
  include Stdlib.List

  (** [count_in_list] is the number of occurences of [x] in [l] *)
  let count_in_list x l = List.filter (fun y -> y = x) l |> List.length

  let sum l = List.fold_left ( + ) 0 l

  (** [pairs] is all neighboring element pairs in [l] *)
  let rec pairs l =
    match l with
    | [] | [ _ ] -> []
    | h1 :: h2 :: t -> (h1, h2) :: pairs (h2 :: t)
end

module String = struct
  include Stdlib.String

  let split_lines s = String.split_on_char '\n' s

  (** [nonempty_lines] is the list of all lines in [s] that aren't blank *)
  let nonempty_lines s = split_lines s |> List.filter (fun x -> x <> "")
end

module Sign = struct
  type sign = Positive | Negative | Zero
  let sign_of_int = function x when x < 0 -> Negative | 0 -> Zero | _ -> Positive
  let sign_of_float = function x when x < 0. -> Negative | 0. -> Zero | _ -> Positive
end