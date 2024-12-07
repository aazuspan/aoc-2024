(** [matrix_of_lines l] is the 2D matrix of chars from string lines [l]*)
let matrix_of_lines (l : string list) =
  let height = List.length l in
  let width = List.hd l |> String.length in
  let letters = Array.make_matrix width height '.' in

  for row = 0 to width - 1 do
    let line = List.nth l row in
    for col = 0 to height - 1 do
      letters.(row).(col) <- line.[col]
    done
  done;
  letters

(** [all_diagonals m] are the main and anti diagonal lists from matrix [m]*)
let all_diagonals m =
  let w = Util.Matrix.width m in
  let h = Util.Matrix.height m in
  let max_dim = max w h in

  let diag_offsets = Util.List.range ~start:(-max_dim + 1) w in
  let main_diags = List.map (Util.Matrix.diagonal m) diag_offsets in
  let anti_diags =
    List.map (Util.Matrix.diagonal (Util.Matrix.flip_lr m)) diag_offsets
  in
  List.concat
    [ List.map Array.to_list main_diags; List.map Array.to_list anti_diags ]

(** [all_lines m] are all possible row, column, and diagonal lists from matrix [m]*)
let all_lines m =
  let rows = Array.to_list m |> List.map Array.to_list in
  let cols = Util.Matrix.columns m |> Array.to_list |> List.map Array.to_list in
  let diags = all_diagonals m in

  let all_lines = List.concat [ rows; cols; diags ] in
  all_lines

(** [get_blocks t w h] returns all [w]x[h] blocks in matrix [t] *)
let get_blocks t ~w ~h =
  let h_strides = Util.List.range (Util.Matrix.width t - w + 1) in
  let v_strides = Util.List.range (Util.Matrix.height t - h + 1) in
  let indexes = Util.List.product h_strides v_strides in
  List.map (fun (x, y) -> Util.Matrix.slice t x y ~w ~h) indexes

(** [count_xmas l] is the count of contiguous XMAS chars in [l] *)
let count_xmas l =
  let rec aux acc l =
    match l with
    | [] -> acc
    | [ 'X'; 'M'; 'A'; 'S' ] -> acc + 1
    | 'X' :: 'M' :: 'A' :: 'S' :: t -> aux (acc + 1) t
    | _ :: t -> aux acc t
  in
  aux 0 l

(** [contains_mas block] is true if diagonal MAS strings exist in 3x3 [block] *)
let contains_mas block =
  let mas = [| 'M'; 'A'; 'S' |] in
  let sam = [| 'S'; 'A'; 'M' |] in

  let main = Util.Matrix.diagonal block 0 in
  let anti = Util.Matrix.diagonal (Util.Matrix.flip_lr block) 0 in
  (main = mas || main = sam) && (anti = mas || anti = sam)

let part1 data =
  let m = Util.String.nonempty_lines data |> matrix_of_lines in
  let lines = all_lines m in
  let fwd_counts = List.map count_xmas lines in
  let rev_counts = List.map count_xmas (List.map List.rev lines) in
  Util.List.sum fwd_counts + Util.List.sum rev_counts

let part2 data =
  let m = Util.String.nonempty_lines data |> matrix_of_lines in
  let blocks = get_blocks m ~w:3 ~h:3 in
  let with_mas = List.filter contains_mas blocks in
  List.length with_mas
