(** [is_gradual] is true if [x] is between 1 and 3, inclusive *)
let is_gradual x = x <> 0 && abs x < 4

(** [is_gradual_monotonic] is true if values in [l] change slowly and monotonically *)
let rec is_gradual_monotonic l =
  match l with
  | [] -> true
  | [ x ] -> is_gradual x
  | [ x; y ] ->
      Util.Sign.sign_of_int x = Util.Sign.sign_of_int y
      && is_gradual x && is_gradual y
  | x :: y :: t -> is_gradual_monotonic [ x; y ] && is_gradual_monotonic (y :: t)

(** [level_is_safe] is true if all distances in [l] are gradual and monotonic *)
let level_is_safe l =
  l |> Util.List.pairs
  |> List.map (fun (x, y) -> int_of_string y - int_of_string x)
  |> is_gradual_monotonic

(** [level_is_mostly_safe] is true if removing any element from [l] makes it safe *)
let level_is_mostly_safe l =
  match l with
  | l when level_is_safe l -> true
  (* Inefficient O(n^2) solution looking at all possible permutations *)
  | l ->
      let perms = List.mapi (fun i _ -> List.filteri (fun j _ -> i <> j) l) l in
      List.filter level_is_safe perms |> List.length > 0

let part1 data =
  let reports = Util.String.nonempty_lines data in
  let levels = List.map (String.split_on_char ' ') reports in
  List.filter level_is_safe levels |> List.length

let part2 data =
  let reports = Util.String.nonempty_lines data in
  let levels = List.map (String.split_on_char ' ') reports in
  List.filter level_is_mostly_safe levels |> List.length
