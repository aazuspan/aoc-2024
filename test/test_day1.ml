open Solutions.Day1

let test_input = {|
  3   4
  4   3
  2   5
  1   3
  3   9
  3   3
|}

let%expect_test _ =
  part1 test_input |> print_int;
  [%expect {| 11 |}]

let%expect_test _ =
  part2 test_input |> print_int;
  [%expect {| 31 |}]
