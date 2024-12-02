open Solutions.Day2

let test_input = {|
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
|}


let%expect_test _ =
  let safe_count = part1 test_input in
  print_string @@ [%show: int] safe_count;
  [%expect {| 2 |}]

let%expect_test _ =
  let mostly_safe_count = part2 test_input in
  print_string @@ [%show: int] mostly_safe_count;
  [%expect {| 4 |}]