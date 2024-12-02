open Solutions.Day3

let test_input = {|
|}


let%expect_test _ =
  let result = part1 test_input in
  print_string @@ [%show: int] result;
  [%expect {| 0 |}]

let%expect_test _ =
  let result = part2 test_input in
  print_string @@ [%show: int] result;
  [%expect {| 0 |}]