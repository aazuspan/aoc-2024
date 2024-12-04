open Solutions.Day3

let test_input = {|
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
|}

let test_input2 = {|
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
|}



let%expect_test _ =
  let result = part1 test_input in
  print_string @@ [%show: int ] result;
  [%expect {| 161 |}]

let%expect_test _ =
  let result = part2 test_input2 in
  print_string @@ [%show: int ] result;
  [%expect {| 48 |}]
