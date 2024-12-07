open Solutions.Day4

let test_input =
  {|
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
|}

let test_input2 =
  {|
.M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
..........
|}

let%expect_test _ =
  let result = part1 test_input in
  print_string @@ [%show: int] result;
  [%expect {| 18 |}]

let%expect_test _ =
  let result = part2 test_input2 in
  print_string @@ [%show: int] result;
  [%expect {| 9 |}]
