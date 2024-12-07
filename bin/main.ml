let () =
  print_endline "-- Day 1 --";
  Solutions.Day1.part1 (Util.IO.string_of_file "./data/day1.txt")
  |> Printf.printf "Part 1: %d\n";
  Solutions.Day1.part2 (Util.IO.string_of_file "./data/day1.txt")
  |> Printf.printf "Part 2: %d\n";

  print_endline "-- Day 2 --";
  Solutions.Day2.part1 (Util.IO.string_of_file "./data/day2.txt")
  |> Printf.printf "Part 1: %d\n";
  Solutions.Day2.part2 (Util.IO.string_of_file "./data/day2.txt")
  |> Printf.printf "Part 2: %d\n";

  print_endline "-- Day 3 --";
  Solutions.Day3.part1 (Util.IO.string_of_file "./data/day3.txt")
  |> Printf.printf "Part 1: %d\n";
  Solutions.Day3.part2 (Util.IO.string_of_file "./data/day3.txt")
  |> Printf.printf "Part 2: %d\n";

  print_endline "-- Day 4 --";
  Solutions.Day4.part1 (Util.IO.string_of_file "./data/day4.txt")
  |> Printf.printf "Part 1: %d\n";
  Solutions.Day4.part2 (Util.IO.string_of_file "./data/day4.txt")
  |> Printf.printf "Part 2: %d\n";
