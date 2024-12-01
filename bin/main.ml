let read_input file =
  let ic = open_in file in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_lines []


let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <day>\n" Sys.argv.(0)
  else
    let day = Sys.argv.(1) in
    let input_file = Printf.sprintf "input/day%02d.txt" (int_of_string day) in
    let input = read_input input_file in
    match day with
    | "1" -> AdventOfCode2024.Day01.solve input
    | _ -> Printf.printf "Day %s not implemented\n" day