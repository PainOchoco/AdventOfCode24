let split_columns lines =
  let parse_line line =
    let parts = Str.split (Str.regexp "   ") line in
    match parts with
    | [a; b] -> (int_of_string a, int_of_string b)
    | _ -> failwith "Invalid input"
  in
  List.map parse_line lines

let part1 input =
  let calculate_distances column1 column2 =
    let sorted_col1 = List.sort compare column1 in
    let sorted_col2 = List.sort compare column2 in
    
    let distances = List.map2 (fun a b -> abs (a - b)) sorted_col1 sorted_col2 in
    
    List.fold_left (+) 0 distances
  in

  let process_input input =
    let columns = split_columns input in
    let column1, column2 = List.split columns in
    let total_distance = calculate_distances column1 column2 in
    Printf.printf "Day 01, Part 1: %d\n" total_distance
  in
  process_input input


let part2 input =
  let calculate_similarity column1 column2 =
    let sorted_col1 = List.sort compare column1 in
    let sorted_col2 = List.sort compare column2 in
    let similarity_score = List.fold_left (fun acc a -> acc + a * (List.length (List.filter ((=) a) sorted_col2))) 0 sorted_col1 in
    similarity_score
  in

  let process_input input =
    let columns = split_columns input in
    let column1, column2 = List.split columns in
    let total_similarity = calculate_similarity column1 column2 in
    Printf.printf "Day 01, Part 2: %d\n" total_similarity
  in
  process_input input

let solve input =
  part1 input;
  part2 input