let split_on_comma str =
  String.split_on_char ',' str |> List.map String.trim