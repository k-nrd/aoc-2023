open Base

let numbers =
  [
    ("one", (String.Search_pattern.create "one", "1"));
    ("two", (String.Search_pattern.create "two", "2"));
    ("three", (String.Search_pattern.create "three", "3"));
    ("four", (String.Search_pattern.create "four", "4"));
    ("five", (String.Search_pattern.create "five", "5"));
    ("six", (String.Search_pattern.create "six", "6"));
    ("seven", (String.Search_pattern.create "seven", "7"));
    ("eight", (String.Search_pattern.create "eight", "8"));
    ("nine", (String.Search_pattern.create "nine", "9"));
  ]

let rec peek_and_match line word current_match line_idx word_idx =
  if equal_string current_match word then Some word
  else if word_idx >= String.length word || line_idx >= String.length line then
    None
  else
    match (String.get word word_idx, String.get line line_idx) with
    | a, b when equal_char a b ->
        peek_and_match line word
          (String.append current_match (Char.to_string a))
          (line_idx + 1) (word_idx + 1)
    | _ -> None

let rec first_some = function
  | [] -> None
  | Some x :: _ -> Some x
  | None :: xs -> first_some xs

let peek_and_match_first line words current line_idx word_idx =
  first_some
    (List.map words ~f:(fun word ->
         peek_and_match line word current line_idx word_idx))

let seq_from_input = function
  | `String i -> Sequence.of_list (String.split_lines i)
  | `In_channel i ->
      Sequence.unfold ~init:i ~f:(fun chan ->
          match In_channel.input_line chan with
          | Some line -> Some (line, chan)
          | None ->
              In_channel.close chan;
              None)

let preprocess raw_line =
  let preprocess_char line char idx =
    let next_idx = idx + 1 in
    match char with
    | 'o' -> peek_and_match line "one" "o" next_idx 1
    | 't' -> peek_and_match_first line [ "two"; "three" ] "t" next_idx 1
    | 'f' -> peek_and_match_first line [ "four"; "five" ] "f" next_idx 1
    | 's' -> peek_and_match_first line [ "six"; "seven" ] "s" next_idx 1
    | 'e' -> peek_and_match line "eight" "e" next_idx 1
    | 'n' -> peek_and_match line "nine" "n" next_idx 1
    | _ -> None
  in
  let rec pp line idx =
    if idx >= String.length line then line
    else
      let char = String.get line idx in
      match preprocess_char line char idx with
      | Some word ->
          let pattern, digit =
            List.Assoc.find_exn ~equal:equal_string numbers word
          in
          pp
            (String.Search_pattern.replace_first pattern ~in_:line ~with_:digit)
            (idx + 1)
      | None -> pp line (idx + 1)
  in
  let line = pp raw_line 0 in
  Stdlib.print_endline line;
  line

let decode input =
  let value = ref 0 in
  let step raw_line =
    let line = preprocess raw_line in
    let first_digit = ref '0' in
    let last_digit = ref ' ' in
    let process_char char =
      match char with
      | c when Char.is_digit c ->
          if equal_char !first_digit '0' then first_digit := c
          else last_digit := c
      | _ -> ()
    in
    String.iter ~f:process_char line;
    let first_str = Char.to_string !first_digit in
    let num =
      String.append first_str
        (if equal_char !last_digit ' ' then first_str
         else Char.to_string !last_digit)
      |> Int.of_string
    in
    value := !value + num
  in
  Sequence.iter ~f:step (seq_from_input input);
  !value
