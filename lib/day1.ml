open Base

let seq_from_input = function
  | `String i -> Sequence.of_list (String.split_lines i)
  | `In_channel i ->
      Sequence.unfold ~init:i ~f:(fun chan ->
          match In_channel.input_line chan with
          | Some line -> Some (line, chan)
          | None ->
              In_channel.close chan;
              None)

let digit_mappings =
  [
    ("one", "1");
    ("two", "2");
    ("three", "3");
    ("four", "4");
    ("five", "5");
    ("six", "6");
    ("seven", "7");
    ("eight", "8");
    ("nine", "9");
  ]

let digit_regex = Re.compile Re.digit

let substitute_digits str =
  let try_match substr =
    let m (prefix, _) = String.is_prefix substr ~prefix in
    m
  in
  let rec step s pos =
    if pos >= String.length s then s
    else
      let substr = String.subo s ~pos in
      match List.find digit_mappings ~f:(try_match substr) with
      | Some (word, digit) ->
          let prefix_len = String.length word in
          step
            (String.concat
               [
                 String.subo s ~len:pos;
                 digit;
                 String.subo s ~pos:(pos + prefix_len);
               ])
            (pos + 1)
      | None -> step s (pos + 1)
  in
  step str 0

let decode input =
  let step sum raw_line =
    let line = substitute_digits raw_line in
    match Re.all digit_regex line with
    | [] -> sum
    | x :: xs -> begin
        let first = Re.Group.get x 0 in
        let value =
          match List.last xs with
          | Some group ->
              let last = Re.Group.get group 0 in
              String.append first last
          | None -> String.append first first
        in
        sum + Int.of_string value
      end
  in
  Sequence.fold ~init:0 ~f:step (seq_from_input input)
