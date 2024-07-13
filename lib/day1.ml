open Base

let digit_strs =
  [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]

let digits = [ "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9" ]
let digit_mapping = List.zip_exn digit_strs digits

let seq_from_input = function
  | `String s -> Sequence.of_list (String.split_lines s)
  | `In_channel ic ->
      let next_line () =
        match In_channel.input_line ic with
        | Some line -> Some (line, ())
        | None ->
            In_channel.close ic;
            None
      in
      Sequence.unfold ~init:() ~f:next_line

let extract_digits str =
  let is_digit_substring whole (digit_str, _digit) =
    String.is_prefix whole ~prefix:digit_str
  in
  let step idx acc chr =
    let maybe_digit = String.of_char chr in
    if List.mem digits maybe_digit ~equal:equal_string then maybe_digit :: acc
    else
      let remaining_str = String.subo str ~pos:idx in
      match List.find digit_mapping ~f:(is_digit_substring remaining_str) with
      | Some (_digit_str, digit) -> digit :: acc
      | None -> acc
  in
  str |> String.foldi ~init:[] ~f:step |> List.rev

let decode input =
  let step sum line =
    match extract_digits line with
    | [] -> sum
    | first :: xs ->
        let value =
          match List.last xs with
          | Some last -> String.append first last
          | None -> String.append first first
        in
        sum + Int.of_string value
  in
  Sequence.fold ~init:0 ~f:step (seq_from_input input)
