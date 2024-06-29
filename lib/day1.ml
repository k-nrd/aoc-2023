open Base
(** The newly-improved calibration document consists of lines of text; each line originally contained a specific calibration value that the Elves now need to recover. On each line, the calibration value can be found by combining the first digit and the last digit (in that order) to form a single two-digit number.

    For example:

    1abc2
    pqr3stu8vwx
    a1b2c3d4e5f
    treb7uchet
    In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.

    Consider your entire calibration document. What is the sum of all of the calibration values? *)

let decode input =
  let seq =
    match input with
    | `String i -> String.to_sequence i
    | `In_channel i ->
        Sequence.unfold ~init:i ~f:(fun chan ->
            match In_channel.input_char chan with
            | Some char -> Some (char, chan)
            | None ->
                In_channel.close chan;
                None)
  in
  let value = ref 0 in
  let create_step () =
    let first_digit = ref '0' in
    let last_digit = ref ' ' in
    let step char =
      match char with
      | '\n' ->
          let first_str = Char.to_string !first_digit in
          let num =
            String.append first_str
              (if equal_char !last_digit ' ' then first_str
               else Char.to_string !last_digit)
            |> Int.of_string
          in
          value := !value + num;
          first_digit := '0';
          last_digit := ' '
      | c when Char.is_digit c ->
          if equal_char !first_digit '0' then first_digit := c
          else last_digit := c
      | _ -> ()
    in
    step
  in
  Sequence.iter ~f:(create_step ()) seq;
  !value
