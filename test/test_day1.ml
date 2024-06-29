(* Build with `ocamlbuild -pkg alcotest simple.byte` *)

(* A module with functions to test *)
open Aoc_2023

(* The tests *)
let test_decode_example () =
  Alcotest.(check int)
    "correct result" 142
    (Day1.decode
       (`String "1abc2\n    pqr3stu8vwx\n    a1b2c3d4e5f\n    treb7uchet\n"))

let test_decode_example_2 () =
  Alcotest.(check int)
    "correct result" 281
    (Day1.decode
       (`String
         "two1nine\n\
          eightwothree\n\
          abcone2threexyz\n\
          xtwone3four\n\
          4nineightseven2\n\
          zoneight234\n\
          7pqrstsixteen"))

let test_decode () =
  Alcotest.(check int)
    "correct result" 54877
    (Day1.decode (`In_channel (In_channel.open_text "./day1-input.txt")))

(* Run it *)
let () =
  let open Alcotest in
  run "Day1"
    [
      ( "decode",
        [
          test_case "Decode example 1" `Quick test_decode_example;
          test_case "Decode example 2" `Quick test_decode_example_2;
          test_case "Decode input" `Quick test_decode;
        ] );
    ]
