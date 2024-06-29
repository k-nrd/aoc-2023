(* Build with `ocamlbuild -pkg alcotest simple.byte` *)

(* A module with functions to test *)
open Aoc_2023

(* The tests *)
let test_decode_example () =
  Alcotest.(check int)
    "correct result" 142
    (Day1.decode
       (`String "1abc2\n    pqr3stu8vwx\n    a1b2c3d4e5f\n    treb7uchet\n"))

let test_decode () =
  Alcotest.(check int)
    "correct result" 54697
    (Day1.decode (`In_channel (In_channel.open_text "./day1-input.txt")))

(* Run it *)
let () =
  let open Alcotest in
  run "Day1"
    [
      ( "decode",
        [
          test_case "Decode example" `Quick test_decode_example;
          test_case "Decode input" `Quick test_decode;
        ] );
    ]
