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

let test_decode_segment_1 () =
  Alcotest.(check int)
    "correct result" 842
    (Day1.decode
       (`String
         "zlmlk1\n\
          vqjvxtc79mvdnktdsxcqc1sevenone\n\
          vsskdclbtmjmvrseven6\n\
          8jkfncbeight7seven8\n\
          six8dbfrsxp\n\
          2zpcbjdxcjfone68six\n\
          zqmzgfivethreefdnlhpeight8798\n\
          fivenineone6\n\
          6sixzvdsprdqlftwonine\n\
          lqztrmztwo8dg\n\
          four6onerv2pfhm\n\
          plvzrs5\n\
          5282gdnc918\n\
          pskjsrchjpxoneonenine96fivefour\n\
          fivefour2hhtprpjndm4\n\
          6qbdcfdjsd1lmldklflteight\n"))

let test_decode_segment_2 () =
  Alcotest.(check int)
    "correct result" 820
    (Day1.decode
       (`String
         "5fiveeightl8veight1pxfptklnhj\n\
          npllktfive45nhvqjcjgpxx\n\
          hbfr9mm\n\
          onerbfkf4threeone\n\
          7eightone\n\
          eight2kmjlsix8one61\n\
          rvrnrdrninenine3zq6jqsr\n\
          lztlntnsevenpplkhkftq1\n\
          tfrrjmcvtbmktnxtxkkrcctmc33four5gfqpcjreight\n\
          33rgcjxfsfqsvxxbxcnrjfndrrfmrtk\n\
          3627837xhhb8\n\
          bmjhkkn4pgf\n\
          qkrsvjclp23\n\
          5fourzllbmcgkxsevengkrzkpvcmvgtxlrv6\n\
          fivetczxxvjrrqfive1sevennvj6one3\n"))

(* Run it *)
let () =
  let open Alcotest in
  run "Day1"
    [
      ( "decode",
        [
          test_case "Decode example 1" `Quick test_decode_example;
          test_case "Decode example 2" `Quick test_decode_example_2;
          test_case "Decode segment 1" `Quick test_decode_segment_1;
          test_case "Decode segment 2" `Quick test_decode_segment_2;
          test_case "Decode input" `Quick test_decode;
        ] );
    ]
