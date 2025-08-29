open Alcotest

(* TODO Fmt *)

let lower () =
  (check string) "1" "abcdef" (Name.lower "AbCdef");
  (check string) "2" "abc'def" (Name.lower "AbÇ’def");
  ()

let crush () =
  (check string) "1" "b" (Name.crush "b");
  (check string) "2" "ebcsgft" (Name.crush "abcsgffuit");
  (check string) "3" "bcsgft" (Name.crush "bcsgffuit");
  (check string) "4" "bcsgft" (Name.crush "bcsg ffuit");
  (check string) "5" "bcsgcft" (Name.crush "bksgqffuit");
  (check string) "6" "bcsgcfft" (Name.crush "bksgqffuiphth");
  (check string) "7" "bcsgcfft" (Name.crush "bbksgqffuiphth");
  (check string) "8" "bcsgcfft" (Name.lower "BBKSGQFFUIPHTH" |> Name.crush);
  (check string) "81" "o brien" (Name.lower "o brien");
  (check string) "82" "o brien" (Name.lower "o'brien");
  (check string) "83" "o’brien" (Name.lower "o’brien");
  (check string) "9" "e'brn" (Name.crush "o'brien");
  (check string) "9" "e’brn" (Name.crush "o’brien");
  (check string) "9" "ebrn" (Name.crush "o brien");
  (check string) "10" "-brn" (Name.crush "-o brien");
  ()

let chars () =
  (check string) "1" "’" (Utf8.char "ab’" 2);
  ()
  
let v =
  [
    ("Lower", [ test_case "Lower" `Quick lower ]);
    ("Crush", [ test_case "Crush" `Quick crush ]);
    ("Chars", [ test_case "Chars" `Quick chars ]);
  ]
