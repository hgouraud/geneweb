open Geneweb
open OUnit2
open NotesLinks

let pp_wiki_link = function
  | WLpage (a,b,c,d,e) ->
    "WLpage " ^
    [%show: int * (string list * string) * string * string * string]
      (a,b,c,d,e)
  | WLperson (a,b,c,d) ->
    "WLperson" ^
    [%show: int * (string * string * int) * string * string option]
      (a,Obj.magic b,c,d)
  | WLwizard (a,b,c) ->
    "WLwizard" ^
    [%show: int * string * string]
      (a,b,c)
  | WLnone -> "WLnone"

let suite =
  [ "wikitext" >:::
    [ "misc_notes_link" >:: begin fun _ ->
          let test exp inp =
            let len = String.length inp in
            let rec loop acc i =
              if i = len then List.rev acc
              else match misc_notes_link inp i with
                | WLpage (j,_,_,_,_)
                | WLperson (j, _, _, _)
                | WLwizard (j, _, _) as x ->
                  loop (x :: acc) j
                | WLnone ->
                  match acc with
                  | [] ->
                    loop (WLnone :: acc) (i + 1)
                  | hd :: _ ->
                    if hd <> WLnone then loop (WLnone :: acc) (i + 1)
                    else loop acc (i + 1)
            in
            let printer list =
              "[" ^ String.concat "; " (List.map pp_wiki_link list) ^ "]"
            in
            assert_equal ~printer exp (loop [] 0)
          in
          test
            [ WLpage (13, ([], "aaa"), "aaa", "", "bbb")
            ; WLnone
            ; WLperson(26, ("ccc", "ddd", 0), "ccc ddd", None)
            ; WLnone
            ]
            "[[[aaa/bbb]]], [[ccc/ddd]], http://site.com/eee#fff"
        ; test
            [ WLnone
            ; WLperson (12, ("aaa", "bbb", 0), "aaa bbb", None)
            ; WLnone
            ; WLperson (25, ("ccc", "ddd", 0), "ccc ddd", None)
            ; WLnone
            ]
            "[[[aaa/bbb]], [[ccc/ddd]], http://site.com/eee#fff"
        ; test [WLnone] "[[aaa]]"
        ; test [WLperson (11, ("aaa", "bbb", 0), "aaa bbb", None)] "[[aaa/bbb]]"
        ; test [WLperson (19, ("aaa", "bbb", 0), "aaa bbb", Some "comment")]
          "[[aaa/bbb;comment]]"
        ; test [WLperson (13, ("aaa", "bbb", 1), "aaa bbb", None)] "[[aaa/bbb/1]]"
        ; test [WLperson (16, ("aaa", "bbb", 0), "text", None)] "[[aaa/bbb/text]]"
        ; test [WLperson (18, ("aaa", "bbb", 1), "text", None)] "[[aaa/bbb/1/text]]"
        ; test [WLperson (26, ("aaa", "bbb", 1), "text", Some "comment")]
          "[[aaa/bbb/1/text;comment]]"
        ; test [ WLnone ] "[[[aaa/"
        ; test [ WLnone ] "[[[]]]"
        ; test [ WLnone ] "[[[w"
        ; test [ WLnone ] "[[]]"
        ; test [ WLnone ] "[[w"
        end
    ]
  ]
