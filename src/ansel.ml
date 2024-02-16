(* $Id: ansel.ml,v 5.3 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

let no_accent =
  function
    '�' | '�' | '�' | '�' | '�' | '�' -> 'a'
  | '�' -> 'c'
  | '�' | '�' | '�' | '�' -> 'e'
  | '�' | '�' | '�' | '�' -> 'i'
  | '�' -> 'n'
  | '�' | '�' | '�' | '�' | '�' | '�' -> 'o'
  | '�' | '�' | '�' | '�' -> 'u'
  | '�' | '�' -> 'y'
  | '�' | '�' | '�' | '�' | '�' | '�' -> 'A'
  | '�' -> 'C'
  | '�' | '�' | '�' | '�' -> 'E'
  | '�' | '�' | '�' | '�' -> 'I'
  | '�' -> 'N'
  | '�' | '�' | '�' | '�' | '�' | '�' -> 'O'
  | '�' | '�' | '�' | '�' -> 'U'
  | '�' -> 'Y'
  | c -> c

let accent_code =
  function
    '�' | '�' | '�' | '�' | '�' | '�' | '�' | '�' | '�' | '�' -> 255
  | '�' | '�' | '�' | '�' | '�' | '�' | '�' | '�' | '�' | '�' | '�' | '�' ->
      226
  | '�' | '�' | '�' | '�' | '�' | '�' | '�' | '�' | '�' | '�' -> 227
  | '�' | '�' | '�' | '�' | '�' | '�' -> 228
  | '�' | '�' | '�' | '�' | '�' | '�' | '�' | '�' | '�' | '�' | '�' -> 232
  | '�' | '�' -> 234
  | '�' | '�' -> 240
  | '�' -> 162
  | '�' -> 178
  | '�' -> 207
  | _ -> 0

let of_iso_8859_1 s =
  let (len, identical) =
    let rec loop i len identical =
      if i = String.length s then len, identical
      else
        match s.[i] with
          '�'..'�' | '�'..'�' | '�'..'�' | '�'..'�' | '�'..'�' | '�'..'�' |
          '�'..'�' | '�'..'�' | '�' ->
            loop (i + 1) (len + 2) false
        | '�' | '�' | '�' -> loop (i + 1) (len + 1) false
        | _ -> loop (i + 1) (len + 1) identical
    in
    loop 0 0 true
  in
  if identical then s
  else
    let s' = Bytes.create len in
    let rec loop i i' =
      if i = String.length s then Bytes.unsafe_to_string s'
      else
        let i' =
          let a = accent_code s.[i] in
          if a > 0 then
            begin
              Bytes.set s' i' (Char.chr a);
              let n = no_accent s.[i] in
              if n = s.[i] then i'
              else begin Bytes.set s' (i' + 1) n; i' + 1 end
            end
          else begin Bytes.set s' i' s.[i]; i' end
        in
        loop (i + 1) (i' + 1)
    in
    loop 0 0

let grave =
  function
    'a' -> '�'
  | 'e' -> '�'
  | 'i' -> '�'
  | 'o' -> '�'
  | 'u' -> '�'
  | 'A' -> '�'
  | 'E' -> '�'
  | 'I' -> '�'
  | 'O' -> '�'
  | 'U' -> '�'
  | x -> x

let acute =
  function
    'a' -> '�'
  | 'e' -> '�'
  | 'i' -> '�'
  | 'o' -> '�'
  | 'u' -> '�'
  | 'y' -> '�'
  | 'A' -> '�'
  | 'E' -> '�'
  | 'I' -> '�'
  | 'O' -> '�'
  | 'U' -> '�'
  | 'Y' -> '�'
  | x -> x

let circum =
  function
    'a' -> '�'
  | 'e' -> '�'
  | 'i' -> '�'
  | 'o' -> '�'
  | 'u' -> '�'
  | 'A' -> '�'
  | 'E' -> '�'
  | 'I' -> '�'
  | 'O' -> '�'
  | 'U' -> '�'
  | x -> x

let uml =
  function
    'a' -> '�'
  | 'e' -> '�'
  | 'i' -> '�'
  | 'o' -> '�'
  | 'u' -> '�'
  | 'y' -> '�'
  | 'A' -> '�'
  | 'E' -> '�'
  | 'I' -> '�'
  | 'O' -> '�'
  | 'U' -> '�'
  | x -> x

let circle =
  function
    'a' -> '�'
  | 'A' -> '�'
  | x -> x

let tilde =
  function
    'a' -> '�'
  | 'n' -> '�'
  | 'o' -> '�'
  | 'A' -> '�'
  | 'N' -> '�'
  | 'O' -> '�'
  | x -> x

let cedil =
  function
    'c' -> '�'
  | 'C' -> '�'
  | x -> x

let to_iso_8859_1 s =
  let (len, identical) =
    let rec loop i len identical =
      if i = String.length s then len, identical
      else if i = String.length s - 1 then len + 1, identical
      else
        match Char.code s.[i] with
          225 | 226 | 227 | 228 | 232 | 234 | 240 ->
            loop (i + 2) (len + 1) false
        | 162 | 178 | 207 -> loop (i + 1) (len + 1) false
        | _ -> loop (i + 1) (len + 1) identical
    in
    loop 0 0 true
  in
  if identical then s
  else
    let s' = Bytes.create len in
    let rec loop i i' =
      if i = String.length s then Bytes.unsafe_to_string s'
      else if i = String.length s - 1 then
        begin Bytes.set s' i' s.[i]; Bytes.unsafe_to_string s' end
      else
        let (c, i) =
          match Char.code s.[i] with
            162 -> '�', i
          | 178 -> '�', i
          | 207 -> '�', i
          | 225 -> grave s.[i+1], i + 1
          | 226 -> acute s.[i+1], i + 1
          | 227 -> circum s.[i+1], i + 1
          | 228 -> tilde s.[i+1], i + 1
          | 232 -> uml s.[i+1], i + 1
          | 234 -> circle s.[i+1], i + 1
          | 240 -> cedil s.[i+1], i + 1
          | _ -> s.[i], i
        in
        Bytes.set s' i' c; loop (i + 1) (i' + 1)
    in
    loop 0 0
