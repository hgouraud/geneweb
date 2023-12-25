(* $Id: ansel.ml,v 5.3 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

let no_accent =
  function
    'à' | 'á' | 'â' | 'ã' | 'ä' | 'å' -> 'a'
  | 'ç' -> 'c'
  | 'è' | 'é' | 'ê' | 'ë' -> 'e'
  | 'ì' | 'í' | 'î' | 'ï' -> 'i'
  | 'ñ' -> 'n'
  | 'ò' | 'ó' | 'ô' | 'õ' | 'ö' | 'ø' -> 'o'
  | 'ù' | 'ú' | 'û' | 'ü' -> 'u'
  | 'ý' | 'ÿ' -> 'y'
  | 'À' | 'Á' | 'Â' | 'Ã' | 'Ä' | 'Å' -> 'A'
  | 'Ç' -> 'C'
  | 'È' | 'É' | 'Ê' | 'Ë' -> 'E'
  | 'Ì' | 'Í' | 'Î' | 'Ï' -> 'I'
  | 'Ñ' -> 'N'
  | 'Ò' | 'Ó' | 'Ô' | 'Õ' | 'Ö' | 'Ø' -> 'O'
  | 'Ù' | 'Ú' | 'Û' | 'Ü' -> 'U'
  | 'Ý' -> 'Y'
  | c -> c

let accent_code =
  function
    'À' | 'È' | 'Ì' | 'Ò' | 'Ù' | 'à' | 'è' | 'ì' | 'ò' | 'ù' -> 255
  | 'Á' | 'É' | 'Í' | 'Ó' | 'Ú' | 'Ý' | 'á' | 'é' | 'í' | 'ó' | 'ú' | 'ý' ->
      226
  | 'Â' | 'Ê' | 'Î' | 'Ô' | 'Û' | 'â' | 'ê' | 'î' | 'ô' | 'û' -> 227
  | 'Ã' | 'Ñ' | 'Õ' | 'ã' | 'ñ' | 'õ' -> 228
  | 'Ä' | 'Ë' | 'Ï' | 'Ö' | 'Ü' | 'ä' | 'ë' | 'ï' | 'ö' | 'ü' | 'ÿ' -> 232
  | 'Å' | 'å' -> 234
  | 'Ç' | 'ç' -> 240
  | 'Ø' -> 162
  | 'ø' -> 178
  | 'ß' -> 207
  | _ -> 0

let of_iso_8859_1 s =
  let (len, identical) =
    let rec loop i len identical =
      if i = String.length s then len, identical
      else
        match s.[i] with
          'À'..'Å' | 'Ç'..'Ï' | 'Ñ'..'Ö' | 'Ù'..'Ý' | 'à'..'å' | 'ç'..'ï' |
          'ñ'..'ö' | 'ù'..'ý' | 'ÿ' ->
            loop (i + 1) (len + 2) false
        | 'Ø' | 'ø' | 'ß' -> loop (i + 1) (len + 1) false
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
    'a' -> 'à'
  | 'e' -> 'è'
  | 'i' -> 'ì'
  | 'o' -> 'ò'
  | 'u' -> 'ù'
  | 'A' -> 'À'
  | 'E' -> 'È'
  | 'I' -> 'Ì'
  | 'O' -> 'Ò'
  | 'U' -> 'Ù'
  | x -> x

let acute =
  function
    'a' -> 'á'
  | 'e' -> 'é'
  | 'i' -> 'í'
  | 'o' -> 'ó'
  | 'u' -> 'ú'
  | 'y' -> 'ý'
  | 'A' -> 'Á'
  | 'E' -> 'É'
  | 'I' -> 'Í'
  | 'O' -> 'Ó'
  | 'U' -> 'Ú'
  | 'Y' -> 'Ý'
  | x -> x

let circum =
  function
    'a' -> 'â'
  | 'e' -> 'ê'
  | 'i' -> 'î'
  | 'o' -> 'ô'
  | 'u' -> 'û'
  | 'A' -> 'Â'
  | 'E' -> 'Ê'
  | 'I' -> 'Î'
  | 'O' -> 'Ô'
  | 'U' -> 'Û'
  | x -> x

let uml =
  function
    'a' -> 'ä'
  | 'e' -> 'ë'
  | 'i' -> 'ï'
  | 'o' -> 'ö'
  | 'u' -> 'ü'
  | 'y' -> 'ÿ'
  | 'A' -> 'Ä'
  | 'E' -> 'Ë'
  | 'I' -> 'Ï'
  | 'O' -> 'Ö'
  | 'U' -> 'Ü'
  | x -> x

let circle =
  function
    'a' -> 'å'
  | 'A' -> 'Å'
  | x -> x

let tilde =
  function
    'a' -> 'ã'
  | 'n' -> 'ñ'
  | 'o' -> 'õ'
  | 'A' -> 'Ã'
  | 'N' -> 'Ñ'
  | 'O' -> 'Õ'
  | x -> x

let cedil =
  function
    'c' -> 'ç'
  | 'C' -> 'Ç'
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
            162 -> 'Ø', i
          | 178 -> 'ø', i
          | 207 -> 'ß', i
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
