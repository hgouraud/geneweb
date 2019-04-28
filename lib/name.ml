(* $Id: name.ml,v 5.12 2018-09-27 10:34:14 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

(* La liste des caractères interdits *)
let forbidden_char = [':'; '@'; '#'; '='; '$']

(* Name.lower *)

let unaccent_iso_8859_1 =
  function
  (*| 'à' | 'á' | 'â' | 'ã' | 'ä' | 'å' | 'æ' -> 'a'*)
  | '\xE0' | '\xE1' | '\xE2' | '\xE3' | '\xE4' | '\xE5' | '\xE6' -> 'a'
  (*| 'ç' -> 'c'*)
  | '\xE7' -> 'c'
  (*| 'è' | 'é' | 'ê' | 'ë' -> 'e'*)
  | '\xE8' | '\xE9' | '\xEA' | '\xEB' -> 'e'
  (*| 'ì' | 'í' | 'î' | 'ï' -> 'i'*)
  | '\xEC' | '\xED' | '\xEE' | '\xEF' -> 'i'
  (*| 'ð' -> 'd'*)
  | '\xF0' -> 'd'
  (*| 'ñ' -> 'n'*)
  | '\xF1' -> 'n'
  (*| 'ò' | 'ó' | 'ô' | 'õ' | 'ö' | 'ø' -> 'o'*)
  | '\xF2' | '\xF3' | '\xF4' | '\xF5' | '\xF6' | '\xF8' -> 'o'
  (*| 'ù' | 'ú' | 'û' | 'ü' -> 'u'*)
  | '\xF9' | '\xFA' | '\xFB' | '\xFC' -> 'u'
  (*| 'ý' | 'ÿ' -> 'y'*)
  | '\xFD' | '\xFF' -> 'y'
  (*| 'þ' -> 'p'*)
  | '\xFE' -> 'p'
  (*| 'ß' -> 's'*)
  | '\xDF' -> 's'
  | c -> c

let nbc c =
  if Char.code c < 0b10000000 then 1
  else if Char.code c < 0b11000000 then -1
  else if Char.code c < 0b11100000 then 2
  else if Char.code c < 0b11110000 then 3
  else if Char.code c < 0b11111000 then 4
  else if Char.code c < 0b11111100 then 5
  else if Char.code c < 0b11111110 then 6
  else -1

let unaccent_utf_8 ?(viet=false) ?(apostr=false) lower  s i =
  let f s = if lower then String.lowercase_ascii s else s in
  let nbc = nbc s.[i] in
  if nbc = 1 || nbc < 0 || i + nbc > String.length s then
    f (String.make 1 s.[i]), i + 1
  else
    let c = Char.code s.[i] in
    let s =
      match c with
        0xC2 -> f (String.make 1 s.[i+1])
      | 0xC3 ->
          begin match Char.code s.[i+1] with
            0x80 | 0x81 | 0x82 | 0x83 | 0x84 | 0x85 -> f "A"
          | 0x86 -> f "AE"
          | 0x87 -> f "C"
          | 0x88 | 0x89 | 0x8A | 0x8B -> f "E"
          | 0x8C | 0x8D | 0x8E | 0x8F -> f "I"
          | 0x90 -> f "D"
          | 0x91 -> f "N"
          | 0x92 | 0x93 | 0x94 | 0x95 | 0x96 | 0x98 -> f "O"
          | 0x99 | 0x9A | 0x9B | 0x9C -> f "U"
          | 0x9D -> f "Y"
          | 0x9E -> f "TH"
          | 0x9F -> "sz"
          | 0xA0 | 0xA1 | 0xA2 | 0xA3 | 0xA4 | 0xA5 -> "a"
          | 0xA6 -> "ae"
          | 0xA7 -> "c"
          | 0xA8 | 0xA9 | 0xAA | 0xAB -> "e"
          | 0xAC | 0xAD | 0xAE | 0xAF -> "i"
          | 0xB0 -> "d"
          | 0xB1 -> "n"
          | 0xB2 | 0xB3 | 0xB4 | 0xB5 | 0xB6 | 0xB8 -> "o"
          | 0xB9 | 0xBA | 0xBB | 0xBC -> "u"
          | 0xBD | 0xBF -> "y"
          | 0xBE -> "th"
          | _ ->
              (* Si le caractère est en dehors de la table ASCII,
                 alors on ignore le caratère. Cela peut se produire
                 si l'entrée est mauvaise, ex: JÃ©rÃÃ¶me /FOO/ *)
              try
                let c =
                  Char.lowercase_ascii (Char.chr (Char.code s.[i+1] + 0x40))
                in
                String.make 1 c
              with Invalid_argument _ -> ""
          end
      | 0xC4 ->
          begin match Char.code s.[i+1] with
            0x80 | 0x82 | 0x84 -> f "A"
          | 0x81 | 0x83 | 0x85 -> "a"
          | 0x86 | 0x88 | 0x8A | 0x8C -> f "C"
          | 0x87 | 0x89 | 0x8B | 0x8D -> "c"
          | 0x8E | 0x90 -> f "D"
          | 0x8F | 0x91 -> "d"
          | 0x92 | 0x94 | 0x96 | 0x98 | 0x9A -> f "E"
          | 0x93 | 0x95 | 0x97 | 0x99 | 0x9B -> "e"
          | 0x9C | 0x9E | 0xA0 | 0xA2 -> f "G"
          | 0x9D | 0x9F | 0xA1 | 0xA3 -> "g"
          | 0xA4 | 0xA6 -> f "H"
          | 0xA5 | 0xA7 -> "h"
          | 0xA8 | 0xAA | 0xAC | 0xAE | 0xB0 -> f "I"
          | 0xA9 | 0xAB | 0xAD | 0xAF | 0xB1 -> "i"
          | 0xB2 -> f "IJ"
          | 0xB3 -> "ij"
          | 0xB4 -> f "J"
          | 0xB5 -> "j"
          | 0xB6 -> f "K"
          | 0xB7 | 0xB8 -> "k"
          | 0xB9 | 0xBB | 0xBD | 0xBF -> f "L"
          | 0xBA | 0xBC | 0xBE -> "l"
          | _ -> String.sub s i nbc
          end
      | 0xC5 ->
          begin match Char.code s.[i+1] with
            0x80 | 0x82 -> "l"
          | 0x81 -> f "L"
          | 0x83 | 0x85 | 0x87 | 0x8A -> f "N"
          | 0x84 | 0x86 | 0x88 | 0x89 | 0x8B -> "n"
          | 0x8C | 0x8E | 0x90 -> f "O"
          | 0x8D | 0x8F | 0x91 -> "o"
          | 0x92 -> f "OE"
          | 0x93 -> "oe"
          | 0x94 | 0x96 | 0x98 -> f "R"
          | 0x95 | 0x97 | 0x99 -> "r"
          | 0x9A | 0x9C | 0x9E | 0xA0 -> f "S"
          | 0x9B | 0x9D | 0x9F | 0xA1 -> "s"
          | 0xA2 | 0xA4 | 0xA6 -> f "T"
          | 0xA3 | 0xA5 | 0xA7 -> "t"
          | 0xA8 | 0xAA | 0xAC | 0xAE | 0xB0 | 0xB2 -> f "U"
          | 0xA9 | 0xAB | 0xAD | 0xAF | 0xB1 | 0xB3 -> "u"
          | 0xB4 -> f "W"
          | 0xB5 -> "w"
          | 0xB6 | 0xB8 -> f "Y"
          | 0xB7 -> "y"
          | 0xB9 | 0xBB | 0xBD -> f "Z"
          | 0xBA | 0xBC | 0xBE -> "z"
          | _ -> String.sub s i nbc
          end
      | 0xC6 ->
          begin match Char.code s.[i+1] with
            0x86 | 0x9F | 0xA0 -> f "O"
          | 0x90 -> f "E"
          | 0x96 | 0x97 -> f "I"
          | 0xA1 -> "o"
          | 0xAF | 0xB1 -> f "U"
          | 0xB0 -> "u"
          | 0xB3 -> f "Y"
          | 0xB4 -> "y"
          | _ -> String.sub s i nbc
          end
      | 0xC7 ->
          begin match Char.code s.[i+1] with
            0x8D | 0x9E | 0xA0 | 0xBA -> f "A"
          | 0x8E | 0x9F | 0xA1 | 0xBB -> "a"
          | 0x8F -> f "I"
          | 0x90 -> "i"
          | 0x91 | 0xAA | 0xAC | 0xBE -> f "O"
          | 0x92 | 0xAB | 0xAD | 0xBF -> "o"
          | 0x93 | 0x95 | 0x97 | 0x99 | 0x9B -> f "U"
          | 0x94 | 0x96 | 0x98 | 0x9A | 0x9C -> "u"
          | 0xBC | 0xA2 -> f "AE"
          | 0xBD | 0xA3 -> "ae"
          | _ -> String.sub s i nbc
          end
      | 0xC8 ->
          begin match Char.code s.[i+1] with
            0x80 | 0x82 | 0xA6 | 0xBA -> f "A"
          | 0x81 | 0x83 | 0xA7 -> "a"
          | 0x84 | 0x86 | 0xA8 -> f "E"
          | 0x85 | 0x87 | 0xA9 -> "e"
          | 0x88 | 0x8A -> f "I"
          | 0x89 | 0x8B -> "i"
          | 0x8C | 0x8E | 0xAA | 0xAC | 0xAE | 0xB0 -> f "O"
          | 0x8D | 0x8F | 0xAB | 0xAD | 0xAF | 0xB1 -> "o"
          | 0x94 | 0x96 -> f "U"
          | 0x95 | 0x97 -> "u"
          | 0xA2 -> f "OU"
          | 0xA3 -> "ou"
          | 0xB2 -> f "Y"
          | 0xB3 -> "y"
          | _ -> String.sub s i nbc
          end
      | 0xC9 ->
          begin match Char.code s.[i+1] with
            0x84 -> f "U"
          | 0x86 -> f "E"
          | 0x87 | 0x9B -> "e"
          | 0x8E -> f "Y"
          | 0x8F -> "y"
          | 0x94 -> "o"
          | 0xA8 | 0xAE -> "i"
          | _ -> String.sub s i nbc
          end
      | 0xCA ->
          begin match Char.code s.[i+1] with
            0x84 -> "u"
          | _ -> String.sub s i nbc
          end
      | 0xCE ->
          (* Greek *)
          begin match Char.code s.[i+1] with
            0x86 -> f "A"
          | 0x88 -> f "E"
          | 0x89 -> f "H"
          | 0x8A -> f "I"
          | 0x8C -> f "O"
          | 0x8E -> f "Y"
          | 0x8F -> f "O"
          | 0x90 -> f "I"
          | 0x91 -> f "A"
          | 0x92 -> f "B"
          | 0x93 -> f "G"
          | 0x94 -> f "D"
          | 0x95 -> f "E"
          | 0x96 -> f "DZ"
          | 0x97 -> f "E"
          | 0x98 -> f "TH"
          | 0x99 -> f "I"
          | 0x9A -> f "K"
          | 0x9B -> f "L"
          | 0x9C -> f "M"
          | 0x9D -> f "N"
          | 0x9E -> f "X"
          | 0x9F -> f "O"
          | 0xA0 -> f "P"
          | 0xA1 -> f "R"
          | 0xA2 | 0xA3 -> f "S"
          | 0xA4 -> f "T"
          | 0xA5 -> f "U"
          | 0xA6 -> f "PH"
          | 0xA7 -> f "KH"
          | 0xA8 -> f "PS"
          | 0xA9 -> f "O"
          | 0xAA -> f "I"
          | 0xAB -> f "Y"
          | 0xAC -> "a"
          | 0xAD -> "e"
          | 0xAE -> "n"
          | 0xAF -> "i"
          | 0xB0 -> "a"
          | 0xB1 -> "a"
          | 0xB2 -> "b"
          | 0xB3 -> "g"
          | 0xB4 -> "d"
          | 0xB5 -> "e"
          | 0xB6 -> "dz"
          | 0xB7 -> "e"
          | 0xB8 -> "th"
          | 0xB9 -> "i"
          | 0xBA -> "k"
          | 0xBB -> "l"
          | 0xBC -> "m"
          | 0xBD -> "n"
          | 0xBE -> "x"
          | 0xBF -> "o"
          | _ -> String.sub s i nbc
          end
      | 0xCF ->
          begin match Char.code s.[i+1] with
            0x80 -> "p"
          | 0x81 -> "r"
          | 0x82 | 0x83 -> "s"
          | 0x84 -> "t"
          | 0x85 -> "u"
          | 0x86 -> "ph"
          | 0x87 -> "kh"
          | 0x88 -> "ps"
          | 0x89  | 0x8C | 0x8E -> "o"
          | 0x8A -> "i"
          | 0x8B | 0x8D -> "u"
          | _ -> String.sub s i nbc
          end
      | 0xD0 ->
          (* Cyrillic *)
          begin match Char.code s.[i+1] with
            0x81 -> f "E"
          | 0x86 -> f "I"
          | 0x90 -> f "A"
          | 0x91 -> f "B"
          | 0x92 -> f "V"
          | 0x93 -> f "G"
          | 0x94 -> f "D"
          | 0x95 -> f "E"
          | 0x96 -> f "J"
          | 0x97 -> f "Z"
          | 0x98 | 0x99 -> f "I"
          | 0x9A -> f "K"
          | 0x9B -> f "L"
          | 0x9C -> f "M"
          | 0x9D -> f "N"
          | 0x9E -> f "O"
          | 0x9F -> f "P"
          | 0xA0 -> f "R"
          | 0xA1 -> f "S"
          | 0xA2 -> f "T"
          | 0xA3 -> f "OU"
          | 0xA4 -> f "F"
          | 0xA5 -> f "KH"
          | 0xA6 -> f "TS"
          | 0xA7 -> f "TCH"
          | 0xA8 -> f "CH"
          | 0xA9 -> f "CHT"
          | 0xAB -> f "Y"
          | 0xAC -> f ""
          | 0xAD -> f "E"
          | 0xAE -> f "YOU"
          | 0xAF -> f "YA"
          | 0xB0 -> "a"
          | 0xB1 -> "b"
          | 0xB2 -> "v"
          | 0xB3 -> "g"
          | 0xB4 -> "d"
          | 0xB5 -> "e"
          | 0xB6 -> "j"
          | 0xB7 -> "z"
          | 0xB8 | 0xB9 -> "i"
          | 0xBA -> "k"
          | 0xBB -> "l"
          | 0xBC -> "m"
          | 0xBD -> "n"
          | 0xBE -> "o"
          | 0xBF -> "p"
          | _ -> String.sub s i nbc
          end
      | 0xD1 ->
          begin match Char.code s.[i+1] with
            0x80 -> "r"
          | 0x81 -> "s"
          | 0x82 -> "t"
          | 0x83 -> "ou"
          | 0x84 -> "f"
          | 0x85 -> "kh"
          | 0x86 -> "ts"
          | 0x87 -> "tch"
          | 0x88 -> "ch"
          | 0x89 -> "cht"
          | 0x8B -> "y"
          | 0x8C -> ""
          | 0x8D -> "e"
          | 0x8E -> "you"
          | 0x8F -> "ya"
          | 0x91 -> "e"
          | _ -> String.sub s i nbc
          end
      | 0xD3 ->
          begin match Char.code s.[i+1] with
            0x80 -> f "I"
          | 0x95 -> "ae"
          | _ -> String.sub s i nbc
          end
      | 0xD4 ->
          (* Armenian *)
          begin match Char.code s.[i+1] with
            0xB1 -> f "A"
          | 0xB2 -> f "B"
          | 0xB3 -> f "G"
          | 0xB4 -> f "D"
          | 0xB5 -> f "E"
          | 0xB6 -> f "Z"
          | 0xB7 -> f "E"
          | 0xB8 -> f "E"
          | 0xB9 -> f "T"
          | 0xBA -> f "Z"
          | 0xBB -> f "I"
          | 0xBC -> f "L"
          | 0xBD -> f "X"
          | 0xBE -> f "C"
          | 0xBF -> f "K"
          | _ -> String.sub s i nbc
          end
      | 0xD5 ->
          begin match Char.code s.[i+1] with
            0x80 -> f "H"
          | 0x81 -> f "J"
          | 0x82 -> f "L"
          | 0x83 -> f "C"
          | 0x84 -> f "M"
          | 0x85 -> f "Y"
          | 0x86 -> f "N"
          | 0x87 -> f "S"
          | 0x88 -> f "O"
          | 0x89 -> f "C"
          | 0x8A -> f "P"
          | 0x8B -> f "J"
          | 0x8C -> f "R"
          | 0x8D -> f "S"
          | 0x8E -> f "V"
          | 0x8F -> f "T"
          | 0x90 -> f "R"
          | 0x91 -> f "C"
          | 0x92 -> f "W"
          | 0x93 -> f "P"
          | 0x94 -> f "K"
          | 0x95 -> f "O"
          | 0x96 -> f "F"
          | 0xA1 -> "a"
          | 0xA2 -> "b"
          | 0xA3 -> "g"
          | 0xA4 -> "d"
          | 0xA5 -> "e"
          | 0xA6 -> "z"
          | 0xA7 -> "e"
          | 0xA8 -> "e"
          | 0xA9 -> "t"
          | 0xAA -> "z"
          | 0xAB -> "i"
          | 0xAC -> "l"
          | 0xAD -> "x"
          | 0xAE -> "c"
          | 0xAF -> "k"
          | 0xB0 -> "h"
          | 0xB1 -> "j"
          | 0xB2 -> "l"
          | 0xB3 -> "c"
          | 0xB4 -> "m"
          | 0xB5 -> "y"
          | 0xB6 -> "n"
          | 0xB7 -> "s"
          | 0xB8 -> "o"
          | 0xB9 -> "c"
          | 0xBA -> "p"
          | 0xBB -> "j"
          | 0xBC -> "r"
          | 0xBD -> "s"
          | 0xBE -> "v"
          | 0xBF -> "t"
          | _ -> String.sub s i nbc
          end
      | 0xD6 ->
          begin match Char.code s.[i+1] with
            0x80 -> "r"
          | 0x81 -> "c"
          | 0x82 -> "w"
          | 0x83 -> "p"
          | 0x84 -> "k"
          | 0x85 -> "o"
          | 0x86 -> "f"
          | _ -> String.sub s i nbc
          end
      | 0xE1 when viet = true ->
          (* Latin extended additionnal - Vietnameese *)
          begin match Char.code s.[i+1] with
            0xB8 ->
            begin match Char.code s.[i+2] with
            | 0x80 -> f "A"
            | 0x81 -> "a"
            | 0x82 -> f "B"
            | 0x83 -> "b"
            | 0x84 -> f "B"
            | 0x85 -> "b"
            | 0x86 -> f "B"
            | 0x87 -> "b"
            | 0x88 -> f "C"
            | 0x89 -> "c"
            | 0x8A -> f "D"
            | 0x8B -> "d"
            | 0x8C -> f "D"
            | 0x8D -> "d"
            | 0x8E -> f "D"
            | 0x8F -> "d"
            | 0x90 -> f "D"
            | 0x91 -> "d"
            | 0x92 -> f "D"
            | 0x93 -> "d"
            | 0x94 -> f "E"
            | 0x95 -> "e"
            | 0x96 -> f "E"
            | 0x97 -> "e"
            | 0x98 -> f "E"
            | 0x99 -> "e"
            | 0x9A -> f "E"
            | 0x9B -> "e"
            | 0x9C -> f "E"
            | 0x9D -> "e"
            | 0x9E -> f "F"
            | 0x9F -> "f"
            | 0xA0 -> f "F"
            | 0xA1 -> "f"
            | 0xA2 -> f "G"
            | 0xA3 -> "g"
            | 0xA4 -> f "H"
            | 0xA5 -> "h"
            | 0xA6 -> f "H"
            | 0xA7 -> "h"
            | 0xA8 -> f "H"
            | 0xA9 -> "h"
            | 0xAA -> f "H"
            | 0xAB -> "h"
            | 0xAC -> f "I"
            | 0xAD -> "i"
            | 0xAE -> f "I"
            | 0xAF -> "i"
            | 0xB0 -> f "K"
            | 0xB1 -> "k"
            | 0xB2 -> f "K"
            | 0xB3 -> "k"
            | 0xB4 -> f "K"
            | 0xB5 -> "k"
            | 0xB6 -> f "L"
            | 0xB7 -> "l"
            | 0xB8 -> f "L"
            | 0xB9 -> "l"
            | 0xBA -> f "L"
            | 0xBB -> "l"
            | 0xBC -> f "L"
            | 0xBD -> "l"
            | 0xBE -> f "M"
            | 0xBF -> "m"
            | _ -> String.sub s i nbc
            end
          | 0xB9 ->
            begin match Char.code s.[i+2] with
            | 0x80 -> f "M"
            | 0x81 -> "m"
            | 0x82 -> f "M"
            | 0x83 -> "m"
            | 0x84 -> f "N"
            | 0x85 -> "n"
            | 0x86 -> f "N"
            | 0x87 -> "n"
            | 0x88 -> f "N"
            | 0x89 -> "n"
            | 0x8A -> f "N"
            | 0x8B -> "n"
            | 0x8C -> f "O"
            | 0x8D -> "o"
            | 0x8E -> f "O"
            | 0x8F -> "o"
            | 0x90 -> f "O"
            | 0x91 -> "o"
            | 0x92 -> f "O"
            | 0x93 -> "o"
            | 0x94 -> f "P"
            | 0x95 -> "p"
            | 0x96 -> f "P"
            | 0x97 -> "p"
            | 0x98 -> f "R"
            | 0x99 -> "r"
            | 0x9A -> f "R"
            | 0x9B -> "r"
            | 0x9C -> f "R"
            | 0x9D -> "r"
            | 0x9E -> f "R"
            | 0x9F -> "r"
            | 0xA0 -> f "S"
            | 0xA1 -> "s"
            | 0xA2 -> f "S"
            | 0xA3 -> "s"
            | 0xA4 -> f "S"
            | 0xA5 -> "s"
            | 0xA6 -> f "S"
            | 0xA7 -> "s"
            | 0xA8 -> f "S"
            | 0xA9 -> "s"
            | 0xAA -> f "T"
            | 0xAB -> "t"
            | 0xAC -> f "T"
            | 0xAD -> "t"
            | 0xAE -> f "T"
            | 0xAF -> "t"
            | 0xB0 -> f "T"
            | 0xB1 -> "t"
            | 0xB2 -> f "U"
            | 0xB3 -> "u"
            | 0xB4 -> f "U"
            | 0xB5 -> "u"
            | 0xB6 -> f "U"
            | 0xB7 -> "u"
            | 0xB8 -> f "U"
            | 0xB9 -> "u"
            | 0xBA -> f "U"
            | 0xBB -> "u"
            | 0xBC -> f "V"
            | 0xBD -> "v"
            | 0xBE -> f "V"
            | 0xBF -> "v"
            | _ -> String.sub s i nbc
            end
          | 0xBA ->
            begin match Char.code s.[i+2] with
            | 0x80 -> f "W"
            | 0x81 -> "w"
            | 0x82 -> f "W"
            | 0x83 -> "w"
            | 0x84 -> f "W"
            | 0x85 -> "w"
            | 0x86 -> f "W"
            | 0x87 -> "w"
            | 0x88 -> f "W"
            | 0x89 -> "w"
            | 0x8A -> f "X"
            | 0x8B -> "x"
            | 0x8C -> f "X"
            | 0x8D -> "x"
            | 0x8E -> f "Y"
            | 0x8F -> "y"
            | 0x90 -> f "Z"
            | 0x91 -> "z"
            | 0x92 -> f "Z"
            | 0x93 -> "z"
            | 0x94 -> f "Z"
            | 0x95 -> "z"
            | 0x96 -> "h"
            | 0x97 -> "t"
            | 0x98 -> "w"
            | 0x99 -> "y"
            | 0x9A -> "a"
            | 0x9B -> "s"
            | 0x9C -> "s"
            | 0x9D -> "r"
            | 0x9E -> "s"
            | 0x9F -> "d"
            | 0xA0 -> f "A"
            | 0xA1 -> "a"
            | 0xA2 -> f "A"
            | 0xA3 -> "a"
            | 0xA4 -> f "A"
            | 0xA5 -> "a"
            | 0xA6 -> f "A"
            | 0xA7 -> "a"
            | 0xA8 -> f "A"
            | 0xA9 -> "a"
            | 0xAA -> f "A"
            | 0xAB -> "a"
            | 0xAC -> f "A"
            | 0xAD -> "a"
            | 0xAE -> f "A"
            | 0xAF -> "a"
            | 0xB0 -> f "A"
            | 0xB1 -> "a"
            | 0xB2 -> f "A"
            | 0xB3 -> "a"
            | 0xB4 -> f "A"
            | 0xB5 -> "a"
            | 0xB6 -> f "A"
            | 0xB7 -> "a"
            | 0xB8 -> f "E"
            | 0xB9 -> "e"
            | 0xBA -> f "E"
            | 0xBB -> "e"
            | 0xBC -> f "E"
            | 0xBD -> "e"
            | 0xBE -> f "E"
            | 0xBF -> "e"
            | _ -> String.sub s i nbc
            end
          | 0xBB ->
            begin match Char.code s.[i+2] with
            | 0x80 -> f "E"
            | 0x81 -> "e"
            | 0x82 -> f "E"
            | 0x83 -> "e"
            | 0x84 -> f "E"
            | 0x85 -> "e"
            | 0x86 -> f "E"
            | 0x87 -> "e"
            | 0x88 -> f "I"
            | 0x89 -> "i"
            | 0x8A -> f "I"
            | 0x8B -> "i"
            | 0x8C -> f "O"
            | 0x8D -> "o"
            | 0x8E -> f "O"
            | 0x8F -> "o"
            | 0x90 -> f "O"
            | 0x91 -> "o"
            | 0x92 -> f "O"
            | 0x93 -> "o"
            | 0x94 -> f "O"
            | 0x95 -> "o"
            | 0x96 -> f "O"
            | 0x97 -> "o"
            | 0x98 -> f "O"
            | 0x99 -> "o"
            | 0x9A -> f "O"
            | 0x9B -> "o"
            | 0x9C -> f "O"
            | 0x9D -> "o"
            | 0x9E -> f "O"
            | 0x9F -> "o"
            | 0xA0 -> f "O"
            | 0xA1 -> "o"
            | 0xA2 -> f "O"
            | 0xA3 -> "o"
            | 0xA4 -> f "U"
            | 0xA5 -> "u"
            | 0xA6 -> f "U"
            | 0xA7 -> "u"
            | 0xA8 -> f "U"
            | 0xA9 -> "u"
            | 0xAA -> f "U"
            | 0xAB -> "u"
            | 0xAC -> f "U"
            | 0xAD -> "u"
            | 0xAE -> f "u"
            | 0xAF -> "u"
            | 0xB0 -> f "U"
            | 0xB1 -> "u"
            | 0xB2 -> f "Y"
            | 0xB3 -> "y"
            | 0xB4 -> f "Y"
            | 0xB5 -> "y"
            | 0xB6 -> f "Y"
            | 0xB7 -> "y"
            | 0xB8 -> f "Y"
            | 0xB9 -> "y"
            | _ -> String.sub s i nbc
            end
          | _ -> String.sub s i nbc
          end (* E1 *)
      (* Code pour supprimer l'apostrophe typographique *)
      | 0xE2 when apostr = true ->
          begin match Char.code s.[i+1] with
          | 0x80 ->
            begin match Char.code s.[i+2] with
            | 0x99 -> " " (* FIXME " " or "" ??? *)
            | _ -> String.sub s i nbc
            end
          | _ -> String.sub s i nbc
          end
      | _ -> String.sub s i nbc
    in
    s, i + nbc

let next_chars_if_equiv s i t j =
  if i >= String.length s || j >= String.length t then None
  else
    let (s1, i1) = unaccent_utf_8 true s i in
    let (t1, j1) = unaccent_utf_8 true t j in
    if s1 = t1 then Some (i1, j1) else None

let lower ?(viet=false) ?(apostr=false) s =
  let rec copy special i len =
    if i = String.length s then Buff.get len
    else if Char.code s.[i] < 0x80 then
      match s.[i] with
      (*'a'..'z' | 'A'..'Z' | 'à'..'ÿ' | 'À'..'Ý' | *)
        'a'..'z' | 'A'..'Z' | '\xE0'..'\xFD' | '\xC0'..'\xDD' |
        '0'..'9' | '.' as c ->
          let len = if special then Buff.store len ' ' else len in
          let c = unaccent_iso_8859_1 (Char.lowercase_ascii c) in
          copy false (i + 1) (Buff.store len c)
      | _ -> copy (len <> 0) (i + 1) len
    else
      let len = if special then Buff.store len ' ' else len in
      let (t, j) = 
        unaccent_utf_8 ~viet:viet ~apostr:apostr true s i 
      in copy false j (Buff.mstore len t)
  in
  copy false 0 0

(* Name.abbrev *)

let abbrev_list =
  ["a", None; "af", None; "d", None; "de", None; "di", None; "ier", Some "i";
   "of", None; "saint", Some "st"; "sainte", Some "ste"; "van", None;
   "von", None; "zu", None; "zur", None]

let rec is_word s i p ip =
  if ip = String.length p then
    if i = String.length s then true else if s.[i] = ' ' then true else false
  else if i = String.length s then false
  else if s.[i] = p.[ip] then is_word s (i + 1) p (ip + 1)
  else false

let rec search_abbrev s i =
  function
    (w, a) :: pl ->
      if is_word s i w 0 then Some (String.length w, a)
      else search_abbrev s i pl
  | [] -> None

let abbrev s =
  let rec copy can_start_abbrev i len =
    if i >= String.length s then Buff.get len
    else
      match s.[i] with
        ' ' -> copy true (i + 1) (Buff.store len ' ')
      | c ->
          if can_start_abbrev then
            match search_abbrev s i abbrev_list with
              None -> copy false (i + 1) (Buff.store len c)
            | Some (n, Some a) -> copy false (i + n) (Buff.mstore len a)
            | Some (n, None) -> copy true (i + n + 1) len
          else copy false (i + 1) (Buff.store len c)
  in
  copy true 0 0

(* Name.strip *)

let strip_c s c =
  let rec copy i len =
    if i = String.length s then Buff.get len
    else if s.[i] = c then copy (i + 1) len
    else copy (i + 1) (Buff.store len s.[i])
  in
  copy 0 0

let strip s = strip_c s ' '


(* ******************************************************************** *)
(*  [Fonc] purge : string -> string                                     *)
(** [Description] : Supprime tous les caractères interdits (défini par
                    forbidden_char) présents dans la chaine passée en
                    argument.
    [Args] :
      - s : string que l'on veut purger
    [Retour] :
      - string : retourne la chaîne délestée des caractères interdits
    [Rem] : Exporté en clair hors de ce module.                         *)
(* ******************************************************************** *)
let purge s = List.fold_left (fun s c -> strip_c s c) s forbidden_char


(* Name.crush *)

let roman_number s i =
  let rec loop i =
    if i = String.length s then Some i
    else if s.[i] = ' ' then Some i
    else
      match s.[i] with
        'i' | 'v' | 'x' | 'l' -> loop (i + 1)
      | _ -> None
  in
  if i = 0 || s.[i-1] = ' ' then loop i else None

let crush s =
  let rec copy i len first_vowel =
    if i = String.length s then Buff.get len
    else if s.[i] = ' ' then copy (i + 1) len true
    else
      match roman_number s i with
        Some j ->
          let rec loop i len =
            if i = j then copy j len true
            else loop (i + 1) (Buff.store len s.[i])
          in
          loop i len
      | _ ->
          match s.[i] with
            'a' | 'e' | 'i' | 'o' | 'u' | 'y' ->
              let len = if first_vowel then Buff.store len 'e' else len in
              copy (i + 1) len false
          | 'h' ->
              let len =
                if i > 0 && s.[i-1] = 'p' then Buff.store (len - 1) 'f'
                else len
              in
              copy (i + 1) len first_vowel
          | 's' | 'z'
            when (i = String.length s - 1 || s.[i+1] = ' ') ->
              let len =
                let rec loop i len =
                  if i > 0 && len > 0 && s.[i] = Bytes.get !(Buff.buff) len &&
                     (s.[i] = 's' || s.[i] = 'z')
                  then
                    loop (i - 1) (len - 1)
                  else len + 1
                in
                loop (i - 1) (len - 1)
              in
              copy (i + 1) len false
          | 's' when i = String.length s - 1 || s.[i+1] = ' ' ->
              copy (i + 1) len false
          | c ->
              if i > 0 && s.[i-1] = c then copy (i + 1) len false
              else
                let c =
                  match c with
                    'k' | 'q' -> 'c'
                  | 'z' -> 's'
                  | c -> c
                in
                copy (i + 1) (Buff.store len c) false
  in
  copy 0 0 true

(* strip_lower *)

let strip_lower s = strip (lower s)

(* crush_lower *)

let crush_lower s = crush (abbrev (lower s))
