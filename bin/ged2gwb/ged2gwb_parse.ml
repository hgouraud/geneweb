(* Copyright (c) 1998-2007 INRIA *)

(* GEDCOM front end for ged2gwb: turns a GEDCOM byte stream into a tree of
   [record]s and decodes field values (names, dates, charsets).  The lexer,
   the recursive-descent date parser and all [Stream] handling are private.
   Extracted from ged2gwb.ml; the record readers at the end (iter_records /
   index_records / record_at) replace the former pass1/2/3 loops and
   find_notes_record / find_sources_record. *)

open Geneweb
open Def
module Stream = Geneweb_compat.Stream

let log_oc = ref stdout

type record = {
  rlab : string;
  rval : string;
  rcont : string;
  rsons : record list;
  rpos : int;
  mutable rused : bool;
}

type month_number_dates =
  | MonthDayDates
  | DayMonthDates
  | NoMonthNumberDates
  | MonthNumberHappened of string

type charset = Ansel | Ascii | Msdos | MacIntosh | Utf8

let charset_option = ref None
let charset = ref Ascii
let try_negative_dates = ref false
let no_negative_dates = ref false
let month_number_dates = ref NoMonthNumberDates
let line_cnt = ref 1
let in_file = ref ""

let print_location pos =
  Printf.fprintf !log_oc "File \"%s\", line %d:\n" !in_file pos

let rec skip_eol (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
  | Some ('\010' | '\013') ->
      Stream.junk strm__;
      skip_eol strm__
  | _ -> ()

let rec get_to_eoln len (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
  | Some ('\010' | '\013') ->
      Stream.junk strm__;
      skip_eol strm__;
      Buff.get len
  | Some '\t' ->
      Stream.junk strm__;
      get_to_eoln (Buff.store len ' ') strm__
  | Some c ->
      Stream.junk strm__;
      get_to_eoln (Buff.store len c) strm__
  | _ -> Buff.get len

let rec skip_to_eoln (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
  | Some ('\010' | '\013') ->
      Stream.junk strm__;
      skip_eol strm__
  | Some _ ->
      Stream.junk strm__;
      skip_to_eoln strm__
  | _ -> ()

let eol_chars = [ '\010'; '\013' ]

let rec get_ident len (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
  | Some (' ' | '\t') ->
      Stream.junk strm__;
      Buff.get len
  | Some c when not (List.mem c eol_chars) ->
      Stream.junk strm__;
      get_ident (Buff.store len c) strm__
  | _ -> Buff.get len

let skip_space (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
  | Some (' ' | '\t') -> Stream.junk strm__
  | _ -> ()

let rec line_start num (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
  | Some ' ' ->
      Stream.junk strm__;
      line_start num strm__
  | Some x when x = num -> Stream.junk strm__
  | _ -> raise Stream.Failure

let ascii_of_msdos s =
  let conv_char i =
    let cc =
      match Char.code s.[i] with
      | 0o200 -> 0o307
      | 0o201 -> 0o374
      | 0o202 -> 0o351
      | 0o203 -> 0o342
      | 0o204 -> 0o344
      | 0o205 -> 0o340
      | 0o206 -> 0o345
      | 0o207 -> 0o347
      | 0o210 -> 0o352
      | 0o211 -> 0o353
      | 0o212 -> 0o350
      | 0o213 -> 0o357
      | 0o214 -> 0o356
      | 0o215 -> 0o354
      | 0o216 -> 0o304
      | 0o217 -> 0o305
      | 0o220 -> 0o311
      | 0o221 -> 0o346
      | 0o222 -> 0o306
      | 0o223 -> 0o364
      | 0o224 -> 0o366
      | 0o225 -> 0o362
      | 0o226 -> 0o373
      | 0o227 -> 0o371
      | 0o230 -> 0o377
      | 0o231 -> 0o326
      | 0o232 -> 0o334
      | 0o233 -> 0o242
      | 0o234 -> 0o243
      | 0o235 -> 0o245
      | 0o240 -> 0o341
      | 0o241 -> 0o355
      | 0o242 -> 0o363
      | 0o243 -> 0o372
      | 0o244 -> 0o361
      | 0o245 -> 0o321
      | 0o246 -> 0o252
      | 0o247 -> 0o272
      | 0o250 -> 0o277
      | 0o252 -> 0o254
      | 0o253 -> 0o275
      | 0o254 -> 0o274
      | 0o255 -> 0o241
      | 0o256 -> 0o253
      | 0o257 -> 0o273
      | 0o346 -> 0o265
      | 0o361 -> 0o261
      | 0o366 -> 0o367
      | 0o370 -> 0o260
      | 0o372 -> 0o267
      | 0o375 -> 0o262
      | c -> c
    in
    Char.chr cc
  in
  String.init (String.length s) conv_char

let ascii_of_macintosh s =
  let conv_char i =
    let cc =
      match Char.code s.[i] with
      | 0o200 -> 0o304
      | 0o201 -> 0o305
      | 0o202 -> 0o307
      | 0o203 -> 0o311
      | 0o204 -> 0o321
      | 0o205 -> 0o326
      | 0o206 -> 0o334
      | 0o207 -> 0o341
      | 0o210 -> 0o340
      | 0o211 -> 0o342
      | 0o212 -> 0o344
      | 0o213 -> 0o343
      | 0o214 -> 0o345
      | 0o215 -> 0o347
      | 0o216 -> 0o351
      | 0o217 -> 0o350
      | 0o220 -> 0o352
      | 0o221 -> 0o353
      | 0o222 -> 0o355
      | 0o223 -> 0o354
      | 0o224 -> 0o356
      | 0o225 -> 0o357
      | 0o226 -> 0o361
      | 0o227 -> 0o363
      | 0o230 -> 0o362
      | 0o231 -> 0o364
      | 0o232 -> 0o366
      | 0o233 -> 0o365
      | 0o234 -> 0o372
      | 0o235 -> 0o371
      | 0o236 -> 0o373
      | 0o237 -> 0o374
      | 0o241 -> 0o260
      | 0o244 -> 0o247
      | 0o245 -> 0o267
      | 0o246 -> 0o266
      | 0o247 -> 0o337
      | 0o250 -> 0o256
      | 0o256 -> 0o306
      | 0o257 -> 0o330
      | 0o264 -> 0o245
      | 0o273 -> 0o252
      | 0o274 -> 0o272
      | 0o276 -> 0o346
      | 0o277 -> 0o370
      | 0o300 -> 0o277
      | 0o301 -> 0o241
      | 0o302 -> 0o254
      | 0o307 -> 0o253
      | 0o310 -> 0o273
      | 0o312 -> 0o040
      | 0o313 -> 0o300
      | 0o314 -> 0o303
      | 0o315 -> 0o325
      | 0o320 -> 0o255
      | 0o326 -> 0o367
      | 0o330 -> 0o377
      | 0o345 -> 0o302
      | 0o346 -> 0o312
      | 0o347 -> 0o301
      | 0o350 -> 0o313
      | 0o351 -> 0o310
      | 0o352 -> 0o315
      | 0o353 -> 0o316
      | 0o354 -> 0o317
      | 0o355 -> 0o314
      | 0o356 -> 0o323
      | 0o357 -> 0o324
      | 0o361 -> 0o322
      | 0o362 -> 0o332
      | 0o363 -> 0o333
      | 0o364 -> 0o331
      | c -> c
    in
    Char.chr cc
  in
  String.init (String.length s) conv_char

let utf8_of_string s =
  match !charset with
  | Ansel -> Ansel.to_utf_8 s
  | Ascii -> Mutil.utf_8_of_iso_8859_1 s
  | Msdos -> Mutil.utf_8_of_iso_8859_1 (ascii_of_msdos s)
  | MacIntosh -> Mutil.utf_8_of_iso_8859_1 (ascii_of_macintosh s)
  | Utf8 -> s

let rec get_lev n (strm__ : _ Stream.t) =
  let _ = line_start n strm__ in
  let _ =
    try skip_space strm__ with Stream.Failure -> raise (Stream.Error "")
  in
  let r1 =
    try get_ident 0 strm__ with Stream.Failure -> raise (Stream.Error "")
  in
  let strm = strm__ in
  let rlab, rval, rcont, l =
    if String.length r1 > 0 && r1.[0] = '@' then parse_address n r1 strm
    else parse_text n r1 strm
  in
  {
    rlab;
    rval = utf8_of_string rval;
    rcont = utf8_of_string rcont;
    rsons = List.rev l;
    rpos = !line_cnt;
    rused = false;
  }

and parse_address n r1 (strm__ : _ Stream.t) =
  let r2 = get_ident 0 strm__ in
  let r3 =
    try get_to_eoln 0 strm__ with Stream.Failure -> raise (Stream.Error "")
  in
  let l =
    try get_lev_list [] (Char.chr (Char.code n + 1)) strm__
    with Stream.Failure -> raise (Stream.Error "")
  in
  (r2, r1, r3, l)

and parse_text n r1 (strm__ : _ Stream.t) =
  let r2 = get_to_eoln 0 strm__ in
  let l =
    try get_lev_list [] (Char.chr (Char.code n + 1)) strm__
    with Stream.Failure -> raise (Stream.Error "")
  in
  (r1, r2, "", l)

and get_lev_list l n (strm__ : _ Stream.t) =
  match try Some (get_lev n strm__) with Stream.Failure -> None with
  | Some x -> get_lev_list (x :: l) n strm__
  | _ -> l

let bad_dates_warned = ref false

let print_bad_date pos d =
  if !bad_dates_warned then ()
  else begin
    bad_dates_warned := true;
    print_location pos;
    Printf.fprintf !log_oc "Can't decode date %s\n" d;
    flush !log_oc
  end

let check_month m =
  if m < 1 || m > 12 then begin
    Printf.fprintf !log_oc "Bad (numbered) month in date: %d\n" m;
    flush !log_oc
  end

let warning_month_number_dates () =
  match !month_number_dates with
  | MonthNumberHappened s ->
      Printf.fprintf !log_oc
        "  Warning: the file holds dates with numbered months (like: \
         12/05/1912).\n\
        \  \n\
        \  GEDCOM standard *requires* that months in dates be identifiers. The\n\
        \  correct form for this example would be 12 MAY 1912 or 5 DEC 1912.\n\
        \  \n\
        \  Consider restarting with option \"-dates_dm\" or \"-dates_md\".\n\
        \  Use option -help to see what they do.\n\
        \  \n\
        \  (example found in gedcom: \"%s\")"
        s;
      flush !log_oc
  | _ -> ()

let rec skip_spaces (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
  | Some ' ' ->
      Stream.junk strm__;
      skip_spaces strm__
  | _ -> ()

let rec ident_slash len (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
  | Some '/' ->
      Stream.junk strm__;
      Buff.get len
  | Some '\t' ->
      Stream.junk strm__;
      ident_slash (Buff.store len ' ') strm__
  | Some c ->
      Stream.junk strm__;
      ident_slash (Buff.store len c) strm__
  | _ -> Buff.get len

let strip c str =
  let start =
    let rec loop i =
      if i = String.length str then i
      else if str.[i] = c then loop (i + 1)
      else i
    in
    loop 0
  in
  let stop =
    let rec loop i =
      if i = -1 then i + 1 else if str.[i] = c then loop (i - 1) else i + 1
    in
    loop (String.length str - 1)
  in
  if start = 0 && stop = String.length str then str
  else if start >= stop then ""
  else String.sub str start (stop - start)

let strip_spaces = strip ' '
let strip_newlines = strip '\n'

let parse_name s =
  let strm__ = Stream.of_string s in
  let _ = skip_spaces strm__ in
  let invert =
    match Stream.peek strm__ with
    | Some '/' ->
        Stream.junk strm__;
        true
    | _ -> false
  in
  let f =
    try ident_slash 0 strm__ with Stream.Failure -> raise (Stream.Error "")
  in
  let _ =
    try skip_spaces strm__ with Stream.Failure -> raise (Stream.Error "")
  in
  let s =
    try ident_slash 0 strm__ with Stream.Failure -> raise (Stream.Error "")
  in
  let f, s = if invert then (s, f) else (f, s) in
  let f = strip_spaces f in
  let s = strip_spaces s in
  ((if f = "" then "x" else f), if s = "" then "?" else s)

let rec find_field lab = function
  | r :: rl ->
      if r.rlab = lab then begin
        r.rused <- true;
        Some r
      end
      else find_field lab rl
  | [] -> None

let rec find_all_fields lab = function
  | r :: rl ->
      if r.rlab = lab then begin
        r.rused <- true;
        r :: find_all_fields lab rl
      end
      else find_all_fields lab rl
  | [] -> []

let rec find_field_with_value lab v = function
  | r :: rl ->
      if r.rlab = lab && r.rval = v then begin
        r.rused <- true;
        true
      end
      else find_field_with_value lab v rl
  | [] -> false

(* --- GEDCOM date lexer (camlp5-free) ------------------------------------
   Replaces the former stream lexer lexing_date/number/ident/text and the
   camlp5 Grammar/Token/Plexing glue (make_date_lexing, date_lexer, date_g,
   the Grammar.Entry.create entries and the roman_int grammar entry). *)

type tok = INT of string | ID of string | TEXT of string | SYM of char | EOI

let lex (s : string) : tok list =
  let n = String.length s in
  let buf = Buffer.create 16 in
  let rec skip i =
    if i < n then match s.[i] with ' ' | '\t' | '\r' -> skip (i + 1) | _ -> i
    else i
  in
  let read_text i =
    Buffer.clear buf;
    let rec loop i =
      if i >= n then i
      else
        match s.[i] with
        | ')' -> i + 1
        | '(' ->
            Buffer.add_char buf '(';
            let j = loop (i + 1) in
            Buffer.add_char buf ')';
            j
        | c ->
            Buffer.add_char buf c;
            loop (i + 1)
    in
    let j = loop i in
    (Buffer.contents buf, j)
  in
  let rec go i acc =
    let i = skip i in
    if i >= n then List.rev (EOI :: acc)
    else
      match s.[i] with
      | '0' .. '9' ->
          let j = ref i in
          while !j < n && s.[!j] >= '0' && s.[!j] <= '9' do
            incr j
          done;
          go !j (INT (String.sub s i (!j - i)) :: acc)
      | 'A' .. 'Z' ->
          let j = ref i in
          while !j < n && s.[!j] >= 'A' && s.[!j] <= 'Z' do
            incr j
          done;
          go !j (ID (String.sub s i (!j - i)) :: acc)
      | '(' ->
          let txt, j = read_text (i + 1) in
          go j (TEXT txt :: acc)
      | c -> go (i + 1) (SYM c :: acc)
  in
  go 0 []

(* camlp5 stream-parser failure discipline, reproduced by hand:
   Fail  = Stream.Failure (backtrack, try next alternative);
   Error = Stream.Error   (committed failure, aborts the parse). *)

exception Fail
exception Error of string

type 'a range = Begin of 'a | End of 'a | BeginEnd of 'a * 'a

let is_roman_int x =
  try
    let _ = Mutil.arabian_of_roman x in
    true
  with Not_found -> false

let start_with_int x =
  try
    let s = String.sub x 0 1 in
    let _ = int_of_string s in
    true
  with _ -> false

let date_str = ref ""

let make_date n1 n2 n3 =
  let n3 =
    if !no_negative_dates then
      match n3 with Some n3 -> Some (abs n3) | None -> None
    else n3
  in
  match (n1, n2, n3) with
  | Some d, Some m, Some y ->
      let d, m =
        match m with
        | Right m -> (d, m)
        | Left m -> (
            match !month_number_dates with
            | DayMonthDates ->
                check_month m;
                (d, m)
            | MonthDayDates ->
                check_month d;
                (m, d)
            | _ ->
                if d >= 1 && m >= 1 && d <= 31 && m <= 31 then
                  if d > 13 && m <= 13 then (d, m)
                  else if m > 13 && d <= 13 then (m, d)
                  else if d > 13 && m > 13 then (0, 0)
                  else begin
                    month_number_dates := MonthNumberHappened !date_str;
                    (0, 0)
                  end
                else (0, 0))
      in
      let d, m = if m < 1 || m > 13 then (0, 0) else (d, m) in
      { Adef.day = d; month = m; year = y; prec = Sure; delta = 0 }
  | None, Some m, Some y ->
      let m = match m with Right m -> m | Left m -> m in
      { Adef.day = 0; month = m; year = y; prec = Sure; delta = 0 }
  | None, None, Some y ->
      { Adef.day = 0; month = 0; year = y; prec = Sure; delta = 0 }
  | Some y, None, None ->
      { Adef.day = 0; month = 0; year = y; prec = Sure; delta = 0 }
  | _ -> raise Fail

let recover_date cal = function
  | Adef.Dgreg (d, Dgregorian) ->
      let d =
        match cal with
        | Adef.Dgregorian -> d
        | Djulian -> Calendar.gregorian_of_julian d
        | Dfrench -> Calendar.gregorian_of_french d
        | Dhebrew -> Calendar.gregorian_of_hebrew d
      in
      Adef.Dgreg (d, cal)
  | d -> d

(* --- Recursive-descent date parser (camlp5-free) ------------------------
   Transcription of the former EXTEND grammar: one function per entry.
   make_date / recover_date above are reused unchanged. *)

let opt p toks =
  try
    let v, r = p toks in
    (Some v, r)
  with Fail -> (None, toks)

let rec list0_syms syms toks =
  match toks with
  | SYM c :: r when List.mem c syms -> list0_syms syms r
  | _ -> toks

let expect_eoi toks =
  match toks with EOI :: _ | [] -> () | _ -> raise (Error "expected EOI")

let p_int toks =
  let toi i = try int_of_string i with Failure _ -> raise Fail in
  match toks with
  | INT i :: ID "BCE" :: r -> (-toi i, r)
  | INT i :: ID "B" :: SYM '.' :: ID "C" :: SYM '.' :: r -> (-toi i, r)
  | INT i :: ID "B" :: SYM '.' :: ID "C" :: r -> (-toi i, r)
  | INT i :: r -> (toi i, r)
  | SYM '-' :: INT i :: r -> (-toi i, r)
  | _ -> raise Fail

let p_month = function
  | ID "JAN" :: r -> (1, r)
  | ID "FEB" :: r -> (2, r)
  | ID "MAR" :: r -> (3, r)
  | ID "APR" :: r -> (4, r)
  | ID "MAY" :: r -> (5, r)
  | ID "JUN" :: r -> (6, r)
  | ID "JUL" :: r -> (7, r)
  | ID "AUG" :: r -> (8, r)
  | ID "SEP" :: r -> (9, r)
  | ID "OCT" :: r -> (10, r)
  | ID "NOV" :: r -> (11, r)
  | ID "DEC" :: r -> (12, r)
  | _ -> raise Fail

let p_french = function
  | ID "VEND" :: r -> (1, r)
  | ID "BRUM" :: r -> (2, r)
  | ID "FRIM" :: r -> (3, r)
  | ID "NIVO" :: r -> (4, r)
  | ID "PLUV" :: r -> (5, r)
  | ID "VENT" :: r -> (6, r)
  | ID "GERM" :: r -> (7, r)
  | ID "FLOR" :: r -> (8, r)
  | ID "PRAI" :: r -> (9, r)
  | ID "MESS" :: r -> (10, r)
  | ID "THER" :: r -> (11, r)
  | ID "FRUC" :: r -> (12, r)
  | ID "COMP" :: r -> (13, r)
  | _ -> raise Fail

let p_hebr = function
  | ID "TSH" :: r -> (1, r)
  | ID "CSH" :: r -> (2, r)
  | ID "KSL" :: r -> (3, r)
  | ID "TVT" :: r -> (4, r)
  | ID "SHV" :: r -> (5, r)
  | ID "ADR" :: r -> (6, r)
  | ID "ADS" :: r -> (7, r)
  | ID "NSN" :: r -> (8, r)
  | ID "IYR" :: r -> (9, r)
  | ID "SVN" :: r -> (10, r)
  | ID "TMZ" :: r -> (11, r)
  | ID "AAV" :: r -> (12, r)
  | ID "ELL" :: r -> (13, r)
  | _ -> raise Fail

let p_roman = function
  | ID x :: r when is_roman_int x -> (Mutil.arabian_of_roman x, r)
  | _ -> raise Fail

let p_gen_month toks =
  match try Some (p_int toks) with Fail -> None with
  | Some (i, r) -> (Left (abs i), r)
  | None ->
      let m, r = p_month toks in
      (Right m, r)

let p_gen_french toks =
  let m, r = p_french toks in
  (Right m, r)

let p_gen_hebr toks =
  let m, r = p_hebr toks in
  (Right m, r)

let p_year_fren toks =
  match try Some (p_int toks) with Fail -> None with
  | Some (i, r) -> (i, r)
  | None -> ( match toks with ID "AN" :: r -> p_roman r | _ -> p_roman toks)

let p_date_greg toks =
  let toks = list0_syms [ '.' ] toks in
  let n1, toks = opt p_int toks in
  let toks = list0_syms [ '.'; '/' ] toks in
  let n2, toks = opt p_gen_month toks in
  let toks = list0_syms [ '.'; '/' ] toks in
  let n3, toks = opt p_int toks in
  let toks = list0_syms [ '.' ] toks in
  (make_date n1 n2 n3, toks)

let p_date_hebr toks =
  let toks = list0_syms [ '.' ] toks in
  let n1, toks = opt p_int toks in
  let toks = list0_syms [ '.'; '/' ] toks in
  let n2, toks = opt p_gen_hebr toks in
  let toks = list0_syms [ '.'; '/' ] toks in
  let n3, toks = opt p_int toks in
  let toks = list0_syms [ '.' ] toks in
  (make_date n1 n2 n3, toks)

let p_date_fren_kont toks =
  let toks = list0_syms [ '.'; '/' ] toks in
  let n2, toks = opt p_gen_french toks in
  let toks = list0_syms [ '.'; '/' ] toks in
  let n3, toks = opt p_year_fren toks in
  let toks = list0_syms [ '.' ] toks in
  ((n2, n3), toks)

let p_date_fren toks =
  let toks = list0_syms [ '.' ] toks in
  match try Some (p_int toks) with Fail -> None with
  | Some (n1, r) ->
      let (n2, n3), r = p_date_fren_kont r in
      (make_date (Some n1) n2 n3, r)
  | None -> (
      match try Some (p_year_fren toks) with Fail -> None with
      | Some (n1, r) -> (make_date (Some n1) None None, r)
      | None ->
          let (n2, n3), r = p_date_fren_kont toks in
          (make_date None n2 n3, r))

let p_date_calendar toks =
  match toks with
  | SYM '@' :: SYM '#' :: ID "DGREGORIAN" :: SYM '@' :: r ->
      let d, r = p_date_greg r in
      ((d, Adef.Dgregorian), r)
  | SYM '@' :: SYM '#' :: ID "DJULIAN" :: SYM '@' :: r ->
      let d, r = p_date_greg r in
      ((Calendar.gregorian_of_julian d, Adef.Djulian), r)
  | SYM '@' :: SYM '#' :: ID "DFRENCH" :: ID "R" :: SYM '@' :: r ->
      let d, r = p_date_fren r in
      ((Calendar.gregorian_of_french d, Adef.Dfrench), r)
  | SYM '@' :: SYM '#' :: ID "DHEBREW" :: SYM '@' :: r ->
      let d, r = p_date_hebr r in
      ((Calendar.gregorian_of_hebrew d, Adef.Dhebrew), r)
  | _ ->
      let d, r = p_date_greg toks in
      ((d, Adef.Dgregorian), r)

(* NB: the AFT->Before / BEF->After mapping is reproduced bug-for-bug from the
   original grammar (ged2gwb date: rule). *)

let p_date toks =
  let withp p r =
    let (d, cal), r = p_date_calendar r in
    (({ d with prec = p }, cal), r)
  in
  match toks with
  | ID "ABT" :: r -> withp Adef.About r
  | ID "ENV" :: r -> withp Adef.About r
  | ID "EST" :: r -> withp Adef.Maybe r
  | ID "AFT" :: r -> withp Adef.Before r
  | ID "BEF" :: r -> withp Adef.After r
  | _ -> p_date_calendar toks

let p_date_range toks =
  match toks with
  | ID "BEF" :: r ->
      let dt, r = p_date r in
      (End dt, r)
  | ID "AFT" :: r ->
      let dt, r = p_date r in
      (Begin dt, r)
  | ID "BET" :: r -> (
      let dt, r = p_date r in
      match r with
      | ID "AND" :: r2 ->
          let dt1, r2 = p_date r2 in
          (BeginEnd (dt, dt1), r2)
      | _ -> raise (Error "BET without AND"))
  | ID "TO" :: r ->
      let dt, r = p_date r in
      (End dt, r)
  | ID "FROM" :: r -> (
      let dt, r = p_date r in
      match r with
      | ID "TO" :: r2 ->
          let dt1, r2 = p_date r2 in
          (BeginEnd (dt, dt1), r2)
      | _ -> (Begin dt, r))
  | _ -> raise Fail

let p_date_or_text toks =
  match try `R (p_date_range toks) with Fail -> `N with
  | `R (dr, r) ->
      let d =
        match dr with
        | Begin (d, cal) -> Adef.Dgreg ({ d with prec = Adef.After }, cal)
        | End (d, cal) -> Adef.Dgreg ({ d with prec = Adef.Before }, cal)
        | BeginEnd ((d1, cal1), (d2, cal2)) ->
            let dmy2 =
              match cal2 with
              | Dgregorian ->
                  {
                    Adef.day2 = d2.day;
                    month2 = d2.month;
                    year2 = d2.year;
                    delta2 = 0;
                  }
              | Djulian ->
                  let d = Calendar.julian_of_gregorian d2 in
                  {
                    Adef.day2 = d.day;
                    month2 = d.month;
                    year2 = d.year;
                    delta2 = 0;
                  }
              | Dfrench ->
                  let d = Calendar.french_of_gregorian d2 in
                  {
                    Adef.day2 = d.day;
                    month2 = d.month;
                    year2 = d.year;
                    delta2 = 0;
                  }
              | Dhebrew ->
                  let d = Calendar.hebrew_of_gregorian d2 in
                  {
                    Adef.day2 = d.day;
                    month2 = d.month;
                    year2 = d.year;
                    delta2 = 0;
                  }
            in
            Adef.Dgreg ({ d1 with prec = Adef.YearInt dmy2 }, cal1)
      in
      (d, r)
  | `N -> (
      match try `D (p_date toks) with Fail -> `N with
      | `D ((d, cal), r) -> (Adef.Dgreg (d, cal), r)
      | `N -> (
          match toks with TEXT s :: r -> (Adef.Dtext s, r) | _ -> raise Fail))

let p_date_value toks =
  let d, r = p_date_or_text toks in
  expect_eoi r;
  d

let p_date_value_recover toks =
  match toks with
  | SYM '@' :: SYM '#' :: ID "DGREGORIAN" :: SYM '@' :: r ->
      recover_date Adef.Dgregorian (p_date_value r)
  | SYM '@' :: SYM '#' :: ID "DJULIAN" :: SYM '@' :: r ->
      recover_date Adef.Djulian (p_date_value r)
  | SYM '@' :: SYM '#' :: ID "DFRENCH" :: ID "R" :: SYM '@' :: r ->
      recover_date Adef.Dfrench (p_date_value r)
  | SYM '@' :: SYM '#' :: ID "DHEBREW" :: SYM '@' :: r ->
      recover_date Adef.Dhebrew (p_date_value r)
  | _ -> raise Fail

let p_date_interval toks =
  let fin dt r =
    expect_eoi r;
    dt
  in
  match toks with
  | ID "BEF" :: r ->
      let dt, r = p_date_or_text r in
      End (fin dt r)
  | ID "AFT" :: r ->
      let dt, r = p_date_or_text r in
      Begin (fin dt r)
  | ID "BET" :: r -> (
      let dt, r = p_date_or_text r in
      match r with
      | ID "AND" :: r2 ->
          let dt1, r2 = p_date_or_text r2 in
          expect_eoi r2;
          BeginEnd (dt, dt1)
      | _ -> raise (Error "BET without AND"))
  | ID "TO" :: r ->
      let dt, r = p_date_or_text r in
      End (fin dt r)
  | ID "FROM" :: r -> (
      let dt, r = p_date_or_text r in
      match r with
      | ID "TO" :: r2 ->
          let dt1, r2 = p_date_or_text r2 in
          expect_eoi r2;
          BeginEnd (dt, dt1)
      | _ ->
          expect_eoi r;
          Begin dt)
  | _ ->
      let dt, r = p_date_or_text toks in
      Begin (fin dt r)

(* Perform a regular expression match. *)

let preg_match pattern subject =
  let re = Str.regexp pattern in
  try
    ignore (Str.search_forward re subject 0);
    true
  with Not_found -> false

let date_of_field d =
  if d = "" then None
  else if preg_match "^[0-9]+$" d && String.length d > 8 then
    Some (Adef.Dtext d)
  else begin
    date_str := d;
    let toks = lex (String.uppercase_ascii d) in
    match try Some (p_date_value toks) with Fail | Error _ -> None with
    | Some v -> Some v
    | None -> (
        let toks = lex (String.uppercase_ascii d) in
        match
          try Some (p_date_value_recover toks) with Fail | Error _ -> None
        with
        | Some v -> Some v
        | None -> Some (Dtext d))
  end

let get_lev0 (strm__ : _ Stream.t) =
  let _ = line_start '0' strm__ in
  let _ =
    try skip_space strm__ with Stream.Failure -> raise (Stream.Error "")
  in
  let r1 =
    try get_ident 0 strm__ with Stream.Failure -> raise (Stream.Error "")
  in
  let r2 =
    try get_ident 0 strm__ with Stream.Failure -> raise (Stream.Error "")
  in
  let r3 =
    try get_to_eoln 0 strm__ with Stream.Failure -> raise (Stream.Error "")
  in
  let l =
    try get_lev_list [] '1' strm__
    with Stream.Failure -> raise (Stream.Error "")
  in
  let rlab, rval = if r2 = "" then (r1, "") else (r2, r1) in
  let rval = utf8_of_string rval in
  let rcont = utf8_of_string r3 in
  { rlab; rval; rcont; rsons = List.rev l; rpos = !line_cnt; rused = false }

let decode_date_interval pos s =
  let toks = lex s in
  match
    try Some (p_date_interval toks) with Fail | Error _ | Not_found -> None
  with
  | Some (BeginEnd (d1, d2)) -> (Some d1, Some d2)
  | Some (Begin d) -> (Some d, None)
  | Some (End d) -> (None, Some d)
  | None ->
      print_bad_date pos s;
      (None, None)

let find_lev0 (strm__ : _ Stream.t) =
  let bp = Stream.count strm__ in
  let _ = line_start '0' strm__ in
  let _ =
    try skip_space strm__ with Stream.Failure -> raise (Stream.Error "")
  in
  let r1 =
    try get_ident 0 strm__ with Stream.Failure -> raise (Stream.Error "")
  in
  let r2 =
    try get_ident 0 strm__ with Stream.Failure -> raise (Stream.Error "")
  in
  let _ =
    try skip_to_eoln strm__ with Stream.Failure -> raise (Stream.Error "")
  in
  (bp, r1, r2)

let open_in_bin_with_bom_check fname =
  let ic = open_in_bin fname in
  (match Bom.check ic with
  | Bom.Utf8 -> charset_option := Some Utf8
  | bom when Bom.is_unsupported bom ->
      close_in ic;
      let base = Filename.remove_extension fname in
      let ext = Filename.extension fname in
      Printf.fprintf !log_oc "Error: %s encoding detected, not supported\n"
        (Bom.to_string bom);
      Printf.fprintf !log_oc
        "Convert to UTF-8 first:\niconv -f %s -t UTF-8 %s > %s_UTF8%s\n"
        (Bom.to_string bom) fname base ext;
      flush !log_oc;
      exit 2
  | _ -> ());
  ic

(* ---- record readers (replace pass1/2/3 & find_notes/sources_record) ---- *)

let iter_records ?(on_strange = fun _ -> ()) fname f =
  let ic = open_in_bin_with_bom_check fname in
  line_cnt := 0;
  let strm =
    Stream.from (fun _ ->
        try
          let c = input_char ic in
          if c = '\n' then incr line_cnt;
          Some c
        with End_of_file -> None)
  in
  let rec loop () =
    match try Some (get_lev0 strm) with Stream.Failure -> None with
    | Some r ->
        f r;
        loop ()
    | None -> (
        match Stream.peek strm with
        | Some '1' .. '9' ->
            Stream.junk strm;
            let (_ : string) = get_to_eoln 0 strm in
            loop ()
        | Some c ->
            Stream.junk strm;
            on_strange c;
            let (_ : string) = get_to_eoln 0 strm in
            loop ()
        | None -> ())
  in
  loop ();
  close_in ic

let index_records fname ~on_note ~on_sour =
  let ic = open_in_bin_with_bom_check fname in
  let strm = Stream.of_channel ic in
  let rec loop () =
    match try Some (find_lev0 strm) with Stream.Failure -> None with
    | Some (bp, r1, r2) ->
        (match r2 with
        | "NOTE" -> on_note r1 bp
        | "SOUR" -> on_sour r1 bp
        | _ -> ());
        loop ()
    | None -> (
        match Stream.peek strm with
        | Some _ ->
            Stream.junk strm;
            skip_to_eoln strm;
            loop ()
        | None -> ())
  in
  loop ();
  close_in ic

let record_at ic pos =
  seek_in ic pos;
  try Some (get_lev0 (Stream.of_channel ic))
  with Stream.Failure | Stream.Error _ -> None
