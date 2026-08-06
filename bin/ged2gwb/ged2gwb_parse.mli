(* Copyright (c) 1998-2007 INRIA *)

(* GEDCOM front end for ged2gwb: byte stream -> record tree + decoded values.
   The lexer, the recursive-descent date parser and all [Stream] handling are
   implementation details and are deliberately not exposed. *)

type record = {
  rlab : string;  (** tag: "INDI", "BIRT", "NOTE", ... *)
  rval : string;  (** value, or an @xref@ *)
  rcont : string;  (** inline (CONT-style) continuation *)
  rsons : record list;  (** sub-records at the next level *)
  rpos : int;  (** source line, for diagnostics *)
  mutable rused : bool;  (** "consumed" flag the driver sets while walking *)
}
(** One GEDCOM record: a (level, tag, value) line together with its sub-records.
*)

(* --- character set --- *)

type charset = Ansel | Ascii | Msdos | MacIntosh | Utf8

val charset : charset ref
val charset_option : charset option ref

val utf8_of_string : string -> string
(** Convert a raw field value from [!charset] to UTF-8. *)

(* --- date interpretation config (read, and mutated, while parsing) --- *)

type month_number_dates =
  | MonthDayDates
  | DayMonthDates
  | NoMonthNumberDates
  | MonthNumberHappened of string

val month_number_dates : month_number_dates ref
val no_negative_dates : bool ref
val try_negative_dates : bool ref
val warning_month_number_dates : unit -> unit

(* --- diagnostics / source position --- *)

val log_oc : out_channel ref
val in_file : string ref
val line_cnt : int ref
val print_location : int -> unit

(* --- opening a GEDCOM file --- *)

val open_in_bin_with_bom_check : string -> in_channel
(** Open [fname] in binary mode, detecting/handling a BOM and setting
    {!charset_option}. Exits on an unsupported encoding. *)

(* --- reading records (the lexer and Stream never surface) --- *)

val iter_records :
  ?on_strange:(char -> unit) -> string -> (record -> unit) -> unit
(** [iter_records ?on_strange fname f] reads every level-0 record of [fname] and
    applies [f], skipping malformed lines. [on_strange] is invoked on an
    unexpected leading character (default: ignore). *)

val index_records :
  string ->
  on_note:(string -> int -> unit) ->
  on_sour:(string -> int -> unit) ->
  unit
(** Pass-1 scan recording the byte offset of every NOTE / SOUR record. *)

val record_at : in_channel -> int -> record option
(** [record_at ic pos] seeks [ic] to byte offset [pos] and reads one record
    (used to resolve @xref@ NOTE / SOUR references). *)

(* --- lookups over a record's sub-records --- *)

val find_field : string -> record list -> record option
val find_all_fields : string -> record list -> record list
val find_field_with_value : string -> string -> record list -> bool

(* --- value decoders --- *)

val strip_spaces : string -> string
val strip_newlines : string -> string

val parse_name : string -> string * string
(** Split a GEDCOM name field into (first name, surname). *)

val date_of_field : string -> Adef.date option
val decode_date_interval : int -> string -> Adef.date option * Adef.date option
val is_roman_int : string -> bool
val start_with_int : string -> bool
