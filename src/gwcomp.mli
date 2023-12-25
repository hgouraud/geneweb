(* $Id: gwcomp.mli,v 5.1 2008-01-12 08:41:18 ddr Exp $ *)
(* Copyright (c) 2007-2008 INRIA *)

open Def

type key = { pk_first_name : string; pk_surname : string; pk_occ : int }
type somebody =
    Undefined of key
  | Defined of (iper, string) gen_person

type gw_syntax =
    Family of
      somebody gen_couple * sex * sex * (somebody * sex) list *
        ((iper, string) gen_person, string) gen_family *
        (iper, string) gen_person gen_descend
  | Notes of key * string
  | Relations of somebody * sex * (somebody, string) gen_relation list
  | Bnotes of string * string
  | Wnotes of string * string
val rgpd_files : string ref
val rgpd : bool ref
val magic_gwo : string
val line_cnt : int ref
val no_fail : bool ref
val comp_families : string -> string -> unit
val no_picture : bool ref
val gwc1 : bool ref
