(* $Id: perso.mli,v 5.7 2007-03-30 18:57:19 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Gwdb
open Config

val string_of_titles : config -> base -> bool -> string -> person -> string
val string_of_marriage_text : config -> base -> family -> string
val interp_templ : string -> config -> base -> person -> unit
val interp_templ_with_menu :
  (bool -> unit) -> string -> config -> base -> person -> unit
val interp_notempl_with_menu :
  (bool -> unit) -> string -> config -> base -> person -> unit

val print : config -> base -> person -> unit
val print_ascend : config -> base -> person -> unit
val print_what_links :
  config -> base -> person -> string -> int -> string -> unit

val build_sosa_ht : config -> base -> unit
val get_sosa_person : config -> base -> person -> Num.t
val get_single_sosa : config -> base -> person -> Num.t
val print_sosa : config -> base -> person -> bool -> unit

val string_of_num : string -> Num.t -> string

(**)

val infinite : int
val limit_desc : config -> int
val make_desc_level_table :
  config -> base -> int -> person -> int array * int array
val default_max_cousin_lev : int

type dup =
    DupFam of Adef.ifam * Adef.ifam
  | DupInd of Adef.iper * Adef.iper
  | NoDup

type excl_dup = (Adef.iper * Adef.iper) list * (Adef.ifam * Adef.ifam) list

val excluded_possible_duplications : config -> excl_dup
val first_possible_duplication : base -> Adef.iper -> excl_dup -> dup
