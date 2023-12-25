(* $Id: updateIndOk.mli,v 5.6 2008-01-21 13:28:12 ddr Exp $ *)

open Config
open Def
open Gwdb

val effective_del :
  config -> base -> (CheckItem.base_warning -> unit) -> person ->
    (iper, istr) gen_person
val effective_mod :
  config -> base -> (Update.key, string) gen_person -> (iper, istr) gen_person
val all_checks_person :
  config -> base -> (iper, istr) gen_person -> ifam gen_ascend ->
    ifam gen_union -> CheckItem.base_warning list
val print_mod_aux :
  config -> base -> ((Update.key, string) gen_person -> unit) -> unit

val print_add : config -> base -> unit
val print_del : config -> base -> unit
val print_mod : config -> base -> unit

