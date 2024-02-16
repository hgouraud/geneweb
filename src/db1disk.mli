(* $Id: database.mli,v 5.2 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

(* from dbdisk.mli *)

module TYPES : sig

  open Def

  type dsk_istr = Adef.istr

  type dsk_person = (iper, dsk_istr) gen_person
  type dsk_ascend = ifam gen_ascend
  type dsk_union = ifam gen_union
  type dsk_family = (iper, dsk_istr) gen_family
  type dsk_couple = iper gen_couple
  type dsk_descend = iper gen_descend

  type dsk_title = dsk_istr gen_title

  type notes =
    { nread : string -> rn_mode -> string;
      norigin_file : string;
      efiles : unit -> string list }

  type 'a record_access =
    { load_array : unit -> unit;
      get : int -> 'a;
      set : int -> 'a -> unit;
      mutable len : int;
      output_array : out_channel -> unit;
      clear_array : unit -> unit }

  type 'istr string_person_index =
    { find : 'istr -> iper list;
      cursor : string -> 'istr;
      next : 'istr -> 'istr }

  type visible_record_access =
    { v_write : unit -> unit; v_get : (dsk_person -> bool) -> int -> bool }

  type base_data =
    { persons : dsk_person record_access;
      ascends : dsk_ascend record_access;
      unions : dsk_union record_access;
      visible : visible_record_access;
      families : dsk_family record_access;
      couples : dsk_couple record_access;
      descends : dsk_descend record_access;
      strings : string record_access;
      particles : string list;
      bnotes : notes;
      bdir : string }

  type base_func =
    { person_of_key : string -> string -> int -> iper option;
      persons_of_name : string -> iper list;
      strings_of_fsname : string -> dsk_istr list;
      persons_of_surname : dsk_istr string_person_index;
      persons_of_first_name : dsk_istr string_person_index;
      patch_person : iper -> dsk_person -> unit;
      patch_ascend : iper -> dsk_ascend -> unit;
      patch_union : iper -> dsk_union -> unit;
      patch_family : ifam -> dsk_family -> unit;
      patch_couple : ifam -> dsk_couple -> unit;
      patch_descend : ifam -> dsk_descend -> unit;
      patch_name : string -> iper -> unit;
      insert_string : string -> dsk_istr;
      commit_patches : unit -> unit;
      commit_notes : string -> string -> unit;
      patched_ascends : unit -> iper list;
      is_patched_person : iper -> bool;
      cleanup : unit -> unit }

  type db1 = { data : base_data; func : base_func }

  type name_index_data = Def.iper array array
  type strings_of_fsname = dsk_istr array array

end

open TYPES

(* from dutil.mli *)

val magic_gwb : string
val magic_gwb_iso_8859_1 : string
val table_size : int

val check_magic : in_channel -> unit
val compare_istr_fun : base_data -> dsk_istr -> dsk_istr -> int
val compare_names : base_data -> string -> string -> int

val dsk_person_misc_names :
  db1 -> dsk_person -> (dsk_person -> dsk_title list) -> string list

val poi : db1 -> Def.iper -> dsk_person
val sou : db1 -> dsk_istr -> string
val p_first_name : db1 -> dsk_person -> string
val p_surname : db1 -> dsk_person -> string

(* from database.mli *)

val opendb : string -> TYPES.db1
