(* $Id: outbase.mli,v 5.3 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Db1disk.TYPES

val output : string -> db1 -> unit
val gen_output : bool -> string -> db1 -> unit
val save_mem : bool ref
