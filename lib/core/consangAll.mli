(* Copyright (c) 2006-2007 INRIA *)

val compute : ?verbosity:int -> Geneweb_db.Driver.base -> bool
(** [compute base from_scratch] [?verbosity] may be 0, 1 or 2 (default is 2)
    Compute consanguinity for each person in the base. Return [true] if base has
    been patched, [false] otherwise. *)
