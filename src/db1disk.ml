(* $Id: database.ml,v 5.19 2007-06-06 15:22:35 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def
open Mutil

(* from dbdisk.mli *)

module TYPES = struct

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

  type name_index_data = iper array array
  type strings_of_fsname = dsk_istr array array

end

open TYPES

(* from dutil.ml *)

open Def
open Mutil

let magic_gwb = "GnWb0020"
let magic_gwb_iso_8859_1 = "GnWb001y"
let table_size = 0x3fff

let poi base i = base.data.persons.get (Adef.int_of_iper i)
let aoi base i = base.data.ascends.get (Adef.int_of_iper i)
let uoi base i = base.data.unions.get (Adef.int_of_iper i)
let coi base i = base.data.couples.get (Adef.int_of_ifam i)
let sou base i = base.data.strings.get (Adef.int_of_istr i)

let p_first_name base p = nominative (sou base p.first_name)
let p_surname base p = nominative (sou base p.surname)

let husbands base p =
  let u = uoi base p.key_index in
  List.map
    (fun ifam ->
       let cpl = coi base ifam in
       let husband = poi base (Adef.father cpl) in
       let husband_surname = p_surname base husband in
       let husband_surnames_aliases =
         List.map (sou base) husband.surnames_aliases
       in
       husband_surname, husband_surnames_aliases)
    (Array.to_list u.family)

let father_titles_places base p nobtit =
  match (aoi base p.key_index).parents with
    Some ifam ->
      let cpl = coi base ifam in
      let fath = poi base (Adef.father cpl) in
      List.map (fun t -> sou base t.t_place) (nobtit fath)
  | None -> []

let dsk_person_misc_names base p nobtit =
  let sou = sou base in
  Futil.gen_person_misc_names (sou p.first_name) (sou p.surname)
    (sou p.public_name) (List.map sou p.qualifiers) (List.map sou p.aliases)
    (List.map sou p.first_names_aliases) (List.map sou p.surnames_aliases)
    (List.map (Futil.map_title_strings sou) (nobtit p))
    (if p.sex = Female then husbands base p else [])
    (father_titles_places base p nobtit)

let check_magic ic =
  let b = really_input_string ic (String.length magic_gwb) in
  Mutil.utf_8_db := true;
  if b <> magic_gwb then
    if b = magic_gwb_iso_8859_1 then Mutil.utf_8_db := false
    else if String.sub magic_gwb 0 4 = String.sub b 0 4 then
      failwith "this is a GeneWeb base, but not compatible"
    else failwith "this is not a GeneWeb base, or it is a very old version"

let unaccent =
  function
    '�' | '�' | '�' | '�' | '�' | '�' | '�' -> 'a'
  | '�' -> 'c'
  | '�' | '�' | '�' | '�' -> 'e'
  | '�' | '�' | '�' | '�' -> 'i'
  | '�' -> 'd'
  | '�' -> 'n'
  | '�' | '�' | '�' | '�' | '�' | '�' -> 'o'
  | '�' | '�' | '�' | '�' -> 'u'
  | '�' | '�' -> 'y'
  | '�' -> 'p'
  | c -> c

let compare_names_1 s1 s2 =
  let compare_aux e1 e2 =
    let rec loop i1 i2 =
      if i1 = e1 && i2 = e2 then 0
      else if i1 = e1 then -1
      else if i2 = e2 then 1
      else
        let c1 = unaccent (Char.lowercase s1.[i1]) in
        let c2 = unaccent (Char.lowercase s2.[i2]) in
        match c1, c2 with
          'a'..'z', 'a'..'z' ->
            if c1 < c2 then -1
            else if c1 > c2 then 1
            else loop (i1 + 1) (i2 + 1)
        | 'a'..'z', _ -> 1
        | _, 'a'..'z' -> -1
        | _ -> loop (i1 + 1) (i2 + 1)
    in
    loop
  in
  if s1 = s2 then 0
  else
    let i1 = initial s1 in
    let i2 = initial s2 in
    match compare_aux (String.length s1) (String.length s2) i1 i2 with
      0 -> compare_aux i1 i2 0 0
    | x -> x

let compare_names base_data s1 s2 =
  if !utf_8_db then compare_after_particle base_data.particles s1 s2
  else compare_names_1 s1 s2

let compare_istr_fun base_data is1 is2 =
  if is1 = is2 then 0
  else
    compare_names base_data (base_data.strings.get (Adef.int_of_istr is1))
      (base_data.strings.get (Adef.int_of_istr is2))

(* from database.ml *)

type person = dsk_person
type ascend = dsk_ascend
type union = dsk_union
type family = dsk_family
type couple = dsk_couple
type descend = dsk_descend

(*
 Files in base (directory .gwb)

    base - the base itself
       magic number (magic_gwb)                 : string of length 8
       number of persons                        : binary_int
       number of families                       : binary_int
       number of strings                        : binary_int
       persons array offset in file             : binary_int
       ascends array offset in file             : binary_int
       unions array offset in file              : binary_int
       families array offset in file            : binary_int
       couples array offset in file             : binary_int
       descends array offset in file            : binary_int
       strings array offset in file             : binary_int
       notes origin file                        : value
       persons array                            : value
       ascends array                            : value
       unions array                             : value
       families array                           : value
       couples array                            : value
       descends array                           : value
       strings array                            : value

    base.acc - direct accesses to arrays inside base
       persons offsets   : array of binary_ints
       ascends offsets   : array of binary_ints
       unions offsets    : array of binary_ints
       families offsets  : array of binary_ints
       couples offsets   : array of binary_ints
       descends offsets  : array of binary_ints
       strings offsets   : array of binary_ints

    names.inx - index for names, strings of first names and surnames
       offset to 2nd index : binary_int
       1st index (names) : value
         array, length = "table_size", associating:
          - a hash value of a "crushed" (module "Name") name (modulo length)
          - to the array of indexes of the corresponding persons
       2nd index (first names and surnames strings) : value
         array, length = "table_size", associating:
          - a hash value of the "crushed" (module "Name") first name or
            surname (modulo length)
          - to the array of the corresponding string indexes

    names.acc - direct accesses to arrays inside names.inx

    strings.inx - index for strings, surnames, first names
       length of the strings offset array : binary_int
       offset of surnames index           : binary_int
       offset of first names index        : binary_int
       strings hash table index           : 2 arrays of binary_ints
         strings offset array (length = prime after 10 * strings array length)
           - associating a hash value of the string modulo length
           - to its index in the string array
         strings list array (length = string array length)
           - associating a string index
           - to the index of the next index holding the same hash value
       -- the following table has been obsolete since version 4.10
       -- it has been replaced by snames.inx/sname.dat which use
       -- much less memory
       surnames index                     : value
         binary tree
          - associating the string index of a surname
          - to the corresponding list of persons holding this surname
       -- the following table has been obsolete since version 4.10
       -- it has been replaced by fnames.inx/fname.dat which use
       -- much less memory
       first_names index                  : value
         binary tree
          - associating the string index of a first name
          - to the corresponding list of persons holding this first name

    snames.inx - index for surnames
       binary tree
        - associating the string index of a surname
        - to a pointer (int) to snames.dat

    snames.dat - data associated with snames.inx
      table of list of persons holding a surname

    fnames.inx - index for first names
       binary tree
        - associating the string index of a first name
        - to a pointer (int) to fnames.dat

    fnames.dat - data associated with fnames.inx
      table of list of persons holding a first name

the corresponding list of persons holding this surname

    patches - patches
       When updated, none of the previous files are modified. Only this one
       is written and rewritten. It holds a record of type "patches", composed
       of association lists "index" - "new value".
*)

(* Search index of a given string in file strings.inx *)

let string_piece s =
  let s = String.escaped s in
  if String.length s > 20 then
    String.sub s 0 10 ^ " ... " ^ String.sub s (String.length s - 10) 10
  else s

exception Found of int

let hashtbl_right_assoc s ht =
  try
    Hashtbl.iter (fun i1 s1 -> if s = s1 then raise (Found i1)) ht;
    raise Not_found
  with Found x -> x

let index_of_string strings ic start_pos hash_len string_patches s =
  try Adef.istr_of_int (hashtbl_right_assoc s string_patches) with
    Not_found ->
      match ic, hash_len with
        Some ic, Some hash_len ->
          let ia = Hashtbl.hash s mod hash_len in
          seek_in ic (start_pos + ia * int_size);
          let i1 = input_binary_int ic in
          let rec loop i =
            if i = -1 then raise Not_found
            else if strings.get i = s then Adef.istr_of_int i
            else
              begin
                seek_in ic (start_pos + (hash_len + i) * int_size);
                loop (input_binary_int ic)
              end
          in
          loop i1
      | _ ->
          Printf.eprintf "Sorry. I really need string.inx\n";
          flush stderr;
          failwith "database access"

let initial s =
  let rec loop i =
    if i = String.length s then 0
    else
      match s.[i] with
        'A'..'Z' | '�'..'�' -> i
      | _ -> loop (succ i)
  in
  loop 0

let rec list_remove_elemq x =
  function
    y :: l -> if x = y then l else y :: list_remove_elemq x l
  | [] -> []

(* compatibility with databases created with versions <= 4.09 *)
(* should be removed after some time (when all databases will have
   been rebuilt with version >= 4.10 *)
let old_persons_of_first_name_or_surname base_data strings params =
  let (ic2, start_pos, proj, person_patches, _, _, _) = params in
  let module IstrTree =
    Btree.Make
      (struct type t = dsk_istr let compare = compare_istr_fun base_data end)
  in
  let bt =
    let btr = ref None in
    let completed = ref false in
    let update_bt gistro bt =
      let bt = ref bt in
      Hashtbl.iter
        (fun i p ->
           let istr = proj p in
           if gistro <> None && gistro <> Some istr then ()
           else
             let ipera = try IstrTree.find istr !bt with Not_found -> [] in
             if List.mem (Adef.iper_of_int i) ipera then ()
             else bt := IstrTree.add istr (Adef.iper_of_int i :: ipera) !bt)
        person_patches;
      if gistro = None then completed := true;
      !bt
    in
    fun gistro ->
      match !btr with
        Some bt ->
          if !completed then bt
          else let bt = update_bt gistro bt in btr := Some bt; bt
      | None ->
          match ic2, start_pos with
            Some ic2, Some start_pos ->
              seek_in ic2 start_pos;
              (*
              let ab1 = Gc.allocated_bytes () in
              *)
              let bt : iper list IstrTree.t = input_value ic2 in
              (*
              let ab2 = Gc.allocated_bytes () in
              *)
              let bt = update_bt gistro bt in
              btr := Some bt;
              (*
              Printf.eprintf "*** old database created by version <= 4.09\n"; flush stderr;
              Printf.eprintf "*** using index allocating here %.0f bytes\n"
                (ab2 -. ab1);
              flush stderr;
              *)
              bt
          | _ ->
              Printf.eprintf "Sorry, I really need strings.inx.\n";
              flush stderr;
              failwith "database access"
  in
  let compare = compare_istr_fun base_data in
  let check_patches istr ipl =
    let ipl = ref ipl in
    Hashtbl.iter
      (fun i p ->
         if List.mem (Adef.iper_of_int i) !ipl then
           if compare istr p.first_name = 0 || compare istr p.surname = 0 then
             ()
           else ipl := list_remove_elemq (Adef.iper_of_int i) !ipl)
      person_patches;
    !ipl
  in
  let find istr =
    try check_patches istr (IstrTree.find istr (bt (Some istr))) with
      Not_found -> []
  in
  let cursor str =
    IstrTree.key_after
      (fun key ->
         compare_names base_data str (strings.get (Adef.int_of_istr key)))
      (bt None)
  in
  let next key = IstrTree.next key (bt None) in
  {find = find; cursor = cursor; next = next}

let new_persons_of_first_name_or_surname base_data strings params =
  let (_, _, proj, person_patches, names_inx, names_dat, bname) = params in
  let module IstrTree =
    Btree.Make
      (struct type t = dsk_istr let compare = compare_istr_fun base_data end)
  in
  let fname_dat = Filename.concat bname names_dat in
  let bt =
    let btr = ref None in
    fun () ->
      match !btr with
        Some bt -> bt
      | None ->
          let fname_inx = Filename.concat bname names_inx in
          let ic_inx = Secure.open_in_bin fname_inx in
          (*
          let ab1 = Gc.allocated_bytes () in
          *)
          let bt : int IstrTree.t = input_value ic_inx in
          (*
          let ab2 = Gc.allocated_bytes () in
          Printf.eprintf "*** new database created by version >= 4.10\n";
          Printf.eprintf "*** using index '%s' allocating here only %.0f bytes\n"
            names_inx (ab2 -. ab1);
          flush stderr;
          *)
          close_in ic_inx;
          btr := Some bt;
          bt
  in
  let find istr =
    let ipera =
      try
        let pos = IstrTree.find istr (bt ()) in
        let ic_dat = Secure.open_in_bin fname_dat in
        seek_in ic_dat pos;
        let len = input_binary_int ic_dat in
        let rec read_loop ipera len =
          if len = 0 then ipera
          else
            let iper = Adef.iper_of_int (input_binary_int ic_dat) in
            read_loop (iper :: ipera) (len - 1)
        in
        let ipera = read_loop [] len in close_in ic_dat; ipera
      with Not_found -> []
    in
    let ipera = ref ipera in
    Hashtbl.iter
      (fun i p ->
         let istr1 = proj p in
         if istr1 <> istr then ()
         else if List.mem (Adef.iper_of_int i) !ipera then ()
         else ipera := Adef.iper_of_int i :: !ipera)
      person_patches;
    !ipera
  in
  let bt_patched =
    let btr = ref None in
    fun () ->
      match !btr with
        Some bt -> bt
      | None ->
          let bt = ref (bt ()) in
          Hashtbl.iter
            (fun i p ->
               let istr1 = proj p in
               try let _ = IstrTree.find istr1 !bt in () with
                 Not_found -> bt := IstrTree.add istr1 0 !bt)
            person_patches;
          btr := Some !bt;
          !bt
  in
  let cursor str =
    IstrTree.key_after
      (fun key ->
         compare_names base_data str (strings.get (Adef.int_of_istr key)))
      (bt_patched ())
  in
  let next key = IstrTree.next key (bt_patched ()) in
  {find = find; cursor = cursor; next = next}

let persons_of_first_name_or_surname base_data strings params =
  let (_, _, _, _, names_inx, _, bname) = params in
  if Sys.file_exists (Filename.concat bname names_inx) then
    new_persons_of_first_name_or_surname base_data strings params
  else old_persons_of_first_name_or_surname base_data strings params

(* Search index for a given name in file names.inx *)

let persons_of_name bname patches =
  let t = ref None in
  fun s ->
    let s = Name.crush_lower s in
    let i = Hashtbl.hash s in
    let ai =
      let ic_inx = Secure.open_in_bin (Filename.concat bname "names.inx") in
      let ai =
        let i = i mod table_size in
        let fname_inx_acc = Filename.concat bname "names.acc" in
        if Sys.file_exists fname_inx_acc then
          let ic_inx_acc = Secure.open_in_bin fname_inx_acc in
          seek_in ic_inx_acc (Iovalue.sizeof_long * i);
          let pos = input_binary_int ic_inx_acc in
          close_in ic_inx_acc;
          seek_in ic_inx pos;
          (Iovalue.input ic_inx : iper array)
        else
          let a =
            match !t with
              Some a -> a
            | None ->
                seek_in ic_inx int_size;
                let a : name_index_data = input_value ic_inx in t := Some a; a
          in
          a.(i)
      in
      close_in ic_inx; ai
    in
    try let l = Hashtbl.find patches i in l @ Array.to_list ai with
      Not_found -> Array.to_list ai

let strings_of_fsname bname strings (_, person_patches) =
  let t = ref None in
  fun s ->
    let s = Name.crush_lower s in
    let i = Hashtbl.hash s in
    let r =
      let ic_inx = Secure.open_in_bin (Filename.concat bname "names.inx") in
      let ai =
        let i = i mod table_size in
        let fname_inx_acc = Filename.concat bname "names.acc" in
        if Sys.file_exists fname_inx_acc then
          let ic_inx_acc = Secure.open_in_bin fname_inx_acc in
          seek_in ic_inx_acc (Iovalue.sizeof_long * (table_size + i));
          let pos = input_binary_int ic_inx_acc in
          close_in ic_inx_acc;
          seek_in ic_inx pos;
          (Iovalue.input ic_inx : dsk_istr array)
        else
          let a =
            match !t with
              Some a -> a
            | None ->
                let pos = input_binary_int ic_inx in
                seek_in ic_inx pos;
                let a : strings_of_fsname = input_value ic_inx in
                t := Some a; a
          in
          a.(i)
      in
      close_in ic_inx; ai
    in
    let l = ref (Array.to_list r) in
    Hashtbl.iter
      (fun _ p ->
         if not (List.mem p.first_name !l) then
           begin let s1 = strings.get (Adef.int_of_istr p.first_name) in
             let s1 = nominative s1 in
             if s = Name.crush_lower s1 then l := p.first_name :: !l
           end;
         if not (List.mem p.surname !l) then
           let s1 = strings.get (Adef.int_of_istr p.surname) in
           let s1 = nominative s1 in
           if s = Name.crush_lower s1 then l := p.surname :: !l)
      person_patches;
    !l
(**)

let lock_file bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then
      Filename.chop_suffix bname ".gwb"
    else bname
  in
  bname ^ ".lck"

(* Restrict file *)

type visible_state = VsNone | VsTrue | VsFalse

let make_visible_record_access bname persons =
  let visible_ref = ref None in
  let fname = Filename.concat bname "restrict" in
  let read_or_create_visible () =
    let visible =
      match try Some (Secure.open_in fname) with Sys_error _ -> None with
        Some ic ->
          if !verbose then
            begin Printf.eprintf "*** read restrict file\n"; flush stderr end;
          let visible = input_value ic in close_in ic; visible
      | None -> Array.make persons.len VsNone
    in
    visible_ref := Some visible; visible
  in
  let v_write () =
    match !visible_ref with
      Some visible ->
        begin try
          let oc = Secure.open_out fname in
          if !verbose then
            begin
              Printf.eprintf "*** write restrict file\n";
              flush stderr
            end;
          output_value oc visible;
          close_out oc
        with Sys_error _ -> ()
        end
    | None -> ()
  in
  let v_get fct i =
    let visible =
      match !visible_ref with
        Some visible -> visible
      | None -> read_or_create_visible ()
    in
    if i < Array.length visible then
      match visible.(i) with
        VsNone ->
          let status = fct (persons.get i) in
          visible.(i) <- if status then VsTrue else VsFalse;
          visible_ref := Some visible;
          status
      | VsTrue -> true
      | VsFalse -> false
    else fct (persons.get i)
  in
  {v_write = v_write; v_get = v_get}

(* Input *)

let apply_patches tab f patches plen =
  if plen = 0 then tab
  else
    let new_tab =
      if plen > Array.length tab then
        let new_tab = Array.make plen (Obj.magic 0) in
        Array.blit tab 0 new_tab 0 (Array.length tab); new_tab
      else tab
    in
    Hashtbl.iter (fun i v -> new_tab.(i) <- f v) patches; new_tab

type patches_ht =
  { h_person : int ref * (int, person) Hashtbl.t;
    h_ascend : int ref * (int, ascend) Hashtbl.t;
    h_union : int ref * (int, union) Hashtbl.t;
    h_family : int ref * (int, family) Hashtbl.t;
    h_couple : int ref * (int, couple) Hashtbl.t;
    h_descend : int ref * (int, descend) Hashtbl.t;
    h_string : int ref * (int, string) Hashtbl.t;
    h_name : (int, iper list) Hashtbl.t }

(* Old structure of file "patches", kept for backward compatibility.
   After conversion, a new change will be saved with a magic number
   (magic_patch) and a record "patch_ht" above. *)

module Old =
  struct
    type patches =
      { p_person : (int * person) list ref;
        p_ascend : (int * ascend) list ref;
        p_union : (int * union) list ref;
        p_family : (int * family) list ref;
        p_couple : (int * couple) list ref;
        p_descend : (int * descend) list ref;
        p_string : (int * string) list ref;
        p_name : (int * iper list) list ref }
  end

let phony_person =
  {first_name = 0; surname = 0; occ = 0; image = 0; first_names_aliases = [];
   surnames_aliases = []; public_name = 0; qualifiers = []; aliases = [];
   titles = []; rparents = []; related = []; occupation = 0; sex = Neuter;
   access = IfTitles; birth = Adef.codate_None; birth_place = 0;
   birth_src = 0; baptism = Adef.codate_None; baptism_place = 0;
   baptism_src = 0; death = DontKnowIfDead; death_place = 0; death_src = 0;
   burial = UnknownBurial; burial_place = 0; burial_src = 0; notes = 0;
   psources = 0; key_index = Adef.iper_of_int 0}

let phony_family =
  {marriage = Adef.codate_None; marriage_place = 0; marriage_src = 0;
   witnesses = [| |]; relation = Married; divorce = NotDivorced; comment = 0;
   origin_file = 0; fsources = 0; fam_index = Adef.ifam_of_int 0}

let ext phony v =
  let rlen = Array.length (Obj.magic v) in
  let alen = Array.length (Obj.magic phony) in
  if rlen = alen then v
  else if rlen < alen then
    let x = Array.copy (Obj.magic phony) in
    Array.blit (Obj.magic v) 0 x 0 rlen; Obj.magic x
  else failwith "this is a GeneWeb base, but not compatible; please upgrade"

let array_ext phony fa =
  let a = Obj.magic fa in
  if Array.length a = 0 then fa
  else
    let rlen = Array.length a.(0) in
    let alen = Array.length (Obj.magic phony) in
    if rlen = alen then fa
    else if rlen < alen then
      begin
        if !verbose then
          begin
            Printf.eprintf "*** extending records from size %d to size %d\n"
              rlen alen;
            flush stderr
          end;
        for i = 0 to Array.length a - 1 do
          let x = Array.copy (Obj.magic phony) in
          Array.blit a.(i) 0 x 0 rlen; a.(i) <- x
        done;
        fa
      end
    else failwith "this is a GeneWeb base, but not compatible; please upgrade"

let make_record_access ic ic_acc shift array_pos (plenr, patches) len name
    input_array input_item =
  let v_ext v =
    if name = "persons" then ext phony_person v
    else if name = "families" then ext phony_family v
    else v
  in
  let v_arr_ext v =
    if name = "persons" then array_ext phony_person v
    else if name = "families" then array_ext phony_family v
    else v
  in
  let tab = ref None in
  let cleared = ref false in
  let gen_get i =
    match !tab with
      Some x -> x.(i)
    | None ->
        try let v = Hashtbl.find patches i in v_ext v with
          Not_found ->
            if i < 0 || i >= len then
              failwith
                ("access " ^ name ^ " out of bounds; i = " ^ string_of_int i)
            else
              match ic_acc with
                Some ic_acc ->
                  seek_in ic_acc (shift + Iovalue.sizeof_long * i);
                  let pos = input_binary_int ic_acc in
                  seek_in ic pos; let v = input_item ic in v_ext v
              | None ->
                  Printf.eprintf "Sorry; I really need base.acc\n";
                  flush stderr;
                  failwith "cannot access database"
  in
  let rec array () =
    match !tab with
      Some x -> x
    | None ->
        if !verbose then
          begin
            Printf.eprintf "*** read %s%s\n" name
              (if !cleared then " (again)" else "");
            flush stderr
          end;
        seek_in ic array_pos;
        let v = input_array ic in
        let v = v_arr_ext v in
        let t = apply_patches v v_ext patches r.len in tab := Some t; t
  and r =
    {load_array = (fun () -> let _ = array () in ()); get = gen_get;
     set = (fun i v -> (array ()).(i) <- v); len = max len !plenr;
     output_array =
       (fun oc -> output_value_no_sharing oc (array () : _ array));
     clear_array = fun () -> cleared := true; tab := None}
  in
  r

let magic_patch = "GnPa0001"
let check_patch_magic ic =
  really_input_string ic (String.length magic_patch) = magic_patch

let input_patches bname =
  match
    try Some (Secure.open_in_bin (Filename.concat bname "patches")) with
      _ -> None
  with
    Some ic ->
      let r =
        if check_patch_magic ic then (input_value ic : patches_ht)
        else
          begin
            (* old implementation of patches *)
            seek_in ic 0;
            let patches : Old.patches = input_value ic in
            let ht =
              {h_person = ref 0, Hashtbl.create 1;
               h_ascend = ref 0, Hashtbl.create 1;
               h_union = ref 0, Hashtbl.create 1;
               h_family = ref 0, Hashtbl.create 1;
               h_couple = ref 0, Hashtbl.create 1;
               h_descend = ref 0, Hashtbl.create 1;
               h_string = ref 0, Hashtbl.create 1; h_name = Hashtbl.create 1}
            in
            let add (ir, ht) (k, v) =
              if k >= !ir then ir := k + 1; Hashtbl.add ht k v
            in
            List.iter (add ht.h_person) !(patches.Old.p_person);
            List.iter (add ht.h_ascend) !(patches.Old.p_ascend);
            List.iter (add ht.h_union) !(patches.Old.p_union);
            List.iter (add ht.h_family) !(patches.Old.p_family);
            List.iter (add ht.h_couple) !(patches.Old.p_couple);
            List.iter (add ht.h_descend) !(patches.Old.p_descend);
            List.iter (add ht.h_string) !(patches.Old.p_string);
            List.iter (add (ref 0, ht.h_name)) !(patches.Old.p_name);
            ht
          end
      in
      close_in ic; r
  | None ->
      {h_person = ref 0, Hashtbl.create 1; h_ascend = ref 0, Hashtbl.create 1;
       h_union = ref 0, Hashtbl.create 1; h_family = ref 0, Hashtbl.create 1;
       h_couple = ref 0, Hashtbl.create 1;
       h_descend = ref 0, Hashtbl.create 1;
       h_string = ref 0, Hashtbl.create 1; h_name = Hashtbl.create 1}

let person_of_key persons strings persons_of_name first_name surname occ =
  if first_name = "?" || surname = "?" then None
  else
    let first_name = nominative first_name in
    let surname = nominative surname in
    let ipl = persons_of_name (first_name ^ " " ^ surname) in
    let first_name = Name.lower first_name in
    let surname = Name.lower surname in
    let rec find =
      function
        ip :: ipl ->
          let p = persons.get (Adef.int_of_iper ip) in
          if occ = p.occ &&
             first_name =
               Name.lower (strings.get (Adef.int_of_istr p.first_name)) &&
             surname = Name.lower (strings.get (Adef.int_of_istr p.surname))
          then
            Some ip
          else find ipl
      | _ -> None
    in
    find ipl

let opendb bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let patches = input_patches bname in
  let particles =
    Mutil.input_particles (Filename.concat bname "particles.txt")
  in
  let ic =
    let ic = Secure.open_in_bin (Filename.concat bname "base") in
    check_magic ic; ic
  in
  let persons_len = input_binary_int ic in
  let families_len = input_binary_int ic in
  let strings_len = input_binary_int ic in
  let persons_array_pos = input_binary_int ic in
  let ascends_array_pos = input_binary_int ic in
  let unions_array_pos = input_binary_int ic in
  let families_array_pos = input_binary_int ic in
  let couples_array_pos = input_binary_int ic in
  let descends_array_pos = input_binary_int ic in
  let strings_array_pos = input_binary_int ic in
  let norigin_file = input_value ic in
  let ic_acc =
    try Some (Secure.open_in_bin (Filename.concat bname "base.acc")) with
      Sys_error _ ->
        Printf.eprintf "File base.acc not found; trying to continue...\n";
        flush stderr;
        None
  in
  let ic2 =
    try Some (Secure.open_in_bin (Filename.concat bname "strings.inx")) with
      Sys_error _ ->
        Printf.eprintf "File strings.inx not found; trying to continue...\n";
        flush stderr;
        None
  in
  let ic2_string_start_pos = 3 * int_size in
  let ic2_string_hash_len =
    match ic2 with
      Some ic2 -> Some (input_binary_int ic2)
    | None -> None
  in
  let ic2_surname_start_pos =
    match ic2 with
      Some ic2 -> Some (input_binary_int ic2)
    | None -> None
  in
  let ic2_first_name_start_pos =
    match ic2 with
      Some ic2 -> Some (input_binary_int ic2)
    | None -> None
  in
  let shift = 0 in
  let persons =
    make_record_access ic ic_acc shift persons_array_pos patches.h_person
      persons_len "persons" (input_value : _ -> person array)
      (Iovalue.input : _ -> person)
  in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let ascends =
    make_record_access ic ic_acc shift ascends_array_pos patches.h_ascend
      persons_len "ascends" (input_value : _ -> ascend array)
      (Iovalue.input : _ -> ascend)
  in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let unions =
    make_record_access ic ic_acc shift unions_array_pos patches.h_union
      persons_len "unions" (input_value : _ -> union array)
      (Iovalue.input : _ -> union)
  in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let families =
    make_record_access ic ic_acc shift families_array_pos patches.h_family
      families_len "families" (input_value : _ -> family array)
      (Iovalue.input : _ -> family)
  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let couples =
    make_record_access ic ic_acc shift couples_array_pos patches.h_couple
      families_len "couples" (input_value : _ -> couple array)
      (Iovalue.input : _ -> couple)
  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let descends =
    make_record_access ic ic_acc shift descends_array_pos patches.h_descend
      families_len "descends" (input_value : _ -> descend array)
      (Iovalue.input : _ -> descend)
  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let strings =
    make_record_access ic ic_acc shift strings_array_pos patches.h_string
      strings_len "strings" (input_value : _ -> string array)
      (Iovalue.input : _ -> string)
  in
  let cleanup_ref =
    ref
      (fun () ->
         close_in ic;
         begin match ic_acc with
           Some ic_acc -> close_in ic_acc
         | None -> ()
         end;
         match ic2 with
           Some ic2 -> close_in ic2
         | None -> ())
  in
  let cleanup () = !cleanup_ref () in
  let commit_patches () =
    let tmp_fname = Filename.concat bname "1patches" in
    let fname = Filename.concat bname "patches" in
    let oc9 =
      try Secure.open_out_bin tmp_fname with
        Sys_error _ ->
          raise (Adef.Request_failure "the database is not writable")
    in
    output_string oc9 magic_patch;
    output_value_no_sharing oc9 (patches : patches_ht);
    close_out oc9;
    remove_file (fname ^ "~");
    (try Sys.rename fname (fname ^ "~") with Sys_error _ -> ());
    try Sys.rename tmp_fname fname with Sys_error _ -> ()
  in
  let patched_ascends () =
    let r = ref [] in
    Hashtbl.iter (fun i _ -> r := Adef.iper_of_int i :: !r)
      (snd patches.h_ascend);
    !r
  in
  let is_patched_person ip =
    Hashtbl.mem (snd patches.h_person) (Adef.int_of_iper ip)
  in
  let patch_person i p =
    let i = Adef.int_of_iper i in
    persons.len <- max persons.len (i + 1);
    fst patches.h_person := persons.len;
    Hashtbl.replace (snd patches.h_person) i p
  in
  let patch_ascend i a =
    let i = Adef.int_of_iper i in
    ascends.len <- max ascends.len (i + 1);
    fst patches.h_ascend := ascends.len;
    Hashtbl.replace (snd patches.h_ascend) i a
  in
  let patch_union i a =
    let i = Adef.int_of_iper i in
    unions.len <- max unions.len (i + 1);
    fst patches.h_union := ascends.len;
    Hashtbl.replace (snd patches.h_union) i a
  in
  let patch_family i f =
    let i = Adef.int_of_ifam i in
    families.len <- max families.len (i + 1);
    fst patches.h_family := families.len;
    Hashtbl.replace (snd patches.h_family) i f
  in
  let patch_couple i c =
    let i = Adef.int_of_ifam i in
    couples.len <- max couples.len (i + 1);
    fst patches.h_couple := couples.len;
    Hashtbl.replace (snd patches.h_couple) i c
  in
  let patch_descend i c =
    let i = Adef.int_of_ifam i in
    descends.len <- max descends.len (i + 1);
    fst patches.h_descend := descends.len;
    Hashtbl.replace (snd patches.h_descend) i c
  in
  let index_of_string =
    index_of_string strings ic2 ic2_string_start_pos ic2_string_hash_len
      (snd patches.h_string)
  in
  let insert_string s =
    try index_of_string s with
      Not_found ->
        let i = strings.len in
        strings.len <- max strings.len (i + 1);
        fst patches.h_string := strings.len;
        Hashtbl.replace (snd patches.h_string) i s;
        Adef.istr_of_int i
  in
  let patch_name s ip =
    let s = Name.crush_lower s in
    let i = Hashtbl.hash s in
    try
      let ipl = Hashtbl.find patches.h_name i in
      if List.mem ip ipl then ()
      else Hashtbl.replace patches.h_name i (ip :: ipl)
    with Not_found -> Hashtbl.add patches.h_name i [ip]
  in
  let read_notes fnotes rn_mode =
    let fname =
      if fnotes = "" then "notes"
      else Filename.concat "notes_d" (fnotes ^ ".txt")
    in
    match
      try Some (Secure.open_in (Filename.concat bname fname)) with
        Sys_error _ -> None
    with
      Some ic ->
        let str =
          match rn_mode with
            RnDeg -> if in_channel_length ic = 0 then "" else " "
          | Rn1Ln -> (try input_line ic with End_of_file -> "")
          | RnAll ->
              let rec loop len =
                match try Some (input_char ic) with End_of_file -> None with
                  Some c -> loop (Buff.store len c)
                | _ -> Buff.get len
              in
              loop 0
        in
        close_in ic; str
    | None -> ""
  in
  let commit_notes fnotes s =
    let fname =
      if fnotes = "" then "notes"
      else
        begin
          begin try Unix.mkdir (Filename.concat bname "notes_d") 0o755 with
            _ -> ()
          end;
          Filename.concat "notes_d" (fnotes ^ ".txt")
        end
    in
    let fname = Filename.concat bname fname in
    (try Sys.remove (fname ^ "~") with Sys_error _ -> ());
    (try Sys.rename fname (fname ^ "~") with _ -> ());
    if s = "" then ()
    else
      let oc = Secure.open_out fname in output_string oc s; close_out oc; ()
  in
  let ext_files () =
    let top = Filename.concat bname "notes_d" in
    let rec loop list subdir =
      let dir = Filename.concat top subdir in
      match try Some (Sys.readdir dir) with Sys_error _ -> None with
        Some files ->
          List.fold_left
            (fun files file ->
               let f = Filename.concat subdir file in
               if Filename.check_suffix f ".txt" then
                 Filename.chop_suffix f ".txt" :: files
               else loop files f)
            list (Array.to_list files)
      | None -> list
    in
    loop [] Filename.current_dir_name
  in
  let bnotes =
    {nread = read_notes; norigin_file = norigin_file; efiles = ext_files}
  in
  let base_data =
    {persons = persons; ascends = ascends; unions = unions;
     visible = make_visible_record_access bname persons; families = families;
     couples = couples; descends = descends; strings = strings;
     particles = particles; bnotes = bnotes; bdir = bname}
  in
  let persons_of_name = persons_of_name bname patches.h_name in
  let base_func =
    {person_of_key = person_of_key persons strings persons_of_name;
     persons_of_name = persons_of_name;
     strings_of_fsname = strings_of_fsname bname strings patches.h_person;
     persons_of_surname =
       persons_of_first_name_or_surname base_data strings
         (ic2, ic2_surname_start_pos, (fun p -> p.surname),
          snd patches.h_person, "snames.inx", "snames.dat", bname);
     persons_of_first_name =
       persons_of_first_name_or_surname base_data strings
         (ic2, ic2_first_name_start_pos, (fun p -> p.first_name),
          snd patches.h_person, "fnames.inx", "fnames.dat", bname);
     patch_person = patch_person; patch_ascend = patch_ascend;
     patch_union = patch_union; patch_family = patch_family;
     patch_couple = patch_couple; patch_descend = patch_descend;
     patch_name = patch_name; insert_string = insert_string;
     commit_patches = commit_patches; patched_ascends = patched_ascends;
     is_patched_person = is_patched_person; commit_notes = commit_notes;
     cleanup = cleanup}
  in
  {data = base_data; func = base_func}
