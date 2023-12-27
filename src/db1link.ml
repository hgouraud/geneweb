(* $Id: db1link.ml,v 5.3 2010-09-23 16:56:51 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Gwcomp
open Printf
open Db1disk.TYPES
open Def
open Mutil

let default_source = ref ""
let do_check = ref true
let do_consang = ref false
let pr_stats = ref false

type file_info =
  { mutable f_curr_src_file : string;
    mutable f_curr_gwo_file : string;
    mutable f_separate : bool;
    mutable f_shift : int;
    mutable f_local_names : (int * int, iper) Hashtbl.t }

type person = dsk_person
type ascend = dsk_ascend
type union = dsk_union
type family = dsk_family
type couple = dsk_couple
type descend = dsk_descend

type ('person, 'string) gen_min_person =
  { mutable m_first_name : 'string;
    mutable m_surname : 'string;
    mutable m_occ : int;
    mutable m_rparents : ('person, 'string) gen_relation list;
    mutable m_related : iper list;
    mutable m_sex : sex;
    mutable m_notes : 'string }

type min_person = (iper, dsk_istr) gen_min_person

type cbase =
  { mutable c_persons : min_person array;
    mutable c_ascends : ascend array;
    mutable c_unions : union array;
    mutable c_couples : couple array;
    mutable c_descends : descend array;
    mutable c_strings : string array;
    mutable c_bnotes : notes }

type gen =
  { mutable g_strings : (string, dsk_istr) Hashtbl.t;
    mutable g_names : (int, iper) Hashtbl.t;
    mutable g_pcnt : int;
    mutable g_fcnt : int;
    mutable g_scnt : int;
    g_file_info : file_info;
    g_base : cbase;
    mutable g_wiznotes : (string * string) list;
    g_patch_p : (int, person) Hashtbl.t;
    mutable g_def : bool array;
    g_first_av_occ : (string * string, int) Hashtbl.t;
    mutable g_errored : bool;
    g_per_index : out_channel;
    g_per : out_channel;
    g_fam_index : out_channel;
    g_fam : out_channel }

let check_error gen = gen.g_errored <- true

let set_error base gen x =
  printf "\nError: "; Check.print_base_error stdout base x; check_error gen

let set_warning base =
  function
    UndefinedSex _ -> ()
  | x -> printf "\nWarning: "; Check.print_base_warning stdout base x

let poi base i = base.c_persons.(Adef.int_of_iper i)
let aoi base i = base.c_ascends.(Adef.int_of_iper i)
let uoi base i = base.c_unions.(Adef.int_of_iper i)
let coi base i = base.c_couples.(Adef.int_of_ifam i)
let sou base i = base.c_strings.(Adef.int_of_istr i)
let p_first_name base p = nominative (sou base p.m_first_name)
let p_surname base p = nominative (sou base p.m_surname)
let designation base p =
  let prenom = p_first_name base p in
  let nom = p_surname base p in
  prenom ^ "." ^ string_of_int p.m_occ ^ " " ^ nom

(*
value output_item_value oc v =
  Marshal.to_channel oc v [Marshal.No_sharing]
;
value input_item_value ic =
  input_value ic
;
*)
let output_item_value = Iovalue.output
let input_item_value = Iovalue.input
(**)

let no_string gen = ""

let unique_string gen x =
  try Hashtbl.find gen.g_strings x with
    Not_found ->
      if gen.g_scnt = Array.length gen.g_base.c_strings then
        begin let arr = gen.g_base.c_strings in
          let new_size = 2 * Array.length arr + 1 in
          let new_arr = Array.make new_size (no_string gen) in
          Array.blit arr 0 new_arr 0 (Array.length arr);
          gen.g_base.c_strings <- new_arr
        end;
      let u = Adef.istr_of_int gen.g_scnt in
      gen.g_base.c_strings.(gen.g_scnt) <- x;
      gen.g_scnt <- gen.g_scnt + 1;
      Hashtbl.add gen.g_strings x u;
      u

let no_family gen =
  let _ = unique_string gen "" in
  let cpl = Adef.couple (Adef.iper_of_int 0) (Adef.iper_of_int 0)
  and des = {children = [| |]} in
  cpl, des

let make_person gen p n occ =
  let empty_string = unique_string gen "" in
  let p =
    {m_first_name = unique_string gen p; m_surname = unique_string gen n;
     m_occ = occ; m_rparents = []; m_related = []; m_sex = Neuter;
     m_notes = empty_string}
  and a = {parents = None; consang = Adef.fix (-1)}
  and u = {family = [| |]} in
  p, a, u

let no_person gen = make_person gen "" "" 0

let new_iper gen =
  if gen.g_pcnt = Array.length gen.g_base.c_persons then
    let per_arr = gen.g_base.c_persons in
    let asc_arr = gen.g_base.c_ascends in
    let uni_arr = gen.g_base.c_unions in
    let new_size = 2 * Array.length per_arr + 1 in
    let (phony_per, phony_asc, phony_uni) = no_person gen in
    let new_per_arr = Array.make new_size phony_per in
    let new_asc_arr = Array.make new_size phony_asc in
    let new_uni_arr = Array.make new_size phony_uni in
    let new_def = Array.make new_size false in
    Array.blit per_arr 0 new_per_arr 0 (Array.length per_arr);
    gen.g_base.c_persons <- new_per_arr;
    Array.blit asc_arr 0 new_asc_arr 0 (Array.length asc_arr);
    gen.g_base.c_ascends <- new_asc_arr;
    Array.blit uni_arr 0 new_uni_arr 0 (Array.length uni_arr);
    gen.g_base.c_unions <- new_uni_arr;
    Array.blit gen.g_def 0 new_def 0 (Array.length gen.g_def);
    gen.g_def <- new_def

let new_ifam gen =
  if gen.g_fcnt = Array.length gen.g_base.c_couples then
    let cpl_arr = gen.g_base.c_couples in
    let des_arr = gen.g_base.c_descends in
    let new_size = 2 * Array.length cpl_arr + 1 in
    let (phony_cpl, phony_des) = no_family gen in
    let new_cpl_arr = Array.make new_size phony_cpl in
    let new_des_arr = Array.make new_size phony_des in
    Array.blit cpl_arr 0 new_cpl_arr 0 (Array.length cpl_arr);
    gen.g_base.c_couples <- new_cpl_arr;
    Array.blit des_arr 0 new_des_arr 0 (Array.length des_arr);
    gen.g_base.c_descends <- new_des_arr

let title_name_unique_string gen =
  function
    Tmain -> Tmain
  | Tname n -> Tname (unique_string gen n)
  | Tnone -> Tnone

let title_unique_string gen t =
  {t_name = title_name_unique_string gen t.t_name;
   t_ident = unique_string gen t.t_ident;
   t_place = unique_string gen t.t_place; t_date_start = t.t_date_start;
   t_date_end = t.t_date_end; t_nth = t.t_nth}

let person_hash first_name surname =
  let first_name = nominative first_name in
  let surname = nominative surname in
  let s = Name.crush_lower (first_name ^ " " ^ surname) in Hashtbl.hash s

let find_person_by_global_name gen first_name surname occ =
  let first_name = nominative first_name in
  let surname = nominative surname in
  let s = Name.crush_lower (first_name ^ " " ^ surname) in
  let key = Hashtbl.hash s in
  let ipl = Hashtbl.find_all gen.g_names key in
  let first_name = Name.lower first_name in
  let surname = Name.lower surname in
  let rec loop =
    function
      [] -> raise Not_found
    | ip :: ipl ->
        let p = poi gen.g_base ip in
        if p.m_occ = occ &&
           Name.lower (p_first_name gen.g_base p) = first_name &&
           Name.lower (p_surname gen.g_base p) = surname
        then
          ip
        else loop ipl
  in
  loop ipl

let find_person_by_local_name gen first_name surname occ =
  let first_name = nominative first_name in
  let surname = nominative surname in
  let s = Name.crush_lower (first_name ^ " " ^ surname) in
  let key = Hashtbl.hash s in
  let ipl = Hashtbl.find_all gen.g_file_info.f_local_names (key, occ) in
  let first_name = Name.lower first_name in
  let surname = Name.lower surname in
  let rec loop =
    function
      [] -> raise Not_found
    | ip :: ipl ->
        let p = poi gen.g_base ip in
        if Name.lower (p_first_name gen.g_base p) = first_name &&
           Name.lower (p_surname gen.g_base p) = surname
        then
          ip
        else loop ipl
  in
  loop ipl

let find_person_by_name gen first_name surname occ =
  if gen.g_file_info.f_separate then
    find_person_by_local_name gen first_name surname occ
  else find_person_by_global_name gen first_name surname occ

let add_person_by_name gen first_name surname occ iper =
  let s = Name.crush_lower (nominative (first_name ^ " " ^ surname)) in
  let key = Hashtbl.hash s in Hashtbl.add gen.g_names key iper

let find_first_available_occ gen fn sn occ =
  let occ =
    try max occ (Hashtbl.find gen.g_first_av_occ (fn, sn)) with
      Not_found -> occ
  in
  let rec loop occ =
    match
      try Some (find_person_by_global_name gen fn sn occ) with
        Not_found -> None
    with
      Some _ -> loop (occ + 1)
    | None -> Hashtbl.add gen.g_first_av_occ (fn, sn) occ; occ
  in
  loop occ

let insert_undefined gen key =
  let occ = key.pk_occ + gen.g_file_info.f_shift in
  let (x, ip) =
    try
      if key.pk_first_name = "?" || key.pk_surname = "?" then raise Not_found
      else
        let ip =
          find_person_by_name gen key.pk_first_name key.pk_surname occ
        in
        poi gen.g_base ip, ip
    with Not_found ->
      let new_occ =
        if gen.g_file_info.f_separate && key.pk_first_name <> "?" &&
           key.pk_surname <> "?"
        then
          find_first_available_occ gen key.pk_first_name key.pk_surname occ
        else occ
      in
      let i = gen.g_pcnt in
      let (x, a, u) =
        make_person gen key.pk_first_name key.pk_surname new_occ
      in
      if key.pk_first_name <> "?" && key.pk_surname <> "?" then
        add_person_by_name gen key.pk_first_name key.pk_surname new_occ
          (Adef.iper_of_int i);
      new_iper gen;
      gen.g_base.c_persons.(i) <- x;
      gen.g_base.c_ascends.(i) <- a;
      gen.g_base.c_unions.(i) <- u;
      gen.g_pcnt <- gen.g_pcnt + 1;
      if key.pk_first_name <> "?" && key.pk_surname <> "?" then
        begin let h = person_hash key.pk_first_name key.pk_surname in
          Hashtbl.add gen.g_file_info.f_local_names (h, occ)
            (Adef.iper_of_int i)
        end;
      seek_out gen.g_per_index (Iovalue.sizeof_long * i);
      output_binary_int gen.g_per_index (pos_out gen.g_per);
      output_char gen.g_per 'U';
      x, Adef.iper_of_int i
  in
  if not gen.g_errored then
    if sou gen.g_base x.m_first_name <> key.pk_first_name ||
       sou gen.g_base x.m_surname <> key.pk_surname
    then
      begin
        printf "\nPerson defined with two spellings:\n";
        printf "  \"%s%s %s\"\n" key.pk_first_name
          (match x.m_occ with
             0 -> ""
           | n -> "." ^ string_of_int n)
          key.pk_surname;
        printf "  \"%s%s %s\"\n" (p_first_name gen.g_base x)
          (match occ with
             0 -> ""
           | n -> "." ^ string_of_int n)
          (p_surname gen.g_base x);
        gen.g_def.(Adef.int_of_iper ip) <- true;
        check_error gen
      end;
  x, ip

let insert_person gen so =
  let occ = so.occ + gen.g_file_info.f_shift in
  let (x, ip) =
    try
      if so.first_name = "?" || so.surname = "?" then raise Not_found
      else
        let ip = find_person_by_name gen so.first_name so.surname occ in
        poi gen.g_base ip, ip
    with Not_found ->
      let new_occ =
        if gen.g_file_info.f_separate && so.first_name <> "?" &&
           so.surname <> "?"
        then
          find_first_available_occ gen so.first_name so.surname occ
        else occ
      in
      let i = gen.g_pcnt in
      let (x, a, u) = make_person gen so.first_name so.surname new_occ in
      if so.first_name <> "?" && so.surname <> "?" then
        add_person_by_name gen so.first_name so.surname new_occ
          (Adef.iper_of_int i);
      new_iper gen;
      gen.g_base.c_persons.(i) <- x;
      gen.g_base.c_ascends.(i) <- a;
      gen.g_base.c_unions.(i) <- u;
      gen.g_pcnt <- gen.g_pcnt + 1;
      if so.first_name <> "?" && so.surname <> "?" then
        begin let h = person_hash so.first_name so.surname in
          Hashtbl.add gen.g_file_info.f_local_names (h, occ)
            (Adef.iper_of_int i)
        end;
      x, Adef.iper_of_int i
  in
  if gen.g_def.(Adef.int_of_iper ip) then
    begin
      printf "\nPerson already defined: \"%s%s %s\"\n" so.first_name
        (match x.m_occ with
           0 -> ""
         | n -> "." ^ string_of_int n)
        so.surname;
      if p_first_name gen.g_base x <> so.first_name ||
         p_surname gen.g_base x <> so.surname
      then
        printf "as name: \"%s%s %s\"\n" (p_first_name gen.g_base x)
          (match occ with
             0 -> ""
           | n -> "." ^ string_of_int n)
          (p_surname gen.g_base x);
      flush stdout;
      check_error gen
    end
  else gen.g_def.(Adef.int_of_iper ip) <- true;
  if not gen.g_errored then
    if sou gen.g_base x.m_first_name <> so.first_name ||
       sou gen.g_base x.m_surname <> so.surname
    then
      begin
        printf "\nPerson defined with two spellings:\n";
        printf "  \"%s%s %s\"\n" so.first_name
          (match x.m_occ with
             0 -> ""
           | n -> "." ^ string_of_int n)
          so.surname;
        printf "  \"%s%s %s\"\n" (p_first_name gen.g_base x)
          (match occ with
             0 -> ""
           | n -> "." ^ string_of_int n)
          (p_surname gen.g_base x);
        gen.g_def.(Adef.int_of_iper ip) <- true;
        check_error gen
      end;
  if not gen.g_errored then
    begin let empty_string = unique_string gen "" in
      let x =
        {first_name = empty_string; surname = empty_string; occ = 0;
         image = unique_string gen so.image;
         first_names_aliases =
           List.map (unique_string gen) so.first_names_aliases;
         surnames_aliases = List.map (unique_string gen) so.surnames_aliases;
         public_name = unique_string gen so.public_name;
         qualifiers = List.map (unique_string gen) so.qualifiers;
         aliases = List.map (unique_string gen) so.aliases;
         titles = List.map (title_unique_string gen) so.titles; rparents = [];
         related = []; occupation = unique_string gen so.occupation;
         sex = Neuter; access = so.access; birth = so.birth;
         birth_place = unique_string gen so.birth_place;
         birth_src = unique_string gen so.birth_src; baptism = so.baptism;
         baptism_place = unique_string gen so.baptism_place;
         baptism_src = unique_string gen so.baptism_src; death = so.death;
         death_place = unique_string gen so.death_place;
         death_src = unique_string gen so.death_src; burial = so.burial;
         burial_place = unique_string gen so.burial_place;
         burial_src = unique_string gen so.burial_src; notes = empty_string;
         psources =
           unique_string gen
             (if so.psources = "" then !default_source else so.psources);
         key_index = ip}
      in
      seek_out gen.g_per_index (Iovalue.sizeof_long * Adef.int_of_iper ip);
      output_binary_int gen.g_per_index (pos_out gen.g_per);
      output_char gen.g_per 'D';
      output_item_value gen.g_per (x : person)
    end;
  x, ip

let insert_somebody gen =
  function
    Undefined key -> insert_undefined gen key
  | Defined so -> insert_person gen so

let check_parents_not_already_defined gen ix fath moth =
  let x = poi gen.g_base ix in
  match (aoi gen.g_base ix).parents with
    Some ifam ->
      let cpl = coi gen.g_base ifam in
      let p = Adef.father cpl in
      let m = Adef.mother cpl in
      printf "
I cannot add \"%s\", child of
    - \"%s\"
    - \"%s\",
because this persons still exists as child of
    - \"%s\"
    - \"%s\".
"
        (designation gen.g_base x) (designation gen.g_base fath)
        (designation gen.g_base moth)
        (designation gen.g_base (poi gen.g_base p))
        (designation gen.g_base (poi gen.g_base m));
      flush stdout;
      (*
              x.birth := Adef.codate_None;
              x.death := DontKnowIfDead;
      *)
      check_error gen
  | _ -> ()

let notice_sex gen p s =
  if p.m_sex = Neuter then p.m_sex <- s
  else if p.m_sex = s || s = Neuter then ()
  else
    begin
      printf "\nInconcistency about the sex of\n  %s %s\n"
        (p_first_name gen.g_base p) (p_surname gen.g_base p);
      check_error gen
    end

let insert_family gen co fath_sex moth_sex witl fo deo =
  let (fath, ifath) = insert_somebody gen (Adef.father co) in
  let (moth, imoth) = insert_somebody gen (Adef.mother co) in
  let witl =
    List.map
      (fun (wit, sex) ->
         let (p, ip) = insert_somebody gen wit in
         notice_sex gen p sex; p.m_related <- ifath :: p.m_related; ip)
      witl
  in
  let children =
    Array.map
      (fun key ->
         let (e, ie) = insert_person gen key in notice_sex gen e key.sex; ie)
      deo.children
  in
  let comment = unique_string gen fo.comment in
  let fsources =
    unique_string gen
      (if fo.fsources = "" then !default_source else fo.fsources)
  in
  new_ifam gen;
  let i = gen.g_fcnt in
  let fam =
    {marriage = fo.marriage;
     marriage_place = unique_string gen fo.marriage_place;
     marriage_src = unique_string gen fo.marriage_src;
     witnesses = Array.of_list witl; relation = fo.relation;
     divorce = fo.divorce; comment = comment;
     origin_file = unique_string gen fo.origin_file; fsources = fsources;
     fam_index = Adef.ifam_of_int i}
  and cpl = Adef.couple ifath imoth
  and des = {children = children} in
  let fath_uni = uoi gen.g_base ifath in
  let moth_uni = uoi gen.g_base imoth in
  seek_out gen.g_fam_index (Iovalue.sizeof_long * i);
  output_binary_int gen.g_fam_index (pos_out gen.g_fam);
  output_item_value gen.g_fam (fam : family);
  gen.g_base.c_couples.(gen.g_fcnt) <- cpl;
  gen.g_base.c_descends.(gen.g_fcnt) <- des;
  gen.g_fcnt <- gen.g_fcnt + 1;
  let fath_uni =
    {family = Array.append fath_uni.family [| Adef.ifam_of_int i |]}
  in
  gen.g_base.c_unions.(Adef.int_of_iper ifath) <- fath_uni;
  let moth_uni =
    {family = Array.append moth_uni.family [| Adef.ifam_of_int i |]}
  in
  gen.g_base.c_unions.(Adef.int_of_iper imoth) <- moth_uni;
  notice_sex gen fath fath_sex;
  notice_sex gen moth moth_sex;
  Array.iter
    (fun ix ->
       let a = gen.g_base.c_ascends.(Adef.int_of_iper ix) in
       check_parents_not_already_defined gen ix fath moth;
       let a = {a with parents = Some (Adef.ifam_of_int i)} in
       gen.g_base.c_ascends.(Adef.int_of_iper ix) <- a)
    children

let insert_notes fname gen key str =
  let occ = key.pk_occ + gen.g_file_info.f_shift in
  match
    try
      Some (find_person_by_name gen key.pk_first_name key.pk_surname occ)
    with Not_found -> None
  with
    Some ip ->
      let p = poi gen.g_base ip in
      if sou gen.g_base p.m_notes <> "" then
        begin
          printf "\nFile \"%s\"\n" fname;
          printf "Notes already defined for \"%s%s %s\"\n" key.pk_first_name
            (if occ = 0 then "" else "." ^ string_of_int occ) key.pk_surname;
          check_error gen
        end
      else p.m_notes <- unique_string gen str
  | None ->
      printf "File \"%s\"\n" fname;
      printf "*** warning: undefined person: \"%s%s %s\"\n" key.pk_first_name
        (if occ = 0 then "" else "." ^ string_of_int occ) key.pk_surname;
      flush stdout

let insert_bnotes fname gen nfname str =
  let old_nread = gen.g_base.c_bnotes.nread in
  let nfname =
    if nfname = "" then ""
    else
      match NotesLinks.check_file_name nfname with
        Some (dl, f) -> List.fold_right Filename.concat dl f
      | None -> "bad"
  in
  let bnotes =
    {nread = (fun f n -> if f = nfname then str else old_nread f n);
     norigin_file = fname;
     efiles =
       if nfname <> "" then
         let efiles = gen.g_base.c_bnotes.efiles () in
         fun () -> nfname :: efiles
       else gen.g_base.c_bnotes.efiles}
  in
  gen.g_base.c_bnotes <- bnotes

let insert_wiznote fname gen wizid str =
  gen.g_wiznotes <- (wizid, str) :: gen.g_wiznotes

let map_option f =
  function
    Some x -> Some (f x)
  | None -> None

let insert_relation_parent gen ip s k =
  let (par, ipar) = insert_somebody gen k in
  par.m_related <- ip :: par.m_related;
  if par.m_sex = Neuter then par.m_sex <- s;
  ipar

let insert_relation gen ip r =
  {r_type = r.r_type;
   r_fath = map_option (insert_relation_parent gen ip Male) r.r_fath;
   r_moth = map_option (insert_relation_parent gen ip Female) r.r_moth;
   r_sources = unique_string gen r.r_sources}

let insert_relations fname gen sb sex rl =
  let (p, ip) = insert_somebody gen sb in
  if p.m_rparents <> [] then
    begin
      printf "\nFile \"%s\"\n" fname;
      printf "Relations already defined for \"%s%s %s\"\n"
        (sou gen.g_base p.m_first_name)
        (if p.m_occ = 0 then "" else "." ^ string_of_int p.m_occ)
        (sou gen.g_base p.m_surname);
      check_error gen
    end
  else
    begin
      notice_sex gen p sex;
      let rl = List.map (insert_relation gen ip) rl in p.m_rparents <- rl
    end

let insert_syntax fname gen =
  function
    Family (cpl, fs, ms, witl, fam, des) ->
      insert_family gen cpl fs ms witl fam des
  | Notes (key, str) -> insert_notes fname gen key str
  | Relations (sb, sex, rl) -> insert_relations fname gen sb sex rl
  | Bnotes (nfname, str) -> insert_bnotes fname gen nfname str
  | Wnotes (wizid, str) -> insert_wiznote fname gen wizid str

let record_access_of tab =
  {load_array = (fun () -> ()); get = (fun i -> tab.(i));
   set = (fun i v -> tab.(i) <- v);
   output_array = (fun oc -> output_value_no_sharing oc (tab : _ array));
   len = Array.length tab; clear_array = fun () -> ()}

let no_istr_iper_index =
  {find = (fun _ -> raise (Match_failure ("./src/db1link.ml4", 694, 35)));
   cursor = (fun _ -> raise (Match_failure ("./src/db1link.ml4", 694, 52)));
   next = fun _ -> raise (Match_failure ("./src/db1link.ml4", 694, 67))}

let persons_record_access gen per_index_ic per_ic persons =
  let read_person_in_temp_file i =
    let mp = persons.(i) in
    let p =
      let c =
        try
          seek_in per_index_ic (Iovalue.sizeof_long * i);
          let pos = input_binary_int per_index_ic in
          seek_in per_ic pos; input_char per_ic
        with End_of_file -> 'U'
      in
      match c with
        'D' -> (input_item_value per_ic : person)
      | 'U' ->
          let empty_string = Adef.istr_of_int 0 in
          {first_name = empty_string; surname = empty_string; occ = 0;
           image = empty_string; first_names_aliases = [];
           surnames_aliases = []; public_name = empty_string; qualifiers = [];
           aliases = []; titles = []; rparents = []; related = [];
           occupation = empty_string; sex = Neuter; access = IfTitles;
           birth = Adef.codate_None; birth_place = empty_string;
           birth_src = empty_string; baptism = Adef.codate_None;
           baptism_place = empty_string; baptism_src = empty_string;
           death = DontKnowIfDead; death_place = empty_string;
           death_src = empty_string; burial = UnknownBurial;
           burial_place = empty_string; burial_src = empty_string;
           notes = empty_string; psources = empty_string;
           key_index = Adef.iper_of_int 0}
      | _ -> assert false
    in
    {p with first_name = mp.m_first_name; surname = mp.m_surname;
     occ = mp.m_occ; rparents = mp.m_rparents; related = mp.m_related;
     sex = mp.m_sex; notes = mp.m_notes; key_index = Adef.iper_of_int i}
  in
  let get_fun i =
    try Hashtbl.find gen.g_patch_p i with
      Not_found -> read_person_in_temp_file i
  in
  let len = Array.length persons in
  {load_array = (fun () -> ()); get = get_fun;
   set = (fun i v -> failwith "bug: setting persons array");
   output_array = (fun oc -> Mutil.output_array_no_sharing oc get_fun len);
   len = len; clear_array = fun () -> ()}

let particules_file = ref ""

let families_record_access fam_index_ic fam_ic len =
  let get_fun i =
    seek_in fam_index_ic (Iovalue.sizeof_long * i);
    let pos = input_binary_int fam_index_ic in
    seek_in fam_ic pos; let fam : family = input_item_value fam_ic in fam
  in
  {load_array = (fun () -> ()); get = get_fun;
   set = (fun i v -> failwith "bug: setting family array");
   output_array = (fun oc -> output_array_no_sharing oc get_fun len);
   len = len; clear_array = fun () -> ()}

let input_particles part_file =
  if part_file = "" then
    ["af "; "d'"; "d’"; "dal "; "de "; "di "; "du "; "of "; "van ";
     "von und zu "; "von "; "zu "; "zur "; "AF "; "D'"; "D’"; "DAL "; "DE ";
     "DI "; "DU "; "OF "; "VAN "; "VON UND ZU "; "VON "; "ZU "; "ZUR "]
  else Mutil.input_particles part_file

let empty_base : cbase =
  {c_persons = [| |]; c_ascends = [| |]; c_unions = [| |]; c_couples = [| |];
   c_descends = [| |]; c_strings = [| |];
   c_bnotes =
     {nread = (fun _ _ -> ""); norigin_file = ""; efiles = fun _ -> []}}

let linked_base gen per_index_ic per_ic fam_index_ic fam_ic bdir =
  (**)
  let _ =
    Printf.eprintf "pcnt %d persons %d\n" gen.g_pcnt
      (Array.length gen.g_base.c_persons);
    flush stderr
  in
  (**)
  let persons =
    let a = Array.sub gen.g_base.c_persons 0 gen.g_pcnt in
    gen.g_base.c_persons <- [| |]; a
  in
  let ascends =
    let a = Array.sub gen.g_base.c_ascends 0 gen.g_pcnt in
    gen.g_base.c_ascends <- [| |]; a
  in
  let unions =
    let a = Array.sub gen.g_base.c_unions 0 gen.g_pcnt in
    gen.g_base.c_unions <- [| |]; a
  in
  (**)
  let _ =
    Printf.eprintf "fcnt %d families %d\n" gen.g_fcnt
      (Array.length gen.g_base.c_couples);
    flush stderr
  in
  (**)
  let couples =
    let a = Array.sub gen.g_base.c_couples 0 gen.g_fcnt in
    gen.g_base.c_couples <- [| |]; a
  in
  let descends =
    let a = Array.sub gen.g_base.c_descends 0 gen.g_fcnt in
    gen.g_base.c_descends <- [| |]; a
  in
  (**)
  let _ =
    Printf.eprintf "scnt %d strings %d\n" gen.g_scnt
      (Array.length gen.g_base.c_strings);
    flush stderr
  in
  (**)
  let strings =
    let a = Array.sub gen.g_base.c_strings 0 gen.g_scnt in
    gen.g_base.c_strings <- [| |]; a
  in
  let particles = input_particles !particules_file in
  let bnotes = gen.g_base.c_bnotes in
  let base_data =
    {persons = persons_record_access gen per_index_ic per_ic persons;
     ascends = record_access_of ascends; unions = record_access_of unions;
     families = families_record_access fam_index_ic fam_ic gen.g_fcnt;
     visible =
       {v_write =
         (fun _ -> raise (Match_failure ("./src/db1link.ml4", 839, 27)));
        v_get =
          (fun _ -> raise (Match_failure ("./src/db1link.ml4", 839, 43)))};
     couples = record_access_of couples; descends = record_access_of descends;
     strings = record_access_of strings; particles = particles;
     bnotes = bnotes; bdir = bdir}
  in
  let base_func =
    {person_of_key =
      (fun _ -> raise (Match_failure ("./src/db1link.ml4", 845, 21)));
     persons_of_name =
       (fun _ -> raise (Match_failure ("./src/db1link.ml4", 846, 23)));
     strings_of_fsname =
       (fun _ -> raise (Match_failure ("./src/db1link.ml4", 846, 51)));
     persons_of_surname = no_istr_iper_index;
     persons_of_first_name = no_istr_iper_index;
     patch_person =
       (fun _ -> raise (Match_failure ("./src/db1link.ml4", 849, 20)));
     patch_ascend =
       (fun _ -> raise (Match_failure ("./src/db1link.ml4", 849, 43)));
     patch_union =
       (fun _ -> raise (Match_failure ("./src/db1link.ml4", 849, 65)));
     patch_family =
       (fun _ -> raise (Match_failure ("./src/db1link.ml4", 850, 20)));
     patch_couple =
       (fun _ -> raise (Match_failure ("./src/db1link.ml4", 850, 43)));
     patch_descend =
       (fun _ -> raise (Match_failure ("./src/db1link.ml4", 850, 67)));
     insert_string =
       (fun _ -> raise (Match_failure ("./src/db1link.ml4", 851, 21)));
     patch_name =
       (fun _ -> raise (Match_failure ("./src/db1link.ml4", 851, 42)));
     commit_patches =
       (fun _ -> raise (Match_failure ("./src/db1link.ml4", 851, 67)));
     commit_notes =
       (fun _ -> raise (Match_failure ("./src/db1link.ml4", 852, 20)));
     patched_ascends =
       (fun _ -> raise (Match_failure ("./src/db1link.ml4", 852, 46)));
     is_patched_person = (fun _ -> false); cleanup = fun () -> ()}
  in
  {data = base_data; func = base_func}

let fold_option fsome vnone =
  function
    Some v -> fsome v
  | None -> vnone

let write_file_contents fname text =
  let oc = open_out fname in output_string oc text; close_out oc

let output_wizard_notes bdir wiznotes =
  let wizdir = Filename.concat bdir "wiznotes" in
  Mutil.remove_dir wizdir;
  if wiznotes = [] then ()
  else
    begin
      (try Unix.mkdir wizdir 0o755 with _ -> ());
      List.iter
        (fun (wizid, text) ->
           let fname = Filename.concat wizdir wizid ^ ".txt" in
           write_file_contents fname text)
        wiznotes
    end

let output_particles_file bdir particles =
  let oc = open_out (Filename.concat bdir "particles.txt") in
  List.iter (fun s -> fprintf oc "%s\n" (Mutil.tr ' ' '_' s)) particles;
  close_out oc

let output_command_line bdir =
  let oc = open_out (Filename.concat bdir "command.txt") in
  fprintf oc "%s" Sys.argv.(0);
  for i = 1 to Array.length Sys.argv - 1 do
    fprintf oc " %s" Sys.argv.(i)
  done;
  fprintf oc "\n";
  close_out oc

let link next_family_fun bdir =
  let tmp_dir = Filename.concat "gw_tmp" bdir in
  (try Mutil.mkdir_p tmp_dir with _ -> ());
  let tmp_per_index = Filename.concat tmp_dir "gwc_per_index" in
  let tmp_per = Filename.concat tmp_dir "gwc_per" in
  let tmp_fam_index = Filename.concat tmp_dir "gwc_fam_index" in
  let tmp_fam = Filename.concat tmp_dir "gwc_fam" in
  let fi =
    {f_local_names = Hashtbl.create 20011; f_curr_src_file = "";
     f_curr_gwo_file = ""; f_separate = false; f_shift = 0}
  in
  let gen =
    {g_strings = Hashtbl.create 20011; g_names = Hashtbl.create 20011;
     g_pcnt = 0; g_fcnt = 0; g_scnt = 0; g_file_info = fi;
     g_base = empty_base; g_patch_p = Hashtbl.create 20011; g_wiznotes = [];
     g_def = [| |]; g_first_av_occ = Hashtbl.create 1; g_errored = false;
     g_per_index = open_out_bin tmp_per_index; g_per = open_out_bin tmp_per;
     g_fam_index = open_out_bin tmp_fam_index; g_fam = open_out_bin tmp_fam}
  in
  let per_index_ic = open_in_bin tmp_per_index in
  let per_ic = open_in_bin tmp_per in
  let fam_index_ic = open_in_bin tmp_fam_index in
  let fam_ic = open_in_bin tmp_fam in
  let istr_empty = unique_string gen "" in
  let istr_quest = unique_string gen "?" in
  assert (istr_empty = Adef.istr_of_int 0);
  assert (istr_quest = Adef.istr_of_int 1);
  Sys.remove tmp_per_index;
  Sys.remove tmp_per;
  Sys.remove tmp_fam_index;
  Sys.remove tmp_fam;
  let next_family = next_family_fun fi in
  begin let rec loop () =
    match next_family () with
      Some fam -> insert_syntax fi.f_curr_src_file gen fam; loop ()
    | None -> ()
  in
    loop ()
  end;
  close_out gen.g_per_index;
  close_out gen.g_per;
  close_out gen.g_fam_index;
  close_out gen.g_fam;
  Hashtbl.clear gen.g_strings;
  Hashtbl.clear gen.g_names;
  Hashtbl.clear fi.f_local_names;
  Gc.compact ();
  let dsk_base =
    linked_base gen per_index_ic per_ic fam_index_ic fam_ic bdir
  in
  Hashtbl.clear gen.g_patch_p;
  let base = Gwdb.base_of_base1 dsk_base in
  if !do_check && gen.g_pcnt > 0 then
    begin let changed_p (ip, p, o_sex, o_rpar) =
      let p = Gwdb.dsk_person_of_person p in
      let p =
        {p with sex = fold_option (fun s -> s) p.sex o_sex;
         rparents =
           fold_option
             (List.map
                (Futil.map_relation_ps (fun p -> p)
                   (fun s -> Adef.istr_of_int 0)))
             p.rparents o_rpar}
      in
      let i = Adef.int_of_iper ip in Hashtbl.replace gen.g_patch_p i p
    in
      Check.check_base base (set_error base gen) (set_warning base)
        (fun i -> gen.g_def.(i)) changed_p !pr_stats;
      flush stdout
    end;
  if not gen.g_errored then
    begin
      if !do_consang then
        (let _ = (ConsangAll.compute base (-1) true false : _ option) in ());
      Gc.compact ();
      Db1out.output bdir dsk_base;
      output_wizard_notes bdir gen.g_wiznotes;
      output_particles_file bdir dsk_base.data.particles;
      (try Mutil.remove_dir tmp_dir with _ -> ());
      (try Unix.rmdir "gw_tmp" with _ -> ());
      output_command_line bdir;
      true
    end
  else false
