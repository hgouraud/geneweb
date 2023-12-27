(* camlp5r *)
(* $Id: db2link.ml,v 5.18 2012-01-27 08:53:53 ddr Exp $ *)
(* Copyright (c) 2006-2008 INRIA *)

open Db2disk.TYPES
open Def
open Futil
open Gwcomp
open Printf

let default_source = ref ""
let do_check = ref true
let do_consang = ref false
let pr_stats = ref false

type file_info =
  { mutable f_curr_src_file : string;
    mutable f_curr_gwo_file : string;
    mutable f_separate : bool;
    mutable f_has_separates : bool;
    mutable f_sep_file_inx : int }

let max_warnings = 50
let max_errors = 50

type family =
  { fam : (iper, string) gen_family;
    cpl : iper gen_couple;
    des : iper gen_descend }

type 'a file_field =
  { oc_dat : out_channel;
    oc_acc : out_channel;
    dname : string;
    start_pos : Iovalue.header_pos option;
    mutable sz32 : int;
    mutable sz64 : int;
    mutable item_cnt : int;
    valu : 'a -> Obj.t }

type 'data file_pair = {
  io_access : Iochan.t ;
  io_data : 'data ;
}

type gen =
  { mutable g_pcnt : int;
    mutable g_fcnt : int;
    mutable g_scnt : int;
    g_file_info : file_info;
    mutable g_error : bool;
    mutable g_error_cnt : int;
    mutable g_warning_cnt : int;
    g_first_av_occ : (Adef.istr * Adef.istr, int) Hashtbl.t;
    g_tmp_dir : string;
    g_particles : string list;
    g_strings : (string, Adef.istr) Hashtbl.t;
    g_index_of_key : (key2, iper) Hashtbl.t;
    mutable g_occ_of_key : (key2, int) Hashtbl.t array;
    g_person_fields : (iper, string) gen_person file_field list;
    g_family_fields : family file_field list;
    g_person_parents :  out_channel file_pair;
    g_person_unions :   out_channel file_pair;
    g_person_rparents : out_channel file_pair;
    g_person_related :  Iochan.t file_pair;
    g_person_notes :    out_channel file_pair }

let person_fields_arr =
  ["first_name", (fun so -> Obj.repr (so.first_name : string));
   "surname", (fun so -> Obj.repr (so.surname : string) );
   "occ", (fun so -> Obj.repr (so.occ : int) );
   "image", (fun so -> Obj.repr (so.image : string));
   "public_name", (fun so -> Obj.repr (so.public_name : string));
   "qualifiers", (fun so -> Obj.repr (so.qualifiers : string list));
   "aliases", (fun so -> Obj.repr (so.aliases: string list));
   "first_names_aliases", (fun so -> Obj.repr (so.first_names_aliases : string list));
   "surnames_aliases", (fun so -> Obj.repr (so.surnames_aliases : string list));
   "titles", (fun so -> Obj.repr (so.titles : string Def.gen_title list));
   "occupation", (fun so -> Obj.repr (so.occupation : string));
   "sex", (fun so -> Obj.repr (so.sex : Def.sex));
   "access", (fun so -> Obj.repr (so.access : Def.access));
   "birth", (fun so -> Obj.repr (so.birth : Adef.codate));
   "birth_place", (fun so -> Obj.repr (so.birth_place : string));
   "birth_src", (fun so -> Obj.repr (so.birth_src : string));
   "baptism", (fun so -> Obj.repr (so.baptism : Adef.codate));
   "baptism_place", (fun so -> Obj.repr (so.baptism_place : string));
   "baptism_src", (fun so -> Obj.repr (so.baptism_src : string));
   "death", (fun so -> Obj.repr (so.death : Def.death));
   "death_place", (fun so -> Obj.repr (so.death_place : string));
   "death_src", (fun so -> Obj.repr (so.death_src : string));
   "burial", (fun so -> Obj.repr (so.burial : Def.burial));
   "burial_place", (fun so -> Obj.repr (so.burial_place : string));
   "burial_src", (fun so -> Obj.repr (so.burial_src : string));
   "psources", (fun so -> Obj.repr (so.psources : string))]

let family_fields_arr =
  ["marriage", (fun so -> Obj.repr (so.fam.marriage : Adef.codate));
   "marriage_place", (fun so -> Obj.repr (so.fam.marriage_place : string));
   "marriage_src", (fun so -> Obj.repr (so.fam.marriage_src : string));
   "witnesses", (fun so -> Obj.repr (so.fam.witnesses : Def.iper array));
   "relation", (fun so -> Obj.repr (so.fam.relation : Def.relation_kind));
   "divorce", (fun so -> Obj.repr (so.fam.divorce : Def.divorce));
   "comment", (fun so -> Obj.repr (so.fam.comment : string));
   "origin_file", (fun so -> Obj.repr (so.fam.origin_file : string));
   "fsources", (fun so -> Obj.repr (so.fam.fsources : string));
   "father", (fun so -> Obj.repr ((Adef.father so.cpl) : Adef.iper));
   "mother", (fun so -> Obj.repr ((Adef.mother so.cpl) : Adef.iper));
   "children", (fun so -> Obj.repr (so.des.children : Adef.iper array))]

let input_particles part_file =
  if part_file = "" then
    ["af "; "d'"; "d’"; "dal "; "de "; "di "; "du "; "of "; "van ";
     "von und zu "; "von "; "zu "; "zur "; "AF "; "D'"; "D’"; "DAL "; "DE ";
     "DI "; "DU "; "OF "; "VAN "; "VON UND ZU "; "VON "; "ZU "; "ZUR "]
  else Mutil.input_particles part_file

let particules_file = ref ""

let unique_key_string gen s =
  let s = Name.lower (Mutil.nominative s) in
  try Hashtbl.find gen.g_strings s with
    Not_found ->
      let istr = Adef.istr_of_int gen.g_scnt in
      Hashtbl.add gen.g_strings s istr; gen.g_scnt <- gen.g_scnt + 1; istr

let key_hashtbl_find ht k = Hashtbl.find ht (Db2disk.key2_of_key k)
let key_hashtbl_add ht k v = Hashtbl.add ht (Db2disk.key2_of_key k) v

let occ_of_key_ht gen =
  let i = gen.g_file_info.f_sep_file_inx in
  let len = Array.length gen.g_occ_of_key in
  if i >= len then
    begin let new_len = max (i + 1) (2 * len + 1) in
      gen.g_occ_of_key <-
        Array.append gen.g_occ_of_key
          (Array.init (new_len - len) (fun _ -> Hashtbl.create 1))
    end;
  gen.g_occ_of_key.(i)

let find_first_available_occ gen so fn sn =
  let occ =
    try Hashtbl.find gen.g_first_av_occ (fn, sn) with Not_found -> 0
  in
  let rec loop occ =
    let k1 = fn, sn, occ in
    match
      try Some (key_hashtbl_find gen.g_index_of_key k1) with Not_found -> None
    with
      Some _ -> loop (occ + 1)
    | None ->
        gen.g_warning_cnt <- gen.g_warning_cnt - 1;
        if gen.g_warning_cnt > 0 then
          begin
            eprintf "Warning: %s: %s.%d %s renumbered %d\n"
              gen.g_file_info.f_curr_gwo_file so.first_name so.occ so.surname
              occ;
            flush stderr
          end;
        key_hashtbl_add (occ_of_key_ht gen) (fn, sn, so.occ) occ;
        Hashtbl.replace gen.g_first_av_occ (fn, sn) occ;
        occ
  in
  loop occ

let output_item v ff =
  Iovalue.size_32 := ff.sz32;
  Iovalue.size_64 := ff.sz64;
  Iovalue.output ff.oc_dat v;
  ff.sz32 <- !(Iovalue.size_32);
  ff.sz64 <- !(Iovalue.size_64);
  ff.item_cnt <- ff.item_cnt + 1

let output_field so ff =
  output_binary_int ff.oc_acc (pos_out ff.oc_dat); output_item (ff.valu so) ff

let int_size = 4

let io_create_pair dir field_name open_out_bin =
  let d = Filename.concat dir field_name in
  (try Mutil.mkdir_p d with _ -> ());
  let io_access = Iochan.openfile (Filename.concat d "access") true in
  let io_data = open_out_bin (Filename.concat d "data") in
  { io_access ; io_data }

let io_close_pair file_pair close_out =
  Iochan.close file_pair.io_access;
  close_out file_pair.io_data

let io_access_set file_pair index pos =
  Iochan.seek file_pair.io_access (int_size * index);
  Iochan.output_binary_int file_pair.io_access pos

let io_access_get file_pair index =
  Iochan.seek file_pair.io_access (int_size * index);
  Iochan.input_binary_int file_pair.io_access

let io_data_add file_pair output v =
  let pos = pos_out file_pair.io_data in
  output file_pair.io_data v;
  pos

let io_data_add_and_set file_pair index output v =
  let pos = io_data_add file_pair output v in
  io_access_set file_pair index pos

let insert_person1 gen so =
  if so.first_name <> "?" && so.surname <> "?" then
    let fn = unique_key_string gen so.first_name in
    let sn = unique_key_string gen so.surname in
    let k = fn, sn, so.occ in
    try
      if gen.g_file_info.f_separate then
        ignore (key_hashtbl_find (occ_of_key_ht gen) k : int)
      else ignore (key_hashtbl_find gen.g_index_of_key k : iper);
      gen.g_error_cnt <- gen.g_error_cnt - 1;
      if gen.g_error_cnt > 0 then
        begin
          eprintf "File \"%s\"\n" gen.g_file_info.f_curr_gwo_file;
          eprintf "Error: already defined %s.%d %s\n" so.first_name so.occ
            so.surname
        end;
      flush stderr;
      gen.g_error <- true
    with Not_found ->
      let (k, so) =
        if gen.g_file_info.f_separate then
          let occ = find_first_available_occ gen so fn sn in
          (fn, sn, occ), {so with occ = occ}
        else k, so
      in
      let (k, so) =
        let psources =
          if so.psources = "" then !default_source else so.psources
        in
        k, {so with psources = psources}
      in
      key_hashtbl_add gen.g_index_of_key k (Adef.iper_of_int gen.g_pcnt);
      List.iter (output_field so) gen.g_person_fields;
      io_access_set gen.g_person_parents gen.g_pcnt (-1);
      io_access_set gen.g_person_unions gen.g_pcnt (-1);
      io_access_set gen.g_person_rparents gen.g_pcnt (-1);
      io_access_set gen.g_person_related gen.g_pcnt (-1);
      io_access_set gen.g_person_notes gen.g_pcnt (-1);
      gen.g_pcnt <- gen.g_pcnt + 1

let insert_somebody1 gen sex =
  function
    Undefined key -> ()
  | Defined so -> let so = {so with sex = sex} in insert_person1 gen so

let insert_family1 gen co fath_sex moth_sex witl fo deo =
  let _ifath = insert_somebody1 gen fath_sex (Adef.father co) in
  let _imoth = insert_somebody1 gen moth_sex (Adef.mother co) in
  Array.iter (fun key -> insert_person1 gen key) deo.children;
  List.iter (fun (so, sex) -> insert_somebody1 gen sex so) witl

let iter_option f =
  function
    Some x -> f x
  | None -> ()

let insert_relation1 gen r =
  iter_option (insert_somebody1 gen Male) r.r_fath;
  iter_option (insert_somebody1 gen Female) r.r_moth

let insert_rparents1 gen sb sex rl =
  insert_somebody1 gen sex sb; List.iter (insert_relation1 gen) rl

let insert_bnotes1 gen notesname str =
  let nfname =
    if notesname = "" then "notes"
    else
      let f =
        match NotesLinks.check_file_name notesname with
          Some (dl, f) -> List.fold_right Filename.concat dl f
        | None -> "bad"
      in
      Filename.concat "notes_d" f
  in
  let fname =
    List.fold_left Filename.concat gen.g_tmp_dir ["base_d"; nfname ^ ".txt"]
  in
  Mutil.mkdir_p (Filename.dirname fname);
  let oc = open_out fname in
  output_string oc str;
  close_out oc;
  if notesname = "" then
    let fname =
      List.fold_left Filename.concat gen.g_tmp_dir ["base_d"; "notes_of.txt"]
    in
    let oc = open_out fname in
    fprintf oc "%s\n" gen.g_file_info.f_curr_src_file; close_out oc

let write_file_contents fname text =
  let oc = open_out fname in output_string oc text; close_out oc

let insert_wiznotes1 gen wizid str =
  let wizdir =
    List.fold_left Filename.concat gen.g_tmp_dir ["base_d"; "wiznotes_d"]
  in
  Mutil.mkdir_p wizdir;
  let fname = Filename.concat wizdir wizid ^ ".txt" in
  write_file_contents fname str

let insert_gwo_1 gen =
  function
    Family (cpl, fs, ms, witl, fam, des) ->
      insert_family1 gen cpl fs ms witl fam des
  | Notes (key, str) -> ()
  | Relations (sb, sex, rl) -> insert_rparents1 gen sb sex rl
  | Bnotes (nfname, str) -> insert_bnotes1 gen nfname str
  | Wnotes (wizid, str) -> insert_wiznotes1 gen wizid str

let empty_person =
  {first_name = ""; surname = ""; occ = 0; image = "";
   first_names_aliases = []; surnames_aliases = []; public_name = "";
   qualifiers = []; aliases = []; titles = []; rparents = []; related = [];
   occupation = ""; sex = Neuter; access = IfTitles; birth = Adef.codate_None;
   birth_place = ""; birth_src = ""; baptism = Adef.codate_None;
   baptism_place = ""; baptism_src = ""; death = DontKnowIfDead;
   death_place = ""; death_src = ""; burial = UnknownBurial;
   burial_place = ""; burial_src = ""; notes = ""; psources = "";
   key_index = Adef.iper_of_int 0}

let insert_undefined2 gen key fn sn sex =
  if key.pk_first_name <> "?" && key.pk_surname <> "?" then
    key_hashtbl_add gen.g_index_of_key (fn, sn, key.pk_occ)
      (Adef.iper_of_int gen.g_pcnt);
  if gen.g_file_info.f_has_separates then
    begin
      gen.g_error_cnt <- gen.g_error_cnt - 1;
      if gen.g_error_cnt > 0 then
        begin
          gen.g_error_cnt <- -1;
          eprintf
            "Error: option -sep does not work when there are undefined persons\n";
          flush stderr
        end;
      gen.g_error <- true
    end
  else if !do_check then
    begin
      gen.g_warning_cnt <- gen.g_warning_cnt - 1;
      if gen.g_warning_cnt > 0 then
        eprintf "Warning: adding undefined %s.%d %s\n"
          (Name.lower key.pk_first_name) key.pk_occ
          (Name.lower key.pk_surname);
      flush stderr
    end;
  let so =
    {empty_person with first_name = key.pk_first_name;
     surname = key.pk_surname; occ = key.pk_occ; sex = sex}
  in
  List.iter (output_field so) gen.g_person_fields;
  io_access_set gen.g_person_parents gen.g_pcnt (-1);
  io_access_set gen.g_person_unions gen.g_pcnt (-1);
  io_access_set gen.g_person_rparents gen.g_pcnt (-1);
  io_access_set gen.g_person_related gen.g_pcnt (-1);
  io_access_set gen.g_person_notes gen.g_pcnt (-1);
  gen.g_pcnt <- gen.g_pcnt + 1;
  Adef.iper_of_int (gen.g_pcnt - 1)

let get_undefined2 gen key sex =
  let fn = unique_key_string gen key.pk_first_name in
  let sn = unique_key_string gen key.pk_surname in
  let occ =
    if gen.g_file_info.f_separate then
      try key_hashtbl_find (occ_of_key_ht gen) (fn, sn, key.pk_occ) with
        Not_found -> key.pk_occ
    else key.pk_occ
  in
  try key_hashtbl_find gen.g_index_of_key (fn, sn, occ) with
    Not_found -> insert_undefined2 gen key fn sn sex

let get_person2 gen so sex =
  if so.first_name <> "?" && so.surname <> "?" then
    let fn = unique_key_string gen so.first_name in
    let sn = unique_key_string gen so.surname in
    let occ =
      if gen.g_file_info.f_separate then
        try key_hashtbl_find (occ_of_key_ht gen) (fn, sn, so.occ) with
          Not_found -> so.occ
      else so.occ
    in
    try key_hashtbl_find gen.g_index_of_key (fn, sn, occ) with
      Not_found ->
        failwith
          (sprintf "*** bug not found %s.%d %s" so.first_name so.occ
             so.surname)
  else
    let so = if so.sex = Neuter then {so with sex = sex} else so in
    List.iter (output_field so) gen.g_person_fields;
    io_access_set gen.g_person_parents gen.g_pcnt (-1);
    io_access_set gen.g_person_unions gen.g_pcnt (-1);
    io_access_set gen.g_person_rparents gen.g_pcnt (-1);
    io_access_set gen.g_person_related gen.g_pcnt (-1);
    io_access_set gen.g_person_notes gen.g_pcnt (-1);
    gen.g_pcnt <- gen.g_pcnt + 1;
    Adef.iper_of_int (gen.g_pcnt - 1)

let get_somebody2 gen sex =
  function
    Undefined key -> get_undefined2 gen key sex
  | Defined so -> get_person2 gen so sex

let insert_related gen irp ip =
  let { io_access ; io_data } = gen.g_person_related in
  let pos1 = io_access_get gen.g_person_related (Adef.int_of_iper irp) in

  (* Fabrice: Any good reason not to use an out_channel ? *)
  let pos2 = Iochan.seek_end io_data in
  Iochan.output_value_no_header io_data (Adef.int_of_iper ip);
  Iochan.output_value_no_header io_data pos1;

  io_access_set gen.g_person_related (Adef.int_of_iper irp) pos2

let insert_family2 gen co fath_sex moth_sex witl fo deo =
  let ifath = get_somebody2 gen fath_sex (Adef.father co) in
  let imoth = get_somebody2 gen moth_sex (Adef.mother co) in
  let children =
    Array.map (fun key -> get_person2 gen key Neuter) deo.children
  in
  let witn =
    List.map
      (fun (so, sex) ->
         let ip = get_somebody2 gen sex so in insert_related gen ip ifath; ip)
      witl
  in
  let fam =
    {fam = {fo with witnesses = Array.of_list witn};
     cpl = Adef.couple ifath imoth; des = {children = children}}
  in
  List.iter (output_field fam) gen.g_family_fields;

  (* Fabrice: update [ "person"; "family" ]: BEWARE

     This format is temporary, lists are created incrementaly, but in
     the second pass of the creation (in `reorder_fields`), this file
     is completely rewritten to become a field file, i.e. it is
     possible to load its content with input_value as an int array
     array
*)
  let pos_data_fath = io_access_get gen.g_person_unions (Adef.int_of_iper ifath) in

  io_data_add_and_set gen.g_person_unions
    (Adef.int_of_iper ifath)
    (fun io_data () ->
       Iovalue.output io_data gen.g_fcnt;
       Iovalue.output io_data pos_data_fath)
    ();

  let pos_data_moth = io_access_get gen.g_person_unions ( Adef.int_of_iper imoth) in

  io_data_add_and_set gen.g_person_unions
    (Adef.int_of_iper imoth)
    (fun io_data () ->
       Iovalue.output io_data gen.g_fcnt;
       Iovalue.output io_data pos_data_moth) ();

  (* update "parents" *)
  let pos_acc_parents = io_data_add gen.g_person_parents
       Iovalue.output ( gen.g_fcnt : int ) in
  Array.iter
    (fun iper ->
       io_access_set gen.g_person_parents (Adef.int_of_iper iper) pos_acc_parents)
    children;

  gen.g_fcnt <- gen.g_fcnt + 1

let insert_notes2 gen key str =
  let ip = get_undefined2 gen key Neuter in
  io_data_add_and_set gen.g_person_notes (Adef.int_of_iper ip)
     Iovalue.output ( str : string )

let map_option f =
  function
    Some x -> Some (f x)
  | None -> None

let insert_relation_parent2 gen ip sex k =
  let irp = get_somebody2 gen sex k in insert_related gen irp ip; irp

let insert_relation2 gen ip r =
  let r_fath = map_option (insert_relation_parent2 gen ip Male) r.r_fath in
  let r_moth = map_option (insert_relation_parent2 gen ip Female) r.r_moth in
  {r_type = r.r_type; r_fath = r_fath; r_moth = r_moth; r_sources = 0}

let insert_rparents2 gen sb sex rl =
  let ip = get_somebody2 gen sex sb in
  let rl = List.map (insert_relation2 gen ip) rl in
  io_data_add_and_set gen.g_person_rparents
    (Adef.int_of_iper ip)
     Iovalue.output (rl : (Def.iper, int) Def.gen_relation list)

let insert_gwo_2 gen =
  function
    Family (cpl, fs, ms, witl, fam, des) ->
      insert_family2 gen cpl fs ms witl fam des
  | Notes (key, str) -> insert_notes2 gen key str
  | Relations (sb, sex, rl) -> insert_rparents2 gen sb sex rl
  | Bnotes (nfname, str) -> ()
  | Wnotes (wizid, str) -> ()

let open_out_field_unknown_size d valu =
  let oc_dat = open_out_bin (Filename.concat d "data1") in
  let oc_acc = open_out_bin (Filename.concat d "access1") in
  {oc_dat = oc_dat; oc_acc = oc_acc; dname = d; start_pos = None; sz32 = 0;
   sz64 = 0; item_cnt = 0; valu = valu}

let close_out_field_known_size pad ff =
  close_out ff.oc_dat;
  close_out ff.oc_acc;
  (* making input_value header of "data" file, and copying "data1" file *)
  let oc_dat = open_out_bin (Filename.concat ff.dname "data") in
  let start_pos = Iovalue.create_output_value_header oc_dat in
  Iovalue.size_32 := ff.sz32;
  Iovalue.size_64 := ff.sz64;
  Iovalue.output_block_header oc_dat 0 ff.item_cnt;
  let acc_shift = pos_out oc_dat in
  let fname1 = Filename.concat ff.dname "data1" in
  let ic_dat = open_in_bin fname1 in
  begin try while true do output_byte oc_dat (input_byte ic_dat) done with
    End_of_file -> ()
  end;
  let _ = (Iovalue.patch_output_value_header oc_dat start_pos : int) in
  close_out oc_dat;
  close_in ic_dat;
  Sys.remove fname1;
  (* making "access" file from "access1" file with shift *)
  let oc_acc = open_out_bin (Filename.concat ff.dname "access") in
  let fname = Filename.concat ff.dname "access1" in
  let ic_acc = open_in_bin fname in
  begin try
    while true do
      output_binary_int oc_acc (input_binary_int ic_acc + acc_shift)
    done
  with End_of_file -> ()
  end;
  close_out oc_acc;
  close_in ic_acc;
  Sys.remove fname;
  (* test *)
  Db2out.check_input_value "Db2link.close_out_field_known_size"
    (Filename.concat ff.dname "data") ff.item_cnt

(* used only by "reorder_fields": should be simplified *)
let open_out_field d len valu =
  let oc_dat = open_out_bin (Filename.concat d "data2") in
  let oc_acc = open_out_bin (Filename.concat d "access2") in
  let start_pos = Iovalue.create_output_value_header oc_dat in
  Iovalue.output_block_header oc_dat 0 (max len Db2out.phony_min_size);
  assert (pos_out oc_dat = Db2disk.first_item_pos len);
  {oc_dat = oc_dat; oc_acc = oc_acc; start_pos = Some start_pos; dname = d;
   sz32 = !(Iovalue.size_32); sz64 = !(Iovalue.size_64); item_cnt = 0;
   valu = valu}

(* used only by "reorder_fields": should be simplified *)
let close_out_field pad ff len =
  close_out ff.oc_acc;
  ff.item_cnt <- len;
  for i = ff.item_cnt + 1 to Db2out.phony_min_size do
    output_item (ff.valu pad) ff
  done;
  Iovalue.size_32 := ff.sz32;
  Iovalue.size_64 := ff.sz64;
  let start_pos =
    match ff.start_pos with
      Some s -> s
    | None -> assert false
  in
  ignore (Iovalue.patch_output_value_header ff.oc_dat start_pos : int);
  Iovalue.output_block_header ff.oc_dat 0 ff.item_cnt;
  assert (pos_out ff.oc_dat = Db2disk.first_item_pos ff.item_cnt);
  close_out ff.oc_dat;
  (* test *)
  Db2out.check_input_value "Db2link.close_out_field"
    (Filename.concat ff.dname "data2") ff.item_cnt

let no_person empty_string ip =
  {first_name = empty_string; surname = empty_string; occ = 0;
   image = empty_string; first_names_aliases = []; surnames_aliases = [];
   public_name = empty_string; qualifiers = []; titles = []; rparents = [];
   related = []; aliases = []; occupation = empty_string; sex = Neuter;
   access = Private; birth = Adef.codate_None; birth_place = empty_string;
   birth_src = empty_string; baptism = Adef.codate_None;
   baptism_place = empty_string; baptism_src = empty_string;
   death = DontKnowIfDead; death_place = empty_string;
   death_src = empty_string; burial = UnknownBurial;
   burial_place = empty_string; burial_src = empty_string;
   notes = empty_string; psources = empty_string; key_index = ip}

let pad_per = no_person "" (Adef.iper_of_int 0)

let no_family empty_string ifam =
  {fam =
    {marriage = Adef.codate_None; marriage_place = empty_string;
     marriage_src = empty_string; witnesses = [| |]; relation = Married;
     divorce = NotDivorced; comment = empty_string;
     origin_file = empty_string; fsources = empty_string; fam_index = ifam};
   cpl = Adef.couple (Adef.iper_of_int 0) (Adef.iper_of_int 0);
   des = {children = [| |]}}

let pad_fam = no_family "" (Adef.ifam_of_int 0)

let compress_type_string len field_d e ic =
  Db2out.output_value_array_compress field_d e len ""
    (fun oc_acc output_item ->
       seek_in ic (Db2disk.first_item_pos len);
       try
         while true do
           let s : string = Iovalue.input ic in
           assert (Obj.tag (Obj.repr s) = Obj.string_tag);
           let pos = output_item s in output_binary_int oc_acc pos
         done
       with End_of_file -> ())

let compress_type_list_string len field_d e ic =
  let oc_acc = open_out_bin (Filename.concat field_d ("access" ^ e)) in
  let oc_dat = open_out_bin (Filename.concat field_d ("data" ^ e)) in
  let oc_ext = open_out_bin (Filename.concat field_d "data2.ext") in
  let ht = Hashtbl.create 1 in
  seek_in ic (Db2disk.first_item_pos len);
  begin try
    let items_cnt = ref 0 in
    while true do
      let sl : string list = Iovalue.input ic in
      if Obj.is_block (Obj.repr sl) then
        begin
          assert (Obj.tag (Obj.repr sl) = 0);
          assert (Obj.size (Obj.repr sl) = 2)
        end
      else assert (Obj.magic sl = 0);
      match sl with
        _ :: _ ->
          output_binary_int oc_acc (pos_out oc_ext);
          let sl =
            List.map
              (Db2out.output_item_compress_return_pos oc_dat ht items_cnt) sl
          in
          Iovalue.output oc_ext (sl : int list)
      | [] -> output_binary_int oc_acc (-1)
    done
  with End_of_file -> ()
  end;
  close_out oc_ext;
  close_out oc_dat;
  close_out oc_acc

let compress_type_list_title len field_d e ic =
  let oc_acc = open_out_bin (Filename.concat field_d ("access" ^ e)) in
  let oc_dat = open_out_bin (Filename.concat field_d ("data" ^ e)) in
  let oc_ext = open_out_bin (Filename.concat field_d "data2.ext") in
  let ht = Hashtbl.create 1 in
  seek_in ic (Db2disk.first_item_pos len);
  begin try
    let items_cnt = ref 0 in
    while true do
      let tl : string gen_title list = Iovalue.input ic in
      match tl with
        _ :: _ ->
          output_binary_int oc_acc (pos_out oc_ext);
          let tl =
            List.map
              (map_title_strings
                 (Db2out.output_item_compress_return_pos oc_dat ht items_cnt))
              tl
          in
          Iovalue.output oc_ext (tl : int gen_title list)
      | [] -> output_binary_int oc_acc (-1)
    done
  with End_of_file -> ()
  end;
  close_out oc_ext;
  close_out oc_dat;
  close_out oc_acc

let compress_fields nper nfam tmp_dir =
  List.iter
    (fun (f1, f2, compress_type, len) ->
       let field_d =
         List.fold_left Filename.concat tmp_dir ["base_d"; f1; f2]
       in
       let ic = open_in_bin (Filename.concat field_d "data") in
       if !(Mutil.verbose) then
         begin eprintf "compressing %s..." f2; flush stderr end;
       compress_type len field_d "2" ic;
       close_in ic;
       List.iter
         (fun n ->
            let f = Filename.concat field_d n in
            Mutil.remove_file f;
            Sys.rename (Filename.concat field_d (n ^ "2")) f)
         ["data"; "access"];
       if !(Mutil.verbose) then begin Printf.eprintf "\n"; flush stderr end)

    ["person", "baptism_place", compress_type_string, nper;
     "person", "baptism_src", compress_type_string, nper;
     "person", "birth_place", compress_type_string, nper;
     "person", "birth_src", compress_type_string, nper;
     "person", "burial_place", compress_type_string, nper;
     "person", "burial_src", compress_type_string, nper;
     "family", "comment", compress_type_string, nfam;
     "person", "death_place", compress_type_string, nper;
     "person", "death_src", compress_type_string, nper;
     "person", "first_name", compress_type_string, nper;
     "family", "fsources", compress_type_string, nfam;
     "person", "image", compress_type_string, nper;
     "family", "marriage_place", compress_type_string, nfam;
     "family", "marriage_src", compress_type_string, nfam;
     "person", "occupation", compress_type_string, nper;
     "family", "origin_file", compress_type_string, nfam;
     "person", "psources", compress_type_string, nper;
     "person", "public_name", compress_type_string, nper;
     "person", "surname", compress_type_string, nper;

     "person", "aliases", compress_type_list_string, nper;
     "person", "first_names_aliases", compress_type_list_string, nper;
     "person", "qualifiers", compress_type_list_string, nper;
     "person", "surnames_aliases", compress_type_list_string, nper;

     "person", "titles", compress_type_list_title, nper]

let read_int_array_field (ic_acc, ic_dat) n =
  seek_in ic_acc (4 * n);
  let pos = input_binary_int ic_acc in
  let rec loop list pos =
    if pos = -1 then Array.of_list list
    else
      begin
        seek_in ic_dat pos;
        let i = Iovalue.input ic_dat in
        loop (i :: list) (Iovalue.input ic_dat)
      end
  in
  loop [] pos

let reorder_type_list_int field_d ic_acc ic_dat ff =
  let item_cnt = ref 0 in
  try
    while true do
      let x = read_int_array_field (ic_acc, ic_dat) !item_cnt in
      output_field x ff; incr item_cnt
    done
  with End_of_file -> ()

let reorder_fields tmp_dir nper =
  List.iter
    (fun (f1, f2, reorder_type, len) ->
       let field_d =
         List.fold_left Filename.concat tmp_dir ["base_d"; f1; f2]
       in
       let ic_acc = open_in_bin (Filename.concat field_d "access") in
       let ic_dat = open_in_bin (Filename.concat field_d "data") in
       if !(Mutil.verbose) then
         begin eprintf "reordering %s..." f2; flush stderr end;
       let ff = open_out_field field_d len Obj.repr in
       reorder_type field_d ic_acc ic_dat ff;
       close_out_field [| |] ff len;
       close_in ic_dat;
       close_in ic_acc;
       List.iter
         (fun n ->
            let f = Filename.concat field_d n in
            Mutil.remove_file f;
            Sys.rename (Filename.concat field_d (n ^ "2")) f)
         ["data"; "access"];
       if !(Mutil.verbose) then begin Printf.eprintf "\n"; flush stderr end)
    ["person", "family", reorder_type_list_int, nper]

let output_command_line bdir =
  let oc = open_out (Filename.concat bdir "command.txt") in
  fprintf oc "%s" Sys.argv.(0);
  for i = 1 to Array.length Sys.argv - 1 do
    fprintf oc " %s" Sys.argv.(i)
  done;
  fprintf oc "\n";
  close_out oc

let output_particles_file tmp_dir particles =
  let fname =
    List.fold_left Filename.concat tmp_dir ["base_d"; "particles.txt"]
  in
  let oc = open_out fname in
  List.iter (fun s -> fprintf oc "%s\n" (Mutil.tr ' ' '_' s)) particles;
  close_out oc

let set_error base x =
  printf "\nError: "; Check.print_base_error stdout base x

let set_warning base =
  function
    UndefinedSex _ -> ()
  | x -> printf "\nWarning: "; Check.print_base_warning stdout base x

let fold_option fsome vnone =
  function
    Some v -> fsome v
  | None -> vnone

let changed_p (ip, p, o_sex, o_rpar) =
  let p = Gwdb.dsk_person_of_person p in
  let _p =
    {p with sex = fold_option (fun s -> s) p.sex o_sex;
     rparents =
       fold_option
         (List.map
            (Futil.map_relation_ps (fun p -> p)
               (fun s -> Adef.istr_of_int 0)))
         p.rparents o_rpar}
  in
  let i = Adef.int_of_iper ip in
  Printf.eprintf "person %d not changed\n" i; flush stderr

let mkdir_and_open_out_field_unknown_size tmp_dir (name, valu) =
  let d = Filename.concat tmp_dir name in
  (try Mutil.mkdir_p d with _ -> ()); open_out_field_unknown_size d valu

(* ******************************************************************** *)
(*  [Fonc] link : (file_info -> unit -> Gwcomp.gw_syntax option) ->     *)
(*                  string -> bool                                      *)
(** [Description] : TODO
    [Args] :
      - next_family_fun :
      - bdir : nom de la base.
    [Retour] :
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let link next_family_fun bdir =
  let tmp_dir = Filename.concat "gw_tmp" bdir in
  Mutil.remove_dir tmp_dir;
  (try Mutil.mkdir_p tmp_dir with _ -> ());
  let person_d =
    List.fold_left Filename.concat tmp_dir ["base_d"; "person"]
  in
  let family_d =
    List.fold_left Filename.concat tmp_dir ["base_d"; "family"]
  in
  (try Mutil.mkdir_p person_d with _ -> ());
  (try Mutil.mkdir_p family_d with _ -> ());
  let person_fields =
    List.map (mkdir_and_open_out_field_unknown_size person_d)
      person_fields_arr
  in
  let family_fields =
    List.map (mkdir_and_open_out_field_unknown_size family_d)
      family_fields_arr
  in
  let person_parents = io_create_pair person_d "parents" open_out_bin in
  let person_unions = io_create_pair person_d "family" open_out_bin in
  let person_rparents = io_create_pair person_d "rparents" open_out_bin in
  let person_related = io_create_pair person_d "related"
      (fun filename -> Iochan.openfile filename true)
  in
  let person_notes = io_create_pair  person_d "notes" open_out_bin in
  let fi =
    {f_curr_src_file = ""; f_curr_gwo_file = ""; f_separate = false;
     f_has_separates = false; f_sep_file_inx = 0}
  in
  let gen =
    {g_pcnt = 0; g_fcnt = 0; g_scnt = 0; g_file_info = fi; g_error = false;
     g_error_cnt = max_errors + 1; g_warning_cnt = max_warnings + 1;
     g_first_av_occ = Hashtbl.create 1; g_tmp_dir = tmp_dir;
     g_particles = input_particles !particules_file;
     g_strings = Hashtbl.create 1; g_index_of_key = Hashtbl.create 1;
     g_occ_of_key = [| |]; g_person_fields = person_fields;
     g_family_fields = family_fields;
     g_person_parents = person_parents;
     g_person_unions = person_unions;
     g_person_rparents = person_rparents;
     g_person_related = person_related;
     g_person_notes = person_notes}
  in
  if !(Mutil.verbose) then
    begin eprintf "pass 1: creating persons...\n"; flush stderr end;
  let next_family = next_family_fun fi in
  begin let rec loop () =
          match next_family () with
            Some fam -> insert_gwo_1 gen fam; loop ()
          | None -> ()
    in
    loop ()
  end;
  Gc.compact ();
  if !(Mutil.verbose) then
    begin eprintf "pass 2: creating families...\n"; flush stderr end;
  let next_family = next_family_fun fi in
  begin let rec loop () =
          match next_family () with
            Some fam -> insert_gwo_2 gen fam; loop ()
          | None -> ()
    in
    loop ()
  end;
  if gen.g_warning_cnt < 0 then
    begin
      eprintf "Warning: %d more warnings...\n" (-gen.g_warning_cnt);
      flush stderr
    end;
  if gen.g_error_cnt < 0 then
    begin
      eprintf "Error: %d more errors...\n" (-gen.g_error_cnt);
      flush stderr
    end;
  List.iter (close_out_field_known_size pad_per) person_fields;
  List.iter (close_out_field_known_size pad_fam) family_fields;
  io_close_pair person_notes    close_out;
  io_close_pair person_related  Iochan.close;
  io_close_pair person_rparents close_out;
  io_close_pair person_unions   close_out;
  io_close_pair person_parents  close_out;
  Gc.compact ();



  let person_of_key_d =
    List.fold_left Filename.concat tmp_dir ["base_d"; "person_of_key"]
  in
  (try Mutil.mkdir_p person_of_key_d with _ -> ());

  Db2out.output_hashtbl ~dir:person_of_key_d ~file:"iper_of_key.ht"
    (gen.g_index_of_key : (key2, iper) Hashtbl.t);
  Hashtbl.clear gen.g_index_of_key;

  Db2out.output_hashtbl person_of_key_d "istr_of_string.ht"
    (gen.g_strings : (string, Adef.istr) Hashtbl.t);
  Hashtbl.clear gen.g_strings;

  Gc.compact ();

  (* (1) remove duplicate strings
     (2) remove nil lists *)
  compress_fields gen.g_pcnt gen.g_fcnt tmp_dir;

  (* replace person/family/data by a standard field file *)
  reorder_fields tmp_dir gen.g_pcnt;

  Db2out.make_indexes (Filename.concat tmp_dir "base_d") gen.g_pcnt
    gen.g_particles;

  output_particles_file tmp_dir gen.g_particles;

  if !(Mutil.verbose) then
    begin
      Printf.eprintf "pcnt %d\n" gen.g_pcnt;
      Printf.eprintf "fcnt %d\n" gen.g_fcnt;
      Printf.eprintf "scnt %d\n" gen.g_scnt;
      flush stderr
    end;
  if not gen.g_error then
    begin
      Mutil.mkdir_p bdir;
      let dir = Filename.concat bdir "base_d" in
      let old_dir = Filename.concat bdir "base_d~" in
      Mutil.remove_dir old_dir;
      (try Sys.rename dir old_dir with Sys_error _ -> ());
      Sys.rename (Filename.concat tmp_dir "base_d") dir;
      Mutil.remove_dir old_dir;
      (try Unix.rmdir tmp_dir with Unix.Unix_error (_, _, _) -> ());
      (try Unix.rmdir "gw_tmp" with Unix.Unix_error (_, _, _) -> ());
      output_command_line bdir;
      if !do_check || !do_consang then
        begin let base = Gwdb.open_base bdir in
          if !do_check then
            Check.check_base base (set_error base) (set_warning base)
              (fun _ -> true) changed_p !pr_stats;
          if !do_consang then
            let _ = (ConsangAll.compute base (-1) true false : _ option) in ()
        end;
      true
    end
  else false
