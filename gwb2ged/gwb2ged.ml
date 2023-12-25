(* $Id: gwb2ged.ml,v 5.29 2008-01-08 11:58:46 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def
open Gutil
open Gwdb
open Mutil
open Printf

type charset = Ansel | Ascii | Utf8

let charset = ref Utf8
let no_notes = ref false
let no_picture = ref false

let month_txt =
  [| "JAN"; "FEB"; "MAR"; "APR"; "MAY"; "JUN"; "JUL"; "AUG"; "SEP"; "OCT";
     "NOV"; "DEC" |]

let french_txt =
  [| "VEND"; "BRUM"; "FRIM"; "NIVO"; "PLUV"; "VENT"; "GERM"; "FLOR"; "PRAI";
     "MESS"; "THER"; "FRUC"; "COMP" |]

let hebrew_txt =
  [| "TSH"; "CSH"; "KSL"; "TVT"; "SHV"; "ADR"; "ADS"; "NSN"; "IYR"; "SVN";
     "TMZ"; "AAV"; "ELL" |]

let ged_month cal m =
  match cal with
    Dgregorian | Djulian ->
      if m >= 1 && m <= Array.length month_txt then month_txt.(m-1)
      else failwith "ged_month"
  | Dfrench ->
      if m >= 1 && m <= Array.length french_txt then french_txt.(m-1)
      else failwith "ged_month"
  | Dhebrew ->
      if m >= 1 && m <= Array.length hebrew_txt then hebrew_txt.(m-1)
      else failwith "ged_month"

let encode s =
  match !charset with
    Ansel ->
      let s = if !(Mutil.utf_8_db) then Mutil.iso_8859_1_of_utf_8 s else s in
      Ansel.of_iso_8859_1 s
  | Ascii -> if !(Mutil.utf_8_db) then Mutil.iso_8859_1_of_utf_8 s else s
  | Utf8 -> if !(Mutil.utf_8_db) then s else Mutil.utf_8_of_iso_8859_1 s

let max_len = 78

let next_char_pair_overflows s len i =
  let rec loop prec_was_space len i =
    if len < max_len then
      if i < String.length s then
        match s.[i] with
          ' ' | '\n' -> loop true (len + 1) (i + 1)
        | _ -> if prec_was_space then loop false (len + 1) (i + 1) else false
      else false
    else true
  in
  loop false (len + 1) (i + 1)

let br = "<br>"
let find_br s ini_i =
  let ini = "<br" in
  let rec loop i j =
    if i = String.length ini then
      let rec loop2 j =
        if j = String.length s then br
        else if s.[j] = '>' then String.sub s ini_i (j - ini_i + 1)
        else loop2 (j + 1)
      in
      loop2 j
    else if j = String.length s then br
    else if String.unsafe_get ini i = String.unsafe_get s j then
      loop (i + 1) (j + 1)
    else br
  in
  loop 0 ini_i

let rec display_note_aux oc s len i =
  if i = String.length s then fprintf oc "\n"
  else
    let c = if s.[i] = '\n' then ' ' else s.[i] in
    let br = find_br s i in
    if i <= String.length s - String.length br &&
       String.lowercase (String.sub s i (String.length br)) = br
    then
      begin
        fprintf oc "\n2 CONT ";
        let i = i + String.length br in
        let i = if i < String.length s && s.[i] = '\n' then i + 1 else i in
        display_note_aux oc s (String.length "2 CONT ") i
      end
    else if len = max_len || c <> ' ' && next_char_pair_overflows s len i then
      begin
        fprintf oc "\n2 CONC %c" c;
        display_note_aux oc s (String.length "2 CONC .") (i + 1)
      end
    else begin output_char oc c; display_note_aux oc s (len + 1) (i + 1) end

let display_note oc s =
  fprintf oc "1 NOTE ";
  display_note_aux oc (encode s) (String.length "1 NOTE ") 0

let ged_header base oc ifile ofile =
  fprintf oc "0 HEAD\n";
  fprintf oc "1 SOUR GeneWeb\n";
  fprintf oc "2 VERS %s\n" Version.txt;
  fprintf oc "2 NAME %s\n" (Filename.basename Sys.argv.(0));
  fprintf oc "2 CORP INRIA\n";
  fprintf oc "3 ADDR http://www.geneweb.org\n";
  fprintf oc "2 DATA %s\n"
    (let fname = Filename.basename ifile in
     if Filename.check_suffix fname ".gwb" then fname else fname ^ ".gwb");
  begin try
    let tm = Unix.localtime (Unix.time ()) in
    let mon = ged_month Dgregorian (tm.Unix.tm_mon + 1) in
    fprintf oc "1 DATE %02d %s %d\n" tm.Unix.tm_mday mon
      (1900 + tm.Unix.tm_year);
    fprintf oc "2 TIME %02d:%02d:%02d\n" tm.Unix.tm_hour tm.Unix.tm_min
      tm.Unix.tm_sec
  with _ -> ()
  end;
  if ofile <> "" then fprintf oc "1 FILE %s\n" (Filename.basename ofile);
  fprintf oc "1 GEDC\n";
  begin match !charset with
    Ansel | Ascii -> fprintf oc "2 VERS 5.5\n"
  | Utf8 -> fprintf oc "2 VERS 5.5.1\n"
  end;
  fprintf oc "2 FORM LINEAGE-LINKED\n";
  begin match !charset with
    Ansel -> fprintf oc "1 CHAR ANSEL\n"
  | Ascii -> fprintf oc "1 CHAR ASCII\n"
  | Utf8 -> fprintf oc "1 CHAR UTF-8\n"
  end;
  if !no_notes then ()
  else
    let s = base_notes_read base "" in
    if s = "" then () else display_note oc s

let sub_string_index s t =
  let rec loop i j =
    if j = String.length t then Some (i - j)
    else if i = String.length s then None
    else if s.[i] = t.[j] then loop (i + 1) (j + 1)
    else loop (i + 1) 0
  in
  loop 0 0

let ged_1st_name base p =
  let fn = sou base (get_first_name p) in
  match get_first_names_aliases p with
    n :: _ ->
      let fna = sou base n in
      begin match sub_string_index fna fn with
        Some i ->
          let j = i + String.length fn in
          String.sub fna 0 i ^ "\"" ^ fn ^ "\"" ^
          String.sub fna j (String.length fna - j)
      | None -> fn
      end
  | [] -> fn

let string_of_list =
  let rec loop r =
    function
      s :: l -> if r = "" then loop s l else loop (r ^ "," ^ s) l
    | [] -> r
  in
  loop ""

let ged_name base oc per =
  fprintf oc "1 NAME %s /%s/\n"
    (encode (Mutil.nominative (ged_1st_name base per)))
    (encode (Mutil.nominative (sou base (get_surname per))));
  let n = sou base (get_public_name per) in
  if n <> "" then fprintf oc "2 GIVN %s\n" (encode n);
  begin match get_qualifiers per with
    nn :: _ -> fprintf oc "2 NICK %s\n" (encode (sou base nn))
  | [] -> ()
  end;
  begin match get_surnames_aliases per with
    [] -> ()
  | list ->
      let list = List.map (fun n -> encode (sou base n)) list in
      fprintf oc "2 SURN %s\n" (string_of_list list)
  end;
  List.iter (fun s -> fprintf oc "1 NAME %s\n" (encode (sou base s)))
    (get_aliases per)

let ged_sex base oc per =
  match get_sex per with
    Male -> fprintf oc "1 SEX M\n"
  | Female -> fprintf oc "1 SEX F\n"
  | Neuter -> ()

let ged_calendar oc =
  function
    Dgregorian -> ()
  | Djulian -> fprintf oc "@#DJULIAN@ "
  | Dfrench -> fprintf oc "@#DFRENCH R@ "
  | Dhebrew -> fprintf oc "@#DHEBREW@ "

let ged_date_dmy oc dt cal =
  begin match dt.prec with
    Sure -> ()
  | About -> fprintf oc "ABT "
  | Maybe -> fprintf oc "EST "
  | Before -> fprintf oc "BEF "
  | After -> fprintf oc "AFT "
  | OrYear i -> fprintf oc "BET "
  | YearInt i -> fprintf oc "BET "
  end;
  ged_calendar oc cal;
  if dt.day <> 0 then fprintf oc "%02d " dt.day;
  if dt.month <> 0 then fprintf oc "%s " (ged_month cal dt.month);
  fprintf oc "%d" dt.year;
  match dt.prec with
    OrYear i -> fprintf oc " AND "; ged_calendar oc cal; fprintf oc "%d" i
  | YearInt i -> fprintf oc " AND "; ged_calendar oc cal; fprintf oc "%d" i
  | _ -> ()

let ged_date oc =
  function
    Dgreg (d, Dgregorian) -> ged_date_dmy oc d Dgregorian
  | Dgreg (d, Djulian) ->
      ged_date_dmy oc (Calendar.julian_of_gregorian d) Djulian
  | Dgreg (d, Dfrench) ->
      ged_date_dmy oc (Calendar.french_of_gregorian d) Dfrench
  | Dgreg (d, Dhebrew) ->
      ged_date_dmy oc (Calendar.hebrew_of_gregorian d) Dhebrew
  | Dtext t -> fprintf oc "(%s)" t

let ged_ev_detail oc n typ d pl src =
  begin match typ, d, pl with
    "", None, "" -> fprintf oc " Y"
  | _ -> ()
  end;
  fprintf oc "\n";
  if typ = "" then () else fprintf oc "%d TYPE %s\n" n typ;
  begin match d with
    Some d -> fprintf oc "%d DATE " n; ged_date oc d; fprintf oc "\n"
  | None -> ()
  end;
  if pl <> "" then fprintf oc "%d PLAC %s\n" n (encode pl);
  if src <> "" then fprintf oc "%d SOUR %s\n" n (encode src)

let adop_fam_list = ref []
let adop_fam_cnt = ref 0

let ged_adoption base (per_sel, fam_sel) oc per r =
  let sel =
    match r.r_fath, r.r_moth with
      Some ip1, Some ip2 -> per_sel ip1 && per_sel ip2
    | Some ip1, _ -> per_sel ip1
    | _, Some ip2 -> per_sel ip2
    | _ -> true
  in
  if sel then
    begin
      fprintf oc "1 ADOP Y\n";
      adop_fam_list :=
        (r.r_fath, r.r_moth, get_key_index per) :: !adop_fam_list;
      incr adop_fam_cnt;
      fprintf oc "2 FAMC @F%d@\n" (nb_of_families base + !adop_fam_cnt);
      fprintf oc "3 ADOP ";
      begin match r.r_fath, r.r_moth with
        Some _, None -> fprintf oc "HUSB"
      | None, Some _ -> fprintf oc "WIFE"
      | Some _, Some _ -> fprintf oc "BOTH"
      | _ -> ()
      end;
      fprintf oc "\n"
    end

let ged_fam_adop base oc i (fath, moth, child) =
  fprintf oc "0 @F%d@ FAM\n" i;
  begin match fath with
    Some i -> fprintf oc "1 HUSB @I%d@\n" (Adef.int_of_iper i + 1)
  | _ -> ()
  end;
  match moth with
    Some i -> fprintf oc "1 WIFE @I%d@\n" (Adef.int_of_iper i + 1)
  | _ -> ()

let ged_ind_ev_str base sel oc per =
  let pl = sou base (get_birth_place per) in
  let src = sou base (get_birth_src per) in
  begin match Adef.od_of_codate (get_birth per), pl with
    None, "" -> ()
  | None, pl -> fprintf oc "1 BIRT"; ged_ev_detail oc 2 "" None pl src
  | od, pl -> fprintf oc "1 BIRT"; ged_ev_detail oc 2 "" od pl src
  end;
  List.iter
    (fun r -> if r.r_type = Adoption then ged_adoption base sel oc per r)
    (get_rparents per);
  let pl = sou base (get_baptism_place per) in
  let src = sou base (get_baptism_src per) in
  begin match Adef.od_of_codate (get_baptism per), pl with
    None, "" -> ()
  | od, pl -> fprintf oc "1 BAPM"; ged_ev_detail oc 2 "" od pl src
  end;
  let pl = sou base (get_death_place per) in
  let src = sou base (get_death_src per) in
  begin match get_death per with
    NotDead -> ()
  | Death (dr, cd) ->
      fprintf oc "1 DEAT";
      ged_ev_detail oc 2 "" (Some (Adef.date_of_cdate cd)) pl src
  | DeadYoung | DeadDontKnowWhen | OfCourseDead ->
      fprintf oc "1 DEAT"; ged_ev_detail oc 2 "" None pl src
  | DontKnowIfDead -> fprintf oc "1 DEAT\n"
  end;
  let pl = sou base (get_burial_place per) in
  let src = sou base (get_burial_src per) in
  match get_burial per with
    UnknownBurial -> ()
  | Buried cod ->
      fprintf oc "1 BURI";
      ged_ev_detail oc 2 "" (Adef.od_of_codate cod) pl src
  | Cremated cod ->
      fprintf oc "1 CREM";
      ged_ev_detail oc 2 "" (Adef.od_of_codate cod) pl src

let ged_title base oc per tit =
  fprintf oc "1 TITL ";
  fprintf oc "%s" (encode (sou base tit.t_ident));
  begin match sou base tit.t_place with
    "" -> ()
  | pl -> fprintf oc ", %s" (encode pl)
  end;
  if tit.t_nth <> 0 then fprintf oc ", %d" tit.t_nth;
  fprintf oc "\n";
  begin match
    Adef.od_of_codate tit.t_date_start, Adef.od_of_codate tit.t_date_end
  with
    None, None -> ()
  | Some sd, None ->
      fprintf oc "2 DATE FROM "; ged_date oc sd; fprintf oc "\n"
  | None, Some sd -> fprintf oc "2 DATE TO "; ged_date oc sd; fprintf oc "\n"
  | Some sd1, Some sd2 ->
      fprintf oc "2 DATE FROM ";
      ged_date oc sd1;
      fprintf oc " TO ";
      ged_date oc sd2;
      fprintf oc "\n"
  end;
  match tit.t_name with
    Tmain ->
      fprintf oc "2 NOTE %s\n" (encode (sou base (get_public_name per)))
  | Tname n -> fprintf oc "2 NOTE %s\n" (encode (sou base n))
  | Tnone -> ()

let ged_ind_attr_str base oc per =
  begin match sou base (get_occupation per) with
    "" -> ()
  | occu -> fprintf oc "1 OCCU %s\n" (encode occu)
  end;
  List.iter (ged_title base oc per) (get_titles per)

let ged_famc base (per_sel, fam_sel) oc asc =
  match get_parents asc with
    Some ifam ->
      if fam_sel ifam then
        fprintf oc "1 FAMC @F%d@\n" (Adef.int_of_ifam ifam + 1)
  | None -> ()

let ged_fams base (per_sel, fam_sel) oc ifam =
  if fam_sel ifam then fprintf oc "1 FAMS @F%d@\n" (Adef.int_of_ifam ifam + 1)

let ged_godparent per_sel oc godp =
  function
    Some ip ->
      if per_sel ip then
        begin
          fprintf oc "1 ASSO @I%d@\n" (Adef.int_of_iper ip + 1);
          fprintf oc "2 TYPE INDI\n";
          fprintf oc "2 RELA %s\n" godp
        end
  | None -> ()

let ged_witness fam_sel oc ifam =
  if fam_sel ifam then
    begin
      fprintf oc "1 ASSO @F%d@\n" (Adef.int_of_ifam ifam + 1);
      fprintf oc "2 TYPE FAM\n";
      fprintf oc "2 RELA witness\n"
    end

let ged_asso base (per_sel, fam_sel) oc per =
  List.iter
    (fun r ->
       if r.r_type = GodParent then
         begin
           ged_godparent per_sel oc "GODF" r.r_fath;
           ged_godparent per_sel oc "GODM" r.r_moth
         end)
    (get_rparents per);
  List.iter
    (fun ic ->
       let c = poi base ic in
       if get_sex c = Male then
         List.iter
           (fun ifam ->
              let fam = foi base ifam in
              if array_mem (get_key_index per) (get_witnesses fam) then
                ged_witness fam_sel oc ifam)
           (Array.to_list (get_family c)))
    (get_related per)

let ged_psource base oc per =
  match sou base (get_psources per) with
    "" -> ()
  | s -> fprintf oc "1 SOUR %s\n" (encode s)

let ged_multimedia_link base oc per =
  match sou base (get_image per) with
    "" -> ()
  | s ->
      if not !no_picture then
        begin fprintf oc "1 OBJE\n"; fprintf oc "2 FILE %s\n" s end

let ged_note base oc per =
  match sou base (get_notes per) with
    "" -> ()
  | s -> display_note oc s

let ged_marriage base oc fam =
  match
    Adef.od_of_codate (get_marriage fam), sou base (get_marriage_place fam),
    get_relation fam
  with d, pl, _ ->
    fprintf oc "1 %s" (if get_relation fam = Engaged then "ENGA" else "MARR");
    let typ =
      if get_relation fam = NoSexesCheckNotMarried ||
         get_relation fam = NoSexesCheckMarried
      then
        "gay"
      else ""
    in
    ged_ev_detail oc 2 typ d pl (sou base (get_marriage_src fam));
    if get_relation fam = NotMarried then fprintf oc "2 PLAC unmarried\n"

let ged_divorce base oc fam =
  match get_divorce fam with
    NotDivorced -> ()
  | Separated -> ()
  | Divorced cd ->
      let d = Adef.od_of_codate cd in
      fprintf oc "1 DIV"; ged_ev_detail oc 2 "" d "" ""

let ged_child base (per_sel, fam_sel) oc chil =
  if per_sel chil then fprintf oc "1 CHIL @I%d@\n" (Adef.int_of_iper chil + 1)

let ged_fsource base oc fam =
  match sou base (get_fsources fam) with
    "" -> ()
  | s -> fprintf oc "1 SOUR %s\n" (encode s)

let ged_comment base oc fam =
  match sou base (get_comment fam) with
    "" -> ()
  | s -> display_note oc s

let has_personal_infos base per =
  if get_parents per <> None then true
  else if sou base (get_first_name per) <> "?" then true
  else if sou base (get_surname per) <> "?" then true
  else if get_birth per <> Adef.codate_None then true
  else if sou base (get_birth_place per) <> "" then true
  else if get_death per <> NotDead && get_death per <> DontKnowIfDead then
    true
  else if sou base (get_occupation per) <> "" then true
  else if get_titles per <> [] then true
  else false

let ged_ind_record base sel oc i =
  let per = poi base (Adef.iper_of_int i) in
  if has_personal_infos base per then
    begin
      fprintf oc "0 @I%d@ INDI\n" (i + 1);
      ged_name base oc per;
      ged_sex base oc per;
      ged_ind_ev_str base sel oc per;
      ged_ind_attr_str base oc per;
      ged_famc base sel oc per;
      Array.iter (ged_fams base sel oc) (get_family per);
      ged_asso base sel oc per;
      ged_psource base oc per;
      ged_multimedia_link base oc per;
      ged_note base oc per
    end

let ged_fam_record base (per_sel, fam_sel as sel) oc i =
  let fam = foi base (Adef.ifam_of_int i) in
  if is_deleted_family fam then ()
  else
    begin
      fprintf oc "0 @F%d@ FAM\n" (i + 1);
      ged_marriage base oc fam;
      ged_divorce base oc fam;
      if has_personal_infos base (poi base (get_father fam)) &&
         per_sel (get_father fam)
      then
        fprintf oc "1 HUSB @I%d@\n" (Adef.int_of_iper (get_father fam) + 1);
      if has_personal_infos base (poi base (get_mother fam)) &&
         per_sel (get_mother fam)
      then
        fprintf oc "1 WIFE @I%d@\n" (Adef.int_of_iper (get_mother fam) + 1);
      Array.iter (ged_child base sel oc) (get_children fam);
      ged_fsource base oc fam;
      ged_comment base oc fam
    end

let find_person base p1 po p2 =
  match person_of_key base p1 p2 po with
    Some ip -> ip
  | None ->
      printf "Not found: %s%s %s\n" p1
        (if po = 0 then "" else " " ^ string_of_int po) p2;
      flush stdout;
      exit 2

let surnames = ref []
let no_spouses_parents = ref false
let censor = ref 0
let with_siblings = ref false

let gwb2ged base ifile ofile anc desc mem =
  let anc =
    match anc with
      Some (p1, po, p2) -> Some (find_person base p1 po p2)
    | None -> None
  in
  let desc =
    match desc with
      Some (p1, po, p2) -> Some (find_person base p1 po p2)
    | None -> None
  in
  if not mem then
    begin
      load_ascends_array base;
      load_unions_array base;
      load_couples_array base;
      load_descends_array base
    end;
  let oc = if ofile = "" then stdout else open_out ofile in
  let (per_sel, fam_sel as sel) =
    Select.functions base anc desc !surnames None !no_spouses_parents !censor
      !with_siblings (-1)
  in
  ged_header base oc ifile ofile;
  flush oc;
  for i = 0 to nb_of_persons base - 1 do
    if per_sel (Adef.iper_of_int i) then ged_ind_record base sel oc i
  done;
  for i = 0 to nb_of_families base - 1 do
    if fam_sel (Adef.ifam_of_int i) then ged_fam_record base sel oc i
  done;
  let _ =
    List.fold_right (fun adop i -> ged_fam_adop base oc i adop; i + 1)
      !adop_fam_list (nb_of_families base + 1)
  in
  fprintf oc "0 TRLR\n"; flush oc; if ofile = "" then () else close_out oc

let ifile = ref ""
let ofile = ref "a.ged"
let mem = ref false
let anc_1st = ref ""
let anc_occ = ref 0
let anc_2nd = ref ""
let desc_1st = ref ""
let desc_occ = ref 0
let desc_2nd = ref ""

type arg_state =
  ASnone | ASwaitAncOcc | ASwaitAncSurn | ASwaitDescOcc | ASwaitDescSurn
let arg_state = ref ASnone

let errmsg =
  "Usage: " ^ Sys.argv.(0) ^ " \
<base> [options]
If both options -a and -d are used, intersection is assumed.
If several options -s are used, union is assumed.
Options are:"

let speclist =
  ["-charset",
   Arg.String
     (fun x ->
        arg_state := ASnone;
        match x with
          "ASCII" -> charset := Ascii
        | "ANSEL" -> charset := Ansel
        | "UTF-8" -> charset := Utf8
        | _ -> raise (Arg.Bad "bad -charset value")),
   "[ASCII|ANSEL|UTF-8]: set charset; default is UTF-8.";
   "-o", Arg.String (fun x -> ofile := x; arg_state := ASnone),
   "<ged>: output file name (default: a.ged)";
   "-mem", Arg.Unit (fun () -> mem := true; arg_state := ASnone),
   ": save memory space, but slower";
   "-a", Arg.String (fun s -> anc_1st := s; arg_state := ASwaitAncOcc),
   "\"<1st_name>\" [num] \"<surname>\": select ancestors of";
   "-d", Arg.String (fun s -> desc_1st := s; arg_state := ASwaitDescOcc),
   "\"<1st_name>\" [num] \"<surname>\": select descendants of";
   "-aws",
   Arg.String
     (fun s ->
        anc_1st := s; arg_state := ASwaitAncOcc; with_siblings := true),
   "\"<1st_name>\" [num] \"<surname>\" : select ancestors with siblings";
   "-s", Arg.String (fun x -> surnames := x :: !surnames),
   "\"<surname>\" : select this surname (option usable several times)";
   "-nsp", Arg.Set no_spouses_parents,
   ": no spouses' parents (for options -s and -d)";
   "-nn", Arg.Set no_notes, ": no (database) notes";
   "-nopicture", Arg.Set no_picture, ": Don't extract individual picture.";
   "-c", Arg.Int (fun i -> censor := i), "\
<num> :
     When a person is born less than <num> years ago, it is not exported unless
     it is Public. All the spouses and descendants are also censored."]

let anonfun s =
  match !arg_state with
    ASnone ->
      if !ifile = "" then ifile := s
      else raise (Arg.Bad "Cannot treat several databases")
  | ASwaitAncOcc ->
      begin try anc_occ := int_of_string s; arg_state := ASwaitAncSurn with
        Failure _ -> anc_occ := 0; anc_2nd := s; arg_state := ASnone
      end
  | ASwaitAncSurn -> anc_2nd := s; arg_state := ASnone
  | ASwaitDescOcc ->
      begin try desc_occ := int_of_string s; arg_state := ASwaitDescSurn with
        Failure _ -> desc_occ := 0; desc_2nd := s; arg_state := ASnone
      end
  | ASwaitDescSurn -> desc_2nd := s; arg_state := ASnone

let main () =
  Argl.parse speclist anonfun errmsg;
  Secure.set_base_dir (Filename.dirname !ifile);
  let anc =
    if !anc_1st <> "" then
      if !anc_2nd = "" then
        begin
          printf "Misused option -a\n";
          printf "Use option -help for usage\n";
          flush stdout;
          exit 2
        end
      else Some (!anc_1st, !anc_occ, !anc_2nd)
    else None
  in
  let desc =
    if !desc_1st <> "" then
      if !desc_2nd = "" then
        begin
          printf "Misused option -d\n";
          printf "Use option -help for usage\n";
          flush stdout;
          exit 2
        end
      else Some (!desc_1st, !desc_occ, !desc_2nd)
    else None
  in
  if !ofile = "-" then ofile := "";
  if !ifile = "" then
    begin
      printf "Missing base name\n";
      printf "Use option -help for usage\n";
      flush stdout;
      exit 2
    end;
  match try Some (Gwdb.open_base !ifile) with Sys_error _ -> None with
    Some base -> gwb2ged base !ifile !ofile anc desc !mem
  | None -> printf "Can't open base %s\n" !ifile; flush stdout; exit 2

let _ = Printexc.catch main ()
