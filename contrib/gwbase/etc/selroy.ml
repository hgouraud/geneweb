(* $Id: selroy.ml,v 4.20 2007-01-19 09:04:03 deraugla Exp $ *)
(* Copyright (c) 2000-2006 INRIA *)

open Def
open Gutil
open Gwdb

type mfam =
  { m_ifam : ifam;
    m_fam : family;
    m_fath : person;
    m_moth : person;
    m_chil : person array }

let soy y = if y == 0 then "-0" else string_of_int y

let print_date_dmy oc d =
  begin match d.prec with
    About -> Printf.fprintf oc "~"
  | Maybe -> Printf.fprintf oc "?"
  | Before -> Printf.fprintf oc "<"
  | After -> Printf.fprintf oc ">"
  | _ -> ()
  end;
  if d.day == 0 && d.month == 0 then Printf.fprintf oc "%s" (soy d.year)
  else if d.day == 0 then Printf.fprintf oc "%d/%s" d.month (soy d.year)
  else Printf.fprintf oc "%d/%d/%s" d.day d.month (soy d.year);
  begin match d.prec with
    OrYear y -> Printf.fprintf oc "|%s" (soy y)
  | YearInt y -> Printf.fprintf oc "..%s" (soy y)
  | _ -> ()
  end;
  ()

let spaces_to_underscore s =
  String.init (String.length s)
    (fun i ->
       match s.[i] with
         ' ' -> '_'
       | x -> x)

let print_date oc =
  function
    Dgreg (d, Dgregorian) -> print_date_dmy oc d
  | Dgreg (d, Djulian) ->
      print_date_dmy oc (Calendar.julian_of_gregorian d);
      Printf.fprintf oc "J";
      ()
  | Dgreg (d, Dfrench) ->
      print_date_dmy oc (Calendar.french_of_gregorian d);
      Printf.fprintf oc "F";
      ()
  | Dgreg (d, Dhebrew) ->
      print_date_dmy oc (Calendar.hebrew_of_gregorian d);
      Printf.fprintf oc "H";
      ()
  | Dtext t -> Printf.fprintf oc "0(%s)" (spaces_to_underscore t)

let print_date_option oc =
  function
    Some d -> print_date oc d
  | None -> ()

let starting_char s =
  match s.[0] with
    'a'..'z' | 'A'..'Z' | 'à'..'ý' | 'À'..'Ý' | ' ' -> true
  | '?' -> if s = "?" then true else false
  | _ -> false

let gen_correct_string no_colon s =
  let rec loop i len =
    if i == String.length s then Buff.get len
    else if i == 0 && not (starting_char s) then
      loop (i + 1) (Buff.store (Buff.store len '_') s.[0])
    else
      match s.[i] with
        ' ' -> loop (i + 1) (Buff.store len '_')
      | '_' | '\\' -> loop (i + 1) (Buff.store (Buff.store len '\\') s.[i])
      | ':' when no_colon ->
          let len = Buff.store len '\\' in
          loop (i + 1) (Buff.store (Buff.store len '\\') s.[i])
      | c -> loop (i + 1) (Buff.store len c)
  in
  loop 0 0

let s_correct_string = gen_correct_string false
let correct_string base is = s_correct_string (sou base is)
let correct_string_no_colon base is = gen_correct_string true (sou base is)

let has_infos_not_dates base p =
  get_first_names_aliases p <> [] || get_surnames_aliases p <> [] ||
  sou base (get_public_name p) <> "" || get_qualifiers p <> [] ||
  get_aliases p <> [] || get_titles p <> [] ||
  sou base (get_occupation p) <> "" || sou base (get_birth_place p) <> "" ||
  sou base (get_baptism_place p) <> "" ||
  sou base (get_death_place p) <> "" || sou base (get_psources p) <> ""

let has_infos base p =
  has_infos_not_dates base p || get_birth p <> Adef.codate_None ||
  get_baptism p <> Adef.codate_None || get_death p <> NotDead

let print_if_no_empty oc base lab is =
  if sou base is = "" then ()
  else Printf.fprintf oc " %s %s" lab (correct_string base is)

let print_first_name_alias oc base is =
  Printf.fprintf oc " {%s}" (correct_string base is)

let print_surname_alias oc base is =
  Printf.fprintf oc " #salias %s" (correct_string base is)

let print_nick_name oc base is =
  Printf.fprintf oc " #nick %s" (correct_string base is)

let print_alias oc base is =
  Printf.fprintf oc " #alias %s" (correct_string base is)

let print_burial oc base b =
  match b with
    Buried cod ->
      Printf.fprintf oc " #buri";
      begin match Adef.od_of_codate cod with
        Some d -> Printf.fprintf oc " "; print_date oc d; ()
      | _ -> ()
      end;
      ()
  | Cremated cod ->
      Printf.fprintf oc " #crem";
      begin match Adef.od_of_codate cod with
        Some d -> Printf.fprintf oc " "; print_date oc d; ()
      | _ -> ()
      end;
      ()
  | UnknownBurial -> ()

let print_title oc base t =
  let t_date_start = Adef.od_of_codate t.t_date_start in
  let t_date_end = Adef.od_of_codate t.t_date_end in
  Printf.fprintf oc " [";
  begin match t.t_name with
    Tmain -> Printf.fprintf oc "*"
  | Tname s -> Printf.fprintf oc "%s" (correct_string base s)
  | Tnone -> ()
  end;
  Printf.fprintf oc ":";
  Printf.fprintf oc "%s" (correct_string_no_colon base t.t_ident);
  Printf.fprintf oc ":";
  Printf.fprintf oc "%s" (correct_string_no_colon base t.t_place);
  if t.t_nth <> 0 then Printf.fprintf oc ":"
  else
    begin match t_date_start, t_date_end with
      Some _, _ | _, Some _ -> Printf.fprintf oc ":"
    | _ -> ()
    end;
  print_date_option oc t_date_start;
  if t.t_nth <> 0 then Printf.fprintf oc ":"
  else
    begin match t_date_end with
      Some _ -> Printf.fprintf oc ":"
    | _ -> ()
    end;
  print_date_option oc t_date_end;
  if t.t_nth <> 0 then Printf.fprintf oc ":%d" t.t_nth;
  Printf.fprintf oc "]";
  ()

(* main_title = backward compatibility (installed version 2.01);
   should be removed one day... *)
let main_title =
  let val_ =
    function
      "empereur" | "impératrice" -> 6
    | "roi" | "reine" -> 5
    | "prince" | "princesse" -> 4
    | "duc" | "duchesse" -> 3
    | "comte" | "comtesse" -> 2
    | "vicomte" | "vicomtesse" -> 1
    | _ -> 0
  in
  fun base p ->
    let rec loop r =
      function
        [] -> r
      | x :: l ->
          if x.t_name == Tmain then Some x
          else
            match r with
              Some t ->
                if val_ (sou base x.t_ident) > val_ (sou base t.t_ident) then
                  loop (Some x) l
                else loop r l
            | None -> loop (Some x) l
    in
    loop None p.titles

let print_infos oc base is_child print_sources p =
  List.iter (print_first_name_alias oc base) (get_first_names_aliases p);
  List.iter (print_surname_alias oc base) (get_surnames_aliases p);
  begin match get_public_name p with
    s when sou base s <> "" ->
      Printf.fprintf oc " (%s)" (correct_string base s)
  | _ -> ()
  end;
  print_if_no_empty oc base "#image" (get_image p);
  List.iter (print_nick_name oc base) (get_qualifiers p);
  List.iter (print_alias oc base) (get_aliases p);
  (* backward compatibility (installed version 2.01)
       match p.titles with
       [ [_] | [] -> ()
       | [t0 :: _] ->
           match main_title base p with
           [ Some t -> if t = t0 then () else t.t_name := Tmain
           | _ -> () ] ];
  *)
  List.iter (print_title oc base) (get_titles p);
  begin match get_access p with
    IfTitles -> ()
  | Public -> Printf.fprintf oc " #apubl"
  | Private -> Printf.fprintf oc " #apriv"
  (* Fabrice: add this line *)
  | Friend | Friend_m -> Printf.fprintf oc " #afriend"
  end;
  print_if_no_empty oc base "#occu" (get_occupation p);
  if print_sources then print_if_no_empty oc base "#src" (get_psources p);
  begin match Adef.od_of_codate (get_birth p) with
    Some d -> Printf.fprintf oc " "; print_date oc d; ()
  | _ ->
      if get_baptism p <> Adef.codate_None then ()
      else
        match get_death p with
          Death (_, _) | DeadYoung | DeadDontKnowWhen ->
            Printf.fprintf oc " 0"
        | DontKnowIfDead
          when
            not is_child && not (has_infos_not_dates base p) &&
            p_first_name base p <> "?" && p_surname base p <> "?" ->
            Printf.fprintf oc " 0"
        | _ -> ()
  end;
  print_if_no_empty oc base "#bp" (get_birth_place p);
  print_if_no_empty oc base "#bs" (get_birth_src p);
  begin match Adef.od_of_codate (get_baptism p) with
    Some d -> Printf.fprintf oc " !"; print_date oc d; ()
  | _ -> ()
  end;
  print_if_no_empty oc base "#pp" (get_baptism_place p);
  print_if_no_empty oc base "#ps" (get_baptism_src p);
  begin match get_death p with
    Death (dr, d) ->
      Printf.fprintf oc " ";
      begin match dr with
        Killed -> Printf.fprintf oc "k"
      | Murdered -> Printf.fprintf oc "m"
      | Executed -> Printf.fprintf oc "e"
      | Disappeared -> Printf.fprintf oc "s"
      | _ -> ()
      end;
      print_date oc (Adef.date_of_cdate d);
      ()
  | DeadYoung -> Printf.fprintf oc " mj"
  | DeadDontKnowWhen -> Printf.fprintf oc " 0"
  | DontKnowIfDead ->
      begin match
        Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p)
      with
        Some _, _ | _, Some _ -> Printf.fprintf oc " ?"
      | _ -> ()
      end
  | NotDead -> ()
  (* Fabrice: add this line *)
  | OfCourseDead -> Printf.fprintf oc " !"
  end;
  print_if_no_empty oc base "#dp" (get_death_place p);
  print_if_no_empty oc base "#ds" (get_death_src p);
  print_burial oc base (get_burial p);
  print_if_no_empty oc base "#rp" (get_burial_place p);
  print_if_no_empty oc base "#rs" (get_burial_src p);
  ()

let print_parent oc base mark fam_sel fam p =
  let has_printed_parents =
    match get_parents p with
      Some ifam -> fam_sel ifam
    | None -> false
  in
  let first_parent_definition =
    if mark.(Adef.int_of_iper (get_key_index p)) then false
    else begin mark.(Adef.int_of_iper (get_key_index p)) <- true; true end
  in
  let pr = not has_printed_parents && first_parent_definition in
  let has_infos = if pr then has_infos base p else false in
  let first_name = sou base (get_first_name p) in
  let surname = sou base (get_surname p) in
  Printf.fprintf oc "%s %s%s" (s_correct_string surname)
    (s_correct_string first_name)
    (if get_occ p == 0 || first_name = "?" || surname = "?" then ""
     else "." ^ string_of_int (get_occ p));
  if pr then
    if has_infos then print_infos oc base false true p
    else if first_name <> "?" && surname <> "?" then Printf.fprintf oc " 0";
  ()

let print_child oc base fam_surname print_sources p =
  Printf.fprintf oc "-";
  begin match get_sex p with
    Male -> Printf.fprintf oc " h"
  | Female -> Printf.fprintf oc " f"
  | _ -> ()
  end;
  Printf.fprintf oc " %s" (s_correct_string (sou base (get_first_name p)));
  if get_occ p == 0 || p_first_name base p = "?" || p_surname base p = "?"
  then
    ()
  else Printf.fprintf oc ".%d" (get_occ p);
  if get_surname p <> fam_surname then
    Printf.fprintf oc " %s" (s_correct_string (sou base (get_surname p)));
  print_infos oc base true print_sources p;
  Printf.fprintf oc "\n";
  ()

let bogus_person base p = p_first_name base p = "?" && p_surname base p = "?"

let common_children_sources base children =
  if Array.length children <= 1 then None
  else
    let rec loop i src =
      if i == Array.length children then
        let s = sou base src in if s = "" then None else Some src
      else
        let p = children.(i) in
        if get_psources p == src then loop (i + 1) src else None
    in
    loop 1 (get_psources children.(0))

let array_forall f a =
  let rec loop i =
    if i == Array.length a then true
    else if f a.(i) then loop (i + 1)
    else false
  in
  loop 0

let empty_family base m =
  bogus_person base m.m_fath && bogus_person base m.m_moth &&
  array_forall (bogus_person base) m.m_chil

let print_witness oc base mark p notes_pl_p =
  Printf.fprintf oc "%s %s%s" (correct_string base (get_surname p))
    (correct_string base (get_first_name p))
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p));
  if Array.length (get_family p) = 0 && get_parents p = None &&
     not mark.(Adef.int_of_iper (get_key_index p))
  then
    begin
      mark.(Adef.int_of_iper (get_key_index p)) <- true;
      if has_infos base p then print_infos oc base false true p
      else Printf.fprintf oc " 0";
      begin match sou base (get_notes p) with
        "" -> ()
      | _ -> notes_pl_p := p :: !notes_pl_p
      end;
      ()
    end;
  ()

let print_family oc base mark (per_sel, fam_sel) fam_done notes_pl_p m =
  let fam = m.m_fam in
  Printf.fprintf oc "fam ";
  print_parent oc base mark fam_sel fam m.m_fath;
  Printf.fprintf oc " +";
  print_date_option oc (Adef.od_of_codate (get_marriage fam));
  begin match get_relation fam with
    NotMarried -> Printf.fprintf oc " #nm"
  | Engaged -> Printf.fprintf oc " #eng"
  | _ -> ()
  end;
  print_if_no_empty oc base "#mp" (get_marriage_place fam);
  print_if_no_empty oc base "#ms" (get_marriage_src fam);
  begin match get_divorce fam with
    NotDivorced | Separated -> ()
  | Divorced d ->
      let d = Adef.od_of_codate d in
      Printf.fprintf oc " -"; print_date_option oc d; ()
  end;
  Printf.fprintf oc " ";
  print_parent oc base mark fam_sel fam m.m_moth;
  Printf.fprintf oc "\n";
  Array.iter
    (fun ip ->
       let p = poi base ip in
       Printf.fprintf oc "wit";
       begin match get_sex p with
         Male -> Printf.fprintf oc " m"
       | Female -> Printf.fprintf oc " f"
       | _ -> ()
       end;
       Printf.fprintf oc ": ";
       print_witness oc base mark p notes_pl_p;
       Printf.fprintf oc "\n";
       ())
    (get_witnesses fam);
  begin match sou base (get_fsources fam) with
    "" -> ()
  | s -> Printf.fprintf oc "src %s\n" (correct_string base (get_fsources fam))
  end;
  let print_sources =
    match common_children_sources base m.m_chil with
      Some s -> Printf.fprintf oc "csrc %s\n" (correct_string base s); false
    | _ -> true
  in
  begin match get_comment fam with
    txt when sou base txt <> "" ->
      Printf.fprintf oc "comm %s\n" (sou base txt)
  | _ -> ()
  end;
  begin match Array.length m.m_chil with
    0 -> ()
  | _ ->
      let fam_surname = get_surname m.m_fath in
      Printf.fprintf oc "beg\n";
      Array.iter
        (fun p ->
           if per_sel (get_key_index p) then
             print_child oc base fam_surname print_sources p)
        m.m_chil;
      Printf.fprintf oc "end\n"
  end;
  fam_done.(Adef.int_of_ifam m.m_ifam) <- true

let get_persons_with_notes base m list =
  let fath = m.m_fath in
  let moth = m.m_moth in
  let list =
    match sou base (get_notes fath), get_parents fath with
      "", _ | _, Some _ -> list
    | _ -> fath :: list
  in
  let list =
    match sou base (get_notes moth), get_parents moth with
      "", _ | _, Some _ -> list
    | _ -> moth :: list
  in
  List.fold_right
    (fun p list ->
       match sou base (get_notes p) with
         "" -> list
       | _ -> p :: list)
    (Array.to_list m.m_chil) list

let print_notes_for_person oc base p =
  let notes = sou base (get_notes p) in
  let surn = s_correct_string (p_surname base p) in
  let fnam = s_correct_string (p_first_name base p) in
  if notes <> "" && surn <> "?" && fnam <> "?" then
    begin
      Printf.fprintf oc "\n";
      Printf.fprintf oc "notes %s %s%s\n" surn fnam
        (if get_occ p == 0 then "" else "." ^ string_of_int (get_occ p));
      Printf.fprintf oc "beg\n";
      Printf.fprintf oc "%s\n" notes;
      Printf.fprintf oc "end notes\n";
      ()
    end

let rec list_memf f x =
  function
    [] -> false
  | a :: l -> f x a || list_memf f x l

let eq_key p1 p2 = get_key_index p1 == get_key_index p2

let print_notes oc base ml per_sel pl =
  let pl = List.fold_right (get_persons_with_notes base) ml pl in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key p pl then pl else p :: pl) pl []
  in
  List.iter
    (fun p ->
       if per_sel (get_key_index p) then print_notes_for_person oc base p)
    pl

let get_persons_with_relations base m list =
  let fath = m.m_fath in
  let moth = m.m_moth in
  let list =
    match get_rparents fath, get_parents fath with
      [], _ | _, Some _ -> list
    | _ -> fath :: list
  in
  let list =
    match get_rparents moth, get_parents moth with
      [], _ | _, Some _ -> list
    | _ -> moth :: list
  in
  let list =
    List.fold_right
      (fun ip list ->
         let p = poi base ip in
         match get_rparents p, get_parents p with
           [], _ | _, Some _ -> list
         | _ -> p :: list)
      (Array.to_list (get_witnesses m.m_fam)) list
  in
  List.fold_right
    (fun p list ->
       match get_rparents p with
         [] -> list
       | _ -> p :: list)
    (Array.to_list m.m_chil) list

let print_relation_parent oc base mark defined_p p =
  Printf.fprintf oc "%s %s%s" (correct_string base (get_surname p))
    (correct_string base (get_first_name p))
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p));
  if Array.length (get_family p) = 0 && get_parents p = None &&
     not mark.(Adef.int_of_iper (get_key_index p))
  then
    begin
      mark.(Adef.int_of_iper (get_key_index p)) <- true;
      if has_infos base p then print_infos oc base false true p
      else Printf.fprintf oc " 0";
      defined_p := p :: !defined_p;
      ()
    end;
  ()

let print_relation_for_person oc base mark def_p p r =
  let fath =
    match r.r_fath with
      Some ip ->
        let p = poi base ip in
        if sou base (get_first_name p) = "?" || sou base (get_surname p) = "?"
        then
          None
        else Some p
    | None -> None
  in
  let moth =
    match r.r_moth with
      Some ip ->
        let p = poi base ip in
        if sou base (get_first_name p) = "?" || sou base (get_surname p) = "?"
        then
          None
        else Some p
    | None -> None
  in
  match fath, moth with
    None, None -> ()
  | _ ->
      Printf.fprintf oc "- ";
      begin match r.r_type with
        Adoption -> Printf.fprintf oc "adop"
      | Recognition -> Printf.fprintf oc "reco"
      | CandidateParent -> Printf.fprintf oc "cand"
      | GodParent -> Printf.fprintf oc "godp"
      | FosterParent -> Printf.fprintf oc "fost"
      end;
      begin match fath, moth with
        Some _, None -> Printf.fprintf oc " fath"
      | None, Some _ -> Printf.fprintf oc " moth"
      | _ -> ()
      end;
      Printf.fprintf oc ": ";
      begin match fath, moth with
        Some fath, None -> print_relation_parent oc base mark def_p fath
      | None, Some moth -> print_relation_parent oc base mark def_p moth
      | Some fath, Some moth ->
          print_relation_parent oc base mark def_p fath;
          Printf.fprintf oc " + ";
          print_relation_parent oc base mark def_p moth;
          ()
      | _ -> ()
      end;
      Printf.fprintf oc "\n";
      ()

let print_relations_for_person oc base mark def_p p =
  let surn = correct_string base (get_surname p) in
  let fnam = correct_string base (get_first_name p) in
  if surn <> "?" || fnam <> "?" then
    begin
      Printf.fprintf oc "\n";
      Printf.fprintf oc "rel %s %s%s\n" surn fnam
        (if get_occ p == 0 then "" else "." ^ string_of_int (get_occ p));
      Printf.fprintf oc "beg\n";
      List.iter (print_relation_for_person oc base mark def_p p)
        (get_rparents p);
      Printf.fprintf oc "end\n";
      ()
    end

let print_relations oc base mark per_sel ml =
  let pl = List.fold_right (get_persons_with_relations base) ml [] in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key p pl then pl else p :: pl) pl []
  in
  let rec loop =
    function
      [] -> ()
    | p :: pl ->
        let def_p = ref [] in
        if get_rparents p <> [] && per_sel (get_key_index p) then
          begin
            print_relations_for_person oc base mark def_p p;
            List.iter (print_notes_for_person oc base) !def_p;
            ()
          end;
        loop (pl @ !def_p)
  in
  loop pl

let rec merge_families ifaml1f ifaml2f =
  match ifaml1f, ifaml2f with
    ifam1 :: ifaml1, ifam2 :: ifaml2 ->
      let m1 = List.memq ifam1 ifaml2 in
      let m2 = List.memq ifam2 ifaml1 in
      if m1 && m2 then merge_families ifaml1 ifaml2
      else if m1 then ifam2 :: merge_families ifaml1f ifaml2
      else if m2 then ifam1 :: merge_families ifaml1 ifaml2f
      else if ifam1 == ifam2 then ifam1 :: merge_families ifaml1 ifaml2
      else ifam1 :: ifam2 :: merge_families ifaml1 ifaml2
  | ifaml1, [] -> ifaml1
  | [], ifaml2 -> ifaml2

let rec filter f =
  function
    x :: l -> if f x then x :: filter f l else filter f l
  | [] -> []

let connected_families base fam_sel ifam cpl =
  let rec loop ifaml ipl_scanned =
    function
      ip :: ipl ->
        if List.memq ip ipl_scanned then loop ifaml ipl_scanned ipl
        else
          let u = poi base ip in
          let ifaml1 = Array.to_list (get_family u) in
          let ifaml1 = filter fam_sel ifaml1 in
          let ifaml = merge_families ifaml ifaml1 in
          let ipl =
            List.fold_right
              (fun ifam ipl ->
                 let cpl = foi base ifam in
                 get_father cpl :: get_mother cpl :: ipl)
              ifaml1 ipl
          in
          loop ifaml (ip :: ipl_scanned) ipl
    | [] -> ifaml
  in
  loop [ifam] [] [get_father cpl]

let find_person base p1 po p2 =
  match Gwdb.person_of_key base p1 p2 po with
    Some ip -> ip
  | None ->
      Printf.printf "Not found: %s%s %s\n" p1
        (if po == 0 then "" else " " ^ string_of_int po) p2;
      flush stdout;
      exit 2

let surnames = ref []
let no_spouses_parents = ref false

let gwu base out_dir out_oc src_oc_list anc desc =
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
  let (per_sel, fam_sel as sel) =
    Select.functions base anc desc !surnames !no_spouses_parents
  in
  let fam_done = Array.make (nb_of_families base) false in
  let mark = Array.make (nb_of_persons base) false in
  let out_oc_first = ref true in
  let origin_file fname =
    if out_dir = "" then out_oc, out_oc_first
    else if fname = "" then out_oc, out_oc_first
    else
      try List.assoc fname !src_oc_list with
        Not_found ->
          let oc = open_out (Filename.concat out_dir fname) in
          let x = oc, ref true in src_oc_list := (fname, x) :: !src_oc_list; x
  in
  for i = 0 to nb_of_families base - 1 do
    let ifam = Adef.ifam_of_int i in
    let fam = foi base ifam in
    if is_deleted_family fam then ()
    else
      begin
        if fam_done.(i) then ()
        else if fam_sel ifam then
          begin let (oc, first) =
            origin_file (sou base (get_origin_file fam))
          in
            let ifaml = connected_families base fam_sel ifam fam in
            let ml =
              List.fold_right
                (fun ifam ml ->
                   let fam = foi base ifam in
                   let m =
                     {m_ifam = ifam; m_fam = fam;
                      m_fath = poi base (get_father fam);
                      m_moth = poi base (get_mother fam);
                      m_chil =
                        Array.map (fun ip -> poi base ip) (get_children fam)}
                   in
                   if empty_family base m then ml else m :: ml)
                ifaml []
            in
            if ml <> [] then
              let notes_pl_p = ref [] in
              if not !first then Printf.fprintf oc "\n";
              first := false;
              List.iter (print_family oc base mark sel fam_done notes_pl_p)
                ml;
              print_notes oc base ml per_sel !notes_pl_p;
              print_relations oc base mark per_sel ml;
              ()
          end;
        ()
      end
  done;
  let s = base_notes_read base "" in
  if s = "" then ()
  else
    let (oc, first) = origin_file (base_notes_origin_file base) in
    if not !first then Printf.fprintf oc "\n";
    Printf.fprintf oc "notes\n";
    Printf.fprintf oc "%s\n" s;
    Printf.fprintf oc "end notes\n"

let in_file = ref ""
let out_file = ref ""
let out_dir = ref ""
let anc_1st = ref ""
let anc_occ = ref 0
let anc_2nd = ref ""
let desc_1st = ref ""
let desc_occ = ref 0
let desc_2nd = ref ""

type arg_state =
  ASnone | ASwaitAncOcc | ASwaitAncSurn | ASwaitDescOcc | ASwaitDescSurn
let arg_state = ref ASnone
let mem = ref false

let speclist =
  ["-o", Arg.String (fun s -> out_file := s),
   "<file>    output file name (else stdout)";
   "-odir", Arg.String (fun s -> out_dir := s),
   "<dir>  create files from original name in directory (else on -o file)";
   "-mem", Arg.Set mem, "        save memory space, but slower";
   "-a", Arg.String (fun s -> anc_1st := s; arg_state := ASwaitAncOcc),
   "\"<1st_name>\" [num] \"<surname>\" : select ancestors of...";
   "-d", Arg.String (fun s -> desc_1st := s; arg_state := ASwaitDescOcc),
   "\"<1st_name>\" [num] \"<surname>\" : select descendants of...";
   "-s", Arg.String (fun x -> surnames := x :: !surnames),
   "\"<surname>\" : select this surname (option usable several times)";
   "-nsp", Arg.Set no_spouses_parents,
   ": no spouses' parents (for option -s)"]

let anon_fun s =
  match !arg_state with
    ASnone -> in_file := s
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

let errmsg =
  "Usage: " ^ Sys.argv.(0) ^ " [options] <base_file>
If both options -a and -d are used, intersection is assumed.
If several options -s are used, union is assumed.
When option -s is used, the options -a and -d are ignored.
Options are:"

let main () =
  Argl.parse speclist anon_fun errmsg;
  if !in_file = "" then
    begin
      Printf.printf "Missing base\n";
      Printf.printf "Use option -help for usage\n";
      flush stdout;
      exit 2
    end;
  let anc =
    if !anc_1st <> "" then
      if !anc_2nd = "" then
        begin
          Printf.printf "Misused option -a\n";
          Printf.printf "Use option -help for usage\n";
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
          Printf.printf "Misused option -d\n";
          Printf.printf "Use option -help for usage\n";
          flush stdout;
          exit 2
        end
      else Some (!desc_1st, !desc_occ, !desc_2nd)
    else None
  in
  let base = Gwdb.open_base !in_file in
  let src_oc_list = ref [] in
  let () = load_ascends_array base in
  let () = load_strings_array base in
  if not !mem then
    begin
      load_couples_array base;
      load_unions_array base;
      load_descends_array base
    end;
  let out_oc = if !out_file = "" then stdout else open_out !out_file in
  gwu base !out_dir out_oc src_oc_list anc desc;
  List.iter (fun (src, (oc, _)) -> flush oc; close_out oc; ()) !src_oc_list;
  flush out_oc;
  if !out_file = "" then () else close_out out_oc;
  ()

let _ = Printexc.catch main ()
