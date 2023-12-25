(* $Id: gwu.ml,v 5.45 2012-01-19 06:28:42 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def
open Gutil
open Gwdb
open Printf

type mfam =
  { m_ifam : ifam;
    m_fam : family;
    m_fath : person;
    m_moth : person;
    m_chil : person array }

let soy y = if y = 0 then "-0" else string_of_int y

let print_date_dmy oc d =
  begin match d.prec with
    About -> fprintf oc "~"
  | Maybe -> fprintf oc "?"
  | Before -> fprintf oc "<"
  | After -> fprintf oc ">"
  | _ -> ()
  end;
  if d.month = 0 then fprintf oc "%s" (soy d.year)
  else if d.day = 0 then fprintf oc "%d/%s" d.month (soy d.year)
  else fprintf oc "%d/%d/%s" d.day d.month (soy d.year);
  match d.prec with
    OrYear y -> fprintf oc "|%s" (soy y)
  | YearInt y -> fprintf oc "..%s" (soy y)
  | _ -> ()

let is_printable =
  function
    '\000'..'\031' -> false
  | _ -> true

let starting_char no_num s =
  match s.[0] with
    'a'..'z' | 'A'..'Z' | 'à'..'ý' | 'À'..'Ý' -> true
  | '0'..'9' -> not no_num
  | '?' -> if s = "?" then true else false
  | _ -> false

let no_newlines s =
  let conv_char i =
    match s.[i] with
      '\n' | '\r' -> ' '
    | _ -> s.[i]
  in
  String.init (String.length s) conv_char

let raw_output = ref false
let no_picture = ref false

let gen_correct_string no_num no_colon s =
  let s = strip_spaces s in
  let s =
    if !(Mutil.utf_8_db) || !raw_output then s
    else Mutil.utf_8_of_iso_8859_1 s
  in
  let rec loop i len =
    if i = String.length s then Buff.get len
    else if len = 0 && not (starting_char no_num s) then
      loop i (Buff.store len '_')
    else
      match s.[i] with
        ' ' | '\n' | '\t' ->
          if i = String.length s - 1 then Buff.get len
          else loop (i + 1) (Buff.store len '_')
      | '_' | '\\' -> loop (i + 1) (Buff.store (Buff.store len '\\') s.[i])
      | ':' when no_colon ->
          let len = Buff.store len '\\' in
          loop (i + 1) (Buff.store (Buff.store len '\\') s.[i])
      | c ->
          let c = if is_printable c then c else '_' in
          loop (i + 1) (Buff.store len c)
  in
  loop 0 0

let s_correct_string s =
  let s = gen_correct_string false false s in if s = "" then "_" else s
let s_correct_string_nonum s =
  let s = gen_correct_string true false s in if s = "" then "_" else s

let correct_string base is = s_correct_string (sou base is)
let correct_string_no_colon base is =
  gen_correct_string false true (sou base is)

let gen_print_date no_colon oc =
  function
    Dgreg (d, Dgregorian) -> print_date_dmy oc d
  | Dgreg (d, Djulian) ->
      print_date_dmy oc (Calendar.julian_of_gregorian d); fprintf oc "J"
  | Dgreg (d, Dfrench) ->
      print_date_dmy oc (Calendar.french_of_gregorian d); fprintf oc "F"
  | Dgreg (d, Dhebrew) ->
      print_date_dmy oc (Calendar.hebrew_of_gregorian d); fprintf oc "H"
  | Dtext t ->
      (* Dans le cas d'une date texte pour un titre, on échappe les ':' *)
      let t = gen_correct_string false no_colon t in fprintf oc "0(%s)" t

let gen_print_date_option no_colon oc =
  function
    Some d -> gen_print_date no_colon oc d
  | None -> ()

let print_date oc = gen_print_date false oc
let print_date_option oc = gen_print_date_option false oc
let print_title_date_option oc = gen_print_date_option true oc

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

let print_if_not_equal_to x oc base lab is =
  if sou base is = x then ()
  else fprintf oc " %s %s" lab (correct_string base is)

let print_if_no_empty = print_if_not_equal_to ""

let print_first_name_alias oc base is =
  fprintf oc " {%s}" (correct_string base is)

let print_surname_alias oc base is =
  fprintf oc " #salias %s" (correct_string base is)

let print_qualifier oc base is =
  fprintf oc " #nick %s" (correct_string base is)

let print_alias oc base is = fprintf oc " #alias %s" (correct_string base is)

let print_burial oc base b =
  match b with
    Buried cod ->
      fprintf oc " #buri";
      begin match Adef.od_of_codate cod with
        Some d -> fprintf oc " "; print_date oc d; ()
      | _ -> ()
      end
  | Cremated cod ->
      fprintf oc " #crem";
      begin match Adef.od_of_codate cod with
        Some d -> fprintf oc " "; print_date oc d; ()
      | _ -> ()
      end
  | UnknownBurial -> ()

let print_title oc base t =
  let t_date_start = Adef.od_of_codate t.t_date_start in
  let t_date_end = Adef.od_of_codate t.t_date_end in
  fprintf oc " [";
  begin match t.t_name with
    Tmain -> fprintf oc "*"
  | Tname s -> fprintf oc "%s" (correct_string_no_colon base s)
  | Tnone -> ()
  end;
  fprintf oc ":";
  fprintf oc "%s" (correct_string_no_colon base t.t_ident);
  fprintf oc ":";
  fprintf oc "%s" (correct_string_no_colon base t.t_place);
  if t.t_nth <> 0 then fprintf oc ":"
  else
    begin match t_date_start, t_date_end with
      Some _, _ | _, Some _ -> fprintf oc ":"
    | _ -> ()
    end;
  print_title_date_option oc t_date_start;
  if t.t_nth <> 0 then fprintf oc ":"
  else
    begin match t_date_end with
      Some _ -> fprintf oc ":"
    | _ -> ()
    end;
  print_title_date_option oc t_date_end;
  if t.t_nth <> 0 then fprintf oc ":%d" t.t_nth;
  fprintf oc "]"

let print_infos oc base is_child csrc cbp p =
  List.iter (print_first_name_alias oc base) (get_first_names_aliases p);
  List.iter (print_surname_alias oc base) (get_surnames_aliases p);
  begin match get_public_name p with
    s when sou base s <> "" -> fprintf oc " (%s)" (correct_string base s)
  | _ -> ()
  end;
  if not !no_picture then print_if_no_empty oc base "#image" (get_image p);
  List.iter (print_qualifier oc base) (get_qualifiers p);
  List.iter (print_alias oc base) (get_aliases p);
  List.iter (print_title oc base) (get_titles p);
  begin match get_access p with
    IfTitles -> ()
  | Public -> fprintf oc " #apubl"
  | Private -> fprintf oc " #apriv"
  | Friend -> fprintf oc " #afriend"
  | Friend_m -> fprintf oc " #afriendm"
  end;
  print_if_no_empty oc base "#occu" (get_occupation p);
  print_if_not_equal_to csrc oc base "#src" (get_psources p);
  begin match Adef.od_of_codate (get_birth p) with
    Some d -> fprintf oc " "; print_date oc d
  | _ ->
      if get_baptism p <> Adef.codate_None then ()
      else
        match get_death p with
          Death (_, _) | DeadYoung | DeadDontKnowWhen | OfCourseDead ->
            fprintf oc " 0"
        | DontKnowIfDead
          when
            not is_child && not (has_infos_not_dates base p) &&
            p_first_name base p <> "?" && p_surname base p <> "?" ->
            fprintf oc " 0"
        | _ -> ()
  end;
  print_if_not_equal_to cbp oc base "#bp" (get_birth_place p);
  print_if_no_empty oc base "#bs" (get_birth_src p);
  begin match Adef.od_of_codate (get_baptism p) with
    Some d -> fprintf oc " !"; print_date oc d
  | _ -> ()
  end;
  print_if_no_empty oc base "#pp" (get_baptism_place p);
  print_if_no_empty oc base "#ps" (get_baptism_src p);
  begin match get_death p with
    Death (dr, d) ->
      fprintf oc " ";
      begin match dr with
        Killed -> fprintf oc "k"
      | Murdered -> fprintf oc "m"
      | Executed -> fprintf oc "e"
      | Disappeared -> fprintf oc "s"
      | _ -> ()
      end;
      print_date oc (Adef.date_of_cdate d)
  | DeadYoung -> fprintf oc " mj"
  | DeadDontKnowWhen -> fprintf oc " 0"
  | DontKnowIfDead ->
      begin match
        Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p)
      with
        Some _, _ | _, Some _ -> fprintf oc " ?"
      | _ -> ()
      end
  | OfCourseDead -> fprintf oc " od"
  | NotDead -> ()
  end;
  print_if_no_empty oc base "#dp" (get_death_place p);
  print_if_no_empty oc base "#ds" (get_death_src p);
  print_burial oc base (get_burial p);
  print_if_no_empty oc base "#rp" (get_burial_place p);
  print_if_no_empty oc base "#rs" (get_burial_src p)

type gen =
  { mark : bool array;
    per_sel : iper -> bool;
    fam_sel : ifam -> bool;
    fam_done : bool array;
    mutable notes_pl_p : person list;
    mutable ext_files : (string * string list ref) list;
    mutable notes_alias : (string * string) list }

let map_notes aliases f = try List.assoc f aliases with Not_found -> f

let add_linked_files gen from s some_linked_files =
  let slen = String.length s in
  let rec loop new_linked_files i =
    if i = slen then new_linked_files
    else if i < slen - 2 && s.[i] = '[' && s.[i+1] = '[' && s.[i+2] = '[' then
      let j =
        let rec loop j =
          if j = slen then j
          else if
            j < slen - 2 && s.[j] = ']' && s.[j+1] = ']' && s.[j+2] = ']'
          then
            j + 3
          else loop (j + 1)
        in
        loop (i + 3)
      in
      if j > i + 6 then
        let b = String.sub s (i + 3) (j - i - 6) in
        let fname =
          try let k = String.index b '/' in String.sub b 0 k with
            Not_found -> b
        in
        let fname = map_notes gen.notes_alias fname in
        let f = from () in
        let new_linked_files =
          try
            let r = List.assoc fname gen.ext_files in
            if List.mem f !r then () else r := f :: !r; new_linked_files
          with Not_found ->
            let lf = fname, ref [f] in
            gen.ext_files <- lf :: gen.ext_files; lf :: new_linked_files
        in
        loop new_linked_files j
      else loop new_linked_files (i + 1)
    else loop new_linked_files (i + 1)
  in
  loop some_linked_files 0

let print_parent oc base gen fam p =
  let has_printed_parents =
    match get_parents p with
      Some ifam -> gen.fam_sel ifam
    | None -> false
  in
  let first_parent_definition =
    if gen.mark.(Adef.int_of_iper (get_key_index p)) then false
    else begin gen.mark.(Adef.int_of_iper (get_key_index p)) <- true; true end
  in
  let pr = not has_printed_parents && first_parent_definition in
  let has_infos = if pr then has_infos base p else false in
  let first_name = sou base (get_first_name p) in
  let surname = sou base (get_surname p) in
  fprintf oc "%s %s%s" (s_correct_string surname)
    (s_correct_string first_name)
    (if get_occ p = 0 || first_name = "?" || surname = "?" then ""
     else "." ^ string_of_int (get_occ p));
  if pr then
    if has_infos then print_infos oc base false "" "" p
    else if first_name <> "?" && surname <> "?" then fprintf oc " 0"

let print_child oc base fam_surname csrc cbp p =
  fprintf oc "-";
  begin match get_sex p with
    Male -> fprintf oc " h"
  | Female -> fprintf oc " f"
  | _ -> ()
  end;
  fprintf oc " %s" (s_correct_string (sou base (get_first_name p)));
  if get_occ p = 0 || p_first_name base p = "?" || p_surname base p = "?" then
    ()
  else fprintf oc ".%d" (get_occ p);
  if not (eq_istr (get_surname p) fam_surname) then
    fprintf oc " %s" (s_correct_string_nonum (sou base (get_surname p)));
  print_infos oc base true csrc cbp p;
  fprintf oc "\n"

let bogus_person base p = p_first_name base p = "?" && p_surname base p = "?"

let common_children proj base children =
  if Array.length children <= 1 then None
  else
    let list =
      List.map (fun p -> sou base (proj p)) (Array.to_list children)
    in
    if List.mem "" list then None
    else
      let list = List.sort compare list in
      let (src_max, n_max, _, _) =
        List.fold_left
          (fun (src_max, n_max, prev_src, n) src ->
             if src = prev_src then
               let n = n + 1 in
               if n > n_max then src, n, src, n else src_max, n_max, src, n
             else src_max, n_max, src, 1)
          ("", 0, "", 0) list
      in
      if n_max > 1 then Some src_max else None

let common_children_sources = common_children get_psources
let common_children_birth_place = common_children get_birth_place

let array_forall f a =
  let rec loop i =
    if i = Array.length a then true
    else if f a.(i) then loop (i + 1)
    else false
  in
  loop 0

let empty_family base m =
  bogus_person base m.m_fath && bogus_person base m.m_moth &&
  array_forall (bogus_person base) m.m_chil

let print_witness oc base gen p =
  fprintf oc "%s %s%s" (correct_string base (get_surname p))
    (correct_string base (get_first_name p))
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p));
  if Array.length (get_family p) = 0 && get_parents p = None &&
     not gen.mark.(Adef.int_of_iper (get_key_index p))
  then
    begin
      gen.mark.(Adef.int_of_iper (get_key_index p)) <- true;
      if has_infos base p then print_infos oc base false "" "" p
      else fprintf oc " 0";
      match sou base (get_notes p) with
        "" -> ()
      | _ -> gen.notes_pl_p <- p :: gen.notes_pl_p
    end

let print_family oc base gen m =
  let fam = m.m_fam in
  fprintf oc "fam ";
  print_parent oc base gen fam m.m_fath;
  fprintf oc " +";
  print_date_option oc (Adef.od_of_codate (get_marriage fam));
  begin match get_relation fam with
    NotMarried -> fprintf oc " #nm"
  | Married -> ()
  | Engaged -> fprintf oc " #eng"
  | NoSexesCheckNotMarried ->
      let c x =
        match get_sex x with
          Male -> 'm'
        | Female -> 'f'
        | Neuter -> '?'
      in
      fprintf oc " #nsck %c%c" (c m.m_fath) (c m.m_moth)
  | NoSexesCheckMarried ->
      let c x =
        match get_sex x with
          Male -> 'm'
        | Female -> 'f'
        | Neuter -> '?'
      in
      fprintf oc " #nsckm %c%c" (c m.m_fath) (c m.m_moth)
  | NoMention -> fprintf oc " #noment"
  end;
  print_if_no_empty oc base "#mp" (get_marriage_place fam);
  print_if_no_empty oc base "#ms" (get_marriage_src fam);
  begin match get_divorce fam with
    NotDivorced -> ()
  | Separated -> fprintf oc " #sep"
  | Divorced d ->
      let d = Adef.od_of_codate d in fprintf oc " -"; print_date_option oc d
  end;
  fprintf oc " ";
  print_parent oc base gen fam m.m_moth;
  fprintf oc "\n";
  Array.iter
    (fun ip ->
       if gen.per_sel ip then
         let p = poi base ip in
         fprintf oc "wit";
         begin match get_sex p with
           Male -> fprintf oc " m"
         | Female -> fprintf oc " f"
         | _ -> ()
         end;
         fprintf oc ": ";
         print_witness oc base gen p;
         fprintf oc "\n")
    (get_witnesses fam);
  let fsources = sou base (get_fsources fam) in
  begin match fsources with
    "" -> ()
  | s -> fprintf oc "src %s\n" (correct_string base (get_fsources fam))
  end;
  let csrc =
    match common_children_sources base m.m_chil with
      Some s -> fprintf oc "csrc %s\n" (s_correct_string s); s
    | _ -> ""
  in
  let cbp =
    match common_children_birth_place base m.m_chil with
      Some s -> fprintf oc "cbp %s\n" (s_correct_string s); s
    | _ -> ""
  in
  begin match get_comment fam with
    txt when sou base txt <> "" ->
      fprintf oc "comm %s\n" (no_newlines (sou base txt))
  | _ -> ()
  end;
  begin match Array.length m.m_chil with
    0 -> ()
  | _ ->
      let fam_surname = get_surname m.m_fath in
      fprintf oc "beg\n";
      Array.iter
        (fun p ->
           if gen.per_sel (get_key_index p) then
             print_child oc base fam_surname csrc cbp p)
        m.m_chil;
      fprintf oc "end\n"
  end;
  gen.fam_done.(Adef.int_of_ifam m.m_ifam) <- true;
  let f _ =
    sprintf "family \"%s.%d %s\" & \"%s.%d %s\"" (p_first_name base m.m_fath)
      (get_occ m.m_fath) (p_surname base m.m_fath)
      (p_first_name base m.m_moth) (get_occ m.m_moth)
      (p_surname base m.m_moth)
  in
  ignore (add_linked_files gen f fsources [] : _ list)

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

let notes_aliases bdir =
  let fname = Filename.concat bdir "notes.alias" in
  match try Some (Secure.open_in fname) with Sys_error _ -> None with
    Some ic ->
      let rec loop list =
        match try Some (input_line ic) with End_of_file -> None with
          Some s ->
            let list =
              try
                let i = String.index s ' ' in
                (String.sub s 0 i,
                 String.sub s (i + 1) (String.length s - i - 1)) ::
                list
              with Not_found -> list
            in
            loop list
        | None -> close_in ic; list
      in
      loop []
  | None -> []

let print_notes_for_person oc base gen p =
  let notes = sou base (get_notes p) in
  let surn = s_correct_string (p_surname base p) in
  let fnam = s_correct_string (p_first_name base p) in
  if notes <> "" && surn <> "?" && fnam <> "?" then
    begin
      fprintf oc "\n";
      fprintf oc "notes %s %s%s\n" surn fnam
        (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p));
      fprintf oc "beg\n";
      fprintf oc "%s\n" notes;
      fprintf oc "end notes\n"
    end;
  let f _ =
    sprintf "person \"%s.%d %s\"" (p_first_name base p) (get_occ p)
      (p_surname base p)
  in
  ignore (add_linked_files gen f notes [] : _ list);
  let s =
    let sl =
      [get_notes; get_birth_src; get_baptism_src; get_death_src;
       get_burial_src; get_psources]
    in
    String.concat " " (List.map (fun f -> sou base (f p)) sl)
  in
  ignore (add_linked_files gen f s [] : _ list)

let rec list_memf f x =
  function
    [] -> false
  | a :: l -> f x a || list_memf f x l

let eq_key p1 p2 = get_key_index p1 = get_key_index p2
let eq_key_fst (p1, _) (p2, _) = get_key_index p1 = get_key_index p2

let print_notes oc base gen ml =
  let pl = List.fold_right (get_persons_with_notes base) ml gen.notes_pl_p in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key p pl then pl else p :: pl) pl []
  in
  List.iter
    (fun p ->
       if gen.per_sel (get_key_index p) then
         print_notes_for_person oc base gen p)
    pl

let is_isolated base p =
  match get_parents p with
    Some _ -> false
  | None -> Array.length (get_family p) = 0

let is_definition_for_parent base p =
  match get_parents p with
    Some _ -> false
  | None -> true

let get_isolated_related base m list =
  let concat_isolated p_relation ip list =
    let p = poi base ip in
    if List.mem_assq p list then list
    else if is_isolated base p then
      match get_rparents p with
        {r_fath = Some x} :: _ when x = get_key_index p_relation ->
          list @ [p, true]
      | {r_fath = None; r_moth = Some x} :: _
        when x = get_key_index p_relation ->
          list @ [p, true]
      | _ -> list
    else list
  in
  let list =
    if is_definition_for_parent base m.m_fath then
      List.fold_right (concat_isolated m.m_fath) (get_related m.m_fath) list
    else list
  in
  let list =
    if is_definition_for_parent base m.m_moth then
      List.fold_right (concat_isolated m.m_moth) (get_related m.m_moth) list
    else list
  in
  let list =
    List.fold_right
      (fun p list -> List.fold_right (concat_isolated p) (get_related p) list)
      (Array.to_list m.m_chil) list
  in
  list

let get_persons_with_relations base m list =
  let fath = m.m_fath in
  let moth = m.m_moth in
  let list =
    match get_rparents fath, get_parents fath with
      [], _ | _, Some _ -> list
    | _ -> (fath, false) :: list
  in
  let list =
    match get_rparents moth, get_parents moth with
      [], _ | _, Some _ -> list
    | _ -> (moth, false) :: list
  in
  let list =
    List.fold_right
      (fun ip list ->
         let p = poi base ip in
         match get_rparents p, get_parents p with
           [], _ | _, Some _ -> list
         | {r_fath = Some x} :: _, _ when x <> get_key_index m.m_fath -> list
         | _ -> (p, false) :: list)
      (Array.to_list (get_witnesses m.m_fam)) list
  in
  List.fold_right
    (fun p list ->
       match get_rparents p with
         [] -> list
       | _ -> (p, false) :: list)
    (Array.to_list m.m_chil) list

let print_relation_parent oc base mark defined_p p =
  fprintf oc "%s %s%s" (correct_string base (get_surname p))
    (correct_string base (get_first_name p))
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p));
  if Array.length (get_family p) = 0 && get_parents p = None &&
     not mark.(Adef.int_of_iper (get_key_index p))
  then
    begin
      mark.(Adef.int_of_iper (get_key_index p)) <- true;
      if has_infos base p then print_infos oc base false "" "" p
      else fprintf oc " 0";
      defined_p := p :: !defined_p
    end

let print_relation_for_person oc base gen def_p p r =
  let fath =
    match r.r_fath with
      Some ip ->
        if gen.per_sel ip then
          let p = poi base ip in
          if sou base (get_first_name p) = "?" ||
             sou base (get_surname p) = "?"
          then
            None
          else Some p
        else None
    | None -> None
  in
  let moth =
    match r.r_moth with
      Some ip ->
        if gen.per_sel ip then
          let p = poi base ip in
          if sou base (get_first_name p) = "?" ||
             sou base (get_surname p) = "?"
          then
            None
          else Some p
        else None
    | None -> None
  in
  match fath, moth with
    None, None -> ()
  | _ ->
      fprintf oc "- ";
      begin match r.r_type with
        Adoption -> fprintf oc "adop"
      | Recognition -> fprintf oc "reco"
      | CandidateParent -> fprintf oc "cand"
      | GodParent -> fprintf oc "godp"
      | FosterParent -> fprintf oc "fost"
      end;
      begin match fath, moth with
        Some _, None -> fprintf oc " fath"
      | None, Some _ -> fprintf oc " moth"
      | _ -> ()
      end;
      fprintf oc ": ";
      begin match fath, moth with
        Some fath, None -> print_relation_parent oc base gen.mark def_p fath
      | None, Some moth -> print_relation_parent oc base gen.mark def_p moth
      | Some fath, Some moth ->
          print_relation_parent oc base gen.mark def_p fath;
          fprintf oc " + ";
          print_relation_parent oc base gen.mark def_p moth
      | _ -> ()
      end;
      fprintf oc "\n"

let print_relations_for_person oc base gen def_p is_definition p =
  let surn = correct_string base (get_surname p) in
  let fnam = correct_string base (get_first_name p) in
  let exist_relation =
    List.exists
      (fun r ->
         match r.r_fath, r.r_moth with
           Some ip1, Some ip2 -> gen.per_sel ip1 && gen.per_sel ip2
         | Some ip1, _ -> gen.per_sel ip1
         | _, Some ip2 -> gen.per_sel ip2
         | _ -> false)
      (get_rparents p)
  in
  if surn <> "?" && fnam <> "?" && exist_relation then
    begin
      fprintf oc "\n";
      fprintf oc "rel %s %s%s" surn fnam
        (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p));
      if is_definition then
        begin
          if has_infos base p then print_infos oc base false "" "" p
          else fprintf oc " 0";
          match get_sex p with
            Male -> fprintf oc " #h"
          | Female -> fprintf oc " #f"
          | Neuter -> ()
        end;
      fprintf oc "\n";
      fprintf oc "beg\n";
      List.iter (print_relation_for_person oc base gen def_p p)
        (get_rparents p);
      fprintf oc "end\n"
    end

let print_relations oc base gen ml =
  let pl = List.fold_right (get_persons_with_relations base) ml [] in
  let pl = List.fold_right (get_isolated_related base) ml pl in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key_fst p pl then pl else p :: pl) pl []
  in
  let rec loop =
    function
      [] -> ()
    | (p, if_def) :: pl ->
        let def_p = ref [] in
        if get_rparents p <> [] && gen.per_sel (get_key_index p) then
          begin
            print_relations_for_person oc base gen def_p if_def p;
            List.iter (print_notes_for_person oc base gen) !def_p
          end;
        loop (pl @ List.map (fun p -> p, false) !def_p)
  in
  loop pl

let rec merge_families ifaml1f ifaml2f =
  match ifaml1f, ifaml2f with
    ifam1 :: ifaml1, ifam2 :: ifaml2 ->
      let m1 = List.mem ifam1 ifaml2 in
      let m2 = List.mem ifam2 ifaml1 in
      if m1 && m2 then merge_families ifaml1 ifaml2
      else if m1 then ifam2 :: merge_families ifaml1f ifaml2
      else if m2 then ifam1 :: merge_families ifaml1 ifaml2f
      else if ifam2 < ifam1 then
        ifam2 :: ifam1 :: merge_families ifaml1 ifaml2
      else if ifam1 < ifam2 then
        ifam1 :: ifam2 :: merge_families ifaml1 ifaml2
      else ifam1 :: merge_families ifaml1 ifaml2
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
        if List.mem ip ipl_scanned then loop ifaml ipl_scanned ipl
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
  match person_of_key base p1 p2 po with
    Some ip -> ip
  | None ->
      printf "Not found: %s%s %s\n" p1
        (if po = 0 then "" else " " ^ string_of_int po) p2;
      flush stdout;
      exit 2

let read_file_contents fname =
  match try Some (open_in fname) with Sys_error _ -> None with
    Some ic ->
      let len = ref 0 in
      begin try
        let rec loop () = len := Buff.store !len (input_char ic); loop () in
        loop ()
      with End_of_file -> close_in ic; Buff.get !len
      end
  | None -> ""

(* Separate option *)

type separate = ToSeparate | NotScanned | BeingScanned | Scanned

let rec find_ancestors base surn p list =
  match get_parents p with
    Some ifam ->
      let cpl = foi base ifam in
      let fath = poi base (get_father cpl) in
      let moth = poi base (get_mother cpl) in
      if not (eq_istr (get_surname fath) surn) &&
         not (eq_istr (get_surname moth) surn)
      then
        p :: list
      else
        let list =
          if eq_istr (get_surname fath) surn then
            find_ancestors base surn fath list
          else list
        in
        let list =
          if eq_istr (get_surname moth) surn then
            find_ancestors base surn moth list
          else list
        in
        list
  | None -> p :: list

let mark_branch base mark surn p =
  let rec loop top p =
    for i = 0 to Array.length (get_family p) - 1 do
      let ifam = (get_family p).(i) in
      match mark.(Adef.int_of_ifam ifam) with
        NotScanned ->
          let ifaml =
            connected_families base (fun _ -> true) ifam (foi base ifam)
          in
          let children =
            List.fold_left
              (fun list ifam ->
                 let desc = foi base ifam in
                 Array.fold_left (fun list ip -> poi base ip :: list) list
                   (get_children desc))
              [] ifaml
          in
          if top ||
             List.exists (fun p -> eq_istr (get_surname p) surn) children
          then
            begin
              List.iter
                (fun ifam -> mark.(Adef.int_of_ifam ifam) <- ToSeparate)
                ifaml;
              List.iter (loop false) children
            end
      | _ -> ()
    done
  in
  loop true p

let mark_someone base mark s =
  match Gutil.person_ht_find_all base s with
    [ip] ->
      let p = poi base ip in
      let plist = find_ancestors base (get_surname p) p [] in
      List.iter (mark_branch base mark (get_surname p)) plist
  | [] -> eprintf "Error: \"%s\" is not found\n" s; flush stderr; exit 2
  | _ -> eprintf "Error: several answers for \"%s\"\n" s; flush stderr; exit 2

let sep_limit = ref 21
let only_file = ref ""
let separate_list = ref []

let scan_connex_component base test_action len ifam =
  let rec loop len ifam =
    let fam = foi base ifam in
    let fath = poi base (get_father fam) in
    let moth = poi base (get_mother fam) in
    let len =
      Array.fold_left
        (fun len ifam1 ->
           if ifam1 = ifam then len else test_action loop len ifam1)
        len (get_family fath)
    in
    let len =
      Array.fold_left
        (fun len ifam1 ->
           if ifam1 = ifam then len else test_action loop len ifam1)
        len (get_family moth)
    in
    let len =
      match get_parents fath with
        Some ifam -> test_action loop len ifam
      | _ -> len
    in
    let len =
      match get_parents moth with
        Some ifam -> test_action loop len ifam
      | _ -> len
    in
    let children = get_children fam in
    let len =
      Array.fold_left
        (fun len ip ->
           Array.fold_left (test_action loop) len (get_family (poi base ip)))
        len children
    in
    len
  in
  loop len ifam

let mark_one_connex_component base mark ifam =
  let origin_file = sou base (get_origin_file (foi base ifam)) in
  let test_action loop len ifam =
    if mark.(Adef.int_of_ifam ifam) = NotScanned &&
       sou base (get_origin_file (foi base ifam)) = origin_file
    then
      begin
        mark.(Adef.int_of_ifam ifam) <- BeingScanned;
        loop (len + 1) ifam
      end
    else len
  in
  let _ = test_action (fun _ _ -> 1) 0 ifam in
  let len = 1 + scan_connex_component base test_action 0 ifam in
  let set_mark x =
    let test_action loop () ifam =
      if mark.(Adef.int_of_ifam ifam) = BeingScanned then
        begin mark.(Adef.int_of_ifam ifam) <- x; loop () ifam end
    in
    test_action (fun _ _ -> ()) () ifam;
    scan_connex_component base test_action () ifam
  in
  if len <= !sep_limit && (!only_file = "" || !only_file = origin_file) then
    set_mark ToSeparate
  else
    begin
      eprintf "%s: group of size %d not included\n" origin_file len;
      let cpl = foi base ifam in
      eprintf "    %s + %s\n" (designation base (poi base (get_father cpl)))
        (designation base (poi base (get_mother cpl)));
      flush stderr;
      set_mark Scanned
    end

let mark_connex_components base mark ifam =
  let test_action loop len ifam =
    if mark.(Adef.int_of_ifam ifam) = NotScanned then
      mark_one_connex_component base mark ifam
  in
  scan_connex_component base test_action () ifam

let add_small_connex_components base mark =
  for i = 0 to nb_of_families base - 1 do
    if mark.(i) = ToSeparate then
      mark_connex_components base mark (Adef.ifam_of_int i)
  done

let separate base =
  match List.rev !separate_list with
    [] -> (fun _ -> false)
  | list ->
      let mark = Array.make (nb_of_families base) NotScanned in
      List.iter (mark_someone base mark) list;
      add_small_connex_components base mark;
      let len =
        let rec loop len i =
          if i = nb_of_families base then len
          else if mark.(i) = ToSeparate then loop (len + 1) (i + 1)
          else loop len (i + 1)
        in
        loop 0 0
      in
      eprintf "*** extracted %d families\n" len;
      flush stderr;
      fun ifam -> mark.(Adef.int_of_ifam ifam) = ToSeparate

let rs_printf oc s =
  let rec loop bol i =
    if i = String.length s then ()
    else if s.[i] = '\n' then begin fprintf oc "\n"; loop true (i + 1) end
    else
      begin
        if bol then fprintf oc "  ";
        fprintf oc "%c" s.[i];
        loop false (i + 1)
      end
  in
  loop true 0

(* Main *)

let surnames = ref []
let no_spouses_parents = ref false
let no_notes = ref false
let censor = ref 0
let with_siblings = ref false
let maxlev = ref (-1)

let gwu base in_dir out_dir out_oc src_oc_ht anc desc ancdesc =
  let to_separate = separate base in
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
  let ancdesc =
    match ancdesc with
      Some (p1, po, p2) -> Some (find_person base p1 po p2)
    | None -> None
  in
  let out_oc_first = ref true in
  let origin_file fname =
    if out_dir = "" then out_oc, out_oc_first
    else if fname = "" then out_oc, out_oc_first
    else
      try Hashtbl.find src_oc_ht fname with
        Not_found ->
          let oc = open_out (Filename.concat out_dir fname) in
          let x = oc, ref true in
          if !raw_output then () else fprintf oc "encoding: utf-8\n\n";
          Hashtbl.add src_oc_ht fname x;
          x
  in
  let gen =
    let mark = Array.make (nb_of_persons base) false in
    let (per_sel, fam_sel) =
      Select.functions base anc desc !surnames ancdesc !no_spouses_parents
        !censor !with_siblings !maxlev
    in
    let fam_done = Array.make (nb_of_families base) false in
    {mark = mark; per_sel = per_sel; fam_sel = fam_sel; fam_done = fam_done;
     notes_pl_p = []; ext_files = []; notes_alias = notes_aliases in_dir}
  in
  let nb_fam = nb_of_families base in
  if !(Mutil.verbose) then ProgrBar.start ();
  for i = 0 to nb_fam - 1 do
    if !(Mutil.verbose) then ProgrBar.run i nb_fam;
    let ifam = Adef.ifam_of_int i in
    let fam = foi base ifam in
    if is_deleted_family fam then ()
    else if gen.fam_done.(i) then ()
    else if gen.fam_sel ifam then
      let ifaml = connected_families base gen.fam_sel ifam fam in
      let (oc, first) =
        if to_separate ifam then out_oc, out_oc_first
        else origin_file (sou base (get_origin_file fam))
      in
      let ml =
        List.fold_right
          (fun ifam ml ->
             let fam = foi base ifam in
             let m =
               {m_ifam = ifam; m_fam = fam;
                m_fath = poi base (get_father fam);
                m_moth = poi base (get_mother fam);
                m_chil = Array.map (fun ip -> poi base ip) (get_children fam)}
             in
             if empty_family base m then
               begin gen.fam_done.(Adef.int_of_ifam m.m_ifam) <- true; ml end
             else m :: ml)
          ifaml []
      in
      (* ajout pour sortir les mariages dans le bon ordre *)
      let ml =
        List.stable_sort
          (fun m1 m2 ->
             match
               Adef.od_of_codate (get_marriage m1.m_fam),
               Adef.od_of_codate (get_marriage m2.m_fam)
             with
               Some d1, Some d2 -> CheckItem.compare_date d1 d2
             | _ -> 0)
          ml
      in
      (* *)
      if ml <> [] then
        begin
          gen.notes_pl_p <- [];
          if not !first then fprintf oc "\n";
          first := false;
          List.iter (print_family oc base gen) ml;
          print_notes oc base gen ml;
          print_relations oc base gen ml
        end
  done;
  if !(Mutil.verbose) then ProgrBar.finish ();
  if not !no_notes then
    let s = base_notes_read base "" in
    let (oc, first) = origin_file (base_notes_origin_file base) in
    if s <> "" then
      begin
        if not !first then fprintf oc "\n";
        first := false;
        fprintf oc "notes-db\n";
        rs_printf oc s;
        fprintf oc "\nend notes-db\n";
        ignore
          (add_linked_files gen (fun _ -> "database notes") s [] : _ list)
      end;
    begin try
      let files =
        Sys.readdir (Filename.concat in_dir (base_wiznotes_dir base))
      in
      Array.sort compare files;
      for i = 0 to Array.length files - 1 do
        let file = files.(i) in
        if Filename.check_suffix file ".txt" then
          let wfile =
            List.fold_left Filename.concat in_dir
              [base_wiznotes_dir base; file]
          in
          let s = read_file_contents wfile in
          ignore
            (add_linked_files gen (fun _ -> "wizard \"" ^ file ^ "\"") s [] :
             _ list)
      done
    with Sys_error _ -> ()
    end;
    let rec loop =
      function
        [] -> ()
      | (f, _) :: files ->
          let fn =
            match NotesLinks.check_file_name f with
              Some (dl, f) -> List.fold_right Filename.concat dl f
            | None -> "bad"
          in
          let s = base_notes_read base fn in
          let files =
            add_linked_files gen (fun _ -> sprintf "extended page \"%s\"" f) s
              files
          in
          loop files
    in
    loop gen.ext_files;
    List.iter
      (fun (f, r) ->
         let fn =
           match NotesLinks.check_file_name f with
             Some (dl, f) -> List.fold_right Filename.concat dl f
           | None -> "bad"
         in
         let s = strip_spaces (base_notes_read base fn) in
         if s <> "" then
           begin
             if not !first then fprintf oc "\n";
             first := false;
             fprintf oc "# extended page \"%s\" used by:\n" f;
             List.iter (fun f -> fprintf oc "#  - %s\n" f)
               (List.sort compare !r);
             fprintf oc "page-ext %s\n" f;
             rs_printf oc s;
             fprintf oc "\nend page-ext\n"
           end)
      (List.sort compare gen.ext_files);
    try
      let files =
        Sys.readdir (Filename.concat in_dir (base_wiznotes_dir base))
      in
      Array.sort compare files;
      for i = 0 to Array.length files - 1 do
        let file = files.(i) in
        if Filename.check_suffix file ".txt" then
          let wizid = Filename.chop_suffix file ".txt" in
          let wfile =
            List.fold_left Filename.concat in_dir
              [base_wiznotes_dir base; file]
          in
          let s = strip_spaces (read_file_contents wfile) in
          fprintf oc "\nwizard-note %s\n" wizid;
          rs_printf oc s;
          fprintf oc "\nend wizard-note\n"
      done
    with Sys_error _ -> ()

let in_file = ref ""
let out_file = ref ""
let out_dir = ref ""
let anc_1st = ref ""
let anc_occ = ref 0
let anc_2nd = ref ""
let desc_1st = ref ""
let desc_occ = ref 0
let desc_2nd = ref ""
let ancdesc_1st = ref ""
let ancdesc_occ = ref 0
let ancdesc_2nd = ref ""

type arg_state =
    ASnone
  | ASwaitAncOcc
  | ASwaitAncSurn
  | ASwaitDescOcc
  | ASwaitDescSurn
  | ASwaitAncdescOcc
  | ASwaitAncdescSurn
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
   "-ad",
   Arg.String (fun s -> ancdesc_1st := s; arg_state := ASwaitAncdescOcc), "\
\"<1st_name>\" [num] \"<surname>\" : select ancestors of...
    and all their descendants (has no effect if -a and/or -d used,
    option -nsp is forced).";
   "-aws",
   Arg.String
     (fun s ->
        anc_1st := s; arg_state := ASwaitAncOcc; with_siblings := true; ()),
   "\"<1st_name>\" [num] \"<surname>\" : select ancestors with siblings";
   "-s", Arg.String (fun x -> surnames := x :: !surnames),
   "\"<surname>\" : select this surname (option usable several times)";
   "-maxlev", Arg.Int (fun i -> maxlev := i),
   "\"<level>\" : maximum level of generations of descendants";
   "-nsp", Arg.Set no_spouses_parents,
   ": no spouses' parents (for options -s and -d)";
   "-nn", Arg.Set no_notes, ": no (database) notes";
   "-nopicture", Arg.Set no_picture, ": Don't extract individual picture.";
   "-c", Arg.Int (fun i -> censor := i), "\
<num> :
     When a person is born less than <num> years ago, it is not exported unless
     it is Public. All the spouses and descendants are also censored.";
   "-raw", Arg.Set raw_output,
   "raw output (without possible utf-8 conversion)";
   "-v", Arg.Set Mutil.verbose, "verbose";
   "-sep", Arg.String (fun s -> separate_list := s :: !separate_list), "\
\"1st_name.num surname\" :
     To use together with the option \"-odir\": separate this person and
     all his ancestors and descendants sharing the same surname. All the
     concerned families are displayed on standard output instead of their
     associated files. This option can be used several times.";
   "-sep_only_file", Arg.String (fun s -> only_file := s), "\
<file> :
     With option \"-sep\", tells to separate only groups of that file.";
   "-sep_limit", Arg.Int (fun i -> sep_limit := i), "\
<num> :
     When using the option \"-sep\", groups of families can become isolated
     in the files. Gwu reconnects them to the separated families (i.e.
     displays them to standard output) if the size of these groups is less
     than " ^
   string_of_int !sep_limit ^ "\
. The present option changes this limit."]

let anonfun s =
  match !arg_state with
    ASnone ->
      if !in_file = "" then in_file := s
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
  | ASwaitAncdescOcc ->
      begin try
        ancdesc_occ := int_of_string s; arg_state := ASwaitAncdescSurn
      with Failure _ ->
        ancdesc_occ := 0; ancdesc_2nd := s; arg_state := ASnone
      end
  | ASwaitAncdescSurn -> ancdesc_2nd := s; arg_state := ASnone

let errmsg =
  "Usage: " ^ Sys.argv.(0) ^ " \
[options] <base_file>
If both options -a and -d are used, intersection is assumed.
If several options -s are used, union is assumed.
Options are:"

let main () =
  Mutil.verbose := false;
  Argl.parse speclist anonfun errmsg;
  if !in_file = "" then
    begin
      printf "Missing base\n";
      printf "Use option -help for usage\n";
      flush stdout;
      exit 2
    end;
  Secure.set_base_dir (Filename.dirname !in_file);
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
  let ancdesc =
    if !ancdesc_1st <> "" then
      if !anc_1st <> "" || !desc_1st <> "" then
        begin printf "Option -ad skipped since -a and/or -d used\n"; None end
      else if !ancdesc_2nd = "" then
        begin
          printf "Misused option -ad\n";
          printf "Use option -help for usage\n";
          flush stdout;
          exit 2
        end
      else
        begin
          no_spouses_parents := true;
          Some (!ancdesc_1st, !ancdesc_occ, !ancdesc_2nd)
        end
    else None
  in
  let base = Gwdb.open_base !in_file in
  let in_dir =
    if Filename.check_suffix !in_file ".gwb" then !in_file
    else !in_file ^ ".gwb"
  in
  let src_oc_ht = Hashtbl.create 1009 in
  let () = load_ascends_array base in
  let () = load_strings_array base in
  if not !mem then
    begin let () = load_couples_array base in
      let () = load_unions_array base in
      let () = load_descends_array base in ()
    end;
  let out_oc = if !out_file = "" then stdout else open_out !out_file in
  if !raw_output then () else fprintf out_oc "encoding: utf-8\n\n";
  gwu base in_dir !out_dir out_oc src_oc_ht anc desc ancdesc;
  Hashtbl.iter (fun _ (oc, _) -> flush oc; close_out oc) src_oc_ht;
  flush out_oc;
  if !out_file = "" then () else close_out out_oc

let _ = Printexc.catch main ()
