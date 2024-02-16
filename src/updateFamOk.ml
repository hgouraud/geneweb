(* camlp5r ./pa_html.cmo *)
(* $Id: updateFamOk.ml,v 5.53 2008-01-08 02:08:00 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Futil
open Gutil
open Gwdb
open Hutil
open Mutil
open Util

(* Liste des string dont on a supprimé un caractère.       *)
(* Utilisé pour le message d'erreur lors de la validation. *)
let removed_string = ref []

type create_info =
  Update.create_info =
    { ci_birth_date : date option;
      ci_birth_place : string;
      ci_death : death;
      ci_death_date : date option;
      ci_death_place : string;
      ci_occupation : string;
      ci_public : bool }

let raw_get conf key =
  match p_getenv conf.env key with
    Some v -> v
  | None -> failwith (key ^ " unbound")

let get conf key =
  match p_getenv conf.env key with
    Some v -> v
  | None -> failwith (key ^ " unbound")

let getn conf var key =
  match p_getenv conf.env (var ^ "_" ^ key) with
    Some v -> v
  | None -> failwith (var ^ "_" ^ key ^ " unbound")

let reconstitute_somebody conf var =
  let first_name = no_html_tags (only_printable (getn conf var "fn")) in
  let surname = no_html_tags (only_printable (getn conf var "sn")) in
  (* S'il y a des caractères interdits, on les supprime *)
  let (first_name, surname) =
    let contain_fn = String.contains first_name in
    let contain_sn = String.contains surname in
    if List.exists contain_fn Name.forbidden_char ||
       List.exists contain_sn Name.forbidden_char
    then
      begin
        removed_string :=
          (Name.purge first_name ^ " " ^ Name.purge surname) ::
          !removed_string;
        Name.purge first_name, Name.purge surname
      end
    else first_name, surname
  in
  let occ = try int_of_string (getn conf var "occ") with Failure _ -> 0 in
  let sex =
    match p_getenv conf.env (var ^ "_sex") with
      Some "M" -> Male
    | Some "F" -> Female
    | _ -> Neuter
  in
  let create =
    match getn conf var "p" with
      "create" -> Update.Create (sex, None)
    | _ -> Update.Link
  in
  first_name, surname, occ, create, var

let reconstitute_parent_or_child conf var default_surname =
  let first_name = no_html_tags (only_printable (getn conf var "fn")) in
  let surname =
    let surname = no_html_tags (only_printable (getn conf var "sn")) in
    if surname = "" then default_surname else surname
  in
  (* S'il y a des caractères interdits, on les supprime *)
  let (first_name, surname) =
    let contain_fn = String.contains first_name in
    let contain_sn = String.contains surname in
    if List.exists contain_fn Name.forbidden_char ||
       List.exists contain_sn Name.forbidden_char
    then
      begin
        removed_string :=
          (Name.purge first_name ^ " " ^ Name.purge surname) ::
          !removed_string;
        Name.purge first_name, Name.purge surname
      end
    else first_name, surname
  in
  let occ = try int_of_string (getn conf var "occ") with Failure _ -> 0 in
  let create_info =
    let b = Update.reconstitute_date conf (var ^ "b") in
    let bpl = getn conf (var ^ "b") "pl" in
    let death =
      match p_getenv conf.env (var ^ "d_yyyy") with
        Some "+" -> DeadDontKnowWhen
      | Some ("-" | "=") -> NotDead
      | _ -> DontKnowIfDead
    in
    let d = Update.reconstitute_date conf (var ^ "d") in
    let dpl = getn conf (var ^ "d") "pl" in
    let occupation = only_printable (getn conf var "occupation") in
    let public = getn conf (var ^ "b") "yyyy" = "p" in
    {ci_birth_date = b; ci_birth_place = bpl; ci_death = death;
     ci_death_date = d; ci_death_place = dpl; ci_occupation = occupation;
     ci_public = public}
  in
  let sex =
    match p_getenv conf.env (var ^ "_sex") with
      Some "M" -> Male
    | Some "F" -> Female
    | _ -> Neuter
  in
  let create =
    match getn conf var "p" with
      "create" -> Update.Create (sex, Some create_info)
    | _ -> Update.Link
  in
  first_name, surname, occ, create, var

let invert_children conf (c, children, ext) i =
  let var = "inv_ch" ^ string_of_int (i + 1) in
  match p_getenv conf.env var, children with
    Some "on", c1 :: children -> c1, c :: children, true
  | _ -> c, children, ext

let insert_child conf (children, ext) i =
  let var = "ins_ch" ^ string_of_int i in
  match p_getenv conf.env var, p_getint conf.env (var ^ "_n") with
    _, Some n when n > 1 ->
      let children =
        let rec loop children n =
          if n > 0 then
            let new_child = "", "", 0, Update.Create (Neuter, None), "" in
            loop (new_child :: children) (n - 1)
          else children
        in
        loop children n
      in
      children, true
  | Some "on", _ ->
      let new_child = "", "", 0, Update.Create (Neuter, None), "" in
      new_child :: children, true
  | _ -> children, ext

let insert_parent conf (parents, ext) i =
  let var = "ins_pa" ^ string_of_int i in
  match p_getenv conf.env var, p_getint conf.env (var ^ "_n") with
    _, Some n when n > 1 ->
      let parents =
        let rec loop parents n =
          if n > 0 then
            let new_parent = "", "", 0, Update.Create (Neuter, None), "" in
            loop (new_parent :: parents) (n - 1)
          else parents
        in
        loop parents n
      in
      parents, true
  | Some "on", _ ->
      let new_parent = "", "", 0, Update.Create (Neuter, None), "" in
      new_parent :: parents, true
  | _ -> parents, ext

let reconstitute_family conf =
  let ext = false in
  let relation =
    match p_getenv conf.env "mrel", p_getenv conf.env "nsck" with
      Some "marr", Some "on" -> NoSexesCheckMarried
    | Some "marr", (Some _ | None) -> Married
    | Some "not_marr", Some "on" -> NoSexesCheckNotMarried
    | Some "not_marr", (Some _ | None) -> NotMarried
    | Some "engaged", _ -> Engaged
    | Some "nsck", _ -> NoSexesCheckNotMarried
    | Some "nsckm", _ -> NoSexesCheckMarried
    | Some "no_ment", _ -> NoMention
    | _ -> Married
  in
  let marriage = Update.reconstitute_date conf "marr" in
  let marriage_place =
    no_html_tags (only_printable (get conf "marr_place"))
  in
  let (witnesses, ext) =
    let rec loop i ext =
      match
        try Some (reconstitute_somebody conf ("witn" ^ string_of_int i)) with
          Failure _ -> None
      with
        Some c ->
          let (witnesses, ext) = loop (i + 1) ext in
          begin match p_getenv conf.env ("ins_witn" ^ string_of_int i) with
            Some "on" ->
              let new_witn = "", "", 0, Update.Create (Neuter, None), "" in
              c :: new_witn :: witnesses, true
          | _ -> c :: witnesses, ext
          end
      | None -> [], ext
    in
    loop 1 ext
  in
  let (witnesses, ext) =
    match p_getenv conf.env "ins_witn0" with
      Some "on" ->
        let new_witn = "", "", 0, Update.Create (Neuter, None), "" in
        new_witn :: witnesses, true
    | _ -> witnesses, ext
  in
  let divorce =
    match p_getenv conf.env "div" with
      Some "not_divorced" -> NotDivorced
    | Some "separated" -> Separated
    | _ -> Divorced (Adef.codate_of_od (Update.reconstitute_date conf "div"))
  in
  let surname = getn conf "pa1" "sn" in
  let (children, ext) =
    let rec loop i ext =
      match
        try
          Some
            (reconstitute_parent_or_child conf ("ch" ^ string_of_int i)
               surname)
        with Failure _ -> None
      with
        Some c ->
          let (children, ext) = loop (i + 1) ext in
          let (c, children, ext) =
            invert_children conf (c, children, ext) i
          in
          let (children, ext) = insert_child conf (children, ext) i in
          c :: children, ext
      | None -> [], ext
    in
    loop 1 ext
  in
  let (children, ext) = insert_child conf (children, ext) 0 in
  let (parents, ext) =
    let rec loop i ext =
      match
        try
          Some (reconstitute_parent_or_child conf ("pa" ^ string_of_int i) "")
        with Failure _ -> None
      with
        Some c ->
          let (parents, ext) = loop (i + 1) ext in
          let (parents, ext) = insert_parent conf (parents, ext) i in
          c :: parents, ext
      | None -> [], ext
    in
    loop 1 ext
  in
  let comment =
    only_printable_or_nl (strip_all_trailing_spaces (get conf "comment"))
  in
  let fsources = only_printable (get conf "src") in
  let origin_file =
    match p_getenv conf.env "origin_file" with
      Some x -> x
    | None -> ""
  in
  let fam_index =
    match p_getint conf.env "i" with
      Some i -> i
    | None -> 0
  in
  let fam =
    {marriage = Adef.codate_of_od marriage; marriage_place = marriage_place;
     marriage_src = strip_spaces (get conf "marr_src");
     witnesses = Array.of_list witnesses; relation = relation;
     divorce = divorce; comment = comment; origin_file = origin_file;
     fsources = fsources; fam_index = Adef.ifam_of_int fam_index}
  and cpl = parent conf.multi_parents (Array.of_list parents)
  and des = {children = Array.of_list children} in
  fam, cpl, des, ext

let strip_array_persons pl =
  let pl =
    List.fold_right
      (fun (f, s, o, c, _ as p) pl -> if f = "" then pl else p :: pl)
      (Array.to_list pl) []
  in
  Array.of_list pl

let error_family conf base err =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  rheader conf title;
  Wserver.wprint "%s\n" (capitale err);
  Update.print_return conf;
  trailer conf;
  raise Update.ModErr

let check_witnesses conf base fam =
  let wl = Array.to_list fam.witnesses in
  let rec loop wl =
    match wl with
      [] -> None
    | (fn, sn, _, _, _) :: l ->
        if fn = "" && sn = "" then loop l
        else if fn = "" || fn = "?" then
          Some
            (transl_nth conf "witness/witnesses" 0 ^ " : " ^
             transl conf "first name missing")
        else if sn = "" || sn = "?" then
          Some
            (transl_nth conf "witness/witnesses" 0 ^ " : " ^
             transl conf "surname missing")
        else loop l
  in
  loop wl

let check_parents conf base cpl =
  let (fa_fn, fa_sn, _, _, _) = father cpl in
  let (mo_fn, mo_sn, _, _, _) = mother cpl in
  match (fa_fn = "", fa_sn = ""), (mo_fn = "", mo_sn = "") with
    (true, true), (true, true) | (true, true), (false, false) |
    (false, false), (true, true) | (false, false), (false, false) ->
      None
  | (false, true), _ ->
      Some
        (transl_nth conf "father/mother" 0 ^ " : " ^
         transl conf "surname missing")
  | (true, false), _ ->
      Some
        (transl_nth conf "father/mother" 0 ^ " : " ^
         transl conf "first name missing")
  | _, (false, true) ->
      Some
        (transl_nth conf "father/mother" 1 ^ " : " ^
         transl conf "surname missing")
  | _, (true, false) ->
      Some
        (transl_nth conf "father/mother" 1 ^ " : " ^
         transl conf "first name missing")

let check_family conf base fam cpl =
  let err_witness = check_witnesses conf base fam in
  let err_parents = check_parents conf base cpl in err_witness, err_parents

let strip_family fam des =
  let fam = {fam with witnesses = strip_array_persons fam.witnesses} in
  let des = {children = strip_array_persons des.children} in fam, des

let print_err_parents conf base p =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  rheader conf title;
  Wserver.wprint "\n";
  Wserver.wprint (fcapitale (ftransl conf "%t already has parents"))
    (fun _ -> Printf.sprintf "\n%s" (referenced_person_text conf base p));
  Wserver.wprint "\n";
  html_p conf;
  Wserver.wprint "<ul>\n";
  html_li conf;
  Wserver.wprint "%s: %d" (capitale (transl conf "first free number"))
    (Gutil.find_free_occ base (p_first_name base p) (p_surname base p) 0);
  Wserver.wprint "</ul>\n";
  Update.print_return conf;
  trailer conf;
  raise Update.ModErr

let print_err_father_sex conf base p =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  rheader conf title;
  Wserver.wprint "\n%s" (referenced_person_text conf base p);
  Wserver.wprint "\n%s\n" (transl conf "should be male");
  Update.print_return conf;
  trailer conf;
  raise Update.ModErr

let print_err_mother_sex conf base p =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  rheader conf title;
  Wserver.wprint "\n%s" (referenced_person_text conf base p);
  Wserver.wprint "\n%s\n" (transl conf "should be female");
  Update.print_return conf;
  trailer conf;
  raise Update.ModErr

let print_err conf base =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  rheader conf title;
  Update.print_return conf;
  trailer conf;
  raise Update.ModErr

let print_error_disconnected conf =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  rheader conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.wprint "%s" (capitale (transl conf "msg error disconnected"));
  trailer conf;
  raise Update.ModErr

let family_exclude pfams efam =
  let pfaml =
    List.fold_right (fun fam faml -> if fam = efam then faml else fam :: faml)
      (Array.to_list pfams) []
  in
  Array.of_list pfaml

let infer_origin_file_from_other_marriages conf base ifam ip =
  let u = poi base ip in
  let ufams = get_family u in
  let rec loop i =
    if i = Array.length ufams then None
    else if ufams.(i) = ifam then loop (i + 1)
    else
      let r = get_origin_file (foi base ufams.(i)) in
      if sou base r <> "" then Some r else loop (i + 1)
  in
  loop 0

let infer_origin_file conf base ifam ncpl ndes =
  let r =
    infer_origin_file_from_other_marriages conf base ifam (Adef.father ncpl)
  in
  let r =
    if r = None then
      infer_origin_file_from_other_marriages conf base ifam (Adef.mother ncpl)
    else r
  in
  let r =
    match r with
      Some r -> r
    | None ->
        let afath = poi base (Adef.father ncpl) in
        let amoth = poi base (Adef.mother ncpl) in
        match get_parents afath, get_parents amoth with
          Some if1, _ when sou base (get_origin_file (foi base if1)) <> "" ->
            get_origin_file (foi base if1)
        | _, Some if2 when sou base (get_origin_file (foi base if2)) <> "" ->
            get_origin_file (foi base if2)
        | _ ->
            let rec loop i =
              if i = Array.length ndes.children then
                Gwdb.insert_string base ""
              else
                let cifams = get_family (poi base ndes.children.(i)) in
                if Array.length cifams = 0 then loop (i + 1)
                else if
                  sou base (get_origin_file (foi base cifams.(0))) <> ""
                then
                  get_origin_file (foi base cifams.(0))
                else loop (i + 1)
            in
            loop 0
  in
  let no_dec =
    try List.assoc "propose_add_family" conf.base_env = "no" with
      Not_found -> false
  in
  if no_dec && sou base r = "" then print_error_disconnected conf else r

let effective_mod conf base sfam scpl sdes =
  let fi = sfam.fam_index in
  let (oorigin, owitnesses) =
    let ofam = foi base fi in get_origin_file ofam, get_witnesses ofam
  in
  let (oarr, ofather, omother) =
    let ocpl = foi base fi in
    get_parent_array ocpl, get_father ocpl, get_mother ocpl
  in
  let ochildren = get_children (foi base fi) in
  let created_p = ref [] in
  let psrc =
    match p_getenv conf.env "psrc" with
      Some s -> strip_spaces s
    | None -> ""
  in
  let ncpl =
    map_couple_p conf.multi_parents
      (Update.insert_person conf base psrc created_p) scpl
  in
  let nfam =
    map_family_ps (Update.insert_person conf base psrc created_p)
      (Gwdb.insert_string base) sfam
  in
  let ndes =
    map_descend_p (Update.insert_person conf base psrc created_p) sdes
  in
  let nfath = poi base (Adef.father ncpl) in
  let nmoth = poi base (Adef.mother ncpl) in
  if sfam.relation <> NoSexesCheckNotMarried &&
     sfam.relation <> NoSexesCheckMarried
  then
    begin
      begin match get_sex nfath with
        Female -> print_err_father_sex conf base nfath
      | Male -> ()
      | Neuter ->
          let nfath = {(gen_person_of_person nfath) with sex = Male} in
          patch_person base nfath.key_index nfath
      end;
      match get_sex nmoth with
        Male -> print_err_mother_sex conf base nmoth
      | Female -> ()
      | Neuter ->
          let nmoth = {(gen_person_of_person nmoth) with sex = Female} in
          patch_person base nmoth.key_index nmoth
    end;
  if Adef.father ncpl = Adef.mother ncpl then print_err conf base;
  let nfam =
    let origin_file =
      if sfam.origin_file = "" then
        if sou base oorigin <> "" then oorigin
        else infer_origin_file conf base fi ncpl ndes
      else nfam.origin_file
    in
    {nfam with origin_file = origin_file; fam_index = fi}
  in
  patch_family base fi nfam;
  patch_couple base fi ncpl;
  patch_descend base fi ndes;
  let narr = Adef.parent_array ncpl in
  for i = 0 to Array.length oarr - 1 do
    if not (array_mem oarr.(i) narr) then
      let ou = poi base oarr.(i) in
      let ou = {family = family_exclude (get_family ou) fi} in
      patch_union base oarr.(i) ou
  done;
  for i = 0 to Array.length narr - 1 do
    if not (array_mem narr.(i) oarr) then
      let nu = poi base narr.(i) in
      let nu = {family = Array.append (get_family nu) [| fi |]} in
      patch_union base narr.(i) nu
  done;
  let cache = Hashtbl.create 101 in
  let find_asc ip =
    try Hashtbl.find cache ip with
      Not_found ->
        let a = poi base ip in
        let a = {parents = get_parents a; consang = get_consang a} in
        Hashtbl.add cache ip a; a
  in
  let same_parents =
    Adef.father ncpl = ofather && Adef.mother ncpl = omother
  in
  Array.iter
    (fun ip ->
       let a = find_asc ip in
       let a =
         {parents = None;
          consang =
            if not (array_mem ip ndes.children) then Adef.fix (-1)
            else a.consang}
       in
       Hashtbl.replace cache ip a)
    ochildren;
  Array.iter
    (fun ip ->
       let a = find_asc ip in
       match a.parents with
         Some _ -> print_err_parents conf base (poi base ip)
       | None ->
           let a =
             {parents = Some fi;
              consang =
                if not (array_mem ip ochildren) || not same_parents then
                  Adef.fix (-1)
                else a.consang}
           in
           Hashtbl.replace cache ip a)
    ndes.children;
  Array.iter
    (fun ip ->
       if not (array_mem ip ndes.children) then
         patch_ascend base ip (find_asc ip))
    ochildren;
  Array.iter
    (fun ip ->
       if not (array_mem ip ochildren) || not same_parents then
         patch_ascend base ip (find_asc ip))
    ndes.children;
  Update.add_misc_names_for_new_persons base !created_p;
  Update.update_misc_names_of_family base Male {family = get_family nfath};
  Update.update_related_pointers base (Adef.father ncpl)
    (Array.to_list owitnesses) (Array.to_list nfam.witnesses);
  fi, nfam, ncpl, ndes

let effective_add conf base sfam scpl sdes =
  let fi = Adef.ifam_of_int (nb_of_families base) in
  let created_p = ref [] in
  let psrc =
    match p_getenv conf.env "psrc" with
      Some s -> strip_spaces s
    | None -> ""
  in
  let ncpl =
    map_couple_p conf.multi_parents
      (Update.insert_person conf base psrc created_p) scpl
  in
  let nfam =
    map_family_ps (Update.insert_person conf base psrc created_p)
      (Gwdb.insert_string base) sfam
  in
  let ndes =
    map_descend_p (Update.insert_person conf base psrc created_p) sdes
  in
  let origin_file = infer_origin_file conf base fi ncpl ndes in
  let nfath_p = poi base (Adef.father ncpl) in
  let nmoth_p = poi base (Adef.mother ncpl) in
  if sfam.relation <> NoSexesCheckNotMarried &&
     sfam.relation <> NoSexesCheckMarried
  then
    begin
      begin match get_sex nfath_p with
        Female -> print_err_father_sex conf base nfath_p
      | Male -> ()
      | _ ->
          let nfath_p = {(gen_person_of_person nfath_p) with sex = Male} in
          patch_person base nfath_p.key_index nfath_p
      end;
      match get_sex nmoth_p with
        Male -> print_err_mother_sex conf base nmoth_p
      | Female -> ()
      | _ ->
          let nmoth_p = {(gen_person_of_person nmoth_p) with sex = Female} in
          patch_person base nmoth_p.key_index nmoth_p
    end
  else if Adef.father ncpl = Adef.mother ncpl then print_err conf base;
  let nfam = {nfam with origin_file = origin_file; fam_index = fi} in
  patch_family base fi nfam;
  patch_couple base fi ncpl;
  patch_descend base fi ndes;
  let nfath_u = {family = Array.append (get_family nfath_p) [| fi |]} in
  let nmoth_u = {family = Array.append (get_family nmoth_p) [| fi |]} in
  patch_union base (Adef.father ncpl) nfath_u;
  patch_union base (Adef.mother ncpl) nmoth_u;
  Array.iter
    (fun ip ->
       let p = poi base ip in
       match get_parents p with
         Some _ -> print_err_parents conf base p
       | None ->
           let a = {parents = Some fi; consang = Adef.fix (-1)} in
           patch_ascend base (get_key_index p) a)
    ndes.children;
  Update.add_misc_names_for_new_persons base !created_p;
  Update.update_misc_names_of_family base Male nfath_u;
  Update.update_related_pointers base (Adef.father ncpl) []
    (Array.to_list nfam.witnesses);
  fi, nfam, ncpl, ndes

let effective_inv conf base ip u ifam =
  let rec loop =
    function
      ifam1 :: ifam2 :: ifaml ->
        if ifam2 = ifam then ifam2 :: ifam1 :: ifaml
        else ifam1 :: loop (ifam2 :: ifaml)
    | _ -> incorrect_request conf; raise Update.ModErr
  in
  let u = {family = Array.of_list (loop (Array.to_list (get_family u)))} in
  patch_union base ip u


(* ************************************************************************ *)
(*  [Fonc] effective_chg_order :
             config -> base -> iper -> person -> ifam -> int -> unit        *)
(** [Description] : Modifie l'ordre de la famille en positionnant la famille
      ifam à la position n. Exemple : [f1 f2 f3 f4] f1 3 => [f2 f3 f1 f4].
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - ip   : iper
      - u    : person
      - ifam : famille à changer de place
      - n    : nouvelle position de la famille
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let effective_chg_order conf base ip u ifam n =
  let fam = UpdateFam.change_order conf base ip u ifam n in
  let u = {family = Array.of_list fam} in patch_union base ip u

let kill_family base ifam1 ip =
  let u = poi base ip in
  let l =
    List.fold_right
      (fun ifam ifaml -> if ifam = ifam1 then ifaml else ifam :: ifaml)
      (Array.to_list (get_family u)) []
  in
  let u = {family = Array.of_list l} in patch_union base ip u

let kill_parents base ip =
  let a = {parents = None; consang = Adef.fix (-1)} in patch_ascend base ip a

let effective_del conf base (ifam, fam) =
  kill_family base ifam (get_father fam);
  kill_family base ifam (get_mother fam);
  Array.iter (kill_parents base) (get_children fam);
  delete_family base ifam

let array_forall2 f a1 a2 =
  if Array.length a1 <> Array.length a2 then invalid_arg "array_forall2"
  else
    let rec loop i =
      if i = Array.length a1 then true
      else if f a1.(i) a2.(i) then loop (i + 1)
      else false
    in
    loop 0

let array_exists f a =
  let rec loop i =
    if i = Array.length a then false
    else if f a.(i) then true
    else loop (i + 1)
  in
  loop 0

let is_a_link =
  function
    _, _, _, Update.Link, _ -> true
  | _ -> false

let is_created_or_already_there ochil_arr nchil schil =
  not (is_a_link schil) || array_mem nchil ochil_arr

(* need_check_noloop: optimization
     The no-loop check being a big work on large databases, this
   optimization tests if this is really necessary or not. It is not
   necessary if:
   1/ either all parents are created,
   2/ or all children are created,
   3/ or the new family have the same parents than the old one *and*
      all linked (not created) new children were already children.
*)
(* Replaced && by || to do more checks. *)
(* Improvement : check the name on the parents/children if they linked *)

let need_check_noloop (scpl, sdes, onfs) =
  if array_exists is_a_link (parent_array scpl) ||
     array_exists is_a_link sdes.children
  then
    match onfs with
      Some ((opar, ochil), (npar, nchil)) ->
        not
          (array_forall2 (is_created_or_already_there opar) npar
             (parent_array scpl)) ||
        not
          (array_forall2 (is_created_or_already_there ochil) nchil
             sdes.children)
    | None -> true
  else false

let all_checks_family conf base ifam fam cpl des scdo =
  let wl = ref [] in
  let ml = ref [] in
  let error = Update.error conf base in
  let warning w = wl := w :: !wl in
  let misc m = ml := m :: !ml in
  if need_check_noloop scdo then
    Consang.check_noloop_for_person_list base error
      (Array.to_list (Adef.parent_array cpl));
  let fam = family_of_gen_family base (fam, cpl, des) in
  CheckItem.family base error warning ifam fam;
  CheckItem.check_other_fields base misc ifam fam;
  List.iter
    (function
       ChangedOrderOfMarriages (p, _, after) ->
         patch_union base (get_key_index p) {family = after}
     | _ -> ())
    !wl;
  List.rev !wl, List.rev !ml

let print_family conf base (wl, ml) cpl des =
  let rdsrc =
    match p_getenv conf.env "rdsrc" with
      Some "on" -> p_getenv conf.env "src"
    | _ -> p_getenv conf.env "dsrc"
  in
  begin match rdsrc with
    Some x ->
      conf.henv <- List.remove_assoc "dsrc" conf.henv;
      if x <> "" then conf.henv <- ("dsrc", code_varenv x) :: conf.henv
  | None -> ()
  end;
  Wserver.wprint "<ul>\n";
  Wserver.wprint "<li>";
  Wserver.wprint "%s"
    (referenced_person_text conf base (poi base (Adef.father cpl)));
  Wserver.wprint "</li>";
  Wserver.wprint "\n";
  Wserver.wprint "<li>";
  Wserver.wprint "%s"
    (referenced_person_text conf base (poi base (Adef.mother cpl)));
  Wserver.wprint "</li>";
  Wserver.wprint "</ul>\n";
  if des.children <> [| |] then
    begin
      Wserver.wprint "<ul>\n";
      Array.iter
        (fun ip ->
           Wserver.wprint "<li>";
           Wserver.wprint "%s"
             (referenced_person_text conf base (poi base ip));
           Wserver.wprint "</li>")
        des.children;
      Wserver.wprint "</ul>\n"
    end;
  Update.print_warnings_and_miscs conf base (wl, ml)

let print_mod_ok conf base (wl, ml) cpl des =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "family modified"))
  in
  header conf title;
  print_link_to_welcome conf true;
  (* Si on a supprimé des caractères interdits *)
  if List.length !removed_string > 0 then
    begin
      Wserver.wprint "<h3 class=\"error\">";
      Wserver.wprint (fcapitale (ftransl conf "%s forbidden char"))
        (List.fold_left (fun acc c -> acc ^ "'" ^ Char.escaped c ^ "' ") " "
           Name.forbidden_char);
      Wserver.wprint "</h3>\n";
      List.iter (Wserver.wprint "<p>%s</p>") !removed_string
    end;
  print_family conf base (wl, ml) cpl des;
  trailer conf

let print_add_ok conf base (wl, ml) cpl des =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "family added")) in
  header conf title;
  print_link_to_welcome conf true;
  (* Si on a supprimé des caractères interdits *)
  if List.length !removed_string > 0 then
    begin
      Wserver.wprint "<h2 class=\"error\">%s</h2>\n"
        (capitale (transl conf "forbidden char"));
      List.iter (Wserver.wprint "<p>%s</p>") !removed_string
    end;
  print_family conf base (wl, ml) cpl des;
  trailer conf

let print_del_ok conf base wl =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "family deleted"))
  in
  header conf title;
  print_link_to_welcome conf true;
  begin match p_getint conf.env "ip" with
    Some i ->
      let p = poi base (Adef.iper_of_int i) in
      Wserver.wprint "<ul>\n";
      Wserver.wprint "<li>\n";
      Wserver.wprint "%s\n" (reference conf base p (person_text conf base p));
      Wserver.wprint "</ul>\n"
  | _ -> ()
  end;
  Update.print_warnings conf base wl;
  trailer conf

let print_inv_ok conf base p =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "inversion done"))
  in
  header conf title;
  print_link_to_welcome conf true;
  Wserver.wprint "\n%s" (referenced_person_text conf base p);
  Wserver.wprint "\n";
  trailer conf

let get_create (_, _, _, create, _) = create

let forbidden_disconnected conf sfam scpl sdes =
  let no_dec =
    try List.assoc "propose_add_family" conf.base_env = "no" with
      Not_found -> false
  in
  if no_dec then
    if get_create (father scpl) = Update.Link ||
       get_create (mother scpl) = Update.Link
    then
      false
    else
      List.for_all (fun p -> get_create p <> Update.Link)
        (Array.to_list sdes.children)
  else false

let print_add o_conf base =
  (* Attention ! On pense à remettre les compteurs à *)
  (* zéro pour la détection des caractères interdits *)
  let () = removed_string := [] in
  let conf = Update.update_conf o_conf in
  try
    let (sfam, scpl, sdes, ext) = reconstitute_family conf in
    let redisp =
      match p_getenv conf.env "return" with
        Some _ -> true
      | _ -> false
    in
    let digest =
      match p_getint conf.env "ip" with
        Some ip ->
          string_of_int
            (Array.length (get_family (poi base (Adef.iper_of_int ip))))
      | None -> ""
    in
    let sdigest = raw_get conf "digest" in
    if digest <> "" && sdigest <> "" && digest <> sdigest then
      Update.error_digest conf
    else if ext || redisp then
      UpdateFam.print_update_fam conf base (sfam, scpl, sdes) ""
    else if forbidden_disconnected conf sfam scpl sdes then
      print_error_disconnected conf
    else
      match check_family conf base sfam scpl with
        Some err, _ | _, Some err -> error_family conf base err
      | None, None ->
          let (sfam, sdes) = strip_family sfam sdes in
          let (ifam, fam, cpl, des) =
            effective_add conf base sfam scpl sdes
          in
          let (wl, ml) =
            all_checks_family conf base ifam fam cpl des (scpl, sdes, None)
          in
          let (changed, act) =
            let fam = Util.string_gen_family base fam in
            let (ip, act) =
              match p_getint conf.env "ip" with
                Some i ->
                  if Adef.int_of_iper (Adef.mother cpl) = i then
                    Adef.mother cpl, "af"
                  else
                    let a = poi base (Adef.iper_of_int i) in
                    begin match get_parents a with
                      Some x when x = ifam -> Adef.iper_of_int i, "aa"
                    | _ -> Adef.father cpl, "af"
                    end
              | None -> Adef.father cpl, "af"
            in
            match act with
              "af" ->
                let gen_p =
                  Util.string_gen_person base
                    (gen_person_of_person (poi base ip))
                in
                U_Add_family (gen_p, fam), "af"
            | _ ->
                let gen_p =
                  Util.string_gen_person base
                    (gen_person_of_person (poi base ip))
                in
                U_Add_parent (gen_p, fam), "aa"
          in
          Util.commit_patches conf base;
          History.record conf base changed act;
          Update.delete_topological_sort conf base;
          print_add_ok conf base (wl, ml) cpl des
  with Update.ModErr -> ()

let print_del conf base =
  match p_getint conf.env "i" with
    Some i ->
      let ifam = Adef.ifam_of_int i in
      let fam = foi base ifam in
      if not (is_deleted_family fam) then
        begin
          effective_del conf base (ifam, fam);
          Util.commit_patches conf base;
          let changed =
            let gen_p =
              let p =
                match p_getint conf.env "ip" with
                  Some i when Adef.int_of_iper (get_mother fam) = i ->
                    poi base (get_mother fam)
                | _ -> poi base (get_father fam)
              in
              Util.string_gen_person base (gen_person_of_person p)
            in
            let gen_fam =
              Util.string_gen_family base (gen_family_of_family fam)
            in
            U_Delete_family (gen_p, gen_fam)
          in
          History.record conf base changed "df";
          Update.delete_topological_sort conf base
        end;
      print_del_ok conf base []
  | _ -> incorrect_request conf

let print_mod_aux conf base callback =
  try
    let (sfam, scpl, sdes, ext) = reconstitute_family conf in
    let redisp =
      match p_getenv conf.env "return" with
        Some _ -> true
      | _ -> false
    in
    let digest =
      let ini_sfam = UpdateFam.string_family_of conf base sfam.fam_index in
      Update.digest_family ini_sfam
    in
    if digest = raw_get conf "digest" then
      if ext || redisp then
        UpdateFam.print_update_fam conf base (sfam, scpl, sdes) digest
      else
        match check_family conf base sfam scpl with
          Some err, _ | _, Some err -> error_family conf base err
        | None, None ->
            let (sfam, sdes) = strip_family sfam sdes in
            callback sfam scpl sdes
    else Update.error_digest conf
  with Update.ModErr -> ()

let family_structure conf base ifam =
  let fam = foi base ifam in get_parent_array fam, get_children fam

let print_mod o_conf base =
  (* Attention ! On pense à remettre les compteurs à *)
  (* zéro pour la détection des caractères interdits *)
  let () = removed_string := [] in
  let o_f =
    let ifam =
      match p_getint o_conf.env "i" with
        Some i -> Adef.ifam_of_int i
      | None -> Adef.ifam_of_int (-1)
    in
    Util.string_gen_family base (gen_family_of_family (foi base ifam))
  in
  let conf = Update.update_conf o_conf in
  let callback sfam scpl sdes =
    let ofs = family_structure conf base sfam.fam_index in
    let (ifam, fam, cpl, des) = effective_mod conf base sfam scpl sdes in
    let s =
      let sl = [fam.comment; fam.fsources] in
      String.concat " " (List.map (sou base) sl)
    in
    Notes.update_notes_links_db conf (NotesLinks.PgFam ifam) s;
    let nfs = Adef.parent_array cpl, des.children in
    let onfs = Some (ofs, nfs) in
    let (wl, ml) =
      all_checks_family conf base ifam fam cpl des (scpl, sdes, onfs)
    in
    Util.commit_patches conf base;
    let changed =
      let ip =
        match p_getint o_conf.env "ip" with
          Some i -> Adef.iper_of_int i
        | None -> Adef.iper_of_int (-1)
      in
      let p =
        Util.string_gen_person base (gen_person_of_person (poi base ip))
      in
      let n_f = Util.string_gen_family base fam in
      U_Modify_family (p, o_f, n_f)
    in
    History.record conf base changed "mf";
    Update.delete_topological_sort conf base;
    print_mod_ok conf base (wl, ml) cpl des
  in
  print_mod_aux conf base callback

let print_inv conf base =
  match p_getint conf.env "i", p_getint conf.env "f" with
    Some ip, Some ifam ->
      let p = poi base (Adef.iper_of_int ip) in
      begin try
        effective_inv conf base (get_key_index p) p (Adef.ifam_of_int ifam);
        Util.commit_patches conf base;
        let changed =
          let gen_p = Util.string_gen_person base (gen_person_of_person p) in
          U_Invert_family (gen_p, Adef.ifam_of_int ifam)
        in
        History.record conf base changed "if"; print_inv_ok conf base p
      with Update.ModErr -> ()
      end
  | _ -> incorrect_request conf

let print_change_order_ok conf base =
  match
    p_getint conf.env "i", p_getint conf.env "f", p_getint conf.env "n"
  with
    Some ip, Some ifam, Some n ->
      let p = poi base (Adef.iper_of_int ip) in
      begin try
        effective_chg_order conf base (get_key_index p) p
          (Adef.ifam_of_int ifam) n;
        Util.commit_patches conf base;
        let changed =
          let gen_p = Util.string_gen_person base (gen_person_of_person p) in
          U_Invert_family (gen_p, Adef.ifam_of_int ifam)
        in
        History.record conf base changed "if"; print_inv_ok conf base p
      with Update.ModErr -> ()
      end
  | _ -> incorrect_request conf
