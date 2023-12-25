(* camlp5r ./def_syn.cmo ./pa_lock.cmo ./pa_html.cmo *)
(* $Id: request.ml,v 5.61 2008-11-03 15:40:10 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gutil
open Gwdb
open Hutil
open Util

let person_is_std_key conf base p k =
  let k = Name.strip_lower k in
  if k = Name.strip_lower (p_first_name base p ^ " " ^ p_surname base p) then
    true
  else if
    List.exists (fun n -> Name.strip n = k)
      (person_misc_names base p (nobtit conf base))
  then
    true
  else false

let select_std_eq conf base pl k =
  List.fold_right
    (fun p pl -> if person_is_std_key conf base p k then p :: pl else pl) pl
    []

let very_unknown conf =
  match p_getenv conf.env "n", p_getenv conf.env "p" with
    Some nom, Some prenom ->
      let title _ =
        Wserver.wprint "%s: \"%s %s\"" (capitale (transl conf "not found"))
          prenom nom
      in
      rheader conf title; print_link_to_welcome conf false; trailer conf
  | _ -> incorrect_request conf

let unknown conf n =
  let title _ =
    Wserver.wprint "%s: \"%s\"" (capitale (transl conf "not found")) n
  in
  rheader conf title; print_link_to_welcome conf false; trailer conf

let relation_print conf base p =
  let p1 =
    match p_getint conf.senv "ei" with
      Some i ->
        conf.senv <- [];
        if i >= 0 && i < nb_of_persons base then
          Some (pget conf base (Adef.iper_of_int i))
        else None
    | None ->
        match find_person_in_env conf base "1" with
          Some p1 -> conf.senv <- []; Some p1
        | None -> None
  in
  Relation.print conf base p p1

let person_selected conf base p =
  match p_getenv conf.senv "em" with
    Some "R" -> relation_print conf base p
  | Some mode -> incorrect_request conf
  | None -> Perso.print conf base p

let compact_list conf base xl =
  let pl = sort_person_list base xl in
  let pl =
    List.fold_right
      (fun p pl ->
         match pl with
           p1 :: _ when get_key_index p = get_key_index p1 -> pl
         | _ -> p :: pl)
      pl []
  in
  pl

let cut_words str =
  let rec loop beg i =
    if i < String.length str then
      match str.[i] with
        ' ' ->
          if beg = i then loop (succ beg) (succ i)
          else String.sub str beg (i - beg) :: loop (succ i) (succ i)
      | _ -> loop beg (succ i)
    else if beg = i then []
    else [String.sub str beg (i - beg)]
  in
  loop 0 0

let try_find_with_one_first_name conf base n =
  let n1 = Name.abbrev (Name.lower n) in
  match Mutil.lindex n1 ' ' with
    Some i ->
      let fn = String.sub n1 0 i in
      let sn = String.sub n1 (i + 1) (String.length n1 - i - 1) in
      let (list, _) =
        Some.persons_of_fsname conf base base_strings_of_surname
          (spi_find (persons_of_surname base)) get_surname sn
      in
      let pl =
        List.fold_left
          (fun pl (_, _, ipl) ->
             List.fold_left
               (fun pl ip ->
                  let p = pget conf base ip in
                  let fn1 =
                    Name.abbrev (Name.lower (sou base (get_first_name p)))
                  in
                  if List.mem fn (cut_words fn1) then p :: pl else pl)
               pl ipl)
          [] list
      in
      pl
  | None -> []

let name_with_roman_number str =
  let rec loop found len i =
    if i = String.length str then if found then Some (Buff.get len) else None
    else
      match str.[i] with
        '0'..'9' as c ->
          let (n, i) =
            let rec loop n i =
              if i = String.length str then n, i
              else
                match str.[i] with
                  '0'..'9' as c ->
                    loop (10 * n + Char.code c - Char.code '0') (i + 1)
                | _ -> n, i
            in
            loop (Char.code c - Char.code '0') (i + 1)
          in
          loop true (Buff.mstore len (Mutil.roman_of_arabian n)) i
      | c -> loop found (Buff.store len c) (i + 1)
  in
  loop false 0 0

let find_all conf base an =
  let sosa_ref = Util.find_sosa_ref conf base in
  let sosa_nb = try Some (Num.of_string an) with Failure _ -> None in
  match sosa_ref, sosa_nb with
    Some p, Some n ->
      if n <> Num.zero then
        match Util.branch_of_sosa conf base (get_key_index p) n with
          Some ((ip, _) :: _) -> [pget conf base ip], true
        | _ -> [], false
      else [], false
  | _ ->
      match person_of_string_key base an with
        Some ip ->
          let pl =
            let p = pget conf base ip in if is_hidden p then [] else [p]
          in
          let pl =
            if not conf.wizard && not conf.friend then
              List.fold_right
                (fun p pl ->
                   if not (is_hide_names conf p) || Util.fast_auth_age conf p
                   then
                     p :: pl
                   else pl)
                pl []
            else pl
          in
          pl, false
      | None ->
          let ipl = person_not_a_key_find_all base an in
          let (an, ipl) =
            if ipl = [] then
              match name_with_roman_number an with
                Some an1 ->
                  let ipl = person_ht_find_all base an1 in
                  if ipl = [] then an, [] else an1, ipl
              | None -> an, ipl
            else an, ipl
          in
          let pl =
            List.fold_left
              (fun l ip ->
                 let p = pget conf base ip in
                 if is_hidden p then l else p :: l)
              [] ipl
          in
          let spl = select_std_eq conf base pl an in
          let pl =
            if spl = [] then
              if pl = [] then try_find_with_one_first_name conf base an
              else pl
            else spl
          in
          let pl =
            if not conf.wizard && not conf.friend then
              List.fold_right
                (fun p pl ->
                   if not (is_hide_names conf p) || Util.fast_auth_age conf p
                   then
                     p :: pl
                   else pl)
                pl []
            else pl
          in
          compact_list conf base pl, false

let specify conf base n pl =
  let title _ = Wserver.wprint "%s : %s" n (transl conf "specify") in
  let n = Name.crush_lower n in
  let ptll =
    List.map
      (fun p ->
         let tl = ref [] in
         let add_tl t =
           tl :=
             let rec add_rec =
               function
                 t1 :: tl1 ->
                   if eq_istr t1.t_ident t.t_ident &&
                      eq_istr t1.t_place t.t_place
                   then
                     t1 :: tl1
                   else t1 :: add_rec tl1
               | [] -> [t]
             in
             add_rec !tl
         in
         let compare_and_add t pn =
           let pn = sou base pn in
           if Name.crush_lower pn = n then add_tl t
           else
             match get_qualifiers p with
               nn :: _ ->
                 let nn = sou base nn in
                 if Name.crush_lower (pn ^ " " ^ nn) = n then add_tl t
             | _ -> ()
         in
         List.iter
           (fun t ->
              match t.t_name, get_public_name p with
                Tname s, _ -> compare_and_add t s
              | _, pn when sou base pn <> "" -> compare_and_add t pn
              | _ -> ())
           (nobtit conf base p);
         p, !tl)
      pl
  in
  header conf title;
  conf.cancel_links <- false;
  print_link_to_welcome conf true;
  (* Si on est dans un calcul de parenté, on affiche *)
  (* l'aide sur la sélection d'un individu.          *)
  Util.print_tips_relationship conf;
  Wserver.wprint "<ul>\n";
  (* Construction de la table des sosa de la base *)
  let () = Perso.build_sosa_ht conf base in
  List.iter
    (fun (p, tl) ->
       Wserver.wprint "<li>\n";
       Perso.print_sosa conf base p true;
       begin match tl with
         [] ->
           Wserver.wprint "\n%s"
             (Util.referenced_person_title_text conf base p)
       | t :: _ ->
           Wserver.wprint "<a href=\"%s%s\">\n" (commd conf)
             (acces conf base p);
           Wserver.wprint "%s" (titled_person_text conf base p t);
           Wserver.wprint "</a>\n";
           List.iter
             (fun t -> Wserver.wprint "%s" (one_title_text conf base p t)) tl
       end;
       Wserver.wprint " [%d]" (get_occ p);
       Wserver.wprint "%s" (Date.short_dates_text conf base p);
       Util.specify_homonymous conf base p true;
       if authorized_age conf base p then
         begin match get_first_names_aliases p with
           [] -> ()
         | fnal ->
             Wserver.wprint "\n<em>(";
             Mutil.list_iter_first
               (fun first fna ->
                  if not first then Wserver.wprint ", ";
                  Wserver.wprint "%s" (sou base fna))
               fnal;
             Wserver.wprint ")</em>"
         end;
       Wserver.wprint "</li>\n")
    ptll;
  Wserver.wprint "</ul>\n";
  trailer conf

(* Make the "special" environement; "em=mode;ei=n" *)

let set_senv conf vm vi =
  conf.senv <- ["em", vm; "ei", vi];
  begin match p_getenv conf.env "image" with
    Some "on" -> conf.senv <- conf.senv @ ["image", "on"]
  | _ -> ()
  end;
  begin match p_getenv conf.env "long" with
    Some "on" -> conf.senv <- conf.senv @ ["long", "on"]
  | _ -> ()
  end;
  begin match p_getenv conf.env "spouse" with
    Some "on" -> conf.senv <- conf.senv @ ["spouse", "on"]
  | _ -> ()
  end;
  begin match p_getenv conf.env "et" with
    Some x -> conf.senv <- conf.senv @ ["et", x]
  | _ -> ()
  end;
  begin match p_getenv conf.env "cgl" with
    Some "on" -> conf.senv <- conf.senv @ ["cgl", "on"]
  | _ -> ()
  end;
  begin match p_getenv conf.env "bd" with
    None | Some ("0" | "") -> ()
  | Some x -> conf.senv <- conf.senv @ ["bd", x]
  end;
  match p_getenv conf.env "color" with
    Some x -> conf.senv <- conf.senv @ ["color", code_varenv x]
  | _ -> ()

let make_senv conf base =
  let get x = Util.p_getenv conf.env x in
  match get "em", get "ei", get "ep", get "en", get "eoc" with
    Some vm, Some vi, _, _, _ -> set_senv conf vm vi
  | Some vm, None, Some vp, Some vn, voco ->
      let voc =
        match voco with
          Some voc -> (try int_of_string voc with Failure _ -> 0)
        | None -> 0
      in
      let ip =
        match person_of_key base vp vn voc with
          Some ip -> ip
        | None -> incorrect_request conf; raise Exit
      in
      let vi = string_of_int (Adef.int_of_iper ip) in set_senv conf vm vi
  | _ -> ()

let updmenu_print = Perso.interp_templ "updmenu"

let family_m conf base =
  match p_getenv conf.env "m" with
    Some "A" ->
      begin match find_person_in_env conf base "" with
        Some p -> Perso.print_ascend conf base p
      | _ -> very_unknown conf
      end
  | Some "ADD_FAM" when conf.wizard && not conf.not_wizard ->
      UpdateFam.print_add conf base
  | Some "ADD_FAM_OK" when conf.wizard && not conf.not_wizard ->
      UpdateFamOk.print_add conf base
  | Some "ADD_IND" when conf.wizard && not conf.not_wizard ->
      UpdateInd.print_add conf base
  | Some "ADD_IND_OK" when conf.wizard && not conf.not_wizard ->
      UpdateIndOk.print_add conf base
  | Some "ADD_PAR" when conf.wizard && not conf.not_wizard ->
      UpdateFam.print_add_parents conf base
  | Some "AN" ->
      begin match p_getenv conf.env "v" with
        Some x -> Birthday.print_birth conf base (int_of_string x)
      | _ -> Birthday.print_menu_birth conf base
      end
  | Some "AD" ->
      begin match p_getenv conf.env "v" with
        Some x -> Birthday.print_dead conf base (int_of_string x)
      | _ -> Birthday.print_menu_dead conf base
      end
  | Some "AM" ->
      begin match p_getenv conf.env "v" with
        Some x -> Birthday.print_marriage conf base (int_of_string x)
      | _ -> Birthday.print_menu_marriage conf base
      end
  | Some "AS_OK" -> AdvSearchOk.print conf base
  | Some "B" when conf.wizard || conf.friend ->
      BirthDeath.print_birth conf base
  | Some "C" ->
      begin match find_person_in_env conf base "" with
        Some p -> Cousins.print conf base p
      | _ -> very_unknown conf
      end
  | Some "CAL" -> Date.print_calendar conf base
  | Some "CHG_CHN" when conf.wizard -> ChangeChildren.print conf base
  | Some "CHG_CHN_OK" when conf.wizard -> ChangeChildren.print_ok conf base
  | Some "CHG_FAM_ORD" when conf.wizard ->
      UpdateFam.print_change_order conf base
  | Some "CHG_FAM_ORD_OK" when conf.wizard ->
      UpdateFamOk.print_change_order_ok conf base
  | Some "CONN_WIZ" when conf.wizard -> Wiznotes.connected_wizards conf base
  | Some "D" ->
      begin match find_person_in_env conf base "" with
        Some p -> Descend.print conf base p
      | _ -> very_unknown conf
      end
  | Some "DAG" -> Dag.print conf base
  | Some "DEL_FAM" when conf.wizard && not conf.not_wizard ->
      UpdateFam.print_del conf base
  | Some "DEL_FAM_OK" when conf.wizard && not conf.not_wizard ->
      UpdateFamOk.print_del conf base
  | Some "DEL_IMAGE"
    when (conf.wizard && not conf.not_wizard) && conf.can_send_image ->
      SendImage.print_del conf base
  | Some "DEL_IMAGE_OK"
    when (conf.wizard && not conf.not_wizard) && conf.can_send_image ->
      SendImage.print_del_ok conf base
  | Some "DEL_IND" when conf.wizard && not conf.not_wizard ->
      UpdateInd.print_del conf base
  | Some "DEL_IND_OK" when conf.wizard && not conf.not_wizard ->
      UpdateIndOk.print_del conf base
  | Some "FORUM" ->
      begin match p_getenv conf.base_env "disable_forum" with
        Some "yes" -> incorrect_request conf
      | _ -> Forum.print conf base
      end
  | Some "FORUM_ADD" ->
      begin match p_getenv conf.base_env "disable_forum" with
        Some "yes" -> incorrect_request conf
      | _ -> Forum.print_add conf base
      end
  | Some "FORUM_ADD_OK" ->
      begin match p_getenv conf.base_env "disable_forum" with
        Some "yes" -> incorrect_request conf
      | _ -> Forum.print_add_ok conf base
      end
  | Some "FORUM_DEL" ->
      begin match p_getenv conf.base_env "disable_forum" with
        Some "yes" -> incorrect_request conf
      | _ -> Forum.print_del conf base
      end
  | Some "FORUM_P_P" ->
      begin match p_getenv conf.base_env "disable_forum" with
        Some "yes" -> incorrect_request conf
      | _ -> Forum.print_access_switch conf base
      end
  | Some "FORUM_SEARCH" ->
      begin match p_getenv conf.base_env "disable_forum" with
        Some "yes" -> incorrect_request conf
      | _ -> Forum.print_search conf base
      end
  | Some "FORUM_VAL" ->
      begin match p_getenv conf.base_env "disable_forum" with
        Some "yes" -> incorrect_request conf
      | _ -> Forum.print_valid conf base
      end
  | Some "FORUM_VIEW" ->
      begin match p_getenv conf.base_env "disable_forum" with
        Some "yes" -> incorrect_request conf
      | _ -> Forum.print conf base
      end
  | Some "H" ->
      begin match p_getenv conf.env "v" with
        Some f -> Srcfile.print conf base f
      | None -> Hutil.incorrect_request conf
      end
  | Some "HIST" -> History.print conf base
  | Some "HIST_CLEAN" when conf.wizard -> History_diff.print_clean conf base
  | Some "HIST_CLEAN_OK" when conf.wizard ->
      History_diff.print_clean_ok conf base
  | Some "HIST_DIFF" -> History_diff.print conf base
  | Some "HIST_SEARCH" -> History.print_search conf base
  | Some "IMH" -> Image.print_html conf base
  | Some "INV_FAM" when conf.wizard && not conf.not_wizard ->
      UpdateFam.print_inv conf base
  | Some "INV_FAM_OK" when conf.wizard && not conf.not_wizard ->
      UpdateFamOk.print_inv conf base
  | Some "KILL_ANC" when conf.wizard && not conf.not_wizard ->
      MergeInd.print_kill_ancestors conf base
  | Some "LB" when conf.wizard || conf.friend ->
      BirthDeath.print_birth conf base
  | Some "LD" when conf.wizard || conf.friend ->
      BirthDeath.print_death conf base
  | Some "LINKED" ->
      let o_fn =
        match p_getenv conf.env "old_fn" with
          Some v -> v
        | None -> ""
      in
      let o_oc =
        int_of_string
          (match p_getenv conf.env "old_occ" with
             Some v -> v
           | None -> "0")
      in
      let o_sn =
        match p_getenv conf.env "old_sn" with
          Some v -> v
        | None -> ""
      in
      begin match find_person_in_env conf base "" with
        Some p -> Perso.print_what_links conf base p o_fn o_oc o_sn
      | _ -> very_unknown conf
      end
  | Some "LL" -> BirthDeath.print_longest_lived conf base
  | Some "LM" when conf.wizard || conf.friend ->
      BirthDeath.print_marriage conf base
  | Some "LEX" -> Srcfile.print_lexicon conf base
  | Some "MISC_NOTES" -> Notes.print_misc_notes conf base
  | Some "MISC_NOTES_SEARCH" -> Notes.print_misc_notes_search conf base
  | Some "MOD_DATA" when conf.wizard && conf.modify_dict || conf.manitou ->
      UpdateData.print_mod conf base
  | Some "MOD_DATA_OK" when conf.wizard && conf.modify_dict || conf.manitou ->
      UpdateData.print_mod_ok conf base
  | Some "MOD_FAM" when conf.wizard && not conf.not_wizard ->
      UpdateFam.print_mod conf base
  | Some "MOD_FAM_OK" when conf.wizard && not conf.not_wizard ->
      UpdateFamOk.print_mod conf base
  | Some "MOD_IND" when conf.wizard && not conf.not_wizard ->
      UpdateInd.print_mod conf base
  | Some "MOD_IND_OK" when conf.wizard && not conf.not_wizard ->
      UpdateIndOk.print_mod conf base
  | Some "MOD_NOTES" when conf.wizard && not conf.not_wizard ->
      Notes.print_mod conf base
  | Some "MOD_NOTES_OK" when conf.wizard && not conf.not_wizard ->
      Notes.print_mod_ok conf base
  | Some "MOD_WIZNOTES" when conf.authorized_wizards_notes ->
      Wiznotes.print_mod conf base
  | Some "MOD_WIZNOTES_OK" when conf.authorized_wizards_notes ->
      Wiznotes.print_mod_ok conf base
  | Some "MRG" when conf.wizard && not conf.not_wizard ->
      begin match find_person_in_env conf base "" with
        Some p -> Merge.print conf base p
      | _ -> very_unknown conf
      end
  | Some "MRG_DUP" when conf.wizard && not conf.not_wizard ->
      MergeDup.main_page conf base
  | Some "MRG_DUP_IND_Y_N" when conf.wizard && not conf.not_wizard ->
      MergeDup.answ_ind_y_n conf base
  | Some "MRG_DUP_FAM_Y_N" when conf.wizard && not conf.not_wizard ->
      MergeDup.answ_fam_y_n conf base
  | Some "MRG_FAM" when conf.wizard && not conf.not_wizard ->
      MergeFam.print conf base
  | Some "MRG_FAM_OK" when conf.wizard && not conf.not_wizard ->
      MergeFamOk.print_merge conf base
  | Some "MRG_MOD_FAM_OK" when conf.wizard && not conf.not_wizard ->
      MergeFamOk.print_mod_merge conf base
  | Some "MRG_IND" when conf.wizard && not conf.not_wizard ->
      MergeInd.print conf base
  | Some "MRG_IND_OK" when conf.wizard && not conf.not_wizard ->
      MergeIndOk.print_merge conf base
  | Some "MRG_MOD_IND_OK" when conf.wizard && not conf.not_wizard ->
      MergeIndOk.print_mod_merge conf base
  | Some "N" ->
      begin match p_getenv conf.env "v" with
        Some v -> Some.surname_print conf base Some.surname_not_found v
      | _ -> Alln.print_surnames conf base
      end
  | Some "NG" ->
      (* Rétro-compatibilité <= 6.06 *)
      let env =
        match p_getenv conf.env "n" with
          Some n ->
            begin match p_getenv conf.env "t" with
              Some "P" -> ("fn", n) :: conf.env
            | Some "N" -> ("sn", n) :: conf.env
            | _ -> conf.env
            end
        | None -> conf.env
      in
      let conf = {conf with env = env} in
      (* Nouveau mode de recherche. *)
      begin match p_getenv conf.env "select" with
        Some "input" | None ->
          (* Récupère le contenu non vide de la recherche. *)
          let real_input label =
            match p_getenv conf.env label with
              Some s -> if s = "" then None else Some s
            | None -> None
          in
          (* Recherche par clé, sosa, alias ... *)
          let search n =
            let (pl, sosa_acc) = find_all conf base n in
            match pl with
              [] ->
                conf.cancel_links <- false;
                Some.surname_print conf base unknown n
            | [p] ->
                if sosa_acc || Gutil.person_of_string_key base n <> None ||
                   person_is_std_key conf base p n
                then
                  person_selected conf base p
                else specify conf base n pl
            | pl -> specify conf base n pl
          in
          begin match real_input "n" with
            Some n -> search n
          | None ->
              match real_input "fn", real_input "sn" with
                Some fn, Some sn -> search (fn ^ " " ^ sn)
              | Some fn, None ->
                  conf.cancel_links <- false;
                  Some.first_name_print conf base fn
              | None, Some sn ->
                  conf.cancel_links <- false;
                  Some.surname_print conf base unknown sn
              | None, None -> incorrect_request conf
          end
      | Some i ->
          relation_print conf base
            (pget conf base (Adef.iper_of_int (int_of_string i)))
      end
  | Some "NOTES" -> Notes.print conf base
  | Some "OA" when conf.wizard || conf.friend ->
      BirthDeath.print_oldest_alive conf base
  | Some "OE" when conf.wizard || conf.friend ->
      BirthDeath.print_oldest_engagements conf base
  | Some "P" ->
      begin match p_getenv conf.env "v" with
        Some v -> Some.first_name_print conf base v
      | None -> Alln.print_first_names conf base
      end
  | Some "POP_PYR" when conf.wizard || conf.friend ->
      BirthDeath.print_population_pyramid conf base
  | Some "PS" -> Place.print_all_places_surnames conf base
  | Some "R" ->
      begin match find_person_in_env conf base "" with
        Some p -> relation_print conf base p
      | _ -> very_unknown conf
      end
  | Some "REQUEST" when conf.wizard ->
      let title _ = () in
      header conf title;
      Wserver.wprint "<pre>\n";
      List.iter (Wserver.wprint "%s\n") conf.request;
      Wserver.wprint "</pre>\n";
      trailer conf
  | Some "RL" -> RelationLink.print conf base
  | Some "RLM" -> Relation.print_multi conf base
  | Some "S" -> SearchName.print conf base specify unknown
  | Some "SND_IMAGE" when conf.wizard && conf.can_send_image ->
      SendImage.print conf base
  | Some "SND_IMAGE_OK" when conf.wizard && conf.can_send_image ->
      SendImage.print_send_ok conf base
  | Some "SRC" ->
      begin match p_getenv conf.env "v" with
        Some f -> Srcfile.print_source conf base f
      | _ -> Hutil.incorrect_request conf
      end
  | Some "STAT" -> BirthDeath.print_statistics conf base
  | Some "CHANGE_WIZ_VIS" when conf.wizard ->
      Wiznotes.change_wizard_visibility conf base
  | Some "TT" -> Title.print conf base
  | Some "U" when conf.wizard && not conf.not_wizard ->
      begin match find_person_in_env conf base "" with
        Some p -> updmenu_print conf base p
      | _ -> very_unknown conf
      end
  | Some "VIEW_WIZNOTES" when conf.wizard && conf.authorized_wizards_notes ->
      Wiznotes.print_view conf base
  | Some "WIZNOTES" when conf.authorized_wizards_notes ->
      Wiznotes.print conf base
  | Some "WIZNOTES_SEARCH" when conf.authorized_wizards_notes ->
      Wiznotes.print_search conf base
  | Some mode -> incorrect_request conf
  | None ->
      match find_person_in_env conf base "" with
        Some p -> person_selected conf base p
      | _ -> very_unknown conf

let print_no_index conf base =
  let title _ =
    Wserver.wprint "%s" (Util.capitale (transl conf "link to use"))
  in
  let link = url_no_index conf base false in
  let http =
    match p_getenv conf.base_env "http_mode" with
      Some "https" -> "https://"
    | _ -> "http://"
  in
  header conf title;
  Wserver.wprint "<ul>\n";
  html_li conf;
  Wserver.wprint "<a href=\"%s%s\">\n" http link;
  Wserver.wprint "%s%s" http link;
  Wserver.wprint "</a>\n";
  Wserver.wprint "</ul>\n";
  print_link_to_welcome conf false;
  trailer conf

let special_vars =
  ["alwsurn"; "cgl"; "dsrc"; "em"; "ei"; "ep"; "en"; "eoc"; "escache"; "et";
   "iz"; "log_cnl"; "log_pwd"; "log_uid"; "long"; "manitou"; "nz"; "ocz";
   "pz"; "pure_xhtml"; "size"; "spouse"; "templ"]

let only_special_env = List.for_all (fun (x, _) -> List.mem x special_vars)

let extract_henv conf base =
  begin match find_sosa_ref conf base with
    Some p ->
      let x =
        let first_name = p_first_name base p in
        let surname = p_surname base p in
        if Util.accessible_by_key conf base p first_name surname then
          ["pz", code_varenv (Name.lower first_name);
           "nz", code_varenv (Name.lower surname);
           "ocz", string_of_int (get_occ p)]
        else ["iz", string_of_int (Adef.int_of_iper (get_key_index p))]
      in
      conf.henv <- conf.henv @ x
  | None -> ()
  end;
  begin match p_getenv conf.env "dsrc" with
    Some "" | None -> ()
  | Some s -> conf.henv <- conf.henv @ ["dsrc", code_varenv s]
  end;
  begin match p_getenv conf.env "templ" with
    None -> ()
  | Some s -> conf.henv <- conf.henv @ ["templ", code_varenv s]
  end;
  begin match p_getenv conf.env "escache" with
    Some _ ->
      let v = escache_value base in conf.henv <- conf.henv @ ["escache", v]
  | None -> ()
  end;
  begin match p_getenv conf.env "alwsurn" with
    Some x -> conf.henv <- conf.henv @ ["alwsurn", x]
  | None -> ()
  end;
  begin match p_getenv conf.env "pure_xhtml" with
    Some x -> conf.henv <- conf.henv @ ["pure_xhtml", x]
  | None -> ()
  end;
  begin match p_getenv conf.env "size" with
    Some x -> conf.henv <- conf.henv @ ["size", x]
  | None -> ()
  end;
  match p_getenv conf.env "manitou" with
    Some "off" -> conf.henv <- conf.henv @ ["manitou", "off"]
  | Some _ | None -> ()

let set_owner conf =
  let s = Unix.stat (Util.base_path [] (conf.bname ^ ".gwb")) in
  try Unix.setgid s.Unix.st_gid; Unix.setuid s.Unix.st_uid with
    Unix.Unix_error (_, _, _) -> ()

let thousand oc x = Num.print (output_string oc) "," (Num.of_int x)

let log_count conf (log_file, log_oc, flush_log) r =
  if conf.cgi && log_file = "" then ()
  else
    match r with
      Some (welcome_cnt, request_cnt, start_date) ->
        let oc = log_oc () in
        Printf.fprintf oc "  #accesses %a (#welcome %a) since %s\n" thousand
          (welcome_cnt + request_cnt) thousand welcome_cnt start_date;
        flush_log oc
    | None -> ()

let print_moved conf base s =
  match Util.open_etc_file "moved" with
    Some ic ->
      let env = ["bname", conf.bname] in
      Util.html conf; Util.nl (); Templ.copy_from_templ conf env ic
  | None ->
      let title _ = Wserver.wprint "%s -&gt; %s" conf.bname s in
      Hutil.header_no_page_title conf title;
      Wserver.wprint "The database %s has moved to:\n<dl><dt><dd>\n"
        conf.bname;
      Wserver.wprint "<a href=\"%s\">" s;
      Wserver.wprint "%s" s;
      Wserver.wprint "</a>";
      Wserver.wprint "\n</dd></dt></dl>\n";
      Hutil.trailer conf

let cnt_trace = ref 50
let trace_keys base (fn, sn, occ) ipo =
  if !cnt_trace < 0 then ()
  else
    match ipo with
      None ->
        Printf.eprintf "(\"%s\", \"%s\", \"%d\") deleted\n" fn sn occ;
        flush stderr
    | Some ip ->
        decr cnt_trace;
        let p = poi base ip in
        let fn1 = sou base (get_first_name p) in
        let sn1 = sou base (get_surname p) in
        let occ1 = get_occ p in
        if Name.lower (Mutil.nominative fn1) = fn &&
           Name.lower (Mutil.nominative sn1) = sn && occ1 = occ
        then
          begin
            Printf.eprintf "(\"%s\", \"%s\", \"%d\") ok\n" fn sn occ;
            flush stderr
          end
        else
          begin
            Printf.eprintf
              "Error %s.%d %s with key = (\"%s\", \"%s\", \"%d\")\n" fn1 occ1
              sn1 fn sn occ;
            flush stderr
          end

let treat_request conf base log =
  begin match
    p_getenv conf.base_env "moved", p_getenv conf.env "opt",
    p_getenv conf.env "m"
  with
    Some s, _, _ -> print_moved conf base s
  | _, Some "no_index", _ -> print_no_index conf base
  | _, _, Some "IM" -> Image.print conf base
  | _, _, Some "DOC" ->
      begin match p_getenv conf.env "s" with
        Some f ->
          if Filename.check_suffix f ".txt" then
            let f = Filename.chop_suffix f ".txt" in
            Srcfile.print_source conf base f
          else Image.print conf base
      | None -> Hutil.incorrect_request conf
      end
  | _ ->
      set_owner conf;
      extract_henv conf base;
      make_senv conf base;
      let conf =
        match Util.default_sosa_ref conf base with
          Some p -> {conf with default_sosa_ref = get_key_index p, Some p}
        | None -> conf
      in
      if only_special_env conf.env then
        begin
          begin match p_getenv conf.base_env "counter" with
            Some "no" -> ()
          | _ ->
              let r = Srcfile.incr_welcome_counter conf in
              log_count conf log r
          end;
          Srcfile.print_start conf base
        end
      else
        begin
          begin match p_getenv conf.base_env "counter" with
            Some "no" -> ()
          | _ ->
              let r = Srcfile.incr_request_counter conf in
              log_count conf log r
          end;
          match p_getenv conf.env "ptempl" with
            Some tname when p_getenv conf.base_env "ptempl" = Some "yes" ->
              begin match find_person_in_env conf base "" with
                Some p -> Perso.interp_templ tname conf base p
              | None -> family_m conf base
              end
          | _ -> family_m conf base
        end
  end;
  Wserver.wflush ()

let treat_request_on_possibly_locked_base conf bfile log =
  match try Left (Gwdb.open_base bfile) with e -> Right e with
    Left base ->
      if !(Mutil.utf_8_db) then ()
      else
        begin
          Hashtbl.clear conf.lexicon;
          let fname = Filename.concat "lang" "lexicon.txt" in
          Mutil.input_lexicon conf.lang conf.lexicon
            (fun () -> Secure.open_in (Util.search_in_lang_path fname));
          conf.charset <-
            try Hashtbl.find conf.lexicon " !charset" with
              Not_found -> "iso-8859-1"
        end;
      begin try treat_request conf base log with
        exc -> close_base base; raise exc
      end;
      close_base base
  | Right e ->
      let transl conf w =
        try Hashtbl.find conf.lexicon w with Not_found -> "[" ^ w ^ "]"
      in
      let title _ =
        Wserver.wprint "%s" (Util.capitale (transl conf "error"))
      in
      Hutil.rheader conf title;
      Wserver.wprint "<ul>";
      Util.html_li conf;
      Wserver.wprint "%s" (Util.capitale (transl conf "cannot access base"));
      Wserver.wprint " \"%s\".</ul>\n" conf.bname;
      begin match e with
        Sys_error _ -> ()
      | _ ->
          Wserver.wprint
            "<em><font size=\"-1\">Internal message: %s</font></em>\n"
            (Printexc.to_string e)
      end;
      Hutil.trailer conf

let this_request_updates_database conf =
  match p_getenv conf.env "m" with
    Some ("FORUM_ADD_OK" | "FORUM_DEL" | "FORUM_VAL") -> true
  | Some x when conf.wizard ->
      begin match x with
        "ADD_FAM_OK" | "ADD_IND_OK" | "CHANGE_WIZ_VIS" | "CHG_CHN_OK" |
        "CHG_FAM_ORD_OK" | "DEL_FAM_OK" | "DEL_IMAGE_OK" | "DEL_IND_OK" |
        "INV_FAM_OK" | "KILL_ANC" | "MOD_FAM_OK" | "MOD_IND_OK" |
        "MOD_NOTES_OK" | "MOD_WIZNOTES_OK" | "MRG_DUP_IND_Y_N" |
        "MRG_DUP_FAM_Y_N" | "MRG_IND" | "MRG_MOD_FAM_OK" | "MRG_MOD_IND_OK" |
        "MOD_DATA_OK" | "SND_IMAGE_OK" ->
          true
      | _ -> false
      end
  | _ -> false

(*
type t = [ Accept | Refuse ];
*)
let treat_request_on_base conf log =
  let bfile = Util.base_path [] (conf.bname ^ ".gwb") in
  if this_request_updates_database conf then
    match
      Lock.control (Mutil.lock_file bfile) false
        (fun () -> treat_request_on_possibly_locked_base conf bfile log)
    with
      Some x ->
        (**)
        x
    | None -> Update.error_locked conf
  else treat_request_on_possibly_locked_base conf bfile log
