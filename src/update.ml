(* camlp5r ./pa_html.cmo *)
(* $Id: update.ml,v 5.48 2008-01-08 11:58:46 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gutil
open Gwdb
open Hutil
open Util

exception ModErr
type create_info =
  { ci_birth_date : date option;
    ci_birth_place : string;
    ci_death : death;
    ci_death_date : date option;
    ci_death_place : string;
    ci_occupation : string;
    ci_public : bool }
type create =
    Create of sex * create_info option
  | Link
type key = string * string * int * create * string

let infer_death conf birth bapt =
  match birth, bapt with
    Some (Dgreg (d, _)), _ ->
      let a = (CheckItem.time_elapsed d conf.today).year in
      if a > 120 then OfCourseDead
      else if a <= 80 then NotDead
      else DontKnowIfDead
  | _, Some (Dgreg (d, _)) ->
      let a = (CheckItem.time_elapsed d conf.today).year in
      if a > 120 then OfCourseDead
      else if a <= 80 then NotDead
      else DontKnowIfDead
  | _ -> DontKnowIfDead

let restrict_to_small_list el =
  let rec begin_list n rl el =
    if n > 25 then
      let rec end_list n sl el =
        if n > 25 then List.rev_append rl (None :: sl)
        else
          match el with
            e :: el -> end_list (n + 1) (Some e :: sl) el
          | [] -> List.rev_append rl sl
      in
      end_list 0 [] (List.rev el)
    else
      match el with
        e :: el -> begin_list (n + 1) (Some e :: rl) el
      | [] -> List.rev rl
  in
  begin_list 0 [] el

let print_person_parents_and_spouses conf base p =
  Wserver.wprint "<a href=\"%s%s\">" (commd conf) (acces conf base p);
  Wserver.wprint "%s.%d %s" (p_first_name base p) (get_occ p)
    (p_surname base p);
  Wserver.wprint "</a>";
  Wserver.wprint "%s" (Date.short_dates_text conf base p);
  let cop = Util.child_of_parent conf base p in
  if cop != "" then Wserver.wprint ", %s" cop;
  let ifam = get_family p in
  let nbfam = Array.length ifam in
  List.iteri
    (fun i ifam ->
       let fam = foi base ifam in
       let sp = Gutil.spouse (get_key_index p) fam in
       let sp = poi base sp in
       Wserver.wprint ", &amp;";
       if nbfam > 1 then Wserver.wprint "%d" (i + 1);
       Wserver.wprint " ";
       Wserver.wprint "%s.%d %s" (p_first_name base sp) (get_occ sp)
         (p_surname base sp);
       Wserver.wprint "%s" (Date.short_dates_text conf base sp))
    (Array.to_list ifam);
  Wserver.wprint "\n"

let print_same_name conf base p =
  Wserver.wprint "<p>\n";
  Wserver.wprint "%s%s\n"
    (capitale (transl conf "persons having the same name")) (transl conf ":");
  Wserver.wprint "<ul>\n";
  let pl = Gutil.find_same_name base p in
  let pl = restrict_to_small_list pl in
  List.iter
    (fun p ->
       Wserver.wprint "<li>";
       begin match p with
         Some p -> print_person_parents_and_spouses conf base p
       | None -> Wserver.wprint "...\n"
       end;
       Wserver.wprint "</li>")
    pl;
  Wserver.wprint "</ul>\n";
  Wserver.wprint "</p>\n"

(*
value print_same_name conf base p =
  match Gutil.find_same_name base p with
  [ [_] -> ()
  | pl ->
      let print_person p =
        do {
          stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p)
          begin
            Wserver.wprint "%s.%d %s" (p_first_name base p)
              (get_occ p) (p_surname base p);
          end;
          Wserver.wprint "%s" (Date.short_dates_text conf base p)
        }
      in
      let pl = restrict_to_small_list pl in
      tag "p" begin
        Wserver.wprint "%s:\n"
          (capitale (transl conf "persons having the same name"));
        tag "ul" begin
          List.iter
            (fun p ->
             stag "li" begin
               match p with
               [ Some p -> do {
                   print_person p;
                   let ifam = get_family p in
                   List.iter
                     (fun ifam -> do {
                       let fam = foi base ifam in
                       let sp = spouse (get_key_index p) fam in
                       let sp = poi base sp in
                       Wserver.wprint ", &amp; ";
                       print_person sp } )
                     (Array.to_list ifam);
                   Wserver.wprint "\n"
                 }
               | None -> Wserver.wprint "...\n" ];
             end)
            pl;
        end;
      end ]
;
*)
let print_return conf =
  Wserver.wprint "<p>\n";
  Wserver.wprint "<form method=\"post\" action=\"%s\">\n" conf.command;
  List.iter
    (fun (x, v) ->
       (* Seul un textarea peut contenir des sauts de ligne. *)
       (* On remplace donc l'input par un textarea.          *)
       if x = "notes" || x = "comment" then
         begin
           Wserver.wprint "<textarea style=\"display:none;\" name=\"%s\">\n"
             x;
           Wserver.wprint "%s" (quote_escaped (decode_varenv v));
           Wserver.wprint "</textarea>\n"
         end
       else
         Wserver.wprint "<input type=\"hidden\" name=\"%s\" value=\"%s\"%s>\n"
           x (quote_escaped (decode_varenv v)) conf.xhs)
    (conf.henv @ conf.env);
  Wserver.wprint "<input type=\"hidden\" name=\"return\" value=\"on\"%s>\n"
    conf.xhs;
  Wserver.wprint "<input type=\"submit\" value=\"%s\"%s>\n"
    (capitale (transl conf "back")) conf.xhs;
  Wserver.wprint "</form>\n";
  Wserver.wprint "</p>\n"

let print_err_unknown conf base (f, s, o) =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  rheader conf title;
  Wserver.wprint "%s: <strong>%s.%d %s</strong>\n"
    (capitale (transl conf "unknown person")) f o s;
  print_return conf;
  trailer conf;
  raise ModErr

let update_misc_names_of_family base p_sex u =
  match p_sex with
    Male ->
      List.iter
        (fun ifam ->
           let fam = foi base ifam in
           List.iter
             (fun ip ->
                List.iter
                  (fun name ->
                     if not (List.mem ip (person_ht_find_all base name)) then
                       person_ht_add base name ip)
                  (person_misc_names base (poi base ip) get_titles))
             (get_mother fam :: Array.to_list (get_children fam)))
        (Array.to_list u.family)
  | _ -> ()

let delete_topological_sort_v conf base =
  let bfile = Util.base_path [] (conf.bname ^ ".gwb") in
  let tstab_file = Filename.concat bfile "tstab_visitor" in
  (try Sys.remove tstab_file with Sys_error _ -> ());
  let tstab_file = Filename.concat bfile "restrict" in
  try Sys.remove tstab_file with Sys_error _ -> ()

let delete_topological_sort conf base =
  let _ = delete_topological_sort_v conf base in
  let bfile = Util.base_path [] (conf.bname ^ ".gwb") in
  let tstab_file = Filename.concat bfile "tstab" in
  try Sys.remove tstab_file with Sys_error _ -> ()

let print_someone conf base p =
  Wserver.wprint "%s%s %s" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))
    (p_surname base p)

let print_first_name conf base p =
  Wserver.wprint "%s%s" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))

let print_someone_strong conf base p =
  Printf.sprintf "<strong>%s%s %s</strong>" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))
    (p_surname base p)

let print_first_name_strong conf base p =
  Wserver.wprint "<strong>%s%s</strong>" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))

let print_error conf base =
  function
    AlreadyDefined p ->
      Wserver.wprint
        (fcapitale (ftransl conf "name %s already used by %tthis person%t"))
        ("\"" ^ p_first_name base p ^ "." ^ string_of_int (get_occ p) ^ " " ^
         p_surname base p ^ "\"")
        (fun _ ->
           Printf.sprintf "<a href=\"%s%s\">" (commd conf)
             (acces conf base p))
        (fun _ -> "</a>.")
  | OwnAncestor p ->
      Wserver.wprint "%s\n%s" (print_someone_strong conf base p)
        (transl conf "would be his/her own ancestor")
  | BadSexOfMarriedPerson p ->
      Wserver.wprint "%s."
        (capitale (transl conf "cannot change sex of a married person"))

let print_someone_ref conf base p =
  Wserver.wprint "<a href=\"%s%s\">\n%s%s %s</a>" (commd conf)
    (acces conf base p) (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))
    (p_surname base p)

let someone_ref_text conf base p =
  "<a href=\"" ^ commd conf ^ acces conf base p ^ "\">\n" ^
  p_first_name base p ^
  (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p)) ^ " " ^
  p_surname base p ^ "</a>"

let print_first_name_ref conf base p =
  Wserver.wprint "%s" (someone_ref_text conf base p)

let print_warning conf base =
  function
    BigAgeBetweenSpouses (fath, moth, a) ->
      Wserver.wprint
        (fcapitale
           (ftransl conf
              "the difference of age between %t and %t is quite important"))
        (fun _ -> print_someone_strong conf base fath)
        (fun _ -> print_someone_strong conf base moth);
      Wserver.wprint ": %s" (Date.string_of_age conf a)
  | BirthAfterDeath p ->
      Wserver.wprint (ftransl conf "%t died before his/her birth")
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (Date.short_dates_text conf base p))
  | BirthAfterBaptism p ->
      Wserver.wprint (ftransl conf "%t baptised before his/her birth")
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (Date.short_dates_text conf base p))
  | ChangedOrderOfChildren (ifam, des, before, after) ->
      let cpl = foi base ifam in
      let fath = poi base (get_father cpl) in
      let moth = poi base (get_mother cpl) in
      Wserver.wprint "%s\n"
        (capitale (transl conf "changed order of children"));
      Wserver.wprint "-&gt;\n";
      Wserver.wprint "%s"
        (someone_ref_text conf base fath ^ "\n" ^ transl_nth conf "and" 0 ^
         " " ^ someone_ref_text conf base moth ^ "\n");
      let print_list arr diff_arr =
        Array.iteri
          (fun i ip ->
             let p = poi base ip in
             Wserver.wprint "<li %s>\n"
               (if diff_arr.(i) then "style=\"background:pink\"" else "");
             if eq_istr (get_surname p) (get_surname fath) then
               print_first_name conf base p
             else print_someone conf base p;
             Wserver.wprint "%s" (Date.short_dates_text conf base p);
             Wserver.wprint "\n";
             Wserver.wprint "</li>\n")
          arr
      in
      let (bef_d, aft_d) = Diff.f before after in
      Wserver.wprint "<table style=\"margin:1em\">\n";
      Wserver.wprint "<tr>\n";
      Wserver.wprint "<td>\n";
      Wserver.wprint "<ul style=\"list-style-type:none\">\n";
      print_list before bef_d;
      Wserver.wprint "</ul>\n";
      Wserver.wprint "</td>\n";
      Wserver.wprint "<td>\n";
      Wserver.wprint "<ul style=\"list-style-type:none\">\n";
      print_list after aft_d;
      Wserver.wprint "</ul>\n";
      Wserver.wprint "</td>\n";
      Wserver.wprint "</tr>\n";
      Wserver.wprint "</table>\n"
  | ChildrenNotInOrder (ifam, des, elder, x) ->
      let cpl = foi base ifam in
      Wserver.wprint
        (fcapitale
           (ftransl conf
              "the following children of %t and %t are not in order"))
        (fun _ -> print_someone_strong conf base (poi base (get_father cpl)))
        (fun _ -> print_someone_strong conf base (poi base (get_mother cpl)));
      Wserver.wprint ":\n";
      Wserver.wprint "<ul>\n";
      Wserver.wprint "<li>";
      print_first_name_strong conf base elder;
      Wserver.wprint "%s" (Date.short_dates_text conf base elder);
      Wserver.wprint "</li>";
      Wserver.wprint "<li>";
      print_first_name_strong conf base x;
      Wserver.wprint "%s" (Date.short_dates_text conf base x);
      Wserver.wprint "</li>";
      Wserver.wprint "</ul>\n"
  | ChangedOrderOfMarriages (p, before, after) ->
      Wserver.wprint "%s\n"
        (capitale (transl conf "changed order of marriages"));
      Wserver.wprint "-&gt;\n";
      let print_list arr diff_arr =
        Array.iteri
          (fun i ifam ->
             let fam = foi base ifam in
             let sp = spouse (get_key_index p) fam in
             let sp = poi base sp in
             Wserver.wprint "<li %s>\n"
               (if diff_arr.(i) then "style=\"background:pink\"" else "");
             print_first_name conf base p;
             Wserver.wprint "  &amp;";
             Wserver.wprint "%s\n"
               (Date.short_marriage_date_text conf base fam p sp);
             print_someone conf base sp;
             Wserver.wprint "\n";
             Wserver.wprint "</li>\n")
          arr
      in
      let (bef_d, aft_d) = Diff.f before after in
      Wserver.wprint "<table style=\"margin:1em\">\n";
      Wserver.wprint "<tr>\n";
      Wserver.wprint "<td>\n";
      Wserver.wprint "<ul style=\"list-style-type:none\">\n";
      print_list before bef_d;
      Wserver.wprint "</ul>\n";
      Wserver.wprint "</td>\n";
      Wserver.wprint "<td>\n";
      Wserver.wprint "<ul style=\"list-style-type:none\">\n";
      print_list after aft_d;
      Wserver.wprint "</ul>\n";
      Wserver.wprint "</td>\n";
      Wserver.wprint "</tr>\n";
      Wserver.wprint "</table>\n"
  | CloseChildren (ifam, des, elder, x) ->
      let cpl = foi base ifam in
      Wserver.wprint
        (fcapitale
           (ftransl conf
              "the following children of %t and %t are born very close"))
        (fun _ -> print_someone_strong conf base (poi base (get_father cpl)))
        (fun _ -> print_someone_strong conf base (poi base (get_mother cpl)));
      Wserver.wprint ":\n";
      Wserver.wprint "<ul>\n";
      Wserver.wprint "<li>";
      print_first_name_strong conf base elder;
      Wserver.wprint "%s" (Date.short_dates_text conf base elder);
      Wserver.wprint "</li>";
      Wserver.wprint "<li>";
      print_first_name_strong conf base x;
      Wserver.wprint "%s" (Date.short_dates_text conf base x);
      Wserver.wprint "</li>";
      Wserver.wprint "</ul>\n"
  | DeadOld (p, a) ->
      Wserver.wprint "%s\n%s\n" (print_someone_strong conf base p)
        (transl_nth conf "died at an advanced age"
           (index_of_sex (get_sex p)));
      Wserver.wprint "(%s)" (Date.string_of_age conf a)
  | DeadTooEarlyToBeFather (father, child) ->
      Wserver.wprint
        (ftransl conf "\
%t is born more than 2 years after the death of his/her father %t")
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base child)
             (Date.short_dates_text conf base child))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base father)
             (Date.short_dates_text conf base father))
  | IncoherentSex (p, _, _) ->
      Wserver.wprint
        (fcapitale
           (ftransl conf "%t's sex is not coherent with his/her relations"))
        (fun _ -> print_someone_strong conf base p)
  | IncoherentAncestorDate (anc, p) ->
      Wserver.wprint "%s has a younger ancestor %s"
        (print_someone_strong conf base p)
        (print_someone_strong conf base anc)
  | MarriageDateAfterDeath p ->
      Wserver.wprint
        (fcapitale
           (ftransl conf "marriage had occured after the death of %t"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (Date.short_dates_text conf base p))
  | MarriageDateBeforeBirth p ->
      Wserver.wprint
        (fcapitale
           (ftransl conf "marriage had occured before the birth of %t"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (Date.short_dates_text conf base p))
  | MotherDeadAfterChildBirth (mother, child) ->
      Wserver.wprint
        (ftransl conf "%t is born after the death of his/her mother %t")
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base child)
             (Date.short_dates_text conf base child))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base mother)
             (Date.short_dates_text conf base mother))
  | ParentBornAfterChild (p, c) ->
      Wserver.wprint "%s\n%s\n%s" (print_someone_strong conf base p)
        (transl conf "is born after his/her child")
        (print_someone_strong conf base c)
  | ParentTooYoung (p, a) ->
      Wserver.wprint "%s\n%s\n" (print_someone_strong conf base p)
        (transl conf "is a very young parent");
      Wserver.wprint "(%s)" (Date.string_of_age conf a)
  | ParentTooOld (p, a) ->
      Wserver.wprint "%s\n%s\n" (print_someone_strong conf base p)
        (transl conf "is a very old parent");
      Wserver.wprint "(%s)" (Date.string_of_age conf a)
  | TitleDatesError (p, t) ->
      Wserver.wprint
        (fcapitale (ftransl conf "%t has incorrect title dates: %t"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (Date.short_dates_text conf base p))
        (fun _ ->
           Printf.sprintf "<strong>%s %s</strong> <em>%s-%s</em>"
             (sou base t.t_ident) (sou base t.t_place)
             (match Adef.od_of_codate t.t_date_start with
                Some d -> Date.string_of_date conf d
              | _ -> "")
             (match Adef.od_of_codate t.t_date_end with
                Some d -> Date.string_of_date conf d
              | _ -> ""))
  | UndefinedSex p ->
      Wserver.wprint (fcapitale (ftransl conf "undefined sex for %t"))
        (fun _ -> print_someone_strong conf base p)
  | WitnessDateAfterDeath p ->
      Wserver.wprint
        (fcapitale (ftransl conf "%t was witness after his/her death"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (Date.short_dates_text conf base p))
  | WitnessDateBeforeBirth p ->
      Wserver.wprint
        (fcapitale (ftransl conf "%t was witness before his/her birth"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (Date.short_dates_text conf base p))
  | YoungForMarriage (p, a) | OldForMarriage (p, a) ->
      Wserver.wprint "%s\n" (print_someone_strong conf base p);
      Wserver.wprint (ftransl conf "married at age %t")
        (fun _ -> Date.string_of_age conf a)

let print_warnings conf base wl =
  if wl = [] then ()
  else
    begin
      Wserver.wprint "%s\n" (capitale (transl conf "warnings"));
      begin
        Wserver.wprint "<ul>\n";
        (* On rend la liste unique, parce qu'il se peut qu'un warning soit *)
        (* levé par plusieurs fonctions différents selon le context.       *)
        begin let wl =
          let ht = Hashtbl.create 1 in
          let rec loop wl accu =
            match wl with
              [] -> accu
            | x :: wl ->
                if Hashtbl.mem ht (Hashtbl.hash x) then loop wl accu
                else
                  begin
                    Hashtbl.add ht (Hashtbl.hash x) true;
                    loop wl (x :: accu)
                  end
          in
          loop wl []
        in
          List.iter
            (fun w ->
               html_li conf; print_warning conf base w; Wserver.wprint "\n")
            wl
        end;
        Wserver.wprint "</ul>\n"
      end
    end


(* ************************************************************************* *)
(*  [Fonc] print_misc : config -> base -> Def.misc -> unit                   *)
(** [Description] : Fonction d'impression des 'informations diverses'.
    [Args] :
      - conf : configuration
      - base : base
      - fun  : Def.misc (miscellaneous)
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
let print_misc conf base =
  function
    MissingSources ->
      Wserver.wprint "<em>";
      Wserver.wprint "%s\n" (capitale (transl conf "missing sources"));
      Wserver.wprint "</em>"


(* ************************************************************************* *)
(*  [Fonc] print_miscs : config -> base -> Def.misc list -> unit             *)
(** [Description] : Affiche la liste des 'informations diverses'.
    [Args] :
      - conf : configuration
      - base : base
      - ml   : Def.misc list (miscellaneous)
    [Retour] :
      - unit
    [Rem] : Exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
let print_miscs conf base ml =
  if ml = [] then ()
  else
    begin
      Wserver.wprint "%s\n"
        (capitale (transl conf "miscellaneous informations"));
      begin
        Wserver.wprint "<ul>\n";
        List.iter
          (fun m -> html_li conf; print_misc conf base m; Wserver.wprint "\n")
          ml;
        Wserver.wprint "</ul>\n"
      end
    end


(* ************************************************************************* *)
(*  [Fonc] print_miscs :
      config -> base -> (Def.warning list * Def.misc list) -> unit           *)
(** [Description] : Affiche sous la même rubrique, la liste des warnings
                    et la liste des 'informations diverses'.
    [Args] :
      - conf : configuration
      - base : base
      - wl   : Def.warning list
      - ml   : Def.misc list (miscellaneous)
    [Retour] :
      - unit
    [Rem] : Exporté en clair hors de ce module.                              *)
(* ************************************************************************* *)
let print_warnings_and_miscs conf base (wl, ml) =
  if wl = [] && ml = [] then ()
  else
    begin
      Wserver.wprint "%s\n" (capitale (transl conf "warnings"));
      begin
        Wserver.wprint "<ul>\n";
        List.iter
          (fun w ->
             html_li conf; print_warning conf base w; Wserver.wprint "\n")
          wl;
        List.iter
          (fun m -> html_li conf; print_misc conf base m; Wserver.wprint "\n")
          ml;
        Wserver.wprint "</ul>\n"
      end
    end

let error conf base x =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  rheader conf title;
  print_error conf base x;
  Wserver.wprint "\n";
  print_return conf;
  trailer conf;
  raise ModErr

let error_locked conf =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  rheader conf title;
  Wserver.wprint "<p>\n";
  Wserver.wprint
    (fcapitale
       (ftransl conf "the file is temporarily locked: please try again"));
  Wserver.wprint ".\n";
  Wserver.wprint "</p>\n";
  Wserver.wprint "<table>\n";
  Wserver.wprint "<tr>\n";
  Wserver.wprint "<td>\n";
  Wserver.wprint "<form method=\"post\" action=\"%s\">\n" conf.command;
  List.iter
    (fun (x, v) ->
       if x = "retry" then ()
       else if x = "notes" then
         begin
           Wserver.wprint "<textarea style=\"display:none;\" name=\"%s\">\n"
             x;
           Wserver.wprint "%s" (quote_escaped (decode_varenv v));
           Wserver.wprint "</textarea>\n"
         end
       else
         Wserver.wprint "<input type=\"hidden\" name=\"%s\" value=\"%s\"%s>\n"
           x (quote_escaped (decode_varenv v)) conf.xhs)
    (conf.henv @ conf.env);
  (* just to see in the traces... *)
  Wserver.wprint "<input type=\"hidden\" name=\"retry\" value=\"%s\"%s>\n"
    (quote_escaped conf.user) conf.xhs;
  Wserver.wprint "<input type=\"submit\" value=\"%s\"%s>\n"
    (capitale (transl conf "try again")) conf.xhs;
  Wserver.wprint "</form>\n";
  Wserver.wprint "</td>\n";
  Wserver.wprint "<td>\n";
  Wserver.wprint "<form method=\"get\" action=\"%s\">\n" conf.command;
  List.iter
    (fun (x, v) ->
       Wserver.wprint "<input type=\"hidden\" name=\"%s\" value=\"%s\"%s>\n" x
         (quote_escaped (decode_varenv v)) conf.xhs)
    conf.henv;
  begin let ip =
    match p_getenv conf.env "ip" with
      Some ip -> Some ip
    | None -> p_getenv conf.env "i"
  in
    match ip with
      Some n ->
        Wserver.wprint "<input type=\"hidden\" name=\"i\" value=\"%s\"%s>\n" n
          conf.xhs
    | None -> ()
  end;
  Wserver.wprint "<input type=\"submit\" value=\"%s\"%s>\n"
    (capitale (transl_nth conf "user/password/cancel" 2)) conf.xhs;
  Wserver.wprint "</form>\n";
  Wserver.wprint "</td>\n";
  Wserver.wprint "</tr>\n";
  Wserver.wprint "</table>\n";
  trailer conf

let error_digest conf =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  rheader conf title;
  print_link_to_welcome conf true;
  Wserver.wprint "<p>\n";
  Wserver.wprint
    (fcapitale
       (ftransl conf "\
the base has changed; do \"back\", \"reload\", and refill the form"));
  Wserver.wprint ".\n";
  Wserver.wprint "</p>\n";
  trailer conf;
  raise ModErr

let digest_person p = Iovalue.digest p
let digest_family (fam, cpl, des) = Iovalue.digest (fam, cpl, des)

let get var key env =
  match p_getenv env (var ^ "_" ^ key) with
    Some v -> v
  | None -> failwith (var ^ "_" ^ key ^ " unbound")

let get_number var key env = p_getint env (var ^ "_" ^ key)

let bad_date conf d =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  rheader conf title;
  Wserver.wprint "%s:\n" (capitale (transl conf "incorrect date"));
  begin match d with
    {day = 0; month = 0; year = a} -> Wserver.wprint "%d" a
  | {day = 0; month = m; year = a} -> Wserver.wprint "%d/%d" m a
  | {day = j; month = m; year = a} -> Wserver.wprint "%d/%d/%d" j m a
  end;
  trailer conf;
  raise ModErr

let int_of_field s =
  try Some (int_of_string (strip_spaces s)) with Failure _ -> None

let reconstitute_date_dmy conf var =
  let (prec, y) =
    let y = get var "yyyy" conf.env in
    let prec = p_getenv conf.env (var ^ "_prec") in
    let len = String.length y in
    if len > 1 then
      match y.[0], y.[len-1] with
        '?', _ -> Some "maybe", String.sub y 1 (len - 1)
      | '~', _ -> Some "about", String.sub y 1 (len - 1)
      | '/', '/' -> Some "about", String.sub y 1 (len - 2)
      | '<', _ | '/', _ -> Some "before", String.sub y 1 (len - 1)
      | '>', _ -> Some "after", String.sub y 1 (len - 1)
      | _, '/' -> Some "after", String.sub y 0 (len - 1)
      | _ -> prec, y
    else prec, y
  in
  let (force_f_cal, m) =
    let m = get var "mm" conf.env in
    match String.uppercase m with
      "VD" -> true, Some 1
    | "BR" -> true, Some 2
    | "FM" -> true, Some 3
    | "NI" -> true, Some 4
    | "PL" -> true, Some 5
    | "VT" -> true, Some 6
    | "GE" -> true, Some 7
    | "FL" -> true, Some 8
    | "PR" -> true, Some 9
    | "ME" -> true, Some 10
    | "TH" -> true, Some 11
    | "FT" -> true, Some 12
    | "JC" -> true, Some 13
    | _ -> false, int_of_field m
  in
  let d =
    match int_of_field y with
      Some y ->
        let prec =
          match prec with
            Some "about" -> About
          | Some "maybe" -> Maybe
          | Some "before" -> Before
          | Some "after" -> After
          | Some "oryear" ->
              begin match get_number var "oryear" conf.env with
                Some y -> OrYear y
              | None -> Sure
              end
          | Some "yearint" ->
              begin match get_number var "oryear" conf.env with
                Some y -> YearInt y
              | None -> Sure
              end
          | _ -> Sure
        in
        begin match m with
          Some m ->
            begin match get_number var "dd" conf.env with
              Some d ->
                let d =
                  {day = d; month = m; year = y; prec = prec; delta = 0}
                in
                if d.day >= 1 && d.day <= 31 && d.month >= 1 && d.month <= 13
                then
                  Some d
                else bad_date conf d
            | None ->
                let d =
                  {day = 0; month = m; year = y; prec = prec; delta = 0}
                in
                if d.month >= 1 && d.month <= 13 then Some d
                else bad_date conf d
            end
        | None -> Some {day = 0; month = 0; year = y; prec = prec; delta = 0}
        end
    | None -> None
  in
  d, force_f_cal

let check_greg_day conf d =
  if d.day > CheckItem.nb_days_in_month d.month d.year then bad_date conf d

let reconstitute_date conf var =
  match reconstitute_date_dmy conf var with
    Some d, false ->
      let (d, cal) =
        match p_getenv conf.env (var ^ "_cal") with
          Some "G" | None -> check_greg_day conf d; d, Dgregorian
        | Some "J" -> Calendar.gregorian_of_julian d, Djulian
        | Some "F" -> Calendar.gregorian_of_french d, Dfrench
        | Some "H" -> Calendar.gregorian_of_hebrew d, Dhebrew
        | _ -> d, Dgregorian
      in
      Some (Dgreg (d, cal))
  | Some d, true -> Some (Dgreg (Calendar.gregorian_of_french d, Dfrench))
  | None, _ ->
      match p_getenv conf.env (var ^ "_text") with
        Some txt ->
          let txt = only_printable (get var "text" conf.env) in
          if txt = "" then None else Some (Dtext txt)
      | _ -> None

let rec parse_int n (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
    Some ('0'..'9' as i) ->
      Stream.junk strm__;
      parse_int (10 * n + Char.code i - Char.code '0') strm__
  | _ -> n

let parse_r_parent (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
    Some 'f' -> Stream.junk strm__; 0
  | Some 'm' -> Stream.junk strm__; 1
  | _ -> raise Stream.Failure

let text_of_var conf =
  function
    "pa1" -> transl_nth conf "him/her" 0
  | "pa2" -> transl_nth conf "him/her" 1
  | var ->
      let (strm__ : _ Stream.t) = Stream.of_string var in
      match Stream.peek strm__ with
        Some 'r' ->
          Stream.junk strm__;
          let pos =
            try parse_int 0 strm__ with
              Stream.Failure -> raise (Stream.Error "")
          in
          begin match Stream.peek strm__ with
            Some '_' ->
              Stream.junk strm__;
              let pn =
                try parse_r_parent strm__ with
                  Stream.Failure -> raise (Stream.Error "")
              in
              transl_nth conf "relation/relations" 0 ^ " " ^
              string_of_int pos ^ " - " ^ transl_nth conf "father/mother" pn
          | _ -> raise (Stream.Error "")
          end
      | Some 'w' ->
          Stream.junk strm__;
          begin match Stream.peek strm__ with
            Some 'i' ->
              Stream.junk strm__;
              begin match Stream.peek strm__ with
                Some 't' ->
                  Stream.junk strm__;
                  begin match Stream.peek strm__ with
                    Some 'n' ->
                      Stream.junk strm__;
                      let pos =
                        try parse_int 0 strm__ with
                          Stream.Failure -> raise (Stream.Error "")
                      in
                      transl_nth conf "witness/witnesses" 0 ^ " " ^
                      string_of_int pos
                  | _ -> raise (Stream.Error "")
                  end
              | _ -> raise (Stream.Error "")
              end
          | _ -> raise (Stream.Error "")
          end
      | Some 'c' ->
          Stream.junk strm__;
          begin match Stream.peek strm__ with
            Some 'h' ->
              Stream.junk strm__;
              let pos =
                try parse_int 0 strm__ with
                  Stream.Failure -> raise (Stream.Error "")
              in
              Util.translate_eval (transl_nth conf "child/children" 0) ^ " " ^
              string_of_int pos
          | _ -> raise (Stream.Error "")
          end
      | _ -> var

let print_create_conflict conf base p var =
  let text = text_of_var conf var in
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  rheader conf title;
  Wserver.wprint (fcapitale (ftransl conf "name %s already used"))
    ("\"" ^ p_first_name base p ^ "." ^ string_of_int (get_occ p) ^ " " ^
     p_surname base p ^ "\" (" ^ text ^ ")");
  let free_n =
    find_free_occ base (p_first_name base p) (p_surname base p) 0
  in
  Wserver.wprint "<form method=\"post\" action=\"%s\">\n" conf.command;
  List.iter
    (fun (x, v) ->
       (* Seul un textarea peut contenir des sauts de ligne. *)
       (* On remplace donc l'input par un textarea.          *)
       if x = "notes" then
         begin
           Wserver.wprint "<textarea style=\"display:none;\" name=\"%s\">\n"
             x;
           Wserver.wprint "%s" (quote_escaped (decode_varenv v));
           Wserver.wprint "</textarea>\n"
         end
       else
         Wserver.wprint "<input type=\"hidden\" name=\"%s\" value=\"%s\"%s>\n"
           x (quote_escaped (decode_varenv v)) conf.xhs)
    (conf.henv @ conf.env);
  Wserver.wprint "<input type=\"hidden\" name=\"field\" value=\"%s\"%s>\n" var
    conf.xhs;
  Wserver.wprint "<input type=\"hidden\" name=\"free_occ\" value=\"%d\"%s>\n"
    free_n conf.xhs;
  Wserver.wprint "<ul>\n";
  Wserver.wprint "<li>";
  Wserver.wprint "%s: %d. \n" (capitale (transl conf "first free number"))
    free_n;
  Wserver.wprint (fcapitale (ftransl conf "click on \"%s\""))
    (transl conf "create");
  Wserver.wprint "%s." (transl conf " to try again with this number");
  Wserver.wprint "</li>";
  Wserver.wprint "<li>";
  Wserver.wprint "%s " (capitale (transl conf "or"));
  Wserver.wprint (ftransl conf "click on \"%s\"") (transl conf "back");
  Wserver.wprint " %s %s." (transl_nth conf "and" 0)
    (transl conf "change it (the number) yourself");
  Wserver.wprint "</li>";
  Wserver.wprint "<li>";
  Wserver.wprint "%s " (capitale (transl conf "or"));
  Wserver.wprint (ftransl conf "click on \"%s\"") (transl conf "back");
  Wserver.wprint " %s %s." (transl_nth conf "and" 0)
    (transl conf "use \"link\" instead of \"create\"");
  Wserver.wprint "</li>";
  Wserver.wprint "</ul>\n";
  Wserver.wprint "<input type=\"submit\" name=\"create\" value=\"%s\"%s>\n"
    (capitale (transl conf "create")) conf.xhs;
  Wserver.wprint "<input type=\"submit\" name=\"return\" value=\"%s\"%s>\n"
    (capitale (transl conf "back")) conf.xhs;
  Wserver.wprint "</form>\n";
  print_same_name conf base p;
  trailer conf;
  raise ModErr

let add_misc_names_for_new_persons base new_persons =
  List.iter
    (fun p ->
       List.iter (fun n -> person_ht_add base n p.key_index)
         (gen_person_misc_names base p (fun p -> p.titles)))
    new_persons

let insert_person conf base src new_persons (f, s, o, create, var) =
  let f = if f = "" then "?" else f in
  let s = if s = "" then "?" else s in
  match create with
    Create (sex, info) ->
      begin try
        if f = "?" || s = "?" then
          if o <= 0 || o >= nb_of_persons base then raise Not_found
          else
            let ip = Adef.iper_of_int o in
            let p = poi base ip in
            if p_first_name base p = f && p_surname base p = s then ip
            else raise Not_found
        else
          match person_of_key base f s o with
            Some ip -> print_create_conflict conf base (poi base ip) var
          | None -> raise Not_found
      with Not_found ->
        let o = if f = "?" || s = "?" then 0 else o in
        let ip = Adef.iper_of_int (nb_of_persons base) in
        let empty_string = Gwdb.insert_string base "" in
        let (birth, birth_place, baptism, baptism_place) =
          match info with
            Some {ci_birth_date = b; ci_birth_place = bpl} ->
              if String.length bpl >= 2 && String.sub bpl 0 2 = "b/" then
                None, "", b, String.sub bpl 2 (String.length bpl - 2)
              else b, bpl, None, ""
          | None -> None, "", None, ""
        in
        let (death, death_place) =
          match info with
            Some {ci_death_date = Some d; ci_death_place = dpl} ->
              Death (Unspecified, Adef.cdate_of_date d), dpl
          | Some {ci_death_date = None; ci_death_place = dpl}
            when dpl <> "" ->
              DeadDontKnowWhen, dpl
          | Some
              {ci_death = DeadDontKnowWhen | NotDead as dead;
               ci_death_date = None; ci_death_place = dpl} ->
              dead, dpl
          | _ -> infer_death conf birth baptism, ""
        in
        let occupation =
          match info with
            Some {ci_occupation = occupation} -> occupation
          | _ -> ""
        in
        let access =
          match info with
            Some {ci_public = p} -> if p then Public else IfTitles
          | None -> IfTitles
        in
        let p =
          {first_name = Gwdb.insert_string base f;
           surname = Gwdb.insert_string base s; occ = o; image = empty_string;
           first_names_aliases = []; surnames_aliases = [];
           public_name = empty_string; qualifiers = []; aliases = [];
           titles = []; rparents = []; related = [];
           occupation = Gwdb.insert_string base occupation; sex = sex;
           access = access; birth = Adef.codate_of_od birth;
           birth_place = Gwdb.insert_string base birth_place;
           birth_src = empty_string; baptism = Adef.codate_of_od baptism;
           baptism_place = Gwdb.insert_string base baptism_place;
           baptism_src = empty_string; death = death;
           death_place = Gwdb.insert_string base death_place;
           death_src = empty_string; burial = UnknownBurial;
           burial_place = empty_string; burial_src = empty_string;
           notes = empty_string;
           psources =
             if f = "?" || s = "?" then empty_string
             else Gwdb.insert_string base (only_printable src);
           key_index = ip}
        in
        let a = {parents = None; consang = Adef.fix (-1)} in
        let u = {family = [| |]} in
        patch_person base p.key_index p;
        patch_ascend base p.key_index a;
        patch_union base p.key_index u;
        if f <> "?" && s <> "?" then
          begin let fn = Util.translate_eval f in
            let sn = Util.translate_eval s in
            patch_key base ip fn sn o;
            person_ht_add base (fn ^ " " ^ sn) ip;
            new_persons := p :: !new_persons
          end;
        ip
      end
  | Link ->
      if f = "?" || s = "?" then
        if o < 0 || o >= nb_of_persons base then
          print_err_unknown conf base (f, s, o)
        else
          let ip = Adef.iper_of_int o in
          let p = poi base ip in
          if p_first_name base p = f && p_surname base p = s then ip
          else print_err_unknown conf base (f, s, o)
      else
        match person_of_key base f s o with
          Some ip -> ip
        | None -> print_err_unknown conf base (f, s, o)

let rec update_conf_env field p occ o_env n_env =
  let get_name (n, v) = n in
  match o_env with
    [] -> n_env
  | head :: rest ->
      let name = get_name head in
      if name = field ^ "p" then
        update_conf_env field p occ rest ((name, p) :: n_env)
      else if name = field ^ "occ" then
        update_conf_env field p occ rest ((name, occ) :: n_env)
      else if
        name = "link" || name = "create" || name = "free_occ" ||
        name = "field" || name = "link_occ"
      then
        update_conf_env field p occ rest n_env
      else update_conf_env field p occ rest (head :: n_env)

let update_conf_create conf =
  let field =
    match p_getenv conf.env "field" with
      Some f -> f ^ "_"
    | _ -> ""
  in
  let occ =
    match p_getenv conf.env "free_occ" with
      Some occ -> occ
    | _ -> ""
  in
  {conf with env = update_conf_env field "create" occ conf.env []}

let update_conf_link conf =
  let field =
    match p_getenv conf.env "field" with
      Some f -> f ^ "_"
    | _ -> ""
  in
  let occ =
    match p_getenv conf.env "link_occ" with
      Some occ -> occ
    | _ -> ""
  in
  {conf with env = update_conf_env field "link" occ conf.env []}

let update_conf conf =
  match p_getenv conf.env "link" with
    Some _ -> update_conf_link conf
  | None ->
      match p_getenv conf.env "create" with
        Some _ -> update_conf_create conf
      | None -> conf

let rec list_except x =
  function
    y :: l -> if x = y then l else y :: list_except x l
  | [] -> invalid_arg "list_except"

let update_related_pointers base pi ol nl =
  let ol = List.sort compare ol in
  let nl = List.sort compare nl in
  let (added_rel, removed_rel) =
    let rec loop (added_rel, removed_rel) ol nl =
      match ol, nl with
        oip :: orl, nip :: nrl ->
          if oip < nip then loop (added_rel, oip :: removed_rel) orl nl
          else if oip > nip then loop (nip :: added_rel, removed_rel) ol nrl
          else loop (added_rel, removed_rel) orl nrl
      | [], _ -> nl @ added_rel, removed_rel
      | _, [] -> added_rel, ol @ removed_rel
    in
    loop ([], []) ol nl
  in
  List.iter
    (fun ip ->
       let p = gen_person_of_person (poi base ip) in
       patch_person base ip {p with related = pi :: p.related})
    added_rel;
  List.iter
    (fun ip ->
       let p = gen_person_of_person (poi base ip) in
       let related =
         if List.mem pi p.related then list_except pi p.related
         else
           begin
             Printf.eprintf "Warning: related pointer was missing\n";
             flush stderr;
             p.related
           end
       in
       patch_person base ip {p with related = related})
    removed_rel
