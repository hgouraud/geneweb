(* camlp5r ./pa_html.cmo *)
(* $Id: merge.ml,v 5.13 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gutil
open Gwdb
open Hutil
open Util

let print_someone conf base p =
  Wserver.wprint "%s%s %s" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))
    (p_surname base p)

let print conf base p =
  let title h =
    Wserver.wprint "%s" (capitale (transl_decline conf "merge" ""));
    if h then () else begin Wserver.wprint ": "; print_someone conf base p end
  in
  let list = Gutil.find_same_name base p in
  let list =
    List.fold_right
      (fun p1 pl ->
         if get_key_index p1 = get_key_index p then pl else p1 :: pl)
      list []
  in
  Perso.interp_notempl_with_menu title "perso_header" conf base p;
  Wserver.wprint "<h2>\n";
  title false;
  Wserver.wprint "</h2>\n";
  Wserver.wprint "\n";
  Wserver.wprint "<form method=\"get\" action=\"%s\">\n" conf.command;
  Wserver.wprint "<p>\n";
  Util.hidden_env conf;
  Wserver.wprint "<input type=\"hidden\" name=\"m\" value=\"MRG_IND\"%s>\n"
    conf.xhs;
  Wserver.wprint "<input type=\"hidden\" name=\"i\" value=\"%d\"%s>\n"
    (Adef.int_of_iper (get_key_index p)) conf.xhs;
  Wserver.wprint "%s " (capitale (transl_decline conf "with" ""));
  if list <> [] then
    begin
      Wserver.wprint ":";
      Wserver.wprint "<br%s>\n" conf.xhs;
      Wserver.wprint "<input \
type=\"radio\" name=\"select\" value=\"input\" checked=\"checked\"%s>\n"
        conf.xhs
    end;
  Wserver.wprint "(%s . %s %s):\n"
    (transl_nth conf "first name/first names" 0) (transl conf "number")
    (transl_nth conf "surname/surnames" 0);
  Wserver.wprint "<input name=\"n\" size=\"30\" maxlength=\"200\"%s>\n"
    conf.xhs;
  Wserver.wprint "<br%s>\n" conf.xhs;
  Wserver.wprint "</p>\n";
  if list <> [] then
    Wserver.wprint
      "<table border=\"0\" cellspacing=\"0\" cellpadding=\"0\">\n";
  List.iter
    (fun p ->
       Wserver.wprint "<tr align=\"%s\">\n" conf.left;
       Wserver.wprint "<td valign=\"top\">\n";
       Wserver.wprint
         "<input type=\"radio\" name=\"select\" value=\"%d\"%s>\n"
         (Adef.int_of_iper (get_key_index p)) conf.xhs;
       Wserver.wprint "</td>\n";
       Wserver.wprint "<td>\n";
       Wserver.wprint "<a href=\"%s%s\">" (commd conf) (acces conf base p);
       Wserver.wprint "%s.%d %s" (sou base (get_first_name p)) (get_occ p)
         (sou base (get_surname p));
       Wserver.wprint "</a>";
       Wserver.wprint "%s" (Date.short_dates_text conf base p);
       begin match main_title conf base p with
         Some t -> Wserver.wprint "%s" (one_title_text conf base p t)
       | None -> ()
       end;
       begin match get_parents p with
         Some ifam ->
           let cpl = foi base ifam in
           Wserver.wprint ",\n%s"
             (Util.translate_eval
                (transl_a_of_b conf
                   (transl_nth conf "son/daughter/child"
                      (index_of_sex (get_sex p)))
                   (person_title_text conf base (poi base (get_father cpl)) ^
                    " " ^ transl_nth conf "and" 0 ^ " " ^
                    person_title_text conf base (poi base (get_mother cpl)))))
       | None -> ()
       end;
       Wserver.wprint "<br%s>\n" conf.xhs;
       Wserver.wprint "</td>\n";
       Wserver.wprint "</tr>\n")
    list;
  if list <> [] then Wserver.wprint "</table>\n";
  Wserver.wprint "<p>\n";
  Wserver.wprint "<input type=\"submit\" value=\"Ok\"%s>\n" conf.xhs;
  Wserver.wprint "</p>\n";
  Wserver.wprint "</form>\n";
  trailer conf

let print_possible_continue_merging conf base =
  match p_getint conf.env "ini1", p_getint conf.env "ini2" with
    Some ini1, Some ini2 ->
      let p1 = poi base (Adef.iper_of_int ini1) in
      let p2 = poi base (Adef.iper_of_int ini2) in
      Wserver.wprint "\n";
      html_p conf;
      Wserver.wprint "<a href=%sm=MRG_IND;i=%d;i2=%d>" (commd conf) ini1 ini2;
      Wserver.wprint "%s" (capitale (transl conf "continue merging"));
      Wserver.wprint "</a>";
      Wserver.wprint "\n";
      print_someone conf base p1;
      Wserver.wprint "\n%s\n" (transl_nth conf "and" 0);
      print_someone conf base p2;
      Wserver.wprint "\n"
  | _ ->
      match p_getint conf.env "ip" with
        Some ip ->
          let s1 =
            match p_getenv conf.env "iexcl" with
              Some "" | None -> ""
            | Some s -> ";iexcl=" ^ s
          in
          let s2 =
            match p_getenv conf.env "fexcl" with
              Some "" | None -> ""
            | Some s -> ";fexcl=" ^ s
          in
          if s1 <> "" || s2 <> "" then
            begin
              Wserver.wprint "<p>\n";
              begin
                Wserver.wprint "<a href=%sm=MRG_DUP;ip=%d%s%s>" (commd conf)
                  ip s1 s2;
                Wserver.wprint "%s"
                  (capitale (transl conf "continue merging"));
                Wserver.wprint "</a>"
              end;
              Wserver.wprint "\n(%s)\n"
                (transl_a_of_b conf (transl conf "possible duplications")
                   (referenced_person_text conf base
                      (poi base (Adef.iper_of_int ip))));
              Wserver.wprint "</p>\n"
            end
      | None -> ()
