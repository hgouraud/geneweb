(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util

let print_elem conf base _is_surname (iper, _xl) =
  Mutil.list_iter_first
    (fun first p ->
       if not first then Wserver.printf "</li>\n<li>\n  ";
       Perso.print_sosa conf base p true;
         Wserver.printf "\n%s%s<em>"
           (referenced_person_text_flip conf base p)
           (DateDisplay.short_dates_text conf base p) ;
         specify_homonymous conf base p false;
       Wserver.printf "</em>")
    [iper]

let print_result conf base max_answers (list, len) =
  let list =
    if len > max_answers then Util.reduce_list max_answers list else list
  in
  if len = 0 then Wserver.printf "%s\n" (Utf8.capitalize (transl conf "no match"))
  else
    let () = Perso.build_sosa_ht conf base in
    Wserver.printf "<ul>\n";
    let list =
      let rec loop acc l =
        match l with
          [] -> acc
        | p :: l ->
          let sn = Gwdb.get_surname p in
          loop (((surname_without_particle base (Gwdb_driver.sou base sn)), p) :: acc) l
      in
        loop [] list
    in
    let list = List.sort compare list in
    print_alphab_list (fun (ord, _) -> Some.first_char ord)
      (fun (_, p) -> print_elem conf base true (p, [])) list;
    Wserver.printf "</ul>\n"

let print conf base =
  let title _ =
    Wserver.printf "%s" (Utf8.capitalize (transl_nth conf "advanced request" 0))
  in
  let max_answers =
    match p_getint conf.env "max" with
      Some n -> n
    | None -> 100
  in
  Hutil.header conf title;
  Wserver.printf "<p>\n";
  Wserver.printf "%s %s." (Utf8.capitalize (transl conf "searching all"))
    (AdvSearchOk.searching_fields conf base);
  Wserver.printf "</p>\n";
  let list = AdvSearchOk.advanced_search conf base max_answers in
  print_result conf base max_answers list;
  Hutil.trailer conf
