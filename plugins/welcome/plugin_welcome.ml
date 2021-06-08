(* Copyright (c) 1998-2007 INRIA *)

open Geneweb.Config
open Geneweb.Gwdb
open Geneweb.Util
open Geneweb.Gutil
open Geneweb.Hutil
open Geneweb.AdvSearchOk
open Geneweb.DateDisplay
open Geneweb.Output
open Def

module Gwdb = Geneweb.Gwdb
module Util = Geneweb.Util
module Gutil = Geneweb.Gutil
module Hutil = Geneweb.Hutil
module AdvSearchOk = Geneweb.AdvSearchOk
module DateDisplay = Geneweb.DateDisplay
module Output = Geneweb.Output
module Request = Gwd_lib.Request

open Plugin_v7_lib

(* Name.abbrev *)

let abbrev_list =
  ["a", None; "af", None; "d", None; "de", None; "di", None; "dit", None; "ier", Some "i";
   "le", None; "of", None; "saint", Some "st"; "sainte", Some "ste"; "van", None;
   "von", None; "zu", None; "zur", None]

let rec is_word s i p ip =
  if ip = String.length p then
    if i = String.length s then true else if s.[i] = ' ' then true else false
  else if i = String.length s then false
  else if s.[i] = p.[ip] then is_word s (i + 1) p (ip + 1)
  else false

let rec search_abbrev s i =
  function
    (w, a) :: pl ->
      if is_word s i w 0 then Some (String.length w, a)
      else search_abbrev s i pl
  | [] -> None

let abbrev s =
  let rec copy can_start_abbrev i len =
    if i >= String.length s then Buff.get len
    else
      match s.[i] with
        ' ' -> copy true (i + 1) (Buff.store len ' ')
      | c ->
          if can_start_abbrev then
            match search_abbrev s i abbrev_list with
              None -> copy false (i + 1) (Buff.store len c)
            | Some (n, Some a) -> copy false (i + n) (Buff.mstore len a)
            | Some (n, None) -> copy true (i + n + 1) len
          else copy false (i + 1) (Buff.store len c)
  in
  copy true 0 0

(* from SearchName *)

let empty_surname_or_firsntame base p =
  is_empty_string (get_surname p) || is_quest_string (get_surname p) ||
  is_empty_string (get_first_name p) || is_quest_string (get_first_name p) ||
  Name.lower (sou base (get_surname p)) = "" ||
  Name.lower (sou base (get_first_name p)) = ""

let person_is_misc_name conf base p k =
  let k = Name.strip_lower k in
  let _ = Printf.eprintf "K: %s" k in
  List.iter (fun n -> Printf.eprintf "Misc: %s" n)
     (person_misc_names base p (nobtit conf base));
  if List.exists (fun n -> Name.strip n = k)
       (person_misc_names base p (nobtit conf base))
  then
    true
  else false

let person_is_approx_key base p k =
  let _ = Printf.eprintf "Person is approx key: %s %s\n" 
    (Gutil.designation base p) k 
  in
  let k = Name.strip_lower k in
  let fn = Name.strip_lower (p_first_name base p) in
  let sn = Name.strip_lower (p_surname base p) in
  if k = fn ^ sn && fn <> "" && sn <> "" then true else false

let select_approx_key conf base pl k =
  let _ = Printf.eprintf "Select approx key: %s, %d\n" k (List.length pl)in
  List.fold_right
    (fun p pl ->
       if person_is_approx_key base p k then p :: pl
       else if person_is_misc_name conf base p k then p :: pl
       else pl)
    pl []

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
  let _ = Printf.eprintf "Try_find_with_one_first_name: %s\n" n in
  let n1 = n in
  match String.index_opt n1 ' ' with
    Some i ->
      let fn = abbrev (Name.lower (String.sub n1 0 i)) in
      let sn = abbrev (Name.lower (String.sub n1 (i + 1) (String.length n1 - i - 1))) in
      let _ = Printf.eprintf "***1 First_name: %s, Surname: %s\n" fn sn in
      let (list, _) =
        V7_some.persons_of_fsname conf base base_strings_of_surname
          (spi_find (persons_of_surname base)) get_surname sn
      in
      let pl =
      List.fold_left
        (fun pl (_, _, ipl) ->
           List.fold_left
             (fun pl ip ->
                let p = pget conf base ip in
                let fn1 =
                  abbrev (Name.lower (sou base (get_first_name p)))
                  ^ " " ^
                  abbrev (Name.lower (sou base (get_public_name p)))
                in
      					let _ = Printf.eprintf "First_names: %s\n" fn1 in
                if List.mem fn (cut_words fn1) then p :: pl else pl
                )
             pl ipl)
        [] list
      in
      let _ = Printf.eprintf "Pl: %d\n" (List.length pl) in
      if pl = [] then
				begin match String.rindex_opt n1 ' ' with
				|	Some i ->
						let fn = abbrev (Name.lower (String.sub n1 0 i)) in
						let sn = abbrev (Name.lower (String.sub n1 (i + 1) (String.length n1 - i - 1))) in
						let _ = Printf.eprintf "***2 First_name: %s, Surname: %s\n" fn sn in
						let (list, _) =
							V7_some.persons_of_fsname conf base base_strings_of_surname
								(spi_find (persons_of_surname base)) get_surname sn
						in
						List.fold_left
							(fun pl (_, _, ipl) ->
								 List.fold_left
									 (fun pl ip ->
											let p = pget conf base ip in
											let fn1 =
												abbrev (Name.lower (sou base (get_first_name p)))
												^ " " ^
												abbrev (Name.lower (sou base (get_public_name p)))
											in
											let _ = Printf.eprintf "First_names: %s\n" fn1 in
											if List.mem fn (cut_words fn1) then p :: pl else pl
											)
									 pl ipl)
							[] list
					| None -> []
				end
			else pl
  | None -> []

let compact_list base xl =
  let pl = Gutil.sort_person_list base xl in
  List.fold_right
    (fun p pl ->
       match pl with
         p1 :: _ when get_iper p = get_iper p1 -> pl
       | _ -> p :: pl)
    pl []

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

(* search functions *)

let search_by_sosa conf base an =
  let sosa_ref = Util.find_sosa_ref conf base in
  let sosa_nb = try Some (Sosa.of_string an) with _ -> None in
  match sosa_ref, sosa_nb with
    Some p, Some n ->
      if n <> Sosa.zero then
        match Util.branch_of_sosa conf base n (pget conf base @@ get_iper p) with
          Some (p :: _) -> [p]
        | _ -> []
      else []
  | _ -> []

let search_partial_key conf base an =
  let _ = Printf.eprintf "Search partial key: %s\n" an in
  let ipl = Gutil.person_not_a_key_find_all base an in
  let (an, ipl) =
    if ipl = [] then
      match name_with_roman_number an with
        Some an1 ->
          let ipl = Gutil.person_ht_find_all base an1 in
          if ipl = [] then an, [] else an1, ipl
      | None -> an, ipl
    else an, ipl
  in
  let pl =
    List.fold_left
      (fun l ip ->
         let p = pget conf base ip in if is_hidden p then l else p :: l)
      [] ipl
  in
  (*
  let pl =
    if pl = [] then try_find_with_one_first_name conf base an else pl
  in
  *)
  let _ = Printf.eprintf "Pl0: %d\n" (List.length pl) in
  let pl =
    let pl1 = try_find_with_one_first_name conf base an in
    let _ = Printf.eprintf "Pl1: %d\n" (List.length pl1) in
    if pl1 = [] then pl else pl1 @ pl
  in
  let pl =
    if not conf.wizard && not conf.friend then
      List.fold_right
        (fun p pl ->
           if not (is_hide_names conf p) || Util.authorized_age conf base p
           then
             p :: pl
           else pl)
        pl []
    else pl
  in
  compact_list base pl

let search_approx_key conf base an =
  let _ = Printf.eprintf "Search approx key: %s\n" an in
  let ipl = Gutil.person_not_a_key_find_all base an in
  let (an, ipl) =
    if ipl = [] then
      match name_with_roman_number an with
        Some an1 ->
          let ipl = Gutil.person_ht_find_all base an1 in
          if ipl = [] then an, [] else an1, ipl
      | None -> an, ipl
    else an, ipl
  in
  let pl =
    List.fold_left
      (fun l ip ->
         let p = pget conf base ip in if is_hidden p then l else p :: l)
      [] ipl
  in
  let pl = select_approx_key conf base pl an in
  let pl =
    if not conf.wizard && not conf.friend then
      List.fold_right
        (fun p pl ->
           if not (is_hide_names conf p) || Util.authorized_age conf base p
           then
             p :: pl
           else pl)
        pl []
    else pl
  in
  let pl =
    List.fold_right
      (fun p pl -> if empty_surname_or_firsntame base p then pl else p :: pl)
      pl []
  in
  compact_list base pl

(* recherche par clé, i.e. prenom.occ nom *)
let search_by_key conf base an =
  match Gutil.person_of_string_key base an with
    Some ip ->
      let pl = let p = pget conf base ip in if is_hidden p then [] else [p] in
      if not conf.wizard && not conf.friend then
        List.fold_right
          (fun p pl ->
             if not (is_hide_names conf p) || Util.authorized_age conf base p
             then
               p :: pl
             else pl)
          pl []
      else pl
  | None -> []

type search_type =
    Sosa
  | Key
  | Surname
  | FirstName
  | FullName
  | ApproxKey
  | PartialKey
  | DefaultSurname

let search conf base an search_order specify unknown =
  let rec loop l =
    match l with
      [] -> unknown conf an
    | Sosa :: l ->
        let _ = Printf.eprintf "Sosa\n" in
        let pl = search_by_sosa conf base an in
        begin match pl with
          [p] ->
            Util.record_visited conf (get_iper p); V7_perso.print conf base p
        | _ -> loop l
        end
    | Key :: l ->
        let _ = Printf.eprintf "Key\n" in
        let pl = search_by_key conf base an in
        begin match pl with
          [] -> loop l
        | [p] ->
            Util.record_visited conf (get_iper p); V7_perso.print conf base p
        | pl -> specify conf base an pl
        end
    | Surname :: l ->
        let _ = Printf.eprintf "Surname\n" in
        let pl = V7_some.search_surname conf base an in
        begin match pl with
          [] -> loop l
        | _ ->
            V7_some.search_surname_print conf base unknown an
        end
    | FirstName :: l ->
        let _ = Printf.eprintf "FirstName\n" in
        let pl = V7_some.search_first_name conf base an in
        begin match pl with
          [] -> loop l
        | _ ->
            V7_some.search_first_name_print conf base an
        end
    | FullName :: l ->
        let _ = Printf.eprintf "FullName\n" in
        let max_answers =
          match p_getint conf.env "max" with
            Some n -> n
          | None -> 100
        in
        let fn = match p_getenv conf.env "p" with
          | Some fn -> fn
          | None -> ""
        in
        let sn = match p_getenv conf.env "n" with
          | Some sn -> sn
          | None -> ""
        in
        let conf = { conf with 
          env = ("first_name", fn) :: ("surname", sn) :: conf.env }
        in
        let (list, len) = AdvSearchOk.advanced_search conf base max_answers in
        let list =
          if len > max_answers then Util.reduce_list max_answers list else list
        in
        begin match list with
        | [] -> (* try again without sn *)
          begin
             let list = search_approx_key conf base fn in
            if list = [] then loop l
            else
              begin
                let list =
                  List.fold_left (fun list p ->
                    if Name.lower sn = Name.lower (p_surname base p) then p :: list
                    else list) [] list
                in
                begin match list with
                | [] -> loop l
                | [p] ->
                    record_visited conf (get_iper p); V7_perso.print conf base p
                | pl -> specify conf base an pl
                end
              end
          end
        | [p] ->
            record_visited conf (get_iper p); V7_perso.print conf base p
        | pl -> specify conf base an pl
        end        
    | ApproxKey :: l ->
        let _ = Printf.eprintf "ApproxKey\n" in
        let pl = search_approx_key conf base an in
        begin match pl with
          [] -> loop l
        | [p] ->
            record_visited conf (get_iper p); V7_perso.print conf base p
        | pl -> specify conf base an pl
        end
    | PartialKey :: l ->
        let _ = Printf.eprintf "PartialKey\n" in
        let pl = search_partial_key conf base an in
        begin match pl with
          [] -> loop l
        | [p] ->
            record_visited conf (get_iper p); V7_perso.print conf base p
        | pl -> specify conf base an pl
        end
    | DefaultSurname :: _ ->
        let _ = Printf.eprintf "DefaultSurname\n" in
        V7_some.search_surname_print conf base unknown an
  in
  loop search_order
(* end SearchName *)

(* from request.ml *)
let specify conf base n pl =
  let title _ = Output.printf conf "%s : %s" n (transl conf "specify") in
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
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  (* Si on est dans un calcul de parenté, on affiche *)
  (* l'aide sur la sélection d'un individu.          *)
  Util.print_tips_relationship conf;
  Output.print_string conf "<ul>\n";
  (* Construction de la table des sosa de la base *)
  let () = V7_sosa.build_sosa_ht conf base in
  List.iter
    (fun (p, tl) ->
       Output.print_string conf "<li>\n";
       V7_sosa.print_sosa conf base p true;
       begin match tl with
           [] ->
           Output.printf conf "\n%s" (referenced_person_title_text conf base p)
         | t :: _ ->
           Output.printf conf "<a href=\"%s%s\">\n" (commd conf)
             (acces conf base p);
           Output.print_string conf (titled_person_text conf base p t);
           Output.print_string conf "</a>\n";
           List.iter
             (fun t -> Output.print_string conf (one_title_text base t)) tl
       end;
       Output.print_string conf (DateDisplay.short_dates_text conf base p);
       if authorized_age conf base p then
         begin match get_first_names_aliases p with
             [] -> ()
           | fnal ->
             Output.print_string conf "\n<em>(";
             Mutil.list_iter_first
               (fun first fna ->
                  if not first then Output.print_string conf ", ";
                  Output.print_string conf (sou base fna))
               fnal;
             Output.print_string conf ")</em>"
         end;
       begin let spouses =
               Array.fold_right
                 (fun ifam spouses ->
                    let cpl = foi base ifam in
                    let spouse = pget conf base (Gutil.spouse (get_iper p) cpl) in
                    if p_surname base spouse <> "?" then spouse :: spouses
                    else spouses)
                 (get_family p) []
         in
         match spouses with
           [] -> ()
         | h :: hl ->
           let s =
             List.fold_left
               (fun s h -> s ^ ",\n" ^ person_title_text conf base h)
               (person_title_text conf base h) hl
           in
           Output.printf conf ", <em>&amp; %s</em>\n" s
       end;
       Output.print_string conf "</li>\n")
    ptll;
  Output.print_string conf "</ul>\n";
  Hutil.trailer conf

let unknown = begin fun conf n ->
      let title _ =
        Output.printf conf "%s: \"%s\"" (Utf8.capitalize_fst (transl conf "not found"))
          (Util.escape_html n)
      in
      Output.status conf Def.Not_Found;
      Hutil.rheader conf title;
      Hutil.print_link_to_welcome conf false;
      Hutil.trailer conf
    end
(* end from request.ml *)

let w_base =
  Request.w_base
    ~none:(fun c -> Gwd_lib.Request.incorrect_request c ; true)
let w_person =
  Request.w_person
    ~none:(fun c b -> Gwd_lib.Request.very_unknown c b ; true)

let home conf base : bool = false

let p =
  w_base begin fun conf base -> match Util.p_getenv conf.env "v" with
    | Some v -> V7_some.first_name_print conf base v ; true
    | None -> false
  end

let s =
  w_base begin fun conf base ->
    let real_input label =
      match p_getenv conf.env label with
        Some s -> if s = "" then None else Some s
      | None -> None
    in
    begin match real_input "p", real_input "n" with
    Some fn, Some sn ->
      let order = [FullName] in
      search conf base (fn ^ " " ^ sn) order specify unknown;
      true
    | Some fn, None ->
      let order =
      [Sosa; Key; FirstName; PartialKey; ApproxKey; DefaultSurname]
      in
      search conf base fn order specify unknown;
      true
    | None, Some sn ->
      let order =
      [Sosa; Key; Surname; PartialKey; ApproxKey; DefaultSurname]
      in
      search conf base sn order specify unknown;
      true
    | None, None -> false
    end
  end

let ns = "welcome"

let _ =
  let aux fn assets conf base =
    Util.add_lang_path assets ;
    fn conf base
  in
  Gwd_lib.GwdPlugin.register ~ns
    [ "", aux home
    ; "S", aux s
    ]
