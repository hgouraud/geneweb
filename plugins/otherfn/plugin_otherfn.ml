(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Config
open Def
open Gwdb
open Util

module StrSet = Mutil.StrSet

let menu_threshold = 20

let is_number t =
  match t.[0] with
    '1'..'9' -> true
  | _ -> false

let hexa_string s =
  let s' = Bytes.create (2 * String.length s) in
  for i = 0 to String.length s - 1 do
    Bytes.set s' (2 * i) "0123456789ABCDEF".[Char.code s.[i] / 16];
    Bytes.set s' (2 * i + 1) "0123456789ABCDEF".[Char.code s.[i] mod 16]
  done;
  Bytes.unsafe_to_string s'

(* from util.ml, with slight adjustment for <p> *)
let print_alphab_list crit print_elem liste =
  Wserver.printf "<p>\n";
  let len = List.length liste in
  if len > menu_threshold then
    begin
      begin let _ =
        List.fold_left
          (fun last e ->
             let t = crit e in
             let same_than_last =
               match last with
                 Some t1 -> t = t1
               | _ -> false
             in
             if not same_than_last then
               Wserver.printf "<a href=\"#ai%s\">%s</a>\n" (hexa_string t) t;
             Some t)
          None liste
      in
        ()
      end;
    end;
  Wserver.printf "</p>\n<ul>\n";
  begin let _ =
    List.fold_left
      (fun last e ->
         let t = crit e in
         let same_than_last =
           match last with
             Some t1 -> t = t1
           | _ -> false
         in
         if len > menu_threshold || is_number t then
           begin
             begin match last with
               Some _ ->
                 if not same_than_last then Wserver.printf "</ul>\n</li>\n"
             | _ -> ()
             end;
             if not same_than_last then
               begin
                 Wserver.printf "<li>\n";
                 Wserver.printf "<a id=\"ai%s\">%s</a>\n" (hexa_string t) t;
                 Wserver.printf "<ul>\n"
               end
           end;
         Wserver.printf "<li>\n  ";
         print_elem e;
         Wserver.printf "</li>\n";
         Some t)
      None liste
  in
    ()
  end;
  if len > menu_threshold then Wserver.printf "</ul>\n</li>\n";
  Wserver.printf "</ul>\n"

let match_fnames word str x =
  if word then
    let rexp = Str.regexp (".*\\b" ^ x ^ "\\b.*") in
    Str.string_match rexp str 0
  else Mutil.contains str x

type 'a env =
    Vlist_data of (string * (string * int) list) list
  | Vlist_ini of string list
  | Vlist_value of (string * (string * int) list) list
  | Venv_keys of (string * int) list
  | Vint of int
  | Vstring of string
  | Vbool of bool
  | Vother of 'a
  | Vnone

let get_vother =
  function
    Vother x -> Some x
  | _ -> None
let set_vother x = Vother x

(* TODO find a way tu use get_vother, set_vother from templ.camlp5 *)
let buttons_fnames conf =
  Hutil.interp_no_header conf "buttons_fnames"
    {Templ.eval_var = (fun _ -> raise Not_found);
     Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
     Templ.eval_predefined_apply = (fun _ -> raise Not_found);
     Templ.get_vother = get_vother; Templ.set_vother = set_vother;
     Templ.print_foreach = (fun _ -> raise Not_found) }
    [] ()

let print_other_list conf _base list =
  let s_title = Printf.sprintf "%s" (Utf8.capitalize (transl conf "see also")) in
  let s_colon = Printf.sprintf "%s" (transl conf ":") in
  Wserver.printf "<span>%s%s</span>\n" s_title s_colon;
  Mutil.list_iter_first (fun first (fn, c) ->
    Wserver.printf "%s<a href=\"%sm=P&v=%s&other=on\">%s</a> (%d)"
      (if first then "" else ", ") (commd conf) (Mutil.encode fn) fn c)
    list

let other_fnames conf base x =
  match Alln.select_names conf base false "" max_int with
  | (Alln.Result list, _len) ->
    let exact = p_getenv conf.env "t" = Some "A" in
    let word = p_getenv conf.env "word" = Some "on" in
    let x = if exact then x else Name.lower x in
    List.fold_left
      (fun l (_k, str, c) ->
        let strl = if exact then str else Name.lower str in
        if (match_fnames word strl x && strl <> x)
        then (str, c) :: l else l)
      [] list
  | (Alln.Specify _l, _len) -> [] (* TODO is [] ok? *)
  
  
let first_name_print_list conf base x1 xl liste =
  let liste =
    let l =
      List.sort
        (fun x1 x2 ->
           match Gutil.alphabetic (p_surname base x1) (p_surname base x2) with
             0 ->
             begin match
                 Adef.od_of_cdate (get_birth x1),
                 Adef.od_of_cdate (get_birth x2)
               with
               | Some d1, Some d2 -> Date.compare_date d1 d2
               | Some _, _ -> 1
               | _ -> -1
             end
           | n -> -n)
        liste
    in
    List.fold_left
      (fun l x ->
         let px = p_surname base x in
         match l with
           (p, l1) :: l when Gutil.alphabetic px p = 0 -> (p, x :: l1) :: l
         | _ -> (px, [x]) :: l)
      [] l
  in
  let title h =
    if h || p_getenv conf.env "t" = Some "A" then Wserver.print_string x1
    else
      Mutil.list_iter_first
        (fun first x ->
           Wserver.printf "%s<a href=\"%sm=P&v=%s&t=A\">%s</a>"
             (if first then "" else ", ") (commd conf) (Mutil.encode x) x)
        (StrSet.elements xl)
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  buttons_fnames conf;
  (* Si on est dans un calcul de parenté, on affiche *)
  (* l'aide sur la sélection d'un individu.          *)
  Util.print_tips_relationship conf;
  let other = p_getenv conf.env "other" = Some "on" in
  if other then
    let listo =
        List.fold_left (fun l x ->
            (other_fnames conf base x) :: l) [] (StrSet.elements xl)
    in
    let listo = List.flatten listo |> List.sort_uniq compare in
    if listo <> [] then print_other_list conf base listo else ()
  else ();
  let list =
    List.map
      (fun (sn, ipl) ->
         let txt = Util.surname_without_particle base sn ^ Util.surname_particle base sn in
         let ord = Some.name_unaccent txt in ord, txt, ipl)
      liste
  in
  let list = List.sort compare list in
  print_alphab_list (fun (ord, _, _) -> Some.first_char ord)
    (fun (_, txt, ipl) -> Some.print_elem conf base true (txt, ipl)) list;
  Hutil.trailer conf

let rec merge_insert (sstr, (strl, iperl) as x) =
  function
    (sstr1, (strl1, iperl1) as y) :: l ->
      if sstr < sstr1 then x :: y :: l
      else if sstr > sstr1 then y :: merge_insert x l
      else (sstr, (StrSet.union strl strl1, iperl @ iperl1)) :: l
  | [] -> [x]

let persons_of_absolute_first_name =
  Some.persons_of_absolute base_strings_of_first_name persons_of_first_name get_first_name

let persons_of_absolute_surname =
  Some.persons_of_absolute base_strings_of_surname persons_of_surname get_surname


let first_name_print conf base x =
  let (list, _) =
    if p_getenv conf.env "t" = Some "A" then
      persons_of_absolute_first_name conf base x,
      (fun _ -> raise (Match_failure ("src/some.ml", 347, 51)))
    else if x = "" then
      [], (fun _ -> raise (Match_failure ("src/some.ml", 348, 29)))
    else
      Some.persons_of_fsname conf base base_strings_of_first_name
        (spi_find (persons_of_first_name base)) get_first_name x
  in
  let list =
    List.map
      (fun (str, _, iperl) ->
         Name.lower str, (StrSet.add str StrSet.empty, iperl))
      list
  in
  let list = List.fold_right merge_insert list [] in
  (* Construction de la table des sosa de la base *)
  let () = Perso.build_sosa_ht conf base in
  match list with
    [] -> Some.first_name_not_found conf x
  | [_, (strl, iperl)] ->
      let iperl = List.sort_uniq compare iperl in
      let pl = List.map (pget conf base) iperl in
      let pl =
        List.fold_right
          (fun p pl ->
             if not (is_hide_names conf p) || authorized_age conf base p then
               p :: pl
             else pl)
          pl []
      in
      first_name_print_list conf base x strl pl
  | _ -> Some.select_first_name conf x list

let () =
  Gwd_lib.GwdPlugin.register ~ns:"otherfn" [ "P", fun assets conf base ->
      let base = match base with Some b -> b | None -> assert false in
      match p_getenv conf.env "v" with
      | Some v -> first_name_print conf base v; true
      | None -> false
    ]

