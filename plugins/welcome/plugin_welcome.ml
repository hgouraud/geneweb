(* Copyright (c) 1998-2007 INRIA *)

open Geneweb.Config
open Gwdb
open Geneweb.Util
open Gutil
open Geneweb.Hutil
open Geneweb.SearchName
open Geneweb.AdvSearchOk
open Geneweb.DateDisplay
open Geneweb.Some
open Geneweb.Output
open Def

module Gwdb = Gwdb
module Util = Geneweb.Util
module Gutil = Gutil
module Hutil = Geneweb.Hutil
module SearchName = Geneweb.SearchName
module AdvSearchOk = Geneweb.AdvSearchOk
module DateDisplay = Geneweb.DateDisplay
module Output = Geneweb.Output
module Request = Gwd_lib.Request

open Plugin_v7_lib
module Some = Plugin_v7_lib.V7_some


(* from SearchName *)
type search_type =
    Sosa
  | Key
  | Surname
  | FirstName
  | FullName
  | ApproxKey
  | PartialKey
  | DefaultSurname

let full_name conf base an fn sn exact_fn exact_sn max_answers =
  let conf = { conf with 
    env = ("first_name", fn) :: ("surname", sn) :: 
        ("exact_first_name", exact_fn) ::
        ("exact_surname", exact_sn) :: conf.env }
  in
  let (list, len) = AdvSearchOk.advanced_search conf base max_answers in
  let list =
    if len > max_answers then Util.reduce_list max_answers list else list
  in
  if exact_fn <> "on" && exact_sn <> "on" then
    let pl1 = SearchName.search_approx_key conf base an in
    let pl2 = SearchName.search_partial_key conf base an in
    List.sort_uniq compare (List.merge compare (List.merge compare list pl1) pl2)
  else list


let search conf base an search_order unknown =
  let rec loop l =
    match l with
      [] -> unknown conf an
    | Sosa :: l ->
        let pl = SearchName.search_by_sosa conf base an in
        begin match pl with
          [p] ->
            Util.record_visited conf (get_iper p); V7_perso.print conf base p
        | _ -> loop l
        end
    | Key :: l ->
        let pl = SearchName.search_by_key conf base an in
        begin match pl with
          [] -> loop l
        | [p] ->
            Util.record_visited conf (get_iper p); V7_perso.print conf base p
        | pl -> Request.specify ~mode:"Key" conf base an pl
        end
    | Surname :: l ->
        let pl = Some.search_surname conf base an in
        begin match pl with
          [] -> loop l
        | _ ->
            Some.search_surname_print conf base unknown an
        end
    | FirstName :: l ->
        let pl = Some.search_first_name conf base an in
        begin match pl with
          [] -> loop l
        | _ ->
            Some.search_first_name_print conf base an
        end
    | FullName :: l ->
        let get_env_aux f = 
          match p_getenv conf.env f with
          | Some s -> s
          | None -> ""
        in
        let max_answers =
          match p_getint conf.env "max" with
            Some n -> n
          | None -> 100
        in
        let fn = get_env_aux "p" in
        let sn = get_env_aux "n" in
        let exact_fn = get_env_aux "exact_first_name" in
        let exact_sn = get_env_aux "exact_surname" in
        let pl = full_name conf base an fn sn exact_fn exact_sn max_answers in
        let parts = String.split_on_char ' ' fn in (* FIXME depends on fn/sn order *)
        let (fn1, sn1) =
          if List.length parts = 2 then (List.nth parts 0, List.nth parts 1) else (fn, sn)
        in
        let pl1 = if (fn, sn) <> (fn1, sn1) then
          full_name conf base an fn1 sn1 exact_fn exact_sn max_answers
          else []
        in
        let pl = List.sort_uniq compare (List.merge compare pl pl1) in
        let pl = 
          if exact_fn = "on" then List.fold_left (fun list p ->
            if Name.lower fn = Name.lower (p_first_name base p) then p :: list
            else list) [] pl
          else pl
        in
        let pl =
          if exact_sn = "on" then List.fold_left (fun list p ->
            if Name.lower sn = Name.lower (p_surname base p) then p :: list
            else list) [] pl
          else pl
        in
        begin match pl with
        | [] -> loop l
        | [p] ->
                if conf.debug then Printf.eprintf "FullName\n";
                record_visited conf (get_iper p); V7_perso.print conf base p
        | pl -> Request.specify ~mode:"FullName" conf base an pl
        end
    | ApproxKey :: l ->
        let pl = SearchName.search_approx_key conf base an in
        begin match pl with
        | [] -> loop l
        | [p] ->
            if conf.debug then Printf.eprintf "ApproxKey\n";
            record_visited conf (get_iper p); V7_perso.print conf base p
        | pl -> Request.specify ~mode:"ApproxKey" conf base an pl
        end
    | PartialKey :: l ->
        let pl = SearchName.search_partial_key conf base an in
        begin match pl with
        | [] -> loop l
        | [p] ->
            if conf.debug then Printf.eprintf "PartialKey 1\n";
            record_visited conf (get_iper p); V7_perso.print conf base p
        | pl -> Request.specify ~mode:"PartialKey 1" conf base an pl
        end
    | DefaultSurname :: _ ->
        Some.search_surname_print conf base unknown an
  in
  loop search_order
(* end SearchName *)

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
    | Some v -> V7_some.search_first_name_print conf base v ; true
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
      search conf base (fn ^ " " ^ sn) order unknown;
      true
    | Some fn, None ->
      let order =
      [Sosa; Key; FirstName; FullName; ApproxKey; PartialKey; DefaultSurname]
      in
      search conf base fn order unknown;
      true
    | None, Some sn ->
      let order =
      [Sosa; Key; Surname; ApproxKey; PartialKey; DefaultSurname]
      in
      search conf base sn order unknown;
      true
    | None, None -> false
    end
  end

let ns = "welcome"

let _ =
  let aux fn assets conf base =
    Secure.add_assets assets ;
    fn conf base
  in
  Gwd_lib.GwdPlugin.register ~ns
    [ "", aux home
    ; "S", aux s
    ]
