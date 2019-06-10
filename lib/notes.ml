(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util
module StrSet = Mutil.StrSet

let file_path conf base fname =
  Util.bpath
    (List.fold_left Filename.concat (conf.bname ^ ".gwb")
       [ base_notes_dir base; fname ^ ".txt" ])

let path_of_fnotes fnotes =
  match NotesLinks.check_file_name fnotes with
  | Some (dl, f) -> List.fold_right Filename.concat dl f
  | None -> ""

let read_notes base fnotes =
  let fnotes = path_of_fnotes fnotes in
  let s = base_notes_read base fnotes in
  Wiki.split_title_and_text s

let merge_possible_aliases conf db =
  let aliases = Wiki.notes_aliases conf in
  let db =
    List.map
      (fun (pg, (sl, il)) ->
        let pg =
          match pg with
          | Def.NLDB.PgMisc f -> Def.NLDB.PgMisc (Wiki.map_notes aliases f)
          | x -> x
        in
        let sl = List.map (Wiki.map_notes aliases) sl in
        (pg, (sl, il)))
      db
  in
  let db = List.sort (fun (pg1, _) (pg2, _) -> compare pg1 pg2) db in
  List.fold_left
    (fun list (pg, (sl, il)) ->
      let sl, _il1, list =
        let list1, list2 =
          match list with
          | ((pg1, _) as x) :: l -> if pg = pg1 then ([ x ], l) else ([], list)
          | [] -> ([], list)
        in
        match list1 with
        | [ (_, (sl1, il1)) ] ->
            let sl =
              List.fold_left
                (fun sl s -> if List.mem s sl then sl else s :: sl)
                sl sl1
            in
            let il =
              List.fold_left
                (fun il i -> if List.mem i il then il else i :: il)
                il il1
            in
            (sl, il, list2)
        | _ -> (sl, il, list)
      in
      (pg, (sl, il)) :: list)
    [] db

let notes_links_db conf base eliminate_unlinked =
  let db = Gwdb.read_nldb base in
  let db = merge_possible_aliases conf db in
  let db2 =
    List.fold_left
      (fun db2 (pg, (sl, _il)) ->
        let record_it =
          let open Def.NLDB in
          match pg with
          | PgInd ip -> pget conf base ip |> authorized_age conf base
          | PgFam ifam ->
              foi base ifam |> get_father |> pget conf base
              |> authorized_age conf base
          | PgNotes | PgMisc _ | PgWizard _ -> true
        in
        if record_it then
          List.fold_left
            (fun db2 s ->
              try
                let list = List.assoc s db2 in
                (s, pg :: list) :: List.remove_assoc s db2
              with Not_found -> (s, [ pg ]) :: db2)
            db2 sl
        else db2)
      [] db
  in
  (* some kind of basic gc... *)
  let misc = Hashtbl.create 1 in
  let set =
    List.fold_left
      (fun set (pg, (sl, _il)) ->
        let open Def.NLDB in
        match pg with
        | PgInd _ | PgFam _ | PgNotes | PgWizard _ ->
            List.fold_left (fun set s -> StrSet.add s set) set sl
        | PgMisc s ->
            Hashtbl.add misc s sl;
            set)
      StrSet.empty db
  in
  let mark = Hashtbl.create 1 in
  (let rec loop = function
     | s :: sl ->
         if Hashtbl.mem mark s then loop sl
         else (
           Hashtbl.add mark s ();
           let sl1 = try Hashtbl.find misc s with Not_found -> [] in
           loop (List.rev_append sl1 sl))
     | [] -> ()
   in
   loop (StrSet.elements set));
  let is_referenced s = Hashtbl.mem mark s in
  let db2 =
    if eliminate_unlinked then
      List.fold_right
        (fun (s, list) db2 -> if is_referenced s then (s, list) :: db2 else db2)
        db2 []
    else db2
  in
  List.sort
    (fun (s1, _) (s2, _) ->
      Gutil.alphabetic_order (Name.lower s1) (Name.lower s2))
    db2

let print_linked_list conf base pgl =
  let typ = p_getenv conf.env "type" in
  Wserver.printf "<ul>\n";
  List.iter
    (fun pg ->
       begin match pg, typ with
         NotesLinks.PgInd ip, None ->
           Wserver.printf "<li>";
           Wserver.printf "<tt>";
           if conf.wizard then
             begin
               Wserver.printf "<a class=\"mx-2\" href=\"%s&i=%d&\">"
                 (commd conf) (Adef.int_of_iper ip);
               Wserver.printf "</sup><i class=\"fa fa-cog\"></i></sup>";
               Wserver.printf "</a>"
             end;
           begin
             let p = pget conf base ip in
             Wserver.printf "<span class=\"mx-2\">";
             Wserver.printf "%s%s"
               (Util.referenced_person_title_text conf base p)
               (Date.short_dates_text conf base p);
             Wserver.printf "</span>"
           end;
           Wserver.printf "</tt>\n";
           Wserver.printf "</li>\n"
       | NotesLinks.PgFam ifam, None ->
           let fam = foi base ifam in
           let fath = pget conf base (get_father fam) in
           let moth = pget conf base (get_mother fam) in
           Wserver.printf "<li>";
           Wserver.printf "<tt>";
           if conf.wizard then
             begin
               Wserver.printf
                 "<a class=\"mx-2\" href=\"%sm=MOD_FAM&i=%d&ip=%d&\">"
                 (commd conf) (Adef.int_of_ifam ifam)
                 (Adef.int_of_iper (Gwdb.get_key_index fath));
               Wserver.printf "</sup><i class=\"fa fa-cog\"></i></sup>";
               Wserver.printf "</a>"
             end;
           Wserver.printf "<span class=\"mx-2\">";
           Wserver.printf "%s%s &amp; %s %s"
             (Util.referenced_person_title_text conf base fath)
             (Date.short_dates_text conf base fath)
             (Util.referenced_person_title_text conf base moth)
             (Date.short_dates_text conf base moth);
           Wserver.printf "</span>";
           Wserver.printf "</tt>\n";
           Wserver.printf "</li>\n"
       | NotesLinks.PgNotes, None ->
           Wserver.printf "<li>";
           Wserver.printf "<tt>";
           if conf.wizard then
             begin
               Wserver.printf "<a class=\"mx-2\" href=\"%sm=MOD_NOTES&\">"
                 (commd conf);
               Wserver.printf "</sup><i class=\"fa fa-cog\"></i></sup>";
               Wserver.printf "</a>"
             end;
           Wserver.printf "<a class=\"mx-2\" href=\"%sm=NOTES\">"
             (commd conf);
           Wserver.printf "%s" (transl_nth conf "note/notes" 1);
           Wserver.printf "</a>\n";
           Wserver.printf "</tt>\n";
           Wserver.printf "</li>\n"
       | NotesLinks.PgMisc fnotes, typ ->
           let (nenv, _) = read_notes base fnotes in
           if match typ with
             | Some t ->
                 let n_type = try List.assoc "TYPE" nenv with Not_found -> "" in
                 t = n_type
             | None -> true
           then begin
           let title = try List.assoc "TITLE" nenv with Not_found -> "" in
           let title = Util.safe_html title in
           Wserver.printf "<li>";
           Wserver.printf "<tt>";
           if conf.wizard then
             begin
               Wserver.printf
                 "<a class=\"mx-2\" href=\"%sm=MOD_NOTES&f=%s&\">"
                 (commd conf) fnotes;
               Wserver.printf "</sup><i class=\"fa fa-cog\"></i></sup>";
               Wserver.printf "</a>"
             end;
           Wserver.printf "<a class=\"mx-2\" href=\"%sm=NOTES&f=%s&\">"
             (commd conf) fnotes;
           Wserver.printf "%s" fnotes;
           Wserver.printf "</a>";
           if title <> "" then Wserver.printf "(%s)" title;
           Wserver.printf "</tt>\n";
           Wserver.printf "</li>\n"
           end
       | NotesLinks.PgWizard wizname, None ->
           Wserver.printf "<li>";
           Wserver.printf "<tt>";
           if conf.wizard then
             begin
               Wserver.printf
                 "<a class=\"mx-2\" href=\"%sm=MOD_WIZNOTES&f=%s&\">"
                 (commd conf) (code_varenv wizname);
               Wserver.printf "</sup><i class=\"fa fa-cog\"></i></sup>";
               Wserver.printf "</a>"
             end;
           Wserver.printf "<a class=\"mx-2\" href=\"%sm=WIZNOTES&v=%s\">"
             (commd conf) (code_varenv wizname);
           Wserver.printf "%s" wizname;
           Wserver.printf "</a>";
           Wserver.printf "<i>";
           Wserver.printf "(%s)"
             (transl_nth conf "wizard/wizards/friend/friends/exterior" 0);
           Wserver.printf "</i>";
           Wserver.printf "</tt>\n";
           Wserver.printf "</li>\n"
       | _ -> ()
       end)
    pgl;
  Wserver.printf "</ul>\n"

let print_what_links conf base fnotes =
  let title h =
    Wserver.printf "%s " (capitale (transl conf "linked pages"));
    if h then Wserver.printf "[%s]" fnotes
    else
      begin
        Wserver.printf "<tt>";
        Wserver.printf "[";
        begin
          Wserver.printf "<a href=\"%sm=NOTES&f=%s\">" (commd conf) fnotes;
          Wserver.printf "%s" fnotes;
          Wserver.printf "</a>"
        end;
        Wserver.printf "]";
        Wserver.printf "</tt>"
      end
  in
  let db = notes_links_db conf base false in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  begin match (try Some (List.assoc fnotes db) with Not_found -> None) with
    Some pgl -> print_linked_list conf base pgl
  | None -> ()
  end;
  Hutil.trailer conf

let print conf base =
  let fnotes =
    match p_getenv conf.env "f" with
      Some f -> if NotesLinks.check_file_name f <> None then f else ""
    | None -> ""
  in
  match p_getenv conf.env "ref" with
    Some "on" -> print_what_links conf base fnotes
  | _ ->
      let (nenv, s) = read_notes base fnotes in
      let templ =
        try
          let fname = "notes_" ^ (List.assoc "TYPE" nenv) in
          Util.open_templ conf fname
        with Not_found -> None
      in
      match templ with
      | Some ic ->
         begin match p_getenv conf.env "ajax" with
         | Some "on" ->
            let charset = if conf.charset = "" then "utf-8" else conf.charset in
            Wserver.header "Content-type: application/json; charset=%s" charset ;
            (* FIXME: safe_html_no_escape replace \" with " for attributes *)
            Wserver.printf "%s" (safe_html_no_escape (string_with_macros conf [] s))
         | _ -> Templ.copy_from_templ conf [] ic
         end
      | None ->
         let title = try List.assoc "TITLE" nenv with Not_found -> "" in
         let title = Util.safe_html title in
         match p_getint conf.env "v" with
         | Some cnt0 -> print_notes_part conf base fnotes title s cnt0
         | None -> print_whole_notes conf base fnotes title s None

let print_mod conf base =
  let fnotes =
    match p_getenv conf.env "f" with
      Some f -> if NotesLinks.check_file_name f <> None then f else ""
    | None -> ""
  in
  let (env, s) = read_notes base fnotes in
  let typ = try List.assoc "TYPE" env with Not_found -> "" in
  let templ =
    let fname = "notes_upd_" ^ typ in
    Util.open_templ conf fname
  in
  let title _ =
    Wserver.printf "%s - %s%s" (capitale (transl conf "base notes"))
      conf.bname (if fnotes = "" then "" else " (" ^ fnotes ^ ")")
  in
  match templ, p_getenv conf.env "notmpl" with
  | Some _, Some "on" ->
     Wiki.print_mod_view_page conf true "NOTES" fnotes title env s
  | Some ic, _ ->
      begin match p_getenv conf.env "ajax" with
      | Some "on" ->
         let s_digest =
           List.fold_left (fun s (k, v) -> s ^ k ^ "=" ^ v ^ "\n") "" env ^ s
         in
         let digest = Iovalue.digest s_digest in
         let charset = if conf.charset = "" then "utf-8" else conf.charset in
         Wserver.header "Content-type: application/json; charset=%s" charset ;
         Wserver.printf "{\"digest\":\"%s\",\"r\":%s}" digest s
      | _ -> Templ.copy_from_templ conf [] ic
      end
  | _ ->
     Wiki.print_mod_view_page conf true "NOTES" fnotes title env s

let update_notes_links_db conf fnotes s =
  let slen = String.length s in
  let list_nt, list_ind =
    let rec loop list_nt list_ind pos i =
      if i = slen then (list_nt, list_ind)
      else if i + 1 < slen && s.[i] = '%' then loop list_nt list_ind pos (i + 2)
      else
        match NotesLinks.misc_notes_link s i with
        | NotesLinks.WLpage (j, _, lfname, _, _) ->
            let list_nt =
              if List.mem lfname list_nt then list_nt else lfname :: list_nt
            in
            loop list_nt list_ind pos j
        | NotesLinks.WLperson (j, key, _, txt) ->
            let list_ind =
              let link = { Def.NLDB.lnTxt = txt; Def.NLDB.lnPos = pos } in
              (key, link) :: list_ind
            in
            loop list_nt list_ind (pos + 1) j
        | NotesLinks.WLwizard (j, _, _) -> loop list_nt list_ind pos j
        | NotesLinks.WLnone -> loop list_nt list_ind pos (i + 1)
    in
    loop [] [] 1 0
  in
  NotesLinks.update_db base fnotes (list_nt, list_ind)

let commit_notes conf base fnotes s =
  let pg = if fnotes = "" then Def.NLDB.PgNotes else Def.NLDB.PgMisc fnotes in
  let fname = path_of_fnotes fnotes in
  let fpath =
    List.fold_left Filename.concat
      (Util.bpath (conf.bname ^ ".gwb"))
      [ base_notes_dir base; fname ]
  in
  Mutil.mkdir_p (Filename.dirname fpath);
  begin try Gwdb.commit_notes base fname s with
    Sys_error m ->
      Printf.eprintf "Sys_error: %s\n" m;
      Hutil.incorrect_request conf; raise Update.ModErr
  end;
  History.record conf base (Def.U_Notes (p_getint conf.env "v", fnotes)) "mn";
  update_notes_links_db base pg s

let wiki_aux pp conf base env str =
  let s = Util.string_with_macros conf env str in
  let lines = pp (Wiki.html_of_tlsw conf s) in
  let wi =
    {
      Wiki.wi_mode = "NOTES";
      Wiki.wi_file_path = file_path conf base;
      Wiki.wi_person_exists = Util.person_exists conf base;
      Wiki.wi_always_show_link = conf.wizard || conf.friend;
    }
  in
  String.concat "\n" lines |> Wiki.syntax_links conf wi |> Util.safe_html

let source conf base str =
  wiki_aux (function [ "<p>"; x; "</p>" ] -> [ x ] | x -> x) conf base [] str

let note conf base env str = wiki_aux (fun x -> x) conf base env str

let person_note conf base p str =
  let env = [ ('i', fun () -> Image.default_portrait_filename base p) ] in
  note conf base env str

let source_note conf base p str =
  let env = [ ('i', fun () -> Image.default_portrait_filename base p) ] in
  wiki_aux (function [ "<p>"; x; "</p>" ] -> [ x ] | x -> x) conf base env str

let source_note_with_env conf base env str =
  wiki_aux (function [ "<p>"; x; "</p>" ] -> [ x ] | x -> x) conf base env str
