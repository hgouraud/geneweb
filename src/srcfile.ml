(* camlp5r ./pa_lock.cmo ./pa_html.cmo pa_extend.cmo *)
(* $Id: srcfile.ml,v 5.41 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open TemplAst
open Util

type counter =
  { mutable welcome_cnt : int;
    mutable request_cnt : int;
    start_date : string;
    mutable wizard_cnt : int;
    mutable friend_cnt : int;
    mutable normal_cnt : int }

let get_date conf =
  Printf.sprintf "%02d/%02d/%d" conf.today.day conf.today.month
    conf.today.year

(*
   On cherche le fichier dans cet ordre :
    - dans la base (bases/bname/cnt/)
    - dans le répertoire des bases (bases/)
*)
let find_cnt_file conf =
  let bases_cnt = Filename.concat !(Util.cnt_dir) "cnt" in
  let bname_cnt =
    List.fold_left Filename.concat !(Util.cnt_dir) [conf.bname; "cnt"]
  in
  if Sys.file_exists bname_cnt then bname_cnt else bases_cnt

let adm_file f = List.fold_right Filename.concat [!(Util.cnt_dir); "cnt"] f

let cnt conf ext = adm_file (conf.bname ^ ext)

let input_int ic =
  try int_of_string (input_line ic) with End_of_file | Failure _ -> 0

let count conf =
  let fname = cnt conf ".txt" in
  try
    let ic = Secure.open_in fname in
    let rd =
      try
        let wc = int_of_string (input_line ic) in
        let rc = int_of_string (input_line ic) in
        let d = input_line ic in
        let wzc = input_int ic in
        let frc = input_int ic in
        let nrc = input_int ic in
        {welcome_cnt = wc; request_cnt = rc; start_date = d; wizard_cnt = wzc;
         friend_cnt = frc; normal_cnt = nrc}
      with _ ->
        {welcome_cnt = 0; request_cnt = 0; start_date = get_date conf;
         wizard_cnt = 0; friend_cnt = 0; normal_cnt = 0}
    in
    close_in ic; rd
  with _ ->
    {welcome_cnt = 0; request_cnt = 0; start_date = get_date conf;
     wizard_cnt = 0; friend_cnt = 0; normal_cnt = 0}

let write_counter conf r =
  let fname = cnt conf ".txt" in
  try
    let oc = Secure.open_out_bin fname in
    output_string oc (string_of_int r.welcome_cnt);
    output_string oc "\n";
    output_string oc (string_of_int r.request_cnt);
    output_string oc "\n";
    output_string oc r.start_date;
    output_string oc "\n";
    output_string oc (string_of_int r.wizard_cnt);
    output_string oc "\n";
    output_string oc (string_of_int r.friend_cnt);
    output_string oc "\n";
    output_string oc (string_of_int r.normal_cnt);
    output_string oc "\n";
    close_out oc
  with _ -> ()

let set_wizard_and_friend_traces conf =
  if conf.wizard && conf.user <> "" then
    let wpf =
      try List.assoc "wizard_passwd_file" conf.base_env with Not_found -> ""
    in
    (if wpf <> "" then
       let fname = adm_file (conf.bname ^ "_w.txt") in
       update_wf_trace conf fname)
  else if conf.friend && not conf.just_friend_wizard && conf.user <> "" then
    let fpf =
      try List.assoc "friend_passwd_file" conf.base_env with Not_found -> ""
    in
    let fp =
      try List.assoc "friend_passwd" conf.base_env with Not_found -> ""
    in
    if fpf <> "" &&
       is_that_user_and_password conf.auth_scheme conf.user fp = false
    then
      let fname = adm_file (conf.bname ^ "_f.txt") in
      update_wf_trace conf fname

let incr_welcome_counter conf =
  let lname = cnt conf ".lck" in
  match
    Lock.control lname true
      (fun () ->
         let r = count conf in
         r.welcome_cnt <- r.welcome_cnt + 1;
         if conf.wizard then r.wizard_cnt <- r.wizard_cnt + 1
         else if conf.friend then r.friend_cnt <- r.friend_cnt + 1
         else r.normal_cnt <- r.normal_cnt + 1;
         write_counter conf r;
         set_wizard_and_friend_traces conf;
         Some (r.welcome_cnt, r.request_cnt, r.start_date))
  with
    Some x -> x
  | None -> None

let incr_request_counter conf =
  let lname = cnt conf ".lck" in
  match
    Lock.control lname true
      (fun () ->
         let r = count conf in
         r.request_cnt <- r.request_cnt + 1;
         if conf.wizard then r.wizard_cnt <- r.wizard_cnt + 1
         else if conf.friend then r.friend_cnt <- r.friend_cnt + 1
         else r.normal_cnt <- r.normal_cnt + 1;
         write_counter conf r;
         set_wizard_and_friend_traces conf;
         Some (r.welcome_cnt, r.request_cnt, r.start_date))
  with
    Some x -> x
  | None -> None

let lang_file_name conf fname =
  let fname1 =
    Util.base_path ["lang"; conf.lang] (Filename.basename fname ^ ".txt")
  in
  if Sys.file_exists fname1 then fname1
  else
    search_in_lang_path
      (Filename.concat conf.lang (Filename.basename fname ^ ".txt"))

let any_lang_file_name fname =
  let fname1 = Util.base_path ["lang"] (Filename.basename fname ^ ".txt") in
  if Sys.file_exists fname1 then fname1
  else
    search_in_lang_path
      (Filename.concat "lang" (Filename.basename fname ^ ".txt"))

let source_file_name conf fname =
  let bname = conf.bname in
  let lang = conf.lang in
  let fname1 =
    List.fold_right Filename.concat [Util.base_path ["src"] bname; lang]
      (Filename.basename fname ^ ".txt")
  in
  if Sys.file_exists fname1 then fname1
  else
    Filename.concat (Util.base_path ["src"] bname)
      (Filename.basename fname ^ ".txt")

let digit =
  function
    '0'..'9' as c -> Char.code c - Char.code '0'
  | _ -> failwith "digit"


(* Parser for %d/%d/%d -> (d,m,y): useless to translate *)
module G =
  Grammar.GMake
    (struct type te = string * string let lexer = Plexer.gmake () end)
let date = G.Entry.create "date"
let _ =
  G.safe_extend (date : 'date G.Entry.e) None
    [None, None,
     [G.production
        (G.r_next
           (G.r_next
              (G.r_next
                 (G.r_next
                    (G.r_next (G.r_next G.r_stop (G.s_token ("INT", "")))
                       (G.s_token ("", "/")))
                    (G.s_token ("INT", "")))
                 (G.s_token ("", "/")))
              (G.s_token ("INT", "")))
           (G.s_token ("EOI", "")),
         (fun _ (y : string) _ (m : string) _ (d : string) (loc : Ploc.t) ->
            (int_of_string d, int_of_string m, int_of_string y : 'date)))]]

let extract_date d =
  try Some (G.Entry.parse date (G.parsable (Stream.of_string d))) with
    Ploc.Exc (_, (Stream.Error _ | Token.Error _)) -> None

let string_of_start_date conf =
  let r = count conf in
  match extract_date r.start_date with
    Some (d, m, y) ->
      let d =
        Dgreg
          ({day = d; month = m; year = y; prec = Sure; delta = 0}, Dgregorian)
      in
      Util.translate_eval (Date.string_of_date conf d)
  | _ -> r.start_date

let string_of_num sep num =
  let len = ref 0 in
  Num.print (fun x -> len := Buff.mstore !len x) sep num; Buff.get !len

let macro conf base =
  function
    'a' ->
      begin match Util.find_sosa_ref conf base with
        Some p -> referenced_person_title_text conf base p
      | None -> ""
      end
  | 'b' ->
      let s =
        try " dir=\"" ^ Hashtbl.find conf.lexicon " !dir" ^ "\"" with
          Not_found -> ""
      in
      s ^ body_prop conf
  | 'c' ->
      let r = count conf in
      string_of_num (transl conf "(thousand separator)")
        (Num.of_int r.welcome_cnt)
  | 'd' -> string_of_start_date conf
  | 'D' -> (count conf).start_date
  | 'e' -> conf.charset
  | 'f' -> conf.command
  | 'g' ->
      conf.command ^ "?" ^ (if conf.cgi then "b=" ^ conf.bname ^ ";" else "")
  | 'i' -> conf.highlight
  | 'k' -> conf.indep_command
  | 'l' -> conf.lang
  | 'L' -> conf.left
  | 'm' ->
      begin try
        let s = List.assoc "latest_event" conf.base_env in
        if s = "" then "20" else s
      with Not_found -> "20"
      end
  | 'n' ->
      string_of_num (transl conf "(thousand separator)")
        (Num.of_int (nb_of_persons base))
  | 'N' ->
      let s = base_notes_read_first_line base "" in
      let len = String.length s in
      if len > 9 && String.sub s 0 5 = "<!-- " &&
         String.sub s (len - 4) 4 = " -->"
      then
        " : " ^ String.sub s 5 (String.length s - 9)
      else ""
  | 'o' -> image_prefix conf
  | 'q' ->
      let r = count conf in
      string_of_num (transl conf "(thousand separator)")
        (Num.of_int (r.welcome_cnt + r.request_cnt))
  | 'R' -> conf.right
  | 's' -> commd conf
  | 't' -> conf.bname
  | 'T' -> Util.doctype conf
  | 'U' ->
      if (conf.wizard || conf.just_friend_wizard) && conf.user <> "" then
        ": " ^ conf.user
      else ""
  | 'v' -> Version.txt
  | 'w' ->
      let s = Hutil.link_to_referer conf in if s = "" then "&nbsp;" else s
  | '/' -> conf.xhs
  | c -> "%" ^ String.make 1 c

module Lbuff = Buff.Make (struct  end)

let rec lexicon_translate conf base nomin strm first_c =
  let (upp, s) =
    let rec loop len c =
      if c = ']' then
        let s = Lbuff.get len in
        if len > 0 && s.[0] = '*' then true, String.sub s 1 (len - 1)
        else false, s
      else loop (Lbuff.store len c) (Stream.next strm)
    in
    loop 0 first_c
  in
  let (n, c) =
    match Stream.next strm with
      '0'..'9' as c -> Char.code c - Char.code '0', ""
    | c -> 0, String.make 1 c
  in
  let r =
    if c = "[" then
      Util.transl_decline conf s
        (lexicon_translate conf base false strm (Stream.next strm))
    else
      let r = Util.transl_nth conf s n in
      match Mutil.lindex r '%' with
        Some i when c = "(" ->
          let sa =
            let rec loop len =
              let c = Stream.next strm in
              if c = ')' then Lbuff.get len
              else
                let len =
                  if c = '%' then
                    let c = Stream.next strm in
                    Lbuff.mstore len (macro conf base c)
                  else Lbuff.store len c
                in
                loop len
            in
            loop 0
          in
          String.sub r 0 i ^ sa ^
          String.sub r (i + 2) (String.length r - i - 2)
      | _ -> (if nomin then Util.translate_eval r else r) ^ c
  in
  if upp then capitale r else r

let browser_cannot_handle_passwords conf =
  let user_agent = Wserver.extract_param "user-agent: " '/' conf.request in
  String.lowercase user_agent = "konqueror"

let get_variable strm =
  let rec loop len =
    match Stream.next strm with
      ';' -> Buff.get len
    | c -> loop (Buff.store len c)
  in
  loop 0

let rec stream_line (strm__ : _ Stream.t) =
  match Stream.peek strm__ with
    Some '\n' -> Stream.junk strm__; ""
  | Some c -> Stream.junk strm__; String.make 1 c ^ stream_line strm__
  | _ -> raise Stream.Failure

type src_mode = Lang | Source

let notes_links conf =
  let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
  let fname = Filename.concat bdir "notes_links" in
  NotesLinks.read_db_from_file fname

let rec copy_from_stream conf base strm mode =
  let echo = ref true in
  let no_tables = browser_doesnt_have_tables conf in
  let (push_echo, pop_echo) =
    let stack = ref [] in
    (fun x -> stack := !echo :: !stack; echo := x),
    (fun () ->
       match !stack with
         x :: l -> stack := l; echo := x
       | [] -> echo := true)
  in
  let rec if_expr =
    function
      'N' -> not (if_expr (Stream.next strm))
    | 'a' -> conf.auth_file <> ""
    | 'c' -> conf.cgi || browser_cannot_handle_passwords conf
    | 'f' -> conf.friend
    | 'h' -> Sys.file_exists (History.file_name conf)
    | 'j' -> conf.just_friend_wizard
    | 'l' -> no_tables
    | 'm' -> notes_links conf <> []
    | 'n' -> not (base_notes_are_empty base "")
    | 'o' -> Sys.file_exists (Wiznotes.dir conf base)
    | 'p' ->
        begin match p_getenv conf.base_env (get_variable strm) with
          Some "" | None -> false
        | Some _ -> true
        end
    | 's' -> p_getenv conf.base_env (get_variable strm) <> Some "no"
    | 'w' -> conf.wizard
    | 'z' -> Util.find_sosa_ref conf base <> None
    | '|' ->
        let a = if_expr (Stream.next strm) in
        let b = if_expr (Stream.next strm) in a || b
    | '&' ->
        let a = if_expr (Stream.next strm) in
        let b = if_expr (Stream.next strm) in a && b
    | c -> Wserver.wprint "!!!!!%c!!!!!" c; true
  in
  try
    while true do
      match Stream.next strm with
        '[' -> src_translate conf base true strm echo mode
      | '<' when no_tables && !echo ->
          let c = Stream.next strm in
          let (slash, c) = if c = '/' then "/", Stream.next strm else "", c in
          let (atag, c) =
            let rec loop len =
              function
                '>' | ' ' | '\n' as c -> Buff.get len, c
              | c -> loop (Buff.store len c) (Stream.next strm)
            in
            loop 0 c
          in
          begin match atag with
            "table" | "tr" | "td" ->
              let rec loop =
                function
                  '>' -> ()
                | _ -> loop (Stream.next strm)
              in
              loop c
          | _ -> Wserver.wprint "<%s%s%c" slash atag c
          end
      | '%' ->
          let c = Stream.next strm in
          begin match c with
            'I' -> push_echo (!echo && if_expr (Stream.next strm))
          | 'E' -> pop_echo ()
          | _ when not !echo -> ()
          | '%' -> Wserver.wprint "%%"
          | '[' | ']' -> Wserver.wprint "%c" c
          | 'h' -> hidden_env conf
          | 'j' -> Templ.include_hed_trl conf (Some base) "hed"
          | 'P' -> let _ = Stream.next strm in ()
          | 'r' -> copy_from_file conf base (stream_line strm) mode
          | 'u' ->
              let lang =
                let rec loop len =
                  let c = Stream.next strm in
                  if c = ';' then Buff.get len else loop (Buff.store len c)
                in
                loop 0
              in
              let lang_def = transl conf " !languages" in
              Wserver.wprint "%s" (Translate.language_name lang lang_def)
          | 'V' ->
              let txt =
                try List.assoc (get_variable strm) conf.base_env with
                  Not_found -> ""
              in
              copy_from_string conf base txt mode
          | c -> Wserver.wprint "%s" (macro conf base c)
          end
      | c -> if !echo then Wserver.wprint "%c" c
    done
  with Stream.Failure -> ()
and src_translate conf base nomin strm echo mode =
  let c = Stream.next strm in
  if c = '\n' then
    let s =
      let rec loop lev len =
        function
          '[' -> loop (lev + 1) (Lbuff.store len '[') (Stream.next strm)
        | ']' ->
            if lev = 0 then Lbuff.get len
            else loop (lev - 1) (Lbuff.store len ']') (Stream.next strm)
        | c -> loop lev (Lbuff.store len c) (Stream.next strm)
      in
      loop 0 0 (Stream.next strm)
    in
    let (s, _) = Translate.inline conf.lang '%' (macro conf base) s in
    if not !echo then ()
    else copy_from_stream conf base (Stream.of_string s) mode
  else
    let s = lexicon_translate conf base nomin strm c in
    if not !echo then () else Wserver.wprint "%s" s
and copy_from_file conf base name mode =
  let fname =
    match mode with
      Lang -> any_lang_file_name name
    | Source -> source_file_name conf name
  in
  match try Some (Secure.open_in fname) with Sys_error _ -> None with
    Some ic -> copy_from_channel conf base ic mode
  | None ->
      Wserver.wprint "<em>... file not found: \"%s.txt\"</em>" name;
      html_br conf
and copy_from_channel conf base ic mode =
  copy_from_stream conf base (Stream.of_channel ic) mode; close_in ic
and copy_from_string conf base str mode =
  copy_from_stream conf base (Stream.of_string str) mode

let gen_print with_logo mode conf base fname =
  let channel =
    match mode with
      Lang ->
        begin try Some (Secure.open_in (lang_file_name conf fname)) with
          Sys_error _ ->
            begin try Some (Secure.open_in (any_lang_file_name fname)) with
              Sys_error _ -> None
            end
        end
    | Source ->
        try Some (Secure.open_in (source_file_name conf fname)) with
          Sys_error _ -> None
  in
  match channel with
    Some ic ->
      Util.html conf;
      Util.nl ();
      copy_from_channel conf base ic mode;
      Hutil.gen_trailer with_logo conf
  | _ ->
      let title _ = Wserver.wprint "Error" in
      Hutil.header conf title;
      Wserver.wprint "<ul>\n";
      html_li conf;
      Wserver.wprint "Cannot access file \"%s.txt\".\n" fname;
      Wserver.wprint "</ul>\n";
      Hutil.gen_trailer with_logo conf;
      raise Exit

let print_source = gen_print true Source

(* welcome page *)

type 'a env =
    Vsosa_ref of person option Lazy.t
  | Vother of 'a
  | Vnone

let get_env v env = try List.assoc v env with Not_found -> Vnone
let get_vother =
  function
    Vother x -> Some x
  | _ -> None
let set_vother x = Vother x

let eval_var conf base env () loc =
  function
    ["base"; "has_notes"] -> VVbool (not (base_notes_are_empty base ""))
  | ["base"; "name"] -> VVstring conf.bname
  | ["base"; "nb_persons"] ->
      VVstring
        (string_of_num (Util.transl conf "(thousand separator)")
           (Num.of_int (nb_of_persons base)))
  | ["base"; "title"] ->
      let s = base_notes_read_first_line base "" in
      let len = String.length s in
      let s =
        if len > 9 && String.sub s 0 5 = "<!-- " &&
           String.sub s (len - 4) 4 = " -->"
        then
          " : " ^ String.sub s 5 (String.length s - 9)
        else ""
      in
      VVstring s
  | ["browsing_with_sosa_ref"] ->
      begin match get_env "sosa_ref" env with
        Vsosa_ref v -> VVbool (Lazy.force v <> None)
      | _ -> raise Not_found
      end
  | ["has_history"] -> VVbool (Sys.file_exists (History.file_name conf))
  | ["has_misc_notes"] -> VVbool (notes_links conf <> [])
  | ["nb_accesses"] ->
      let r = count conf in
      let s =
        string_of_num (transl conf "(thousand separator)")
          (Num.of_int (r.welcome_cnt + r.request_cnt))
      in
      VVstring s
  | ["nb_accesses_to_welcome"] ->
      let r = count conf in
      let s =
        string_of_num (transl conf "(thousand separator)")
          (Num.of_int r.welcome_cnt)
      in
      VVstring s
  | ["random"; "init"] -> Random.self_init (); VVstring ""
  | ["random"; s] ->
      begin try VVstring (string_of_int (Random.int (int_of_string s))) with
        Failure _ | Invalid_argument _ -> raise Not_found
      end
  | ["sosa_ref"] ->
      begin match get_env "sosa_ref" env with
        Vsosa_ref v ->
          begin match Lazy.force v with
            Some p -> VVstring (referenced_person_title_text conf base p)
          | None -> raise Not_found
          end
      | _ -> raise Not_found
      end
  | ["start_date"] -> VVstring (string_of_start_date conf)
  | ["wiznotes_dir_exists"] ->
      VVbool (Sys.file_exists (Wiznotes.dir conf base))
  | _ -> raise Not_found

let eval_predefined_apply conf env f vl = raise Not_found

let print_start conf base =
  let new_welcome =
    match p_getenv conf.base_env "old_welcome" with
      Some "yes" -> false
    | Some _ | None -> !(Mutil.utf_8_db)
  in
  if new_welcome then
    let env =
      let sosa_ref_l =
        let sosa_ref () = Util.find_sosa_ref conf base in
        Lazy.from_fun sosa_ref
      in
      ["sosa_ref", Vsosa_ref sosa_ref_l]
    in
    Wserver.wrap_string := Util.xml_pretty_print;
    Hutil.interp conf base "welcome"
      {Templ.eval_var = eval_var conf base;
       Templ.eval_transl = (fun env -> Templ.eval_transl conf);
       Templ.eval_predefined_apply = eval_predefined_apply conf;
       Templ.get_vother = get_vother; Templ.set_vother = set_vother;
       Templ.print_foreach =
         fun _ -> raise (Match_failure ("./src/srcfile.ml4", 627, 29))}
      env ()
  else
    let fname =
      if Sys.file_exists (lang_file_name conf conf.bname) then conf.bname
      else if Sys.file_exists (any_lang_file_name conf.bname) then conf.bname
      else if !(Mutil.utf_8_db) then "start_utf8"
      else "start"
    in
    gen_print false Lang conf base fname

(* code déplacé et modifié pour gérer advanced.txt *)
let print conf base fname =
  if Sys.file_exists (Util.etc_file_name conf fname) then
    begin
      Wserver.wrap_string := Util.xml_pretty_print;
      Hutil.interp conf base fname
        {Templ.eval_var = eval_var conf base;
         Templ.eval_transl = (fun env -> Templ.eval_transl conf);
         Templ.eval_predefined_apply = eval_predefined_apply conf;
         Templ.get_vother = get_vother; Templ.set_vother = set_vother;
         Templ.print_foreach =
           fun _ -> raise (Match_failure ("./src/srcfile.ml4", 649, 29))}
        [] ()
    end
  else gen_print true Lang conf base fname

(* lexicon (info) *)

let print_lexicon conf base =
  let title _ = Wserver.wprint "Lexicon" in
  let fname =
    let f = if !(Mutil.utf_8_db) then "lex_utf8.txt" else "lexicon.txt" in
    search_in_lang_path (Filename.concat "lang" f)
  in
  Hutil.header conf title;
  begin match (try Some (Secure.open_in fname) with Sys_error _ -> None) with
    Some ic ->
      Wserver.wprint "<pre dir=\"ltr\">\n";
      begin try
        while true do
          match input_char ic with
            '<' -> Wserver.wprint "&lt;"
          | c -> Wserver.wprint "%c" c
        done
      with End_of_file -> ()
      end;
      Wserver.wprint "</pre>\n";
      close_in ic
  | None ->
      Wserver.wprint "<em>... file not found: \"%s.txt\"</em>" "lexicon";
      html_br conf
  end;
  Hutil.trailer conf
