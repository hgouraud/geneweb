(* camlp4r ./pa_html.cmo *)
(* $Id: history.ml,v 3.14 2001-01-06 09:55:54 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Config;
open Def;
open Util;
open Gutil;

value file_name conf =
  let bname =
    if Filename.check_suffix conf.bname ".gwb" then conf.bname
    else conf.bname ^ ".gwb"
  in
  List.fold_right Filename.concat [Util.base_dir.val; bname] "history"
;

(* Record history when committing updates *)

value ext_flags =
  [Open_wronly; Open_append; Open_creat; Open_text; Open_nonblock]
;

value record conf base (fn, sn, occ) action =
  let do_it =
    match p_getenv conf.base_env "history" with
    [ Some "yes" -> True
    | _ -> False ]
  in
  if do_it then
    let fname = file_name conf in
    match
      try Some (open_out_gen ext_flags 0o644 fname) with
      [ Sys_error _ -> None ]
    with
    [ Some oc ->
        let (hh, mm, ss) = conf.time in
        do Printf.fprintf oc "%04d-%02d-%02d %02d:%02d:%02d [%s] %s %s.%d %s\n"
	     conf.today.year conf.today.month conf.today.day hh mm ss conf.user
             action fn occ sn;
           close_out oc;
        return ()
    | None -> () ]
  else ()
;

(* Request for history printing *)

exception Begin_of_file;

value buff_get_rev len =
  let s = String.create len in
  do for i = 0 to len - 1 do s.[i] := Buff.buff.val.[len - 1 - i]; done;
  return s
;

value rev_input_char ic (rbuff, rpos) pos =
  do if rpos.val == 0 then
       do if String.length rbuff.val < 65536 then
            let len =
              if rbuff.val = "" then 1024 else 2 * String.length rbuff.val
            in
            rbuff.val := String.create len
          else ();
       return
       let ppos = max (pos - String.length rbuff.val) 0 in
(*
do Printf.eprintf "rev_input_char pos %d reading %d chars..." pos (pos - ppos); flush stderr; return
*)
       do seek_in ic ppos;
          really_input ic rbuff.val 0 (pos - ppos);
(*
do Printf.eprintf " ok\n"; flush stderr; return
*)
          rpos.val := pos - ppos;
       return ()
     else ();
     decr rpos;
  return rbuff.val.[rpos.val]
;

value rev_input_line ic pos (rbuff, rpos) =
  if pos <= 0 then raise Begin_of_file
  else
    loop 0 (pos - 1) where rec loop len pos =
      if pos <= 0 then (buff_get_rev len, pos)
      else
        match rev_input_char ic (rbuff, rpos) pos with
        [ '\n' -> (buff_get_rev len, pos)
        | c -> loop (Buff.store len c) (pos - 1) ]
;

value action_text conf =
  fun
  [ "ap" -> transl_decline conf "add" (transl_nth conf "person/persons" 0)
  | "mp" -> transl_decline conf "modify" (transl_nth conf "person/persons" 0)
  | "dp" -> transl_decline conf "delete" (transl_nth conf "person/persons" 0)
  | "fp" -> transl_decline conf "merge" (transl_nth conf "person/persons" 1)
  | "si" -> transl_decline conf "send" (transl_nth conf "image/images" 0)
  | "di" -> transl_decline conf "delete" (transl_nth conf "image/images" 0)
  | "af" -> transl_decline conf "add" (transl_nth conf "family/families" 0)
  | "mf" -> transl_decline conf "modify" (transl_nth conf "family/families" 0)
  | "df" -> transl_decline conf "delete" (transl_nth conf "family/families" 0)
  | "sf" -> transl_decline conf "switch" (transl_nth conf "family/families" 1)
  | "ff" -> transl_decline conf "merge" (transl_nth conf "family/families" 1)
  | "cn" -> transl conf "change children's names"
  | "aa" -> transl_decline conf "add" (transl conf "parents")
  | x -> x ]
;

value line_tpl = "0000-00-00 00:00:00 xx .";

value line_fields conf base line =
  if String.length line > String.length line_tpl then
    let time = String.sub line 0 19 in
    let (user, i) =
      match (line.[20], Gutil.lindex line ']') with
      [ ('[', Some i) ->
          let user = String.sub line 21 (i - 21) in
          (user, i + 2)
      | _ -> ("", 20) ]
    in
    let action = action_text conf (String.sub line i 2) in
    let key = String.sub line (i + 3) (String.length line - i - 3) in
    let p =
      match person_ht_find_all base key with
      [ [ip] -> Right (poi base ip)
      | _ -> Left key ]
    in
    Some (time, user, action, p)
  else None
;

value print_history_line conf base line wiz k i =
  match line_fields conf base line with
  [ Some (time, user, action, p) ->
      if wiz = "" || user = wiz then
        do if i = 0 then Wserver.wprint "<dl>\n" else ();
           Wserver.wprint "<dt><tt><b>*</b> %s</tt>\n" time;
           Wserver.wprint "(%s)\n" action;
           if user <> "" then
             do Wserver.wprint "<em>";
                if wiz = "" then
                  Wserver.wprint "<a href=\"%sm=HIST;k=%d;wiz=%s\">"
                    (commd conf) k (Util.code_varenv user)
                else ();
                Wserver.wprint "%s" user;
                if wiz = "" then Wserver.wprint "</a>" else ();
                Wserver.wprint "</em>";
             return ()
           else ();
           Wserver.wprint " :\n<dd>";
           match p with
           [ Left key -> Wserver.wprint "%s" key
           | Right p ->
               do Wserver.wprint "%s"
                    (referenced_person_title_text conf base p);
                  Wserver.wprint "%s" (Date.short_dates_text conf base p);
               return () ];
           Wserver.wprint "\n";
        return i + 1
      else i
  | None -> i ]
;

value print_history conf base ic =
  let k =
    match p_getint conf.env "k" with
    [ Some x -> x
    | _ -> 3 ]
  in
  let pos =
    match p_getint conf.env "pos" with
    [ Some x -> x
    | _ -> in_channel_length ic ]
  in
  let wiz =
    match p_getenv conf.env "wiz" with
    [ Some x ->
        match p_getenv conf.env "n" with
        [ Some "" | None -> x
        | _ -> "" ]
    | _ -> "" ]
  in
  let (pos, n) =
    let vv = (ref (""), ref 0) in
    loop pos 0 where rec loop pos i =
      if i >= k then (pos, i)
      else
	match
          try Some (rev_input_line ic pos vv) with [ Begin_of_file -> None ]
        with
	[ Some (line, pos) ->
            let i = print_history_line conf base line wiz k i in
            loop pos i
	| _ -> (pos, i) ]
  in
  do if n > 0 then Wserver.wprint "</dl>\n" else ();
     if pos > 0 then
       tag "form" "method=GET action=\"%s\"" conf.command begin
         Util.hidden_env conf;
         Wserver.wprint "<input type=hidden name=m value=HIST>\n";
         Wserver.wprint "<input name=k size=3 value=%d>\n" k;
         Wserver.wprint "<input type=hidden name=pos value=%d>\n" pos;
         if wiz <> "" then
           do Wserver.wprint "<input type=hidden name=wiz value=\"%s\">\n" wiz;
              Wserver.wprint "(%s)\n" wiz;
           return ()
         else ();
         Wserver.wprint "<input type=submit value=\"&gt;&gt;\">\n";
         if wiz <> "" then
           Wserver.wprint "<input type=submit name=n value=\"&gt;&gt;\">\n"
         else ();
       end
     else ();
  return ()
;

value print conf base =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "history of updates"))
  in
  do header conf title;
     print_link_to_welcome conf True;
     let fname = file_name conf in
     match try Some (open_in_bin fname) with [ Sys_error _ -> None ] with
     [ Some ic ->
         do print_history conf base ic;
            close_in ic;
         return ()
     | _ -> () ];
     trailer conf;
  return ()
;
