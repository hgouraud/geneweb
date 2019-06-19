(* $Id: sendImage.ml,v 5.7 2019-03-12 09:58:44 ddr Exp $ *)

open Config
open Def
open Gwdb
open Util
open Path

type image_type = JPEG | GIF | PNG

let image_types = [JPEG; GIF; PNG]

let extension_of_type =
  function
    JPEG -> ".jpg"
  | GIF -> ".gif"
  | PNG -> ".png"

let incorrect conf = Hutil.incorrect_request conf; raise Update.ModErr

let incorrect_content_type conf base p s =
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "<p>\n";
  Wserver.printf "<em style=\"font-size:smaller\">";
  Wserver.printf "Error: incorrect image content type: %s" s;
  Wserver.printf "</em>\n";
  Wserver.printf "</p>\n";
  Wserver.printf "<ul>\n";
  Wserver.printf "<li>\n";
  Wserver.printf "%s" (referenced_person_title_text conf base p);
  Wserver.printf "</li>\n";
  Wserver.printf "</ul>\n";
  Hutil.trailer conf;
  raise Update.ModErr

let error_too_big_image conf base p len max_len =
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  Hutil.rheader conf title;
  Hutil.print_link_to_welcome conf true;
  Wserver.printf "<p><em style=\"font-size:smaller\">";
  Wserver.printf "Error: this image is too big: %d bytes<br%s>\n" len
    conf.xhs;
  Wserver.printf "Maximum authorized in this database: %d bytes<br%s>\n"
    max_len conf.xhs;
  Wserver.printf "</em></p>\n";
  Wserver.printf "<ul>\n";
  Wserver.printf "<li>\n";
  Wserver.printf "%s" (referenced_person_title_text conf base p);
  Wserver.printf "</li>\n";
  Wserver.printf "</ul>\n";
  Hutil.trailer conf;
  raise Update.ModErr

let raw_get conf key =
  try List.assoc key conf.env with Not_found -> incorrect conf

(* print delete image link *)
let print_link_delete_image conf base p =
  if Util.has_image conf base p then
    begin
      Wserver.printf "<p>\n";
      begin
        Wserver.printf "<a href=\"%sm=DEL_IMAGE&i=%d\">" (commd conf)
          (Adef.int_of_iper (get_key_index p));
        Wserver.printf "%s %s" (capitale (transl conf "delete"))
          (transl_nth conf "image/images" 0);
        Wserver.printf "</a>"
      end;
      Wserver.printf "</p>\n"
    end

(* Send image form *)

let print_send_image conf base p =
  let title h =
    if Util.has_image conf base p then
      Wserver.printf "%s"
        (capitale
           (transl_decline conf "modify" (transl_nth conf "image/images" 0)))
    else
      Wserver.printf "%s"
        (capitale
           (transl_decline conf "add" (transl_nth conf "image/images" 0)));
    if h then ()
    else
      let fn = p_first_name base p in
      let sn = p_surname base p in
      Wserver.printf ": ";
      Wserver.printf "%s %s" fn sn;
      Util.print_reference conf fn (get_occ p) sn
  in
  let digest = Update.digest_person (UpdateInd.string_person_of base p) in
  Perso.interp_notempl_with_menu title "perso_header" conf base p;
  Wserver.printf "<h2>\n";
  title false;
  Wserver.printf "</h2>\n";
  Wserver.printf
    "<form method=\"post\" action=\"%s\" enctype=\"multipart/form-data\">\n"
    conf.command;
  Wserver.printf "<p>\n";
  Util.hidden_env conf;
  Wserver.printf
    "<input type=\"hidden\" name=\"m\" value=\"SND_IMAGE_OK\"%s>\n" conf.xhs;
  Wserver.printf "<input type=\"hidden\" name=\"i\" value=\"%d\"%s>\n"
    (Adef.int_of_iper (get_key_index p)) conf.xhs;
  Wserver.printf "<input type=\"hidden\" name=\"digest\" value=\"%s\"%s>\n"
    digest conf.xhs;
  Wserver.printf "%s%s\n" (capitale (transl conf "file")) (Util.transl conf ":");
  Wserver.printf "<input \
type=\"file\" class=\"form-control\" name=\"file\" size=\"50\" maxlength=\"250\" accept=\"image/*\"%s>\n"
    conf.xhs;
  Wserver.printf "</p>\n";
  begin match p_getint conf.base_env "max_images_size" with
    Some len ->
      Wserver.printf "<p>\n";
      Wserver.printf "(maximum authorized size = %d bytes)\n" len;
      Wserver.printf "</p>\n"
  | None -> ()
  end;
  Wserver.printf
    "<button type=\"submit\" class=\"btn btn-secondary btn-lg mt-2\">\n";
  Wserver.printf "%s" (capitale (transl_nth conf "validate/delete" 0));
  Wserver.printf "</button>\n";
  Wserver.printf "</form>\n";
  print_link_delete_image conf base p;
  Hutil.trailer conf

let print conf base =
  match p_getint conf.env "i" with
    Some ip ->
      let p = poi base (Adef.iper_of_int ip) in
      let fn = p_first_name base p in
      let sn = p_surname base p in
      if sou base (get_image p) <> "" || fn = "?" || sn = "?" then
        Hutil.incorrect_request conf
      else print_send_image conf base p
  | _ -> Hutil.incorrect_request conf

(* Delete image form *)

let print_delete_image conf base p =
  let title h =
    Wserver.printf "%s"
      (capitale
         (transl_decline conf "delete" (transl_nth conf "image/images" 0)));
    if h then ()
    else
      let fn = p_first_name base p in
      let sn = p_surname base p in
      let occ =
        if fn = "?" || sn = "?" then Adef.int_of_iper (get_key_index p)
        else get_occ p
      in
      Wserver.printf ": "; Wserver.printf "%s.%d %s" fn occ sn
  in
  Hutil.header conf title;
  Wserver.printf "\n";
  Wserver.printf "<form method=\"post\" action=\"%s\">\n" conf.command;
  html_p conf;
  Util.hidden_env conf;
  Wserver.printf
    "<input type=\"hidden\" name=\"m\" value=\"DEL_IMAGE_OK\">\n";
  Wserver.printf "<input type=\"hidden\" name=\"i\" value=\"%d\">\n\n"
    (Adef.int_of_iper (get_key_index p));
  Wserver.printf "\n";
  html_p conf;
  Wserver.printf
    "<button type=\"submit\" class=\"btn btn-secondary btn-lg\">\n";
  Wserver.printf "%s" (capitale (transl_nth conf "validate/delete" 0));
  Wserver.printf "</button>\n";
  Wserver.printf "</form>\n";
  Wserver.printf "\n";
  Hutil.trailer conf

let print_del conf base =
  match p_getint conf.env "i" with
    Some ip ->
      let p = poi base (Adef.iper_of_int ip) in
      if sou base (get_image p) <> "" then Hutil.incorrect_request conf
      else
        begin match auto_image_file conf base p with
          Some _ -> print_delete_image conf base p
        | _ -> Hutil.incorrect_request conf
        end
  | _ -> Hutil.incorrect_request conf

(* Send image form validated *)

let print_sent conf base p =
  let title _ =
    Wserver.printf "%s" (capitale (transl conf "image received"))
  in
  Hutil.header conf title;
  Wserver.printf "<ul>\n";
  Wserver.printf "<li>";
  Wserver.printf "%s" (referenced_person_text conf base p);
  Wserver.printf "</li>";
  Wserver.printf "</ul>\n";
  Hutil.trailer conf

let write_file fname content =
  let oc = Secure.open_out_bin fname in
  output_string oc content; flush oc; close_out oc

(* Move fname to old_dir if it exists with some extension.
   Returns the number of moved files *)
let move_file_to_old conf fname bfname =
  List.fold_left
    (fun cnt typ ->
       let ext = extension_of_type typ in
       let new_file = fname ^ ext in
       if Sys.file_exists new_file then
         (*let old_dir =
           Filename.concat (Util.base_path ["images"] conf.bname) "old"
         in*)
         let old_dir =
           String.concat Filename.dir_sep [conf.path.dir_root; "images"; conf.bname; "old"]
         in
         let old_file = Filename.concat old_dir bfname ^ ext in
         if Sys.file_exists old_file then
           (try Sys.remove old_file with Sys_error _ -> ());
         (try Unix.mkdir old_dir 0o777 with Unix.Unix_error (_, _, _) -> ());
         begin try Unix.rename new_file old_file with
           Unix.Unix_error (_, _, _) -> ()
         end;
         cnt + 1
       else cnt)
    0 image_types

let normal_image_type s =
  if String.length s > 10 && Char.code s.[0] = 0xff && Char.code s.[1] = 0xd8
  then
    Some JPEG
  else if String.length s > 4 && String.sub s 0 4 = "\137PNG" then Some PNG
  else if String.length s > 4 && String.sub s 0 4 = "GIF8" then Some GIF
  else None

let string_search s v =
  let rec loop i j =
    if j = String.length v then Some (i - String.length v)
    else if i = String.length s then None
    else if s.[i] = v.[j] then loop (i + 1) (j + 1)
    else loop (i + 1) 0
  in
  loop 0 0

(* get the image type, possibly removing spurious header *)

let image_type s =
  match normal_image_type s with
    Some t -> Some (t, s)
  | None ->
      match string_search s "JFIF" with
        Some i when i > 6 ->
          let s = String.sub s (i - 6) (String.length s - i + 6) in
          Some (JPEG, s)
      | _ ->
          match string_search s "\137PNG" with
            Some i ->
              let s = String.sub s i (String.length s - i) in Some (PNG, s)
          | _ ->
              match string_search s "GIF8" with
                Some i ->
                  let s = String.sub s i (String.length s - i) in
                  Some (GIF, s)
              | None -> None

let dump_bad_image conf s =
  match p_getenv conf.base_env "dump_bad_images" with
    Some "yes" ->
      begin try
        let oc = Secure.open_out_bin "bad-image" in
        output_string oc s; flush oc; close_out oc
      with Sys_error _ -> ()
      end
  | _ -> ()

let effective_send_ok conf base p file =
  let strm = Stream.of_string file in
  let (request, content) = Wserver.get_request_and_content strm in
  let content =
    let s =
      let rec loop len (strm__ : _ Stream.t) =
        match Stream.peek strm__ with
          Some x -> Stream.junk strm__; loop (Buff.store len x) strm
        | _ -> Buff.get len
      in
      loop 0 strm
    in
    content ^ s
  in
  let (typ, content) =
    match image_type content with
      None ->
        let ct = Wserver.extract_param "content-type: " '\n' request in
        dump_bad_image conf content; incorrect_content_type conf base p ct
    | Some (typ, content) ->
        match p_getint conf.base_env "max_images_size" with
          Some len when String.length content > len ->
            error_too_big_image conf base p (String.length content) len
        | _ -> typ, content
  in
  let bfname = default_image_name base p in
  let bfdir =
    (*let bfdir = Util.base_path ["images"] conf.bname in*)
    let bfdir =
       String.concat Filename.dir_sep [conf.path.dir_root; "images"; conf.bname]
    in
    if Sys.file_exists bfdir then bfdir
    else
      let d = Filename.concat (Secure.base_dir ()) "images" in
      let d1 = Filename.concat d conf.bname in
      (try Unix.mkdir d 0o777 with Unix.Unix_error (_, _, _) -> ());
      (try Unix.mkdir d1 0o777 with Unix.Unix_error (_, _, _) -> ());
      d1
  in
  let fname = Filename.concat bfdir bfname in
  let _moved = move_file_to_old conf fname bfname in
  write_file (fname ^ extension_of_type typ) content;
  let changed =
    U_Send_image (Util.string_gen_person base (gen_person_of_person p))
  in
  History.record conf base changed "si"; print_sent conf base p

let print_send_ok conf base =
  try
    let ip =
      let s = raw_get conf "i" in
      try int_of_string s with Failure _ -> incorrect conf
    in
    let p = poi base (Adef.iper_of_int ip) in
    let digest = Update.digest_person (UpdateInd.string_person_of base p) in
    if digest = raw_get conf "digest" then
      let file = raw_get conf "file" in effective_send_ok conf base p file
    else Update.error_digest conf
  with Update.ModErr -> ()

(* Delete image form validated *)

let print_deleted conf base p =
  let title _ =
    Wserver.printf "%s" (capitale (transl conf "image deleted"))
  in
  Hutil.header conf title;
  Wserver.printf "<ul>\n";
  html_li conf;
  Wserver.printf "\n%s" (referenced_person_text conf base p);
  Wserver.printf "\n";
  Wserver.printf "</ul>\n";
  Hutil.trailer conf

let effective_delete_ok conf base p =
  let bfname = default_image_name base p in
  (*let fname = Filename.concat (Util.base_path ["images"] conf.bname) bfname in*)
  let fname =
     String.concat
      Filename.dir_sep [conf.path.dir_root; "images"; conf.bname; bfname]
  in
  if move_file_to_old conf fname bfname = 0 then incorrect conf;
  let changed =
    U_Delete_image (Util.string_gen_person base (gen_person_of_person p))
  in
  History.record conf base changed "di"; print_deleted conf base p

let print_del_ok conf base =
  try
    match p_getint conf.env "i" with
      Some ip ->
        let p = poi base (Adef.iper_of_int ip) in
        effective_delete_ok conf base p
    | None -> incorrect conf
  with Update.ModErr -> ()


(* *************************************************************************** *)
(* Code for carousel                                                           *)

type 'a env =
    Vstring of string
  | Vint of int
  | Vother of 'a
  | Vcnt of int ref
  | Vbool of bool
  | Vnone

let clean_old_portrait conf bfname =
  let file_name = Filename.remove_extension
    (String.concat
      Filename.dir_sep
        [conf.path.dir_root; "documents"; "portraits"; "saved"; bfname])
  in
  Mutil.rm (file_name ^ ".jpg") ;
  Mutil.rm (file_name ^ ".png") ;
  Mutil.rm (file_name ^ ".gif")

let delete_old_file conf folder bfname =
  let file_name =
    String.concat
      Filename.dir_sep
        [conf.path.dir_root; "documents"; folder; "saved"; bfname]
  in
  let file_name_t = (Filename.remove_extension file_name) ^ ".txt" in
  Mutil.rm file_name ;
  Mutil.rm file_name_t ;
  1

let space_to_unders = Mutil.tr ' ' '_'

let get conf key =
  match p_getenv conf.env key with
    Some v -> v
  | None -> failwith (key ^ " unbound")

let get_extension conf keydir =
  let f =
    String.concat
      Filename.dir_sep
        [conf.path.dir_root; "documents"; "portraits"; keydir]
  in
  if Sys.file_exists (f ^ ".jpg") then ".jpg"
  else if Sys.file_exists (f ^ ".png") then ".png"
  else if Sys.file_exists (f ^ ".gif") then ".gif"
  else "."

let get_extension_old conf keydir =
  let f =
    String.concat
      Filename.dir_sep
        [conf.path.dir_root; "documents"; "portraits"; "saved"; keydir]
  in
  if Sys.file_exists (f ^ ".jpg") then ".jpg"
  else if Sys.file_exists (f ^ ".png") then ".png"
  else if Sys.file_exists (f ^ ".gif") then ".gif"
  else "."

(* bfname is basename or keydir/basename *)
let move_file_to_old conf fname bfname mode keydir =
  let old_dir_p =
    String.concat
      Filename.dir_sep
        [conf.path.dir_root; "documents"; "portraits"; "saved"]
  in
  let old_dir_i0 =
    String.concat
      Filename.dir_sep
        [conf.path.dir_root; "documents"; "images"; keydir;]
  in
  let old_dir_i =
    String.concat
      Filename.dir_sep
        [conf.path.dir_root; "documents"; "images"; keydir; "saved"]
  in
  Mutil.mkdir_p old_dir_p;
  Mutil.mkdir_p old_dir_i0;
  Mutil.mkdir_p old_dir_i;
  let saved_file =
    if mode = "portraits" then Filename.concat old_dir_p bfname
    else Filename.concat old_dir_i bfname
  in
  Mutil.rn fname saved_file;
  let fname_t = (Filename.remove_extension fname) ^ ".txt" in
  let saved_file_t = (Filename.remove_extension saved_file) ^ ".txt" in
  Mutil.rn fname_t saved_file_t;
  if Sys.file_exists saved_file then 1 else 0

let print_confirm_c conf base save_m report =
  match p_getint conf.env "i" with
  | Some ip ->
      let p = poi base (Adef.iper_of_int ip) in
      let sp = UpdateInd.string_person_of base p in
      let digest = default_image_name base p in
      let new_env =
        List.fold_left
          (fun accu (k, v) ->
             if k = "m" then ("m", "IMAGE_C") :: accu
             else if k = "idigest" || k = "" then accu
             else (k, v) :: accu)
          [] conf.env
      in
      let new_env =
        if save_m = "IMAGE_C" then new_env
        else ("em", save_m) :: new_env
      in
      let new_env = ("idigest", digest) :: new_env in
      let new_env = ("report", report) :: new_env in
      let conf = { (conf) with env = new_env } in
      Hutil.interp conf "images"
        {Templ.eval_var = UpdateInd.eval_var conf base;
         Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
         Templ.eval_predefined_apply = (fun _ -> raise Not_found);
         Templ.get_vother = UpdateInd.get_vother; 
         Templ.set_vother = UpdateInd.set_vother;
         Templ.print_foreach = UpdateInd.print_foreach conf base}
        [] sp
  | None -> Hutil.incorrect_request conf

let print_image conf base = print_confirm_c conf base ""

(* ************************************************************************ *)
(*  send, delete and reset functions                                        *)
(*                                                                          *)
(* ************************************************************************ *)

let effective_send_c_ok conf base p file file_name mode =
  let notes = match Util.p_getenv conf.env "notes" with
    Some v ->
      Util.safe_html
        (only_printable_or_nl (Mutil.strip_all_trailing_spaces v))
    | None -> ""
  in
  let strm = Stream.of_string file in
  let (request, content) = Wserver.get_request_and_content strm in
  let content =
    if mode = "comment" then ""
    else
      let s =
        let rec loop len (strm__ : _ Stream.t) =
          match Stream.peek strm__ with
          | Some x -> Stream.junk strm__; loop (Buff.store len x) strm
          | _ -> Buff.get len
        in
        loop 0 strm
      in
      content ^ s
  in
  let (typ, content) =
    if content <> "" then
      match image_type content with
      | None ->
          let ct = Wserver.extract_param "content-type: " '\n' request in
          dump_bad_image conf content; incorrect_content_type conf base p ct
      | Some (typ, content) ->
          match p_getint conf.base_env "max_images_size" with
          | Some len when String.length content > len ->
              error_too_big_image conf base p (String.length content) len
          | _ -> typ, content
    else GIF, content (* we dont care which type, content = "" *)
  in
  let keydir = default_image_name base p in
  let full_dir =
    if mode = "portraits" then
      String.concat Filename.dir_sep
        [conf.path.dir_root; "documents"; "portraits";]
    else
      String.concat Filename.dir_sep
        [conf.path.dir_root; "documents"; "images"; keydir]
  in
  let _ =
    if not (Sys.file_exists full_dir) then
      let d1 = String.concat
        Filename.dir_sep [conf.path.dir_root; "documents"; "portraits"] in
      let d2 = String.concat
        Filename.dir_sep [conf.path.dir_root; "documents"; "images"; keydir] in
      Mutil.mkdir_p d1;
      Mutil.mkdir_p d2;
  in
  let full_name = Filename.concat full_dir
     (if mode = "portraits" then
      keydir ^ (extension_of_type typ)
     else file_name)
  in
  if mode = "portraits" then
    begin match Util.auto_image_file conf base p with
    | Some f ->
        clean_old_portrait conf (Filename.basename f);
        if (move_file_to_old conf f (Filename.basename f) mode keydir) = 0 then
          incorrect conf;
    | None -> ()
    end
  else
    begin
    if (Sys.file_exists full_name) && content <> "" then
      let bfname = Filename.concat keydir (Filename.basename full_name) in
      if (move_file_to_old conf full_name bfname mode keydir) = 0 then
        incorrect conf
    end;
  if content <> "" then write_file full_name content;
  if notes <> "" then 
    write_file ((Filename.remove_extension full_name) ^ ".txt") notes;
  let changed =
    U_Send_image (Util.string_gen_person base (gen_person_of_person p))
  in
  History.record conf base changed
    (if mode = "portraits" then "si" else
      if file_name <> "" && notes <> "" then "sb"
      else if file_name <> "" then "so"
      else if notes <> "" then "sc" else "sn");
  file_name

(* removes portrait or other image and saves it into old folder *)
(* if delete=on permanently deletes the file in old folder *)

let effective_delete_c_ok conf base p =
  let keydir = default_image_name base p in
  let file_name = try List.assoc "file_name" conf.env with Not_found -> "" in
  let file_name = Wserver.decode file_name in
  let mode = try List.assoc "mode" conf.env with Not_found -> "portraits" in
  let delete =
    try (List.assoc "delete" conf.env = "on") with Not_found -> false
  in
  let file_name =
    if file_name = "" then
      keydir ^
        (if delete then get_extension_old conf keydir
         else get_extension conf keydir)
    else file_name
  in
  let bfname = file_name in
  let folder =
    if mode = "portraits" then "portraits"
    else (Filename.concat "images" keydir)
  in
  let saved = if delete then "saved" else "" in
  let full_name =
    String.concat Filename.dir_sep
      [conf.path.dir_root; "documents"; folder; saved; bfname]
  in
  let res =
    if (Sys.file_exists full_name) && delete then
      delete_old_file conf folder bfname
    else if (Sys.file_exists full_name) then
    (* TODO verify we dont destroy a saved image having the same name as portrait! *)
      begin
        begin match Util.auto_image_file ~bak:true conf base p with
        | Some _ -> clean_old_portrait conf bfname
        | None -> ()
        end;
        move_file_to_old conf full_name bfname mode keydir;
      end
    else 1
  in
  if res = 0 then incorrect conf else ();
  let changed =
    U_Delete_image (Util.string_gen_person base (gen_person_of_person p))
  in
  History.record conf base changed
    (if mode = "portraits" then "di" else "do");
  file_name

(* reset portrait or image from old folder to portrait or others *)

let swap_files conf file1 file2 txt =
  let tmp_file =
    String.concat Filename.dir_sep
      [conf.path.dir_root; "documents"; "tempfile.tmp"]
  in
  let ext_1 = Filename.extension file1 in
  let ext_2 = Filename.extension file2 in
  Mutil.rn file1 tmp_file;
  Mutil.rn file2 ((Filename.remove_extension file1) ^ ext_2);
  Mutil.rn tmp_file ((Filename.remove_extension file2) ^ ext_1);
  if txt then
    let tmp_file_t =
      String.concat Filename.dir_sep
        [conf.path.dir_root; "documents"; "tempfile.tmp"]
    in
    let file1_t = (Filename.remove_extension file1) ^ ".txt" in
    let file2_t = (Filename.remove_extension file2) ^ ".txt" in
    Mutil.rn file1_t tmp_file_t;
    Mutil.rn file2_t file1_t;
    Mutil.rn tmp_file_t file2_t

let rename_files file1 file2 txt =
  Mutil.rn file1 file2;
  if txt then
    let file1_t = (Filename.remove_extension file1) ^ ".txt" in
    let file2_t = (Filename.remove_extension file2) ^ ".txt" in
    Mutil.rn file1_t file2_t

let effective_reset_c_ok conf base p =
  let mode = try List.assoc "mode" conf.env with Not_found -> "portraits" in
  let keydir = default_image_name base p in
  if mode = "portraits" then
    begin
      let file_name = keydir in
      let ext_o = get_extension_old conf keydir in
      let ext = get_extension conf keydir in
      let ext = if ext = "." then ext_o else ext in
      let file_in_old =
        String.concat Filename.dir_sep
          [conf.path.dir_root; "documents"; "portraits"; "saved";
            (file_name ^ ext_o)]
      in
      let file_in_portraits =
        String.concat Filename.dir_sep
          [conf.path.dir_root; "documents"; "portraits";
            (file_name ^ ext)]
      in
      if Sys.file_exists file_in_portraits then
        swap_files conf file_in_old file_in_portraits false
      else
        rename_files file_in_old file_in_portraits false
    end
  else
    begin
      let file_name =
        try List.assoc "file_name" conf.env with Not_found -> ""
      in
      let file_name = Wserver.decode file_name in
      let file_in_old =
        String.concat Filename.dir_sep
          [conf.path.dir_root; "documents"; "images";
            keydir; "saved"; file_name]
      in
      let new_file =
        String.concat Filename.dir_sep
          [conf.path.dir_root; "documents"; "images"; keydir; file_name]
      in
      if Sys.file_exists new_file then
        swap_files conf file_in_old new_file true
      else
        rename_files file_in_old new_file true
    end;
  let file_name =
    try List.assoc "file_name" conf.env with Not_found -> ""
  in
  file_name

let print_c conf base =
  (* if em="" this is the first pass, do it *)
  begin match p_getenv conf.env "em" with
  | None ->
    begin match p_getenv conf.env "m" with
    | Some m ->
      let save_m = m in
      begin match p_getint conf.env "i" with
      | Some ip ->
          let p = poi base (Adef.iper_of_int ip) in
          let digest = default_image_name base p in
          let (conf, report) =
            begin match m with
            | "SND_IMAGE_C_OK" ->
                let mode =
                  try List.assoc "mode" conf.env with Not_found -> "portraits"
                in
                let file_name =
                    (try List.assoc "file_name" conf.env with Not_found -> "")
                in
                let file_name =
                  if file_name = "" then
                    (try List.assoc "file_name_2" conf.env with Not_found -> "")
                  else file_name
                in
                let file_name_2 = Filename.remove_extension file_name in
                let new_env =
                  List.fold_left
                    (fun accu (k, v) ->
                       if k = "file_name_2" then (k, file_name_2) :: accu
                       else (k, v) :: accu)
                    [] conf.env
                in
                let conf = { (conf) with env = new_env } in
                let file_name = Wserver.decode file_name in
                let file =
                  if mode <> "comment" then raw_get conf "file"
                  else "file_name"
                in
                let idigest = try List.assoc "idigest" conf.env with Not_found -> "" in
                if digest = idigest then
                  conf, effective_send_c_ok conf base p file file_name mode
                else 
                  conf, "digest error";
            | "DEL_IMAGE_C_OK" ->
                let idigest = try List.assoc "idigest" conf.env with Not_found -> "" in
                if digest = idigest then
                  conf, effective_delete_c_ok conf base p
                else conf, "digest error"
            | "RESET_IMAGE_C_OK" ->
                let idigest = try List.assoc "idigest" conf.env with Not_found -> "" in
                if digest = idigest then
                  conf, effective_reset_c_ok conf base p
                else conf, "digest error"
            | "IMAGE_C" -> conf, "image"
            | _ -> conf, "incorrect request"
            end;
          in
          begin match report with
          | "digest error" -> Update.error_digest conf
          | "incorrect request" -> Hutil.incorrect_request conf
          | _ -> print_confirm_c conf base save_m report
          end
      | None -> Hutil.incorrect_request conf
      end
    | None -> Hutil.incorrect_request conf
    end
  (* em!="" second pass, ignore *)
  | Some _ -> print_confirm_c conf base "IMAGE_C" ""
  end


