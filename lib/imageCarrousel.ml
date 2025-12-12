(* Copyright (c) 1998-2007 INRIA *)
(* Copyright (c) 2025 - Refactored *)

(** HTTP handlers for carrousel image management.
    This module handles web requests for:
    - Uploading images (portrait, blason, carrousel)
    - Deleting images
    - Swapping current/saved versions
    - Blason operations (copy, move, stop)

    File operations are delegated to ImageOps module.
    Access control is delegated to ImageAccess module. *)

open Config
open Def
open Util
open ImageTypes
open ImageUtil

module Logs = Geneweb_logs.Logs
module Driver = Geneweb_db.Driver

(* ========================================================================== *)
(* Error handling                                                             *)
(* ========================================================================== *)

let raise_modErr s = raise @@ Update.ModErr (Update.UERR s)

let incorrect conf str =
  Hutil.incorrect_request conf ~comment:str;
  failwith (__FILE__ ^ " (" ^ str ^ ")" :> string)

let incorrect_content_type conf base p content_type =
  let title _ =
    Output.print_sstring conf (Utf8.capitalize (transl conf "error"))
  in
  Hutil.header conf title;
  Output.print_sstring conf "<p>\n<em style=\"font-size:smaller\">";
  Output.printf conf "Error: incorrect image content type: %s" content_type;
  Output.printf conf "</em>\n</p>\n<ul>\n<li>\n%s</li>\n</ul>\n"
    (referenced_person_title_text conf base p :> string);
  Hutil.trailer conf;
  failwith (__FILE__ ^ " " ^ string_of_int __LINE__ :> string)

let error_too_big_image conf base p len max_len =
  let title _ =
    Output.print_sstring conf (Utf8.capitalize (transl conf "error"))
  in
  Hutil.header ~error:true conf title;
  Output.print_sstring conf "<p><em style=\"font-size:smaller\">";
  Output.printf conf "Error: this image is too big: %d bytes<br>\n" len;
  Output.printf conf "Maximum authorized in this database: %d bytes<br>\n"
    max_len;
  Output.printf conf "</em></p>\n<ul>\n<li>\n%s</li>\n</ul>\n"
    (referenced_person_title_text conf base p :> string);
  Hutil.trailer conf;
  failwith (__FILE__ ^ " " ^ string_of_int __LINE__ :> string)

(* ========================================================================== *)
(* Request parameter helpers                                                  *)
(* ========================================================================== *)

let raw_get conf key =
  try List.assoc key conf.env
  with Not_found -> incorrect conf ("raw_get: " ^ key)

let get_mode conf =
  try (List.assoc "mode" conf.env :> string) with Not_found -> "portraits"

let get_mode_typed conf =
  match get_mode conf with
  | "portraits" -> Portrait
  | "blasons" -> Blason
  | "carrousel" -> Carrousel
  | _ -> Portrait

let get_delete_flag conf =
  try List.assoc "delete" conf.env = Adef.encoded "on"
  with Not_found -> false

let get_filename conf =
  try List.assoc "file_name" conf.env |> Mutil.decode with Not_found -> ""

let get_filename_2 conf =
  try List.assoc "file_name_2" conf.env |> Mutil.decode with Not_found -> ""

let get_image_url conf =
  try (List.assoc "image_url" conf.env :> string) with Not_found -> ""

let get_image_name conf =
  try (List.assoc "image_name" conf.env :> string) with Not_found -> ""

let get_note conf =
  match p_getenv conf.env "note" with
  | Some v -> safe_html (only_printable_or_nl (Mutil.strip_all_trailing_spaces v))
  | None -> Adef.safe ""

let get_source conf =
  match p_getenv conf.env "source" with
  | Some v -> safe_html (only_printable_or_nl (Mutil.strip_all_trailing_spaces v))
  | None -> Adef.safe ""

let get_max_image_size conf =
  Option.bind (List.assoc_opt "max_images_size" conf.base_env) (fun s ->
      try Some (int_of_string s) with Failure _ -> None)

(* ========================================================================== *)
(* Image content processing                                                   *)
(* ========================================================================== *)

(** [parse_multipart_content file_content] extracts image data from
    multipart form submission *)
let parse_multipart_content file_content =
  let strm = Stream.of_string file_content in
  let request, content = Wserver.get_request_and_content strm in
  let remaining =
    let rec loop len (strm__ : _ Stream.t) =
      match Stream.peek strm__ with
      | Some x ->
          Stream.junk strm__;
          loop (Buff.store len x) strm
      | None -> Buff.get len
    in
    loop 0 strm
  in
  (request, (content :> string) ^ remaining)

(** [validate_image_content conf base p content] validates and extracts
    image data, checking format and size limits *)
let validate_image_content conf base p content =
  match ImageOps.detect_image_type content with
  | None ->
      ImageOps.dump_bad_image conf content;
      None
  | Some format -> (
      let content = ImageOps.extract_clean_content content format in
      match get_max_image_size conf with
      | Some max_len when String.length content > max_len ->
          error_too_big_image conf base p (String.length content) max_len
      | _ -> Some (format, content))

(* ========================================================================== *)
(* UI generation - Forms                                                      *)
(* ========================================================================== *)

let print_link_delete_image conf base p =
  if Option.is_some @@ ImageAccess.get_portrait conf base p then (
    Output.print_sstring conf {|<div><a class="btn btn-danger mt-3" href="|};
    Output.print_string conf (commd conf);
    Output.print_sstring conf "m=DEL_IMAGE&i=";
    Output.print_string conf
      (Driver.get_iper p |> Driver.Iper.to_string |> Mutil.encode);
    Output.print_sstring conf {|">|};
    transl conf "delete" |> Utf8.capitalize_fst |> Output.print_sstring conf;
    Output.print_sstring conf " ";
    transl_nth conf "image/images" 0 |> Output.print_sstring conf;
    Output.print_sstring conf "</a></div>")

let print_send_image conf base mode p =
  let title h =
    if Option.is_some @@ ImageAccess.get_portrait conf base p then
      transl_nth conf "image/images" 0
      |> transl_decline conf "modify"
      |> Utf8.capitalize_fst |> Output.print_sstring conf
    else
      transl_nth conf "image/images" 0
      |> transl_decline conf "add" |> Utf8.capitalize_fst
      |> Output.print_sstring conf;
    if not h then (
      Output.print_sstring conf (transl conf ":");
      Output.print_sstring conf " ";
      Output.print_string conf (escape_html (Driver.p_first_name base p));
      Output.print_sstring conf (Format.sprintf ".%d " (Driver.get_occ p));
      Output.print_string conf (escape_html (Driver.p_surname base p)))
  in
  Hutil.header conf title;
  Output.printf conf
    "<form method=\"post\" action=\"%s\" enctype=\"multipart/form-data\">\n"
    conf.command;
  Output.print_sstring conf
    "<div class=\"d-inline-flex align-items-center mt-2\">\n";
  hidden_env conf;
  hidden_input conf "m" (Adef.encoded "SND_IMAGE_C_OK");
  hidden_input conf "i"
    (Driver.get_iper p |> Driver.Iper.to_string |> Mutil.encode);
  hidden_input conf "mode" (Adef.encoded mode);
  Output.print_sstring conf (Utf8.capitalize_fst (transl conf "file"));
  Output.print_sstring conf (transl conf ":");
  Output.print_sstring conf " ";
  Output.print_sstring conf
    {|<input type="file" class="form-control-file ml-1" name="file" accept="image/*">|};
  (match get_max_image_size conf with
  | Some len ->
      Output.print_sstring conf "<p>(maximum authorized size = ";
      Output.print_sstring conf (string_of_int len);
      Output.print_sstring conf " bytes)</p>"
  | None -> ());
  Output.print_sstring conf
    {|<span>></span><button type="submit" class="btn btn-primary ml-3">|};
  transl_nth conf "validate/delete" 0
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf "</button></div></form>";
  print_link_delete_image conf base p;
  Hutil.trailer conf

let print_sent conf base p =
  let title _ =
    transl conf "image received"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.header conf title;
  Output.print_sstring conf "<ul><li>";
  Output.print_string conf (referenced_person_text conf base p);
  Output.print_sstring conf "</li></ul>";
  Hutil.trailer conf

let print_delete_image conf base p =
  let title h =
    transl_nth conf "image/images" 0
    |> transl_decline conf "delete"
    |> Utf8.capitalize_fst |> Output.print_sstring conf;
    if not h then (
      let fn = Driver.p_first_name base p in
      let sn = Driver.p_surname base p in
      let occ = Driver.get_occ p in
      Output.print_sstring conf (transl conf ":");
      Output.print_sstring conf " ";
      Output.print_string conf (escape_html fn);
      Output.print_sstring conf ".";
      Output.print_sstring conf (string_of_int occ);
      Output.print_sstring conf " ";
      Output.print_string conf (escape_html sn))
  in
  Hutil.header conf title;
  Output.printf conf "<form method=\"post\" action=\"%s\">" conf.command;
  hidden_env conf;
  hidden_input conf "m" (Adef.encoded "DEL_IMAGE_OK");
  hidden_input conf "i"
    (Driver.get_iper p |> Driver.Iper.to_string |> Mutil.encode);
  Output.print_sstring conf
    {|<div class="mt-3"><button type="submit" class="btn btn-danger">|};
  transl_nth conf "validate/delete" 1
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf {|</button></div></form>|};
  Hutil.trailer conf

let print_deleted conf base p =
  let title _ =
    transl conf "image deleted"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.header conf title;
  Output.print_sstring conf "<ul><li>";
  Output.print_string conf (referenced_person_text conf base p);
  Output.print_sstring conf "</li></ul>";
  Hutil.trailer conf

(* ========================================================================== *)
(* Effective operations - Send                                                *)
(* ========================================================================== *)

let effective_send_ok conf base p file =
  let mode = get_mode conf in
  let _request, content = parse_multipart_content file in
  match validate_image_content conf base p content with
  | None ->
      Mutil.extract_param "content-type: " '\n' []
      |> incorrect_content_type conf base p
  | Some (format, content) ->
      let key = ImagePath.person_key base p in
      let dir =
        if mode = "portraits" || mode = "blasons" then
          ImagePath.portrait_dir conf
        else ImagePath.carrousel_dir conf
      in
      ImageOps.ensure_dir dir;
      let filename =
        if mode = "portraits" || mode = "blasons" then
          key ^ extension_of_format format
        else key
      in
      let filepath = Filename.concat dir filename in
      let _ = ImageOps.move_to_saved dir filename in
      ImageOps.write_file filepath content;
      let changed =
        U_Send_image
          (string_gen_person base (Driver.gen_person_of_person p))
      in
      History.record conf base changed "si";
      print_sent conf base p

(** [effective_send_c_ok conf base p file file_name] handles carrousel
    image upload with support for URLs, notes, and sources *)
let effective_send_c_ok conf base p file file_name =
  let mode = get_mode conf in
  let mode_typed = get_mode_typed conf in
  let image_url = get_image_url conf in
  let image_name =
    let user_name = get_image_name conf in
    if user_name = "" && image_url <> "" then
      ImagePath.extract_filename_from_url image_url
    else user_name
  in
  let note = get_note conf in
  let source = get_source conf in
  (* Parse content if not a URL or note/source only submission *)
  let format_and_content =
    if mode = "note" || mode = "source" || image_url <> "" then None
    else
      let _request, content = parse_multipart_content file in
      if content = "" then None
      else validate_image_content conf base p content
  in
  let keydir = ImagePath.person_key base p in
  let dir =
    match mode_typed with
    | Portrait | Blason -> ImagePath.portrait_dir conf
    | Carrousel -> Filename.concat (ImagePath.carrousel_dir conf) keydir
  in
  ImageOps.ensure_dir dir;
  (* Determine filename and handle existing files *)
  let filename =
    match (mode_typed, format_and_content) with
    | (Portrait | Blason), Some (format, _) ->
        keydir ^ extension_of_format format
    | _ -> file_name
  in
  (* Move pre-existing file to saved for portraits/blasons *)
  (match mode_typed with
  | Portrait | Blason -> (
      let existing =
        if mode = "portraits" then ImageAccess.get_portrait conf base p
        else ImageAccess.get_blason conf base p ~self:true
      in
      match existing with
      | Some (Path path) ->
          if not (ImageOps.move_to_saved dir (Filename.basename path)) then
            incorrect conf "effective send (portrait/blason)"
      | Some (Url url) ->
          let key =
            if mode = "portraits" then ImagePath.person_key base p
            else ImagePath.blason_key base p
          in
          let _ = ImageOps.save_url_to_file conf mode_typed base p url in
          ignore key
      | None -> ())
  | Carrousel ->
      if Option.is_some format_and_content && Sys.file_exists (Filename.concat dir filename) then
        if not (ImageOps.move_to_saved dir filename) then
          incorrect conf "effective send (image)");
  (* Write the file *)
  let final_filename =
    if image_url <> "" then Filename.concat dir (image_name ^ ".url")
    else Filename.concat dir filename
  in
  (match format_and_content with
  | Some (_, content) -> ImageOps.write_file final_filename content
  | None when image_url <> "" -> ImageOps.write_file final_filename image_url
  | None -> ());
  (* Write note if provided *)
  if note <> Adef.safe "" then
    ImageOps.write_file
      (Filename.remove_extension final_filename ^ ".txt")
      (note :> string);
  (* Write source if provided *)
  if source <> Adef.safe "" then
    ImageOps.write_file
      (Filename.remove_extension final_filename ^ ".src")
      (source :> string);
  (* Record history *)
  let changed =
    U_Send_image (string_gen_person base (Driver.gen_person_of_person p))
  in
  let history_code =
    match mode with
    | "portraits" -> "sp"
    | "blasons" -> "sb"
    | "carrousel" ->
        if file_name <> "" && note <> Adef.safe "" && source <> Adef.safe ""
        then "s3"
        else if file_name <> "" then "sf"
        else if note <> Adef.safe "" then "so"
        else if source <> Adef.safe "" then "ss"
        else "sx"
    | "note" -> "so"
    | "source" -> "ss"
    | _ -> "s?"
  in
  History.record conf base changed history_code;
  file_name

(* ========================================================================== *)
(* Effective operations - Delete                                              *)
(* ========================================================================== *)

(* Refactored image deletion functions for imageCarrousel.ml *)
(* Replaces effective_delete_ok (lines 395-410) and effective_delete_c_ok (lines 412-458) *)

(** [do_delete_image conf base p] performs the actual image deletion/move.
    This is the core deletion logic shared by both old and new interfaces.

    @return the filename that was deleted/moved *)
let do_delete_image conf base p =
  let keydir = ImagePath.person_key base p in
  let mode = get_mode conf in
  let mode_typed = get_mode_typed conf in
  let file_name_raw = get_filename conf in

  (* Step 1: Derive filename if empty *)
  let file_name =
    if file_name_raw = "" then
      Image.default_image_filename mode base p
    else
      file_name_raw
  in

  (* Step 2: Extract basename if full path *)
  let file_name =
    if String.contains file_name '/' then (
      Filename.basename file_name
    ) else
      file_name
  in

  (* Step 3: For blasons, ensure .blason is in filename *)
  let file_name =
    match mode_typed with
    | Blason ->
        let has_blason =
          match ImageUtil.StringUtil.find_substring file_name "blason" with
          | Some _ -> true
          | None -> false
        in
        if not has_blason then (
          let blason_name = Image.default_image_filename "blasons" base p in
          blason_name
        ) else
          file_name
    | _ -> file_name
  in

  (* Step 4: Determine directory *)
  let dir =
    match mode_typed with
    | Portrait | Blason -> ImagePath.portrait_dir conf
    | Carrousel -> Filename.concat (ImagePath.carrousel_dir conf) keydir
  in

  (* Step 5: Get file extension *)
  let ext =
    ImagePath.get_extension_for_file conf ~keydir ~mode:mode_typed ~saved:false
      file_name
  in

  (* Step 6: Build final filename (handle double extension) *)
  let current_ext = Filename.extension file_name in
  let filename =
    if ext <> "." then
      if current_ext = ext then
        file_name  (* Already has extension *)
      else
        file_name ^ ext  (* Add extension *)
    else
      file_name
  in

  (* Step 7: Perform deletion or move *)
  let delete_permanent = get_delete_flag conf in

  if delete_permanent then (
    (* Permanent delete *)
    let file_path = Filename.concat dir filename in
    MetadataFiles.delete_with_metadata file_path;
  ) else (
    (* Move to saved *)
    if not (ImageOps.move_to_saved dir filename) then
      incorrect conf "Failed to move file to saved"
  );

  (* Step 8: Record in history *)
  let changed =
    U_Delete_image (string_gen_person base (Driver.gen_person_of_person p))
  in
  let history_code =
    match mode with
    | "portraits" -> if delete_permanent then "dpp" else "dp"
    | "blasons" -> if delete_permanent then "dbp" else "db"
    | "carrousel" -> if delete_permanent then "dcp" else "dc"
    | _ -> if delete_permanent then "dip" else "di"
  in
  History.record conf base changed history_code;

  (* Return filename for caller *)
  filename


(** [effective_delete_ok conf base p] handles old-style deletion (DEL_IMAGE_OK).
    Uses core deletion logic, then displays success page.
    This maintains backward compatibility with old interface. *)
let effective_delete_ok conf base p =
  let _filename = do_delete_image conf base p in
  (* Display success page (old interface expects this) *)
  print_deleted conf base p


(** [effective_delete_c_ok conf base p] handles new-style deletion (DEL_IMAGE_C_OK).
    Uses core deletion logic, then redirects to carrousel page.

    @param f_name Optional filename parameter (for compatibility)
    @return the filename that was deleted *)
let effective_delete_c_ok conf base ?(_f_name = "") p =
  (* Ignore f_name parameter - do_delete_image gets it from env *)
  let file_name = do_delete_image conf base p in

  (* The new interface returns the filename for redirect/display *)
  file_name


(* ========================================================================== *)
(* Effective operations - Reset (swap current/saved)                          *)
(* ========================================================================== *)

(* CORRECT FIX - Handles person key format properly *)
(* Person keys like "henri.0.gouraud" are BASE filenames, not filename.extension *)

(* Refactored image reset/restore functions for imageCarrousel.ml *)
(* For consistency with deletion refactoring *)

(** [do_reset_image conf base p] performs the actual image restoration.
    This is the core reset logic that can be shared if we add an old-style
    reset interface in the future.

    @return the filename that was restored *)
let do_reset_image conf base p =
  let mode = get_mode conf in
  let mode_typed = get_mode_typed conf in
  let keydir = ImagePath.person_key base p in
  let file_name_raw = get_filename conf in

  (* Step 1: Derive filename if empty *)
  let file_name =
    if file_name_raw = "" then
      Image.default_image_filename mode base p
    else
      file_name_raw
  in

  (* Step 2: Extract basename if full path *)
  let file_name =
    if String.contains file_name '/' then (
      let basename = Filename.basename file_name in
      basename
    ) else
      file_name
  in

  (* Step 3: For blasons, ensure .blason is in filename *)
  let file_name =
    match mode_typed with
    | Blason ->
        let has_blason =
          match ImageUtil.StringUtil.find_substring file_name "blason" with
          | Some _ -> true
          | None -> false
        in
        if not has_blason then (
          let blason_name = Image.default_image_filename "blasons" base p in
          blason_name
        ) else
          file_name
    | _ -> file_name
  in

  (* Step 4: Determine directory *)
  let dir =
    match mode_typed with
    | Portrait | Blason -> ImagePath.portrait_dir conf
    | Carrousel -> Filename.concat (ImagePath.carrousel_dir conf) keydir
  in

  (* Step 5: Determine extensions for current and saved files *)
  let ext =
    ImagePath.get_extension_for_file conf ~keydir ~mode:mode_typed ~saved:false
      file_name
  in
  let old_ext =
    ImagePath.get_extension_for_file conf ~keydir ~mode:mode_typed ~saved:true
      file_name
  in

  (* Step 6: Handle blason .stop files *)
  let ext =
    match mode_typed with
    | Blason when ext = ".stop" -> old_ext
    | _ -> ext
  in

  (* Step 7: Build filename to restore (handle double extension) *)
  let current_ext = Filename.extension file_name in
  let filename_to_restore =
    if old_ext <> "." then
      if current_ext = old_ext then
        file_name  (* Already has extension *)
      else
        file_name ^ old_ext  (* Add extension *)
    else if ext <> "." then
      if current_ext = ext then
        file_name
      else
        file_name ^ ext
    else
      file_name
  in

  let saved_file = String.concat Filename.dir_sep [ dir; "saved"; filename_to_restore ] in

  (* Step 8: Check if saved file exists *)
  if not (Sys.file_exists saved_file) then (
    incorrect conf "No saved version to restore")
  else (
    (* Step 9: If current file exists, move it to saved first *)
    let current_file_path =
      if ext <> "." then Filename.concat dir (file_name ^ ext)
      else Filename.concat dir filename_to_restore
    in
    let current_exists =
      Sys.file_exists current_file_path &&
      not (Sys.is_directory current_file_path)
    in


    if current_exists then (
      let current_basename = Filename.basename current_file_path in

      ImageOps.ensure_dir (Filename.concat dir "saved");

      let move_result = ImageOps.move_to_saved dir current_basename in

      if not move_result then
        incorrect conf "Failed to save current version before restore"
    ) else (
      Logs.syslog `LOG_INFO
        "No current file exists, skipping move to saved"
    );

    (* Step 10: Restore from saved *)
    let basename_to_restore = Filename.basename saved_file in

    let restore_result = ImageOps.restore_from_saved dir basename_to_restore in
    if not restore_result then
      incorrect conf "Failed to restore from saved";

    (* Step 11: Record in history *)
    let changed =
      U_Send_image (string_gen_person base (Driver.gen_person_of_person p))
    in
    let history_code =
      match mode with
      | "portraits" -> "rp"
      | "blasons" -> "rb"
      | "carrousel" -> "rc"
      | _ -> "r?"
    in
    History.record conf base changed history_code;

    (* Return filename for caller *)
    filename_to_restore
  )

(** [effective_reset_c_ok conf base p] handles image restoration (RESET_IMAGE_C_OK).
    Uses core reset logic, then returns the filename.

    @return the filename that was restored *)
let effective_reset_c_ok conf base p =
  do_reset_image conf base p

(* ========================================================================== *)
(* Effective operations - Blason special                                      *)
(* ========================================================================== *)

let effective_copy_portrait_to_blason conf base p =
  ImageOps.copy_portrait_to_blason conf base p

let effective_copy_image_to_blason conf base p =
  let filename = get_filename conf in
  ImageOps.copy_carrousel_image_to_blason conf base p filename

(* ========================================================================== *)
(* HTTP entry points                                                          *)
(* ========================================================================== *)

let print_send_ok conf base =
  let ip =
    try raw_get conf "i" |> Mutil.decode |> Driver.Iper.of_string
    with Failure _ -> incorrect conf "print send ok"
  in
  let p = Driver.poi base ip in
  raw_get conf "file" |> Adef.as_string |> effective_send_ok conf base p

let print_del_ok conf base =
  match p_getenv conf.env "i" with
  | Some ip ->
      let p = Driver.poi base (Driver.Iper.of_string ip) in
      effective_delete_ok conf base p
  | None -> incorrect conf "print del ok"

let print_del conf base =
  match p_getenv conf.env "i" with
  | None -> Hutil.incorrect_request conf
  | Some ip -> (
      let p = Driver.poi base (Driver.Iper.of_string ip) in
      match ImageAccess.get_portrait conf base p with
      | Some _ -> print_delete_image conf base p
      | None -> Hutil.incorrect_request conf)

let print conf base =
  match p_getenv conf.env "i" with
  | None -> Hutil.incorrect_request conf
  | Some ip ->
      let p = Driver.poi base (Driver.Iper.of_string ip) in
      let fn = Driver.p_first_name base p in
      let sn = Driver.p_surname base p in
      if fn = "?" || sn = "?" then Hutil.incorrect_request conf
      else print_send_image conf base "portraits" p

let print_family conf base =
  match p_getenv conf.env "i" with
  | None -> Hutil.incorrect_request conf
  | Some ip ->
      let p = Driver.poi base (Driver.Iper.of_string ip) in
      let sn = Driver.p_surname base p in
      if sn = "?" then Hutil.incorrect_request conf
      else print_send_image conf base "blasons" p

(* ========================================================================== *)
(* Carrousel main handler with HTTP redirects                                 *)
(* ========================================================================== *)

let print_main_c conf base =
  match p_getenv conf.env "em" with
  | None -> (
      (* Process action and redirect with success message *)
      match p_getenv conf.env "m" with
      | Some m -> (
          match p_getenv conf.env "i" with
          | Some ip -> (
              let p = Driver.poi base (Driver.Iper.of_string ip) in
              let processed_filename =
                match m with
                | "SND_IMAGE_C_OK" ->
                    let file_name = get_filename conf in
                    let file_name =
                      if file_name = "" then get_filename_2 conf else file_name
                    in
                    let file_name =
                      (Mutil.decode (Adef.encoded file_name) :> string)
                    in
                    let file =
                      let mode = get_mode conf in
                      if mode = "note" || mode = "source" then "file_name"
                      else
                        match p_getenv conf.env "image_url" with
                        | Some url when url <> "" -> ""
                        | _ -> (
                            try (raw_get conf "file" :> string) with _ -> "")
                    in
                    effective_send_c_ok conf base p file file_name
                | "DEL_IMAGE_C_OK" -> effective_delete_c_ok conf base p
                | "RESET_IMAGE_C_OK" -> effective_reset_c_ok conf base p
                | "BLASON_MOVE_TO_ANC" ->
                    if ImageAccess.has_blason conf base p ~self:true then
                      match p_getenv conf.env "ia" with
                      | Some ia ->
                          let fa = Driver.poi base (Driver.Iper.of_string ia) in
                          Filename.basename
                            (ImageOps.move_blason_file conf base p fa)
                      | None -> ""
                    else ""
                | "PORTRAIT_TO_BLASON" ->
                    Filename.basename
                      (effective_copy_portrait_to_blason conf base p)
                | "IMAGE_TO_BLASON" ->
                    Filename.basename
                      (effective_copy_image_to_blason conf base p)
                | "BLASON_STOP" ->
                    let has_blason_self =
                      ImageAccess.has_blason conf base p ~self:true
                    in
                    let has_blason =
                      ImageAccess.has_blason conf base p ~self:false
                    in
                    if has_blason && not has_blason_self then
                      Filename.basename (ImageOps.create_blason_stop conf base p)
                    else "error"
                | _ -> "incorrect request"
              in
              (* HTTP redirect to main page with success message *)
              match processed_filename with
              | "error" -> Hutil.incorrect_request conf
              | "incorrect request" ->
                  Hutil.incorrect_request conf ~comment:"incorrect request"
              | _ ->
                  let mode = get_mode conf in
                  let display_filename =
                    if Filename.extension processed_filename = "" then
                      let keydir = ImagePath.person_key base p in
                      let ext =
                        ImagePath.get_extension_for_file conf ~keydir
                          ~mode:(get_mode_typed conf) ~saved:false
                          processed_filename
                      in
                      if ext <> "." then processed_filename ^ ext
                      else processed_filename
                    else processed_filename
                  in
                  let base_url =
                    Printf.sprintf "%sm=SND_IMAGE_C&i=%s&em=%s&file_name=%s&mode=%s"
                      (commd conf :> string)
                      ip
                      (Mutil.encode m :> string)
                      (Mutil.encode display_filename :> string)
                      (Mutil.encode mode :> string)
                  in
                  let url_params = ref [] in
                  if get_delete_flag conf then
                    url_params := ("delete", "on") :: !url_params;
                  let fn2 = get_filename_2 conf in
                  if fn2 <> "" then
                    url_params := ("file_name_2", fn2) :: !url_params;
                  if m = "IMAGE_TO_BLASON" then (
                    let ext = Filename.extension processed_filename in
                    if ext <> "" then
                      url_params := ("ext", ext) :: !url_params);
                  let params_string =
                    List.fold_left
                      (fun acc (key, value) ->
                        acc ^ "&" ^ key ^ "=" ^ (Mutil.encode value :> string))
                      "" !url_params
                  in
                  let redirect_url = base_url ^ params_string in
                  Output.status conf Moved_Temporarily;
                  Output.header conf "Location: %s" redirect_url;
                  Output.flush conf)
          | None -> Hutil.incorrect_request conf ~comment:"missing person index")
      | None -> Hutil.incorrect_request conf ~comment:"missing action")
  (* Display main page with success message from URL params *)
  | Some _ ->
      let p =
        match p_getint conf.env "i" with
        | Some ip -> Driver.poi base (Driver.Iper.of_string (string_of_int ip))
        | None -> failwith "No person index in success display"
      in
      Perso.interp_templ "carrousel" conf base p

(** [print_c] serves carrousel images or portrait/blason files *)
let print_c ?(saved = false) ?(portrait = true) conf base =
  let mode = if portrait then Portrait else Blason in
  match (p_getenv conf.env "s", find_person_in_env conf base "") with
  | Some filename, Some p ->
      let key = ImagePath.person_key base p in
      let full_filename = Filename.concat key filename in
      let path =
        if saved then ImagePath.insert_saved_in_path full_filename
        else full_filename
      in
      ImageDisplay.print_source conf path
  | Some filename, _ -> ImageDisplay.print_source conf filename
  | _, Some p -> (
      let source =
        if saved then ImageAccess.get_old_portrait_or_blason conf base
            (string_of_mode mode) p
        else if portrait then ImageAccess.get_portrait conf base p
        else ImageAccess.get_blason conf base p ~self:false
      in
      match source with
      | Some (Path path) -> (
          match ImageDisplay.print_image_file conf path with
          | Ok () -> ()
          | Error _ -> Hutil.incorrect_request conf)
      | Some (Url url) -> ImageDisplay.serve_url conf url
      | None -> Hutil.incorrect_request conf)
  | None, None -> Hutil.incorrect_request conf
