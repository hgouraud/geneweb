(* Copyright (c) 1998-2007 INRIA *)
(* Copyright (c) 2025 - Refactored *)

(** File operations for image management.
    This module handles all filesystem operations for images:
    - Creating, moving, and deleting files
    - Managing the "saved" archive directory
    - Renaming images when person keys change
    - Swapping current and saved versions *)

open Config
open ImageTypes

module Logs = Geneweb_logs.Logs
module Driver = Geneweb_db.Driver

(* ========================================================================== *)
(* Basic file operations                                                      *)
(* ========================================================================== *)

(** [write_file path content] writes content to a file, creating it if needed *)
let write_file path content =
  let oc = Secure.open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () ->
      output_string oc content;
      flush oc)

(** [safe_rename src dst] renames a file, logging errors instead of raising *)
let safe_rename src dst =
  try
    if Sys.file_exists src then Sys.rename src dst
  with Failure msg ->
    Logs.syslog `LOG_ERR
      (Format.sprintf "Rename failed: %s to %s: %s" src dst msg)

(** [ensure_dir path] ensures a directory exists, creating it if needed *)
let ensure_dir path = Filesystem.create_dir ~parent:true path

(* ========================================================================== *)
(* Saved directory management                                                 *)
(* ========================================================================== *)

(** [move_to_saved dir filename] moves a file and its metadata to saved/ subdirectory.
    
    @param dir The directory containing the file
    @param filename The filename (basename) to move
    @return true if successful, false otherwise *)
let move_to_saved dir filename =
  try
    let save_dir = Filename.concat dir "saved" in
    ensure_dir save_dir;
    let src = Filename.concat dir filename in
    if not (Sys.file_exists src) then (
      Logs.syslog `LOG_ERR
        (Format.sprintf "move_to_saved: source not found: %s" src);
      false)
    else
      let dst = Filename.concat save_dir filename in
      (* Remove existing saved file if present *)
      if Sys.file_exists dst then Mutil.rm dst;
      Sys.rename src dst;
      (* Also move associated metadata files *)
      let base_src = Filename.remove_extension src in
      let base_dst = Filename.remove_extension dst in
      List.iter
        (fun ext ->
          let meta_src = base_src ^ ext in
          let meta_dst = base_dst ^ ext in
          if Sys.file_exists meta_src then (
            if Sys.file_exists meta_dst then Mutil.rm meta_dst;
            Sys.rename meta_src meta_dst))
        [ ".txt"; ".src" ];
      true
  with
  | Sys_error msg ->
      Logs.syslog `LOG_ERR (Format.sprintf "move_to_saved error: %s" msg);
      false
  | _ -> false

(** [restore_from_saved dir filename] moves a file from saved/ back to main directory *)
let restore_from_saved dir filename =
  try
    let save_dir = Filename.concat dir "saved" in
    let src = Filename.concat save_dir filename in
    let dst = Filename.concat dir filename in
    if not (Sys.file_exists src) then false
    else (
      if Sys.file_exists dst then Mutil.rm dst;
      Sys.rename src dst;
      (* Also restore metadata files *)
      let base_src = Filename.remove_extension src in
      let base_dst = Filename.remove_extension dst in
      List.iter
        (fun ext ->
          let meta_src = base_src ^ ext in
          let meta_dst = base_dst ^ ext in
          if Sys.file_exists meta_src then (
            if Sys.file_exists meta_dst then Mutil.rm meta_dst;
            Sys.rename meta_src meta_dst))
        [ ".txt"; ".src" ];
      true)
  with Sys_error msg ->
    Logs.syslog `LOG_ERR (Format.sprintf "restore_from_saved error: %s" msg);
    false

(** [delete_from_saved dir filename] permanently deletes a file from saved/ *)
let delete_from_saved dir filename =
  try
    let save_dir = Filename.concat dir "saved" in
    let path = Filename.concat save_dir filename in
    if Sys.file_exists path then (
      Mutil.rm path;
      (* Also delete metadata *)
      let base = Filename.remove_extension path in
      List.iter
        (fun ext ->
          let meta = base ^ ext in
          if Sys.file_exists meta then Mutil.rm meta)
        [ ".txt"; ".src" ];
      true)
    else false
  with Sys_error msg ->
    Logs.syslog `LOG_ERR (Format.sprintf "delete_from_saved error: %s" msg);
    false

(** [clean_saved_portrait path] removes all versions of a saved portrait *)
let clean_saved_portrait path =
  let base = Filename.remove_extension path in
  Array.iter (fun ext -> Mutil.rm (base ^ ext)) ImageTypes.image_extensions

(* ========================================================================== *)
(* Swapping current and saved versions                                        *)
(* ========================================================================== *)

(** [swap_files_aux dir file old_file] swaps two files, handling extension changes *)
let swap_files_aux dir file old_file =
  let ext = Filename.extension file in
  let old_ext = Filename.extension old_file in
  let tmp_file = Filename.concat dir "tempfile.tmp" in
  if ext <> old_ext then (
    (* Extensions differ: swap and rename *)
    safe_rename file (Filename.remove_extension old_file ^ ext);
    safe_rename old_file (Filename.remove_extension file ^ old_ext))
  else (
    (* Same extension: simple three-way swap *)
    safe_rename file tmp_file;
    safe_rename old_file file;
    safe_rename tmp_file old_file)

(** [swap_with_saved dir filename] exchanges current and saved versions of a file.
    Also swaps associated .txt and .src metadata files. *)
let swap_with_saved dir filename =
  let current = Filename.concat dir filename in
  let saved = String.concat Filename.dir_sep [ dir; "saved"; filename ] in
  if Sys.file_exists current && Sys.file_exists saved then (
    swap_files_aux dir current saved;
    (* Swap metadata files too *)
    let swap_meta ext =
      let current_meta = Filename.remove_extension current ^ ext in
      let saved_meta = Filename.remove_extension saved ^ ext in
      if Sys.file_exists current_meta || Sys.file_exists saved_meta then
        swap_files_aux dir current_meta saved_meta
    in
    swap_meta ".txt";
    swap_meta ".src";
    true)
  else false

(* ========================================================================== *)
(* Image format handling                                                      *)
(* ========================================================================== *)

(** [detect_image_type content] detects the image format from binary content.
    More lenient than ImageSize.detect_format - tries to find magic bytes
    even if not at the start (for malformed uploads). *)
let detect_image_type content =
  let len = String.length content in
  (* Check standard positions first *)
  if len > 10 && Char.code content.[0] = 0xff && Char.code content.[1] = 0xd8
  then Some JPEG
  else if len > 4 && String.sub content 0 4 = "\137PNG" then Some PNG
  else if len > 4 && String.sub content 0 4 = "GIF8" then Some GIF
  else
    (* Try to find markers within content (handles spurious headers) *)
    let find_substring s pattern =
      let plen = String.length pattern in
      let rec loop i =
        if i + plen > String.length s then None
        else if String.sub s i plen = pattern then Some i
        else loop (i + 1)
      in
      loop 0
    in
    match find_substring content "JFIF" with
    | Some i when i > 6 -> Some JPEG
    | _ -> (
        match find_substring content "\137PNG" with
        | Some _ -> Some PNG
        | None -> (
            match find_substring content "GIF8" with
            | Some _ -> Some GIF
            | None -> None))

(** [extract_clean_content content format] extracts clean image data from
    potentially malformed content by finding the actual start of image data *)
let extract_clean_content content format =
  let find_substring s pattern =
    let plen = String.length pattern in
    let rec loop i =
      if i + plen > String.length s then None
      else if String.sub s i plen = pattern then Some i
      else loop (i + 1)
    in
    loop 0
  in
  match format with
  | JPEG -> (
      match find_substring content "JFIF" with
      | Some i when i >= 6 ->
          String.sub content (i - 6) (String.length content - i + 6)
      | _ -> content)
  | PNG -> (
      match find_substring content "\137PNG" with
      | Some i -> String.sub content i (String.length content - i)
      | None -> content)
  | GIF -> (
      match find_substring content "GIF8" with
      | Some i -> String.sub content i (String.length content - i)
      | None -> content)

(* ========================================================================== *)
(* Person image renaming (when key changes)                                   *)
(* ========================================================================== *)

(** [rename_person_images conf base p new_key] renames all images when a
    person's key changes (e.g., name correction).
    
    Renames:
    - Portrait files
    - Blason files
    - Carrousel directory
    - Saved versions of all above *)
let rename_person_images conf base p (new_fn, new_sn, new_occ) =
  let old_key = ImagePath.person_key base p in
  let new_key = ImagePath.person_key_of_strings new_fn new_sn new_occ in
  if old_key = new_key then ()
  else
    let portrait_dir = ImagePath.portrait_dir conf in
    let carrousel_dir = ImagePath.carrousel_dir conf in
    (* Rename carrousel directory *)
    let old_carrousel = Filename.concat carrousel_dir old_key in
    let new_carrousel = Filename.concat carrousel_dir new_key in
    if Sys.file_exists old_carrousel && Sys.is_directory old_carrousel then (
      try Sys.rename old_carrousel new_carrousel
      with Sys_error msg ->
        Logs.syslog `LOG_ERR
          (Format.sprintf "Error renaming carrousel directory %s to %s: %s"
             old_carrousel new_carrousel msg));
    (* Helper to rename files with various extensions *)
    let rename_files_in_dir dir base_name =
      Array.iter
        (fun ext ->
          let blason_suffix =
            if Filename.check_suffix base_name ".blason" then ".blason" else ""
          in
          let old_file = Filename.concat dir (base_name ^ ext) in
          if Sys.file_exists old_file then
            let new_file = Filename.concat dir (new_key ^ blason_suffix ^ ext) in
            try Sys.rename old_file new_file
            with Sys_error msg ->
              Logs.syslog `LOG_ERR
                (Format.sprintf "Error renaming %s to %s: %s" old_file new_file
                   msg))
        ImageTypes.rename_extensions
    in
    (* Rename portrait files *)
    rename_files_in_dir portrait_dir old_key;
    (* Rename blason files *)
    rename_files_in_dir portrait_dir (old_key ^ ".blason");
    (* Rename saved versions *)
    let saved_dir = Filename.concat portrait_dir "saved" in
    if Sys.file_exists saved_dir then (
      rename_files_in_dir saved_dir old_key;
      rename_files_in_dir saved_dir (old_key ^ ".blason"))

(* ========================================================================== *)
(* Blason special operations                                                  *)
(* ========================================================================== *)

(** [create_blason_stop conf base p] creates a .stop file to prevent
    blason inheritance for this person *)
let create_blason_stop conf base p =
  let blason_dir = ImagePath.portrait_dir conf in
  let key = ImagePath.blason_key base p in
  let stop_path = Filename.concat blason_dir (key ^ ".stop") in
  let oc = open_out stop_path in
  close_out oc;
  stop_path

(** [move_blason_file conf base src dst] moves a blason from one person to another *)
let move_blason_file conf base src dst =
  let blason_dir = ImagePath.portrait_dir conf in
  let src_name = ImageAccess.get_blason_name conf base src in
  if src_name = "" then ""
  else
    let src_path = Filename.concat blason_dir src_name in
    if
      ImageAccess.has_blason conf base src ~self:true
      && Sys.file_exists src_path
      && not (ImageAccess.has_blason conf base dst ~self:true)
    then (
      let dst_key = ImagePath.blason_key base dst in
      let dst_path =
        Filename.concat blason_dir (dst_key ^ Filename.extension src_path)
      in
      safe_rename src_path dst_path;
      dst_path)
    else ""

(** [copy_portrait_to_blason conf base p] copies portrait to blason position *)
let copy_portrait_to_blason conf base p =
  let dir = ImagePath.portrait_dir conf in
  let portrait_key = ImagePath.person_key base p in
  let blason_key = ImagePath.blason_key base p in
  (* Check if portrait is a URL in database *)
  match ImageAccess.resolve_db_image conf base p with
  | Some (Url url) ->
      (* Create .url file for blason *)
      let url_path = Filename.concat dir (blason_key ^ ".url") in
      write_file url_path url;
      url_path
  | _ ->
      (* Copy file from portrait to blason *)
      let ext =
        ImagePath.get_extension_for_file conf ~keydir:portrait_key ~mode:Portrait
          ~saved:false portrait_key
      in
      if ext = "." then ""
      else
        let src = Filename.concat dir (portrait_key ^ ext) in
        let dst = Filename.concat dir (blason_key ^ ext) in
        if ImageAccess.has_blason conf base p ~self:true then (
          (* Move existing blason to saved *)
          let _ = move_to_saved dir (Filename.basename dst) in
          ());
        Filesystem.copy_file ~perm:0o666 src dst;
        dst

(** [copy_carrousel_image_to_blason conf base p filename] copies a carrousel
    image to the blason position *)
let copy_carrousel_image_to_blason conf base p filename =
  let portrait_dir = ImagePath.portrait_dir conf in
  let carrousel_dir = ImagePath.carrousel_dir conf in
  let person_key = ImagePath.person_key base p in
  let blason_key = ImagePath.blason_key base p in
  let src = String.concat Filename.dir_sep [ carrousel_dir; person_key; filename ] in
  if not (Sys.file_exists src) then ""
  else
    let ext = Filename.extension src in
    let dst = Filename.concat portrait_dir (blason_key ^ ext) in
    if ImageAccess.has_blason conf base p ~self:true then (
      let _ = move_to_saved portrait_dir (Filename.basename dst) in
      ());
    Filesystem.copy_file ~perm:0o666 src dst;
    dst

(* ========================================================================== *)
(* URL file operations                                                        *)
(* ========================================================================== *)

(** [create_url_file dir key url] creates a .url file containing the URL *)
let create_url_file dir key url =
  let path = Filename.concat dir (key ^ ".url") in
  write_file path url;
  path

(** [save_url_to_file conf mode base p url] saves a URL reference to the
    appropriate saved directory *)
let save_url_to_file conf mode base p url =
  let dir = ImagePath.base_dir_for_mode conf mode in
  let save_dir = Filename.concat dir "saved" in
  ensure_dir save_dir;
  let key = ImagePath.key_for_mode base p mode in
  let path = Filename.concat save_dir (key ^ ".url") in
  write_file path url;
  path

(* ========================================================================== *)
(* Dump bad images for debugging                                              *)
(* ========================================================================== *)

(** [dump_bad_image conf content] saves unrecognized image content for debugging
    if enabled in base configuration *)
let dump_bad_image conf content =
  match List.assoc_opt "dump_bad_images" conf.base_env with
  | Some "yes" -> (
      try write_file "bad-image" content with Sys_error _ -> ())
  | _ -> ()
