(* Copyright (c) 1998-2007 INRIA *)
(* Copyright (c) 2025 - Refactored *)

(** Path resolution and filename construction for images.
    This module handles all path-related operations including:
    - Directory location lookups
    - Person key generation for filenames
    - File extension resolution
    - URL detection and parsing *)

open Config
open ImageTypes

module Driver = Geneweb_db.Driver

(** [find_substring s pattern] finds the first occurrence of pattern in s *)
let find_substring s pattern =
  let plen = String.length pattern in
  let slen = String.length s in
  let rec loop i =
    if i + plen > slen then None
    else if String.sub s i plen = pattern then Some i
    else loop (i + 1)
  in
  loop 0

(* ========================================================================== *)
(* Directory accessors                                                        *)
(* ========================================================================== *)

(** [portrait_dir conf] returns the portraits directory for the current base *)
let portrait_dir conf = !GWPARAM.portraits_d conf.bname

(** [carrousel_dir conf] returns the images/carrousel directory for the current base *)
let carrousel_dir conf = !GWPARAM.images_d conf.bname

(* ========================================================================== *)
(* Person key generation                                                      *)
(* ========================================================================== *)

(** [normalize_name name] converts a name to filesystem-safe format:
    lowercase with spaces replaced by underscores *)
let normalize_name name =
  name |> Name.lower |> Mutil.tr ' ' '_'

(** [person_key_of_strings first_name surname occ] generates the canonical
    filename key for a person from their name components.
    Example: person_key_of_strings "Jean Claude" "DUPOND" 3 = "jean_claude.3.dupond" *)
let person_key_of_strings first_name surname occ =
  let fn = normalize_name first_name in
  let sn = normalize_name surname in
  Format.sprintf "%s.%d.%s" fn occ sn

(** [person_key base p] generates the canonical filename key for a person.
    Example: "jean_claude.3.dupond" *)
let person_key base p =
  person_key_of_strings
    (Driver.p_first_name base p)
    (Driver.p_surname base p)
    (Driver.get_occ p)

(** [blason_key_of_strings first_name surname occ] generates the blason filename key *)
let blason_key_of_strings first_name surname occ =
  person_key_of_strings first_name surname occ ^ ".blason"

(** [blason_key base p] generates the blason filename key for a person.
    Example: "jean_claude.3.dupond.blason" *)
let blason_key base p =
  person_key base p ^ ".blason"

(** [key_for_mode base p mode] generates the appropriate key for the given mode *)
let key_for_mode base p = function
  | Portrait | Carrousel -> person_key base p
  | Blason -> blason_key base p

(* ========================================================================== *)
(* Path resolution                                                            *)
(* ========================================================================== *)

(** [base_dir_for_mode conf mode] returns the base directory for a given image mode *)
let base_dir_for_mode conf = function
  | Portrait | Blason -> portrait_dir conf
  | Carrousel -> carrousel_dir conf

(** [resolve_path conf ~key ~saved ~mode] constructs the full base path
    (without extension) for an image file.
    
    @param conf Configuration
    @param key The person key (e.g., "jean_claude.3.dupond")
    @param saved If true, look in the "saved" subdirectory
    @param mode The image mode (Portrait, Blason, or Carrousel) *)
let resolve_path conf ~key ~saved ~mode =
  let base = base_dir_for_mode conf mode in
  let dir = if saved then Filename.concat base "saved" else base in
  Filename.concat dir key

(** [carrousel_item_path conf base p ~filename ~saved] constructs the path
    for a specific carrousel item.
    
    @param conf Configuration
    @param base Database
    @param p Person
    @param filename The image filename within the carrousel
    @param saved If true, look in the "saved" subdirectory *)
let carrousel_item_path conf base p ~filename ~saved =
  let key = person_key base p in
  let subdir = if saved then Filename.concat key "saved" else key in
  let parts = [ carrousel_dir conf; subdir ] in
  let parts = if filename = "" then parts else parts @ [ filename ] in
  String.concat Filename.dir_sep parts

(** [insert_saved_in_path path] transforms a path to its "saved" equivalent
    by inserting "saved" before the filename.
    Example: "/dir/file.jpg" -> "/dir/saved/file.jpg" *)
let insert_saved_in_path path =
  let parts = String.split_on_char Filename.dir_sep.[0] path |> List.rev in
  let parts =
    match parts with
    | filename :: rest -> List.rev (filename :: "saved" :: rest)
    | _ -> List.rev parts
  in
  String.concat Filename.dir_sep parts

(* ========================================================================== *)
(* File extension resolution                                                  *)
(* ========================================================================== *)

(** [find_with_extension base_path] attempts to find an image file by trying
    each supported extension. Returns the image source type found.
    
    Priority order:
    1. .url file (external URL reference)
    2. .stop file (inheritance stopper)
    3. Image files (.jpg, .jpeg, .png, .gif)
    
    @param base_path Path without extension
    @return Found source, Stopped, or NotFound *)
let find_with_extension base_path =
  let try_ext ext =
    let path = base_path ^ ext in
    if Sys.file_exists path then Some path else None
  in
  (* Check .url first for external references *)
  match try_ext ".url" with
  | Some url_file ->
      let ic = open_in url_file in
      let url = input_line ic in
      close_in ic;
      Found (Url url)
  | None -> (
      (* Check for stop marker *)
      match try_ext ".stop" with
      | Some stop_path -> Found (Path stop_path)
      | None -> (
          (* Try image extensions *)
          match Mutil.array_find_map try_ext image_extensions with
          | Some path -> Found (Path path)
          | None -> NotFound))

(** [find_file_with_extension base_path] finds a file trying all managed extensions.
    Returns the full path if found, empty string otherwise. *)
let find_file_with_extension base_path =
  let ext = Filename.extension base_path in
  (* If base_path already has a valid extension, return it if it exists *)
  if Array.mem ext all_extensions then
    if Sys.file_exists base_path then base_path else ""
  else
    let try_ext ext =
      let path = base_path ^ ext in
      if Sys.file_exists path then Some path else None
    in
    match Mutil.array_find_map try_ext all_extensions with
    | Some path -> path
    | None -> ""

(** [get_extension_for_file conf ~keydir ~mode ~saved filename] determines
    the actual extension of an existing file by checking the filesystem.
    Returns "." if no file found. *)
let get_extension_for_file conf ~keydir ~mode ~saved filename =
  let temp_ext = Filename.extension filename in
  let filename =
    if Array.mem temp_ext all_extensions then Filename.remove_extension filename
    else filename
  in
  let dir =
    match mode with
    | Portrait | Blason -> portrait_dir conf
    | Carrousel -> Filename.concat (carrousel_dir conf) keydir
  in
  let base_path =
    if saved then String.concat Filename.dir_sep [ dir; "saved"; filename ]
    else Filename.concat dir filename
  in
  (* Try each extension *)
  let extensions_to_try =
    [| ".jpg"; ".jpeg"; ".png"; ".gif"; ".url"; ".stop" |]
  in
  let rec try_extensions i =
    if i >= Array.length extensions_to_try then "."
    else
      let ext = extensions_to_try.(i) in
      if Sys.file_exists (base_path ^ ext) then ext else try_extensions (i + 1)
  in
  try_extensions 0

(* ========================================================================== *)
(* URL detection and parsing                                                  *)
(* ========================================================================== *)

(** [is_url s] checks if a string looks like a URL *)
let is_url s =
  Mutil.start_with "http://" 0 s
  || Mutil.start_with "https://" 0 s
  || Mutil.start_with "file://" 0 s

(** [parse_url_or_path conf s] parses a string as either a URL or a path.
    For relative paths, resolves against the portraits directory. *)
let parse_url_or_path conf s =
  if is_url s then Url s
  else if Filename.is_implicit s then
    (* Relative path - resolve against portraits directory *)
    let s = Filename.basename s in
    match List.assoc_opt "images_path" conf.base_env with
    | Some p when p <> "" -> Path (Filename.concat p s)
    | _ -> Path (Filename.concat (portrait_dir conf) s)
  else Path s

(** [extract_filename_from_url url] extracts a reasonable filename from a URL.
    Falls back to a timestamped name if extraction fails. *)
let extract_filename_from_url url =
  try
    (* Remove protocol *)
    let without_protocol =
      match find_substring url "://" with
      | Some pos -> String.sub url (pos + 3) (String.length url - pos - 3)
      | None -> url
    in
    (* Remove query parameters *)
    let without_params =
      match String.index_opt without_protocol '?' with
      | Some pos -> String.sub without_protocol 0 pos
      | None -> without_protocol
    in
    let filename = Filename.basename without_params in
    if filename = "" || filename = "/" || not (String.contains filename '.')
    then "image_" ^ string_of_int (int_of_float (Unix.time ()))
    else filename
  with _ -> "image_" ^ string_of_int (int_of_float (Unix.time ()))

(* ========================================================================== *)
(* Size-info parsing (for legacy format "path(WxH)")                          *)
(* ========================================================================== *)

(** [parse_size_info s] parses a string in format "path(WxH)" returning
    the path and dimensions. Returns Error if parsing fails. *)
let parse_size_info conf s =
  let len = String.length s in
  if len > 0 && s.[len - 1] = ')' then
    try
      let pos1 = String.index s '(' in
      let pos2 = String.index_from s pos1 'x' in
      let w = String.sub s (pos1 + 1) (pos2 - pos1 - 1) |> int_of_string in
      let h = String.sub s (pos2 + 1) (len - pos2 - 2) |> int_of_string in
      let path_str = String.sub s 0 pos1 in
      Ok (parse_url_or_path conf path_str, (w, h))
    with Not_found | Failure _ -> Error "Failed to parse size info"
  else Error "Not a size-info string"

(** [has_size_info s] checks if string ends with size info format "(WxH)" *)
let has_size_info s =
  let len = String.length s in
  len > 0 && s.[len - 1] = ')'
