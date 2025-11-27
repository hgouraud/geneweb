(* Copyright (c) 1998-2007 INRIA *)
(* Copyright (c) 2025 - Refactored *)

(** Image access control and retrieval.
    This module handles:
    - Permission checks for viewing images
    - Retrieving portraits, blasons, and carrousel images
    - Handling both filesystem and database-stored image references *)

open Config
open ImageTypes

module Driver = Geneweb_db.Driver

(* ========================================================================== *)
(* Privacy and access control                                                 *)
(* ========================================================================== *)

(** [is_private_path path] checks if a path contains "private" directory *)
let is_private_path path =
  Mutil.contains path ("private" ^ Filename.dir_sep)

(** [can_view_image conf base p ~mode] determines if the current user
    has permission to view the specified type of image for person p.
    
    Access is granted if:
    - User is wizard or friend, OR
    - Images are not disabled AND person's age is authorized AND
      (for portraits/blasons: image exists and is not private)
      (for carrousel: person's name is not hidden) *)
let can_view_image conf base p ~mode =
  if conf.wizard || conf.friend then true
  else if conf.no_image then false
  else if not (Util.authorized_age conf base p) then false
  else
    match mode with
    | Portrait | Blason ->
        let img = Driver.get_image p in
        let has_db_image = not (Driver.Istr.is_empty img) in
        let has_file_image =
          let key = ImagePath.key_for_mode base p mode in
          let base_path =
            ImagePath.resolve_path conf ~key ~saved:false ~mode
          in
          match ImagePath.find_with_extension base_path with
          | Found _ -> true
          | Stopped | NotFound -> false
        in
        (has_db_image || has_file_image)
        && not (is_private_path (Driver.sou base img))
    | Carrousel -> not (Util.is_hide_names conf p)

(* ========================================================================== *)
(* Database image field parsing                                               *)
(* ========================================================================== *)

(** Source types from database image field *)
type db_image_source =
  | DbEmpty
  | DbUrl of string
  | DbPath of string
  | DbWithSize of string  (** Format: "path(WxH)" *)

(** [parse_db_image_field conf base p] parses the image field from the database *)
let parse_db_image_field _conf base p =
  let s = Driver.sou base (Driver.get_image p) in
  if s = "" then DbEmpty
  else if ImagePath.has_size_info s then DbWithSize s
  else if ImagePath.is_url s then DbUrl s
  else DbPath s

(** [resolve_db_image conf base p] resolves the database image field to a source *)
let resolve_db_image conf base p =
  match parse_db_image_field conf base p with
  | DbEmpty -> None
  | DbUrl url -> Some (Url url)
  | DbPath path_str ->
      let source = ImagePath.parse_url_or_path conf path_str in
      (match source with
      | Path p when Sys.file_exists p -> Some source
      | Path _ -> None
      | Url _ -> Some source)
  | DbWithSize s -> (
      match ImagePath.parse_size_info conf s with
      | Ok (source, _size) -> Some source
      | Error _ -> None)

(** [resolve_db_image_with_size conf base p] resolves with dimensions if available *)
let resolve_db_image_with_size conf base p =
  match parse_db_image_field conf base p with
  | DbEmpty -> None
  | DbUrl url -> Some { source = Url url; size = None }
  | DbPath path_str -> (
      let source = ImagePath.parse_url_or_path conf path_str in
      match source with
      | Path path when Sys.file_exists path ->
          Some { source; size = ImageSize.size_from_file path }
      | Path _ -> None
      | Url _ -> Some { source; size = None })
  | DbWithSize s -> (
      match ImagePath.parse_size_info conf s with
      | Ok (source, size) -> Some { source; size = Some size }
      | Error _ -> None)

(* ========================================================================== *)
(* Portrait retrieval                                                         *)
(* ========================================================================== *)

(** [get_portrait_path conf base p] returns the full filesystem path
    to a person's portrait if accessible and it exists. *)
let get_portrait_path conf base p =
  if not (can_view_image conf base p ~mode:Portrait) then None
  else
    let key = ImagePath.person_key base p in
    let base_path = ImagePath.resolve_path conf ~key ~saved:false ~mode:Portrait in
    match ImagePath.find_with_extension base_path with
    | Found (Path _ as source) -> Some source
    | Found (Url _ as source) -> Some source
    | Stopped | NotFound -> None

(** [get_portrait conf base p] retrieves a person's portrait.
    First checks the database image field, then falls back to filesystem. *)
let get_portrait conf base p =
  if not (can_view_image conf base p ~mode:Portrait) then None
  else
    (* First try database image field *)
    match resolve_db_image conf base p with
    | Some _ as result -> result
    | None ->
        (* Fall back to filesystem *)
        let key = ImagePath.person_key base p in
        let base_path =
          ImagePath.resolve_path conf ~key ~saved:false ~mode:Portrait
        in
        (match ImagePath.find_with_extension base_path with
        | Found source -> Some source
        | Stopped | NotFound -> None)

(** [get_portrait_with_size conf base p] retrieves portrait with dimensions *)
let get_portrait_with_size conf base p =
  if not (can_view_image conf base p ~mode:Portrait) then None
  else
    (* First try database image field *)
    match resolve_db_image_with_size conf base p with
    | Some _ as result -> result
    | None ->
        (* Fall back to filesystem *)
        let key = ImagePath.person_key base p in
        let base_path =
          ImagePath.resolve_path conf ~key ~saved:false ~mode:Portrait
        in
        (match ImagePath.find_with_extension base_path with
        | Found (Path path) ->
            Some { source = Path path; size = ImageSize.size_from_file path }
        | Found (Url url) -> Some { source = Url url; size = None }
        | Stopped | NotFound -> None)

(** [get_old_portrait conf base p] retrieves the saved/old version of a portrait *)
let get_old_portrait conf base p =
  if not (can_view_image conf base p ~mode:Portrait) then None
  else
    let key = ImagePath.person_key base p in
    let base_path = ImagePath.resolve_path conf ~key ~saved:true ~mode:Portrait in
    match ImagePath.find_with_extension base_path with
    | Found source -> Some source
    | Stopped | NotFound -> None

(** [get_portrait_name conf base p] returns the filename of the portrait *)
let get_portrait_name conf base p =
  let key = ImagePath.person_key base p in
  let base_path = ImagePath.resolve_path conf ~key ~saved:false ~mode:Portrait in
  match ImagePath.find_with_extension base_path with
  | Found (Path path) -> Filename.basename path
  | Found (Url _) -> key ^ ".url"
  | Stopped | NotFound -> ""

(** [get_old_portrait_name conf base p] returns the filename of the saved portrait *)
let get_old_portrait_name conf base p =
  let key = ImagePath.person_key base p in
  let base_path = ImagePath.resolve_path conf ~key ~saved:true ~mode:Portrait in
  match ImagePath.find_with_extension base_path with
  | Found (Path path) -> Filename.basename path
  | Found (Url _) -> key ^ ".url"
  | Stopped | NotFound -> ""

(* ========================================================================== *)
(* Blason retrieval                                                           *)
(* ========================================================================== *)

(** [get_blason conf base p ~self ~saved] retrieves a person's family blason.
    
    @param self If true, only return the person's own blason.
                If false, inherit from father if person has no blason.
    @param saved If true, look in saved directory *)
let get_blason_aux conf base p ~self ~saved =
  if not (can_view_image conf base p ~mode:Blason) then None
  else
    let rec loop p =
      let key = ImagePath.blason_key base p in
      let base_path = ImagePath.resolve_path conf ~key ~saved ~mode:Blason in
      match ImagePath.find_with_extension base_path with
      | Found (Path path) when ImageTypes.is_stop_file path -> None
      | Found source -> Some source
      | Stopped -> None
      | NotFound ->
          if self then None
          else
            (* Try to inherit from father *)
            (match Driver.get_parents p with
            | Some ifam ->
                let cpl = Driver.foi base ifam in
                let father = Driver.poi base (Driver.get_father cpl) in
                loop father
            | None -> None)
    in
    loop p

(** [get_blason conf base p ~self] retrieves a person's family blason. *)
let get_blason conf base p ~self = get_blason_aux conf base p ~self ~saved:false

(** [get_blason_with_size conf base p ~self] retrieves blason with dimensions *)
let get_blason_with_size conf base p ~self =
  match get_blason conf base p ~self with
  | None -> None
  | Some (Path path) ->
      Some { source = Path path; size = ImageSize.size_from_file path }
  | Some (Url url) -> Some { source = Url url; size = None }

(** [get_old_blason conf base p ~self] retrieves the saved version of a blason *)
let get_old_blason conf base p ~self = get_blason_aux conf base p ~self ~saved:true

(** [get_blason_name conf base p] returns the filename of the blason *)
let get_blason_name conf base p =
  let key = ImagePath.blason_key base p in
  let base_path = ImagePath.resolve_path conf ~key ~saved:false ~mode:Blason in
  match ImagePath.find_with_extension base_path with
  | Found (Path path) -> Filename.basename path
  | Found (Url _) -> key ^ ".url"
  | Stopped | NotFound -> ""

(** [get_old_blason_name conf base p] returns the filename of the saved blason *)
let get_old_blason_name conf base p =
  let key = ImagePath.blason_key base p in
  let base_path = ImagePath.resolve_path conf ~key ~saved:true ~mode:Blason in
  match ImagePath.find_with_extension base_path with
  | Found (Path path) -> Filename.basename path
  | Found (Url _) -> key ^ ".url"
  | Stopped | NotFound -> ""

(** [has_blason conf base p ~self] checks if a person has a blason *)
let has_blason conf base p ~self =
  match get_blason conf base p ~self with
  | None -> false
  | Some (Path path) -> not (ImageTypes.is_stop_file path)
  | Some (Url _) -> true

(** [has_blason_stop conf base p] checks if person has a .stop file for blason *)
let has_blason_stop conf base p =
  let key = ImagePath.blason_key base p in
  let base_path = ImagePath.resolve_path conf ~key ~saved:false ~mode:Blason in
  match ImagePath.find_with_extension base_path with
  | Found (Path path) -> ImageTypes.is_stop_file path
  | _ -> false

(** [get_blason_owner conf base p] finds the ancestor who owns the inherited blason *)
let get_blason_owner conf base p =
  if not (can_view_image conf base p ~mode:Blason) then None
  else
    let rec loop p =
      match Driver.get_parents p with
      | Some ifam ->
          let cpl = Driver.foi base ifam in
          let father_iper = Driver.get_father cpl in
          let father = Driver.poi base father_iper in
          if has_blason conf base father ~self:true then Some father_iper
          else loop father
      | None -> None
    in
    loop p

(* ========================================================================== *)
(* Combined portrait/blason retrieval                                         *)
(* ========================================================================== *)

(** [get_old_portrait_or_blason conf base mode p] retrieves saved version
    of either portrait or blason based on mode string *)
let get_old_portrait_or_blason conf base mode_str p =
  if not (can_view_image conf base p ~mode:Portrait) then None
  else
    let key =
      if mode_str = "blasons" then ImagePath.blason_key base p
      else ImagePath.person_key base p
    in
    let mode = if mode_str = "blasons" then Blason else Portrait in
    let base_path = ImagePath.resolve_path conf ~key ~saved:true ~mode in
    match ImagePath.find_with_extension base_path with
    | Found source -> Some source
    | Stopped | NotFound -> None

(* ========================================================================== *)
(* Carrousel retrieval                                                        *)
(* ========================================================================== *)

(** [get_carrousel_file_content conf base p ~filename ~kind ~saved]
    reads the content of a carrousel metadata file (.txt or .src) *)
let get_carrousel_file_content conf base p ~filename ~kind ~saved =
  let path = ImagePath.carrousel_item_path conf base p ~filename ~saved in
  let meta_path = Filename.chop_extension path ^ kind in
  if Sys.file_exists meta_path then
    try
      let ic = Secure.open_in meta_path in
      Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () ->
          let content = really_input_string ic (in_channel_length ic) in
          if content = "" then None else Some content)
    with Sys_error _ -> None
  else None

(** [get_carrousel_images conf base p ~saved] retrieves all carrousel images
    for a person.
    
    @param saved If true, retrieves from the "saved" subdirectory *)
let get_carrousel_images conf base p ~saved =
  if not (can_view_image conf base p ~mode:Carrousel) then []
  else
    let dir_path = ImagePath.carrousel_item_path conf base p ~filename:"" ~saved in
    if not (Sys.file_exists dir_path && Sys.is_directory dir_path) then []
    else
      try
        Sys.readdir dir_path
        |> Array.fold_left
             (fun acc filename ->
               let ext = Filename.extension filename in
               (* Skip hidden files and non-image files *)
               if
                 filename = ""
                 || filename.[0] = '.'
                 || not
                      (Array.mem ext ImageTypes.image_extensions || ext = ".url")
               then acc
               else
                 let base_path =
                   Filename.chop_extension
                     (ImagePath.carrousel_item_path conf base p ~filename ~saved)
                 in
                 match ImagePath.find_with_extension base_path with
                 | NotFound | Stopped -> acc
                 | Found (Path path) ->
                     let note =
                       Option.value ~default:""
                         (get_carrousel_file_content conf base p ~filename
                            ~kind:".txt" ~saved:false)
                     in
                     let source =
                       Option.value ~default:""
                         (get_carrousel_file_content conf base p ~filename
                            ~kind:".src" ~saved:false)
                     in
                     { path; url = ""; source; note } :: acc
                 | Found (Url url) ->
                     let note =
                       Option.value ~default:""
                         (get_carrousel_file_content conf base p ~filename
                            ~kind:".txt" ~saved:false)
                     in
                     let source =
                       Option.value ~default:""
                         (get_carrousel_file_content conf base p ~filename
                            ~kind:".src" ~saved:false)
                     in
                     let path =
                       Filename.chop_extension (Filename.basename filename)
                       ^ ".url"
                     in
                     { path; url; source; note } :: acc)
             []
      with Sys_error _ -> []

(** [get_carrousel_imgs conf base p] retrieves current carrousel images *)
let get_carrousel_imgs conf base p = get_carrousel_images conf base p ~saved:false

(** [get_carrousel_old_imgs conf base p] retrieves saved carrousel images *)
let get_carrousel_old_imgs conf base p = get_carrousel_images conf base p ~saved:true
