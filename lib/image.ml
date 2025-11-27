(* Copyright (c) 1998-2007 INRIA *)
(* Copyright (c) 2025 - Refactored *)

(** Backward-compatible API for image handling.
    
    This module re-exports functions from the refactored modules
    to maintain compatibility with existing code. New code should
    use the specific modules directly:
    - ImageTypes: Type definitions and constants
    - ImagePath: Path resolution and filename utilities
    - ImageSize: Image dimension detection
    - ImageAccess: Access control and retrieval
    - ImageOps: File operations *)

module Driver = Geneweb_db.Driver

(* ========================================================================== *)
(* Re-exported types (for backward compatibility)                             *)
(* ========================================================================== *)

(** Image source: either a file path or a URL *)
type image_source = [ `Path of string | `Url of string ]

(* ========================================================================== *)
(* Re-exported constants                                                      *)
(* ========================================================================== *)

let ext_list_1 = ImageTypes.image_extensions
let ext_list_2 = ImageTypes.all_extensions

(* ========================================================================== *)
(* Path utilities                                                             *)
(* ========================================================================== *)

let path_str = function
  | Some (`Path pa) -> pa
  | Some (`Url u) -> u
  | None -> ""

let portrait_folder conf = ImagePath.portrait_dir conf
let carrousel_folder conf = ImagePath.carrousel_dir conf

let default_image_filename_of_key mode first_name surname occ =
  if mode = "blasons" then
    ImagePath.blason_key_of_strings first_name surname occ
  else ImagePath.person_key_of_strings first_name surname occ

let default_image_filename mode base p =
  if mode = "blasons" then ImagePath.blason_key base p
  else ImagePath.person_key base p

let find_img_opt base_path =
  match ImagePath.find_with_extension base_path with
  | ImageTypes.Found (ImageTypes.Path p) -> Some (`Path p)
  | ImageTypes.Found (ImageTypes.Url u) -> Some (`Url u)
  | ImageTypes.Stopped | ImageTypes.NotFound -> None

let find_file_without_ext = ImagePath.find_file_with_extension

let path_of_filename conf fname =
  let fname1 = Filename.concat (ImagePath.carrousel_dir conf) fname in
  if Sys.file_exists fname1 then fname1
  else Util.search_in_assets (Filename.concat "images" fname)

let is_url = ImagePath.is_url

(* ========================================================================== *)
(* Size utilities                                                             *)
(* ========================================================================== *)

let png_size ic =
  match ImageSize.read_png_size ic with
  | Some (w, h) -> Ok (w, h)
  | None -> Error ()

let gif_size ic =
  match ImageSize.read_gif_size ic with
  | Some (w, h) -> Ok (w, h)
  | None -> Error ()

let jpeg_size ic =
  match ImageSize.read_jpeg_size ic with
  | Some (w, h) -> Ok (w, h)
  | None -> Error ()

let size_from_path fname =
  match ImageSize.size_from_file fname with
  | Some (w, h) -> Ok (w, h)
  | None -> Error ()

let scale_to_fit = ImageSize.scale_to_fit

(* ========================================================================== *)
(* Source conversion                                                          *)
(* ========================================================================== *)

let src_to_string = function `Url s | `Path s -> s

let urlorpath_of_string conf s =
  match ImagePath.parse_url_or_path conf s with
  | ImageTypes.Path p -> `Path p
  | ImageTypes.Url u -> `Url u

type src_result =
  [ `Empty | `Path of string | `Url of string | `Src_with_size_info of string ]

let src_of_string conf s : src_result =
  if s = "" then `Empty
  else if ImagePath.has_size_info s then `Src_with_size_info s
  else
    match ImagePath.parse_url_or_path conf s with
    | ImageTypes.Path p -> `Path p
    | ImageTypes.Url u -> `Url u

let parse_src_with_size_info conf (`Src_with_size_info s) =
  match ImagePath.parse_size_info conf s with
  | Ok (source, size) ->
      let src =
        match source with
        | ImageTypes.Path p -> `Path p
        | ImageTypes.Url u -> `Url u
      in
      Ok (src, size)
  | Error msg -> Error msg

(* ========================================================================== *)
(* Access control                                                             *)
(* ========================================================================== *)

let is_not_private_img _conf fname =
  not (Mutil.contains fname ("private" ^ Filename.dir_sep))

let has_access_to_image mode conf base p =
  let mode_typed =
    if mode = "blasons" then ImageTypes.Blason else ImageTypes.Portrait
  in
  ImageAccess.can_view_image conf base p ~mode:mode_typed

let has_access_to_carrousel conf base p =
  ImageAccess.can_view_image conf base p ~mode:ImageTypes.Carrousel

(* ========================================================================== *)
(* Image path resolution                                                      *)
(* ========================================================================== *)

let full_image_path mode conf base p saved =
  let mode_typed =
    if mode = "blasons" then ImageTypes.Blason else ImageTypes.Portrait
  in
  let key = ImagePath.key_for_mode base p mode_typed in
  let base_path = ImagePath.resolve_path conf ~key ~saved ~mode:mode_typed in
  match ImagePath.find_with_extension base_path with
  | ImageTypes.Found (ImageTypes.Path p) -> Some (`Path p)
  | ImageTypes.Found (ImageTypes.Url u) -> Some (`Url u)
  | ImageTypes.Stopped | ImageTypes.NotFound -> None

let get_portrait_path conf base p =
  match ImageAccess.get_portrait_path conf base p with
  | Some (ImageTypes.Path p) -> Some (`Path p)
  | Some (ImageTypes.Url u) -> Some (`Url u)
  | None -> None

(* ========================================================================== *)
(* Portrait retrieval                                                         *)
(* ========================================================================== *)

let get_portrait conf base p =
  match ImageAccess.get_portrait conf base p with
  | Some (ImageTypes.Path p) -> Some (`Path p)
  | Some (ImageTypes.Url u) -> Some (`Url u)
  | None -> None

let get_old_portrait conf base p =
  match ImageAccess.get_old_portrait conf base p with
  | Some (ImageTypes.Path p) -> Some (`Path p)
  | Some (ImageTypes.Url u) -> Some (`Url u)
  | None -> None

let get_portrait_name conf base p = ImageAccess.get_portrait_name conf base p

let get_old_portrait_name conf base p =
  ImageAccess.get_old_portrait_name conf base p

let get_portrait_with_size conf base p =
  match ImageAccess.get_portrait_with_size conf base p with
  | Some { ImageTypes.source; size } ->
      let src =
        match source with
        | ImageTypes.Path p -> `Path p
        | ImageTypes.Url u -> `Url u
      in
      Some (src, size)
  | None -> None

(* ========================================================================== *)
(* Blason retrieval                                                           *)
(* ========================================================================== *)

let get_blason conf base p self =
  match ImageAccess.get_blason conf base p ~self with
  | Some (ImageTypes.Path p) -> Some (`Path p)
  | Some (ImageTypes.Url u) -> Some (`Url u)
  | None -> None

let get_old_blason conf base p self =
  match ImageAccess.get_old_blason conf base p ~self with
  | Some (ImageTypes.Path p) -> Some (`Path p)
  | Some (ImageTypes.Url u) -> Some (`Url u)
  | None -> None

let get_blason_name conf base p = ImageAccess.get_blason_name conf base p
let get_old_blason_name conf base p = ImageAccess.get_old_blason_name conf base p
let has_blason conf base p self = ImageAccess.has_blason conf base p ~self
let has_blason_stop conf base p = ImageAccess.has_blason_stop conf base p

let get_blason_owner conf base p =
  ImageAccess.get_blason_owner conf base p

let get_blason_with_size conf base p self =
  match ImageAccess.get_blason_with_size conf base p ~self with
  | Some { ImageTypes.source; size } ->
      let src =
        match source with
        | ImageTypes.Path p -> `Path p
        | ImageTypes.Url u -> `Url u
      in
      Some (src, size)
  | None -> None

(* ========================================================================== *)
(* Combined portrait/blason                                                   *)
(* ========================================================================== *)

let get_old_portrait_or_blason conf base mode p =
  match ImageAccess.get_old_portrait_or_blason conf base mode p with
  | Some (ImageTypes.Path p) -> Some (`Path p)
  | Some (ImageTypes.Url u) -> Some (`Url u)
  | None -> None

(* ========================================================================== *)
(* Renaming                                                                   *)
(* ========================================================================== *)

let rename_portrait_and_blason conf base p new_key =
  ImageOps.rename_person_images conf base p new_key

(* ========================================================================== *)
(* Carrousel                                                                  *)
(* ========================================================================== *)

let carrousel_file_path conf base p fname old =
  ImagePath.carrousel_item_path conf base p ~filename:fname ~saved:old

let get_carrousel_file_content conf base p fname kind old =
  ImageAccess.get_carrousel_file_content conf base p ~filename:fname ~kind
    ~saved:old

let get_carrousel_imgs conf base p =
  ImageAccess.get_carrousel_imgs conf base p
  |> List.map (fun entry ->
         (entry.ImageTypes.path, entry.url, entry.source, entry.note))

let get_carrousel_old_imgs conf base p =
  ImageAccess.get_carrousel_old_imgs conf base p
  |> List.map (fun entry ->
         (entry.ImageTypes.path, entry.url, entry.source, entry.note))

(* ========================================================================== *)
(* Auxiliary functions used by imageCarrousel (kept for compatibility)        *)
(* ========================================================================== *)

(** Internal use: default filename with saved path option *)
let default_image_filename_aux mode base p saved =
  let key =
    if mode = "blasons" then ImagePath.blason_key base p
    else ImagePath.person_key base p
  in
  if saved then Filename.concat "saved" key else key
