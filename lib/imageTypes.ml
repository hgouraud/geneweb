(* Copyright (c) 1998-2007 INRIA *)
(* Copyright (c) 2025 - Refactored *)

(** Image-related type definitions and constants.
    This module centralizes all image types to avoid duplication
    and ensure consistency across image-handling modules. *)

(** Supported image formats with binary detection *)
type image_format = JPEG | PNG | GIF

(** Image source: either a local file path or a remote URL *)
type image_source = Path of string | Url of string

(** Image with optional dimensions *)
type image_with_size = { source : image_source; size : (int * int) option }

(** Result of looking for an image file *)
type image_lookup_result =
  | Found of image_source
  | Stopped  (** A .stop file was found, indicating inheritance should stop *)
  | NotFound

(** Image mode for different storage locations *)
type image_mode = Portrait | Blason | Carrousel

(** Carrousel image entry with metadata *)
type carrousel_entry = {
  path : string;  (** File path or filename for .url files *)
  url : string;  (** URL content if this is a .url reference, empty otherwise *)
  source : string;  (** Source/attribution text *)
  note : string;  (** Note/caption text *)
}

(** Supported image file extensions (actual image files) *)
let image_extensions = [| ".jpg"; ".jpeg"; ".png"; ".gif" |]

(** All managed file extensions including metadata files *)
let all_extensions = [| ".jpg"; ".jpeg"; ".png"; ".gif"; ".url"; ".stop" |]

(** Extensions to rename when changing person key *)
let rename_extensions = [| ".jpg"; ".jpeg"; ".png"; ".gif"; ".stop" |]

(** MIME type mapping from file extension *)
let mime_types =
  [
    (".jpg", "image/jpeg");
    (".jpeg", "image/jpeg");
    (".pjpeg", "image/jpeg");
    (".png", "image/png");
    (".gif", "image/gif");
    (".pdf", "application/pdf");
    (".htm", "text/html");
    (".html", "text/html");
  ]

(** [extension_of_format fmt] returns the canonical extension for an image format *)
let extension_of_format = function JPEG -> ".jpg" | PNG -> ".png" | GIF -> ".gif"

(** [mime_of_extension ext] returns the MIME type for a file extension *)
let mime_of_extension ext =
  List.assoc_opt (String.lowercase_ascii ext) mime_types

(** [string_of_mode mode] converts mode to string for path construction *)
let string_of_mode = function
  | Portrait -> "portraits"
  | Blason -> "blasons"
  | Carrousel -> "carrousel"

(** [mode_of_string s] parses a mode string *)
let mode_of_string = function
  | "portraits" -> Some Portrait
  | "blasons" -> Some Blason
  | "carrousel" -> Some Carrousel
  | _ -> None

(** [source_to_string src] extracts the string from an image source *)
let source_to_string = function Path s -> s | Url s -> s

(** [is_stop_file path] checks if a path refers to a .stop marker file *)
let is_stop_file path = Filename.extension path = ".stop"
