(* Copyright (c) 1998-2007 INRIA *)
(* Copyright (c) 2025 - Henri Gouraud *)

(** Utility functions for image handling.
    
    This module provides common utilities to avoid duplication
    across image modules and improve safety/security. *)

(** String manipulation utilities *)
module StringUtil : sig
  (** [find_substring s pattern] finds the first occurrence of pattern in s.
      @return Some offset if found, None otherwise *)
  val find_substring : string -> string -> int option

  (** [safe_sub s start len] safely extracts a substring with bounds checking.
      @return Ok substring or Error with descriptive message *)
  val safe_sub : string -> int -> int -> (string, string) result

  (** [safe_sub_exn s start len] like safe_sub but raises Invalid_argument *)
  val safe_sub_exn : string -> int -> int -> string
end

(** Named constants replacing magic numbers *)
module Constants : sig
  (** PNG format constants *)
  module PNG : sig
    val magic : string
    val magic_size : int
    val dimensions_offset : int
  end

  (** GIF format constants *)
  module GIF : sig
    val magic : string
    val magic_size : int
    val dimensions_offset : int
  end

  (** JPEG format constants *)
  module JPEG : sig
    val magic_byte1 : int
    val magic_byte2 : int
    val jfif_marker : string
    val exif_marker : string
    val sof0_marker : int
    val sof3_marker : int
    val sos_marker : int
    val min_header_size : int
    val jfif_offset : int
  end

  (** HTTP and streaming constants *)
  module HTTP : sig
    val cache_max_age_seconds : int
    val file_buffer_size : int
    val large_file_threshold : int
    val large_file_buffer_size : int
  end

  (** Metadata file extensions *)
  val metadata_extensions : string list
end

(** Path security and validation *)
module PathSecurity : sig
  (** [is_safe_filename filename] checks if filename doesn't contain dangerous patterns *)
  val is_safe_filename : string -> bool

  (** [normalize_filename filename] removes dangerous characters and leading slashes *)
  val normalize_filename : string -> (string, string) result

  (** [safe_join base_dir filename] safely joins paths with validation.
      Ensures the result stays within base_dir. *)
  val safe_join : string -> string -> (string, string) result
end

(** Metadata file operations *)
module MetadataFiles : sig
  (** [iter_metadata_files f base_path] applies f to base file and metadata *)
  val iter_metadata_files : (string -> unit) -> string -> unit

  (** [move_with_metadata src dst] moves file and its metadata *)
  val move_with_metadata : string -> string -> unit

  (** [delete_with_metadata path] deletes file and its metadata *)
  val delete_with_metadata : string -> unit

  (** [copy_metadata_files src_base dst_base] copies metadata from src to dst *)
  val copy_metadata_files : string -> string -> unit
end

(** Error handling *)
module ErrorHandling : sig
  (** Comprehensive error type for image operations *)
  type image_error =
    | FileNotFound of string
    | InvalidFormat of { path : string; reason : string }
    | UnknownMimeType of string
    | PermissionDenied of string
    | IoError of { operation : string; path : string; message : string }
    | InvalidDimensions of { path : string; reason : string }
    | PathTraversal of string
    | TooLarge of { size : int; max_size : int; path : string }
    | ContentMismatch of { claimed : string; detected : string; path : string }

  type 'a image_result = ('a, image_error) result

  (** [error_to_string err] converts error to human-readable message *)
  val error_to_string : image_error -> string

  (** [wrap_io operation path f] wraps an I/O operation with error handling *)
  val wrap_io : string -> string -> (unit -> 'a) -> 'a image_result
end

(** Efficient file streaming *)
module Streaming : sig
  (** [optimal_buffer_size file_size] determines optimal buffer based on file size *)
  val optimal_buffer_size : int -> int

  (** [stream_channel ic output_fn len] streams len bytes from ic efficiently.
      
      @param ic Input channel to read from
      @param output_fn Function to output string data (e.g., Output.print_sstring conf)
      @param len Total bytes to stream
      
      Example usage:
        stream_channel ic (fun s -> Output.print_sstring conf s) file_size
      or:
        stream_channel ic (Output.print_sstring conf) file_size *)
  val stream_channel : in_channel -> (string -> unit) -> int -> unit
end

(** Efficient extension checking *)
module Extensions : sig
  module StringSet : Set.S with type elt = string

  (** Set of image extensions *)
  val image_set : StringSet.t

  (** Set of all managed extensions *)
  val all_managed_set : StringSet.t

  (** Set of metadata extensions *)
  val metadata_set : StringSet.t

  (** [is_image_extension ext] checks if extension is a supported image format *)
  val is_image_extension : string -> bool

  (** [is_managed_extension ext] checks if extension is managed by system *)
  val is_managed_extension : string -> bool

  (** [is_metadata_extension ext] checks if extension is metadata *)
  val is_metadata_extension : string -> bool

  (* For backward compatibility *)
  val image_extensions : string array
  val all_extensions : string array
  val rename_extensions : string array
end
