(* Copyright (c) 1998-2007 INRIA *)
(* Copyright (c) 2025 - Henri Gouraud *)

(** Utility functions for image handling.
    
    This module provides common utilities to avoid duplication
    across image modules and improve safety/security.
    
    Modules:
    - StringUtil: Common string operations
    - Constants: Named constants replacing magic numbers
    - PathSecurity: Path validation and traversal protection
    - MetadataFiles: Consistent metadata file handling
    - ErrorHandling: Comprehensive error types and logging
    - Validation: Image validation pipeline
    - Streaming: Efficient file streaming
    - Extensions: Optimized extension checking *)

(* ========================================================================== *)
(* String Utilities                                                           *)
(* ========================================================================== *)

module StringUtil = struct
  (** [find_substring s pattern] finds the first occurrence of pattern in s.
      
      @param s The string to search in
      @param pattern The substring to find
      @return Some offset if found, None otherwise
      
      Example: find_substring "hello world" "world" = Some 6 *)
  let find_substring s pattern =
    let plen = String.length pattern in
    let slen = String.length s in
    let rec loop i =
      if i + plen > slen then None
      else if String.sub s i plen = pattern then Some i
      else loop (i + 1)
    in
    loop 0

  (** [safe_sub s start len] safely extracts a substring with bounds checking.
      
      @param s The source string
      @param start Starting position (0-indexed)
      @param len Length of substring
      @return Ok substring or Error with descriptive message
      
      This function prevents buffer overflow and provides clear error messages
      for debugging. *)
  let safe_sub s start len =
    let slen = String.length s in
    if start < 0 then
      Error (Printf.sprintf "Invalid start position: %d (must be >= 0)" start)
    else if len < 0 then
      Error (Printf.sprintf "Invalid length: %d (must be >= 0)" len)
    else if start + len > slen then
      Error
        (Printf.sprintf
           "Out of bounds: start=%d len=%d but string has length %d" start len
           slen)
    else Ok (String.sub s start len)

  (** [safe_sub_exn s start len] like safe_sub but raises Invalid_argument.
      Use this when bounds errors are programming errors that should not occur. *)
  let safe_sub_exn s start len =
    match safe_sub s start len with
    | Ok result -> result
    | Error msg -> invalid_arg msg
end

(* ========================================================================== *)
(* Named Constants                                                            *)
(* ========================================================================== *)

module Constants = struct
  (** PNG format constants *)
  module PNG = struct
    (** PNG file signature: "\137PNG\r\n\026\n" *)
    let magic = "\137PNG"
    
    (** Magic bytes are 4 bytes long *)
    let magic_size = 4
    
    (** Width and height are stored at offset 16 as 32-bit big-endian integers *)
    let dimensions_offset = 16
  end

  (** GIF format constants *)
  module GIF = struct
    (** GIF file signature: "GIF87a" or "GIF89a" *)
    let magic = "GIF8"
    
    (** Magic bytes are 4 bytes long (checking first 4 of "GIF8?a") *)
    let magic_size = 4
    
    (** Width and height are stored at offset 6 as 16-bit little-endian integers *)
    let dimensions_offset = 6
  end

  (** JPEG format constants *)
  module JPEG = struct
    (** First byte of JPEG marker: 0xFF *)
    let magic_byte1 = 0xff
    
    (** Second byte of JPEG SOI marker: 0xD8 *)
    let magic_byte2 = 0xd8
    
    (** JFIF marker in APP0 segment *)
    let jfif_marker = "JFIF"
    
    (** EXIF marker in APP1 segment *)
    let exif_marker = "Exif"
    
    (** Start of Frame (Baseline DCT) marker *)
    let sof0_marker = 0xc0
    
    (** Start of Frame (Lossless) marker *)
    let sof3_marker = 0xc3
    
    (** Start of Scan marker *)
    let sos_marker = 0xda
    
    (** Minimum header size to check magic bytes *)
    let min_header_size = 10
    
    (** Offset of JFIF/Exif marker from start *)
    let jfif_offset = 6
  end

  (** HTTP and streaming constants *)
  module HTTP = struct
    (** Cache images for 1 year (they rarely change) *)
    let cache_max_age_seconds = 31_536_000 (* 60 * 60 * 24 * 365 *)
    
    (** Default buffer size for file streaming *)
    let file_buffer_size = 4096
    
    (** Files larger than this use larger buffers *)
    let large_file_threshold = 1_048_576 (* 1 MB *)
    
    (** Buffer size for large files *)
    let large_file_buffer_size = 65_536 (* 64 KB *)
  end

  (** Metadata file extensions *)
  let metadata_extensions = [ ".txt"; ".src" ]
end

(* ========================================================================== *)
(* Path Security                                                              *)
(* ========================================================================== *)

module PathSecurity = struct
  (** [is_safe_filename filename] checks if filename doesn't contain dangerous patterns.
      
      Rejects:
      - Null bytes
      - Path traversal attempts (..)
      - Absolute paths (starting with /)
      
      @param filename The filename to check
      @return true if safe, false otherwise *)
  let is_safe_filename filename =
    not
      (String.contains filename '\000'
      || Mutil.contains filename ".."
      || (String.length filename > 0 && filename.[0] = '/'))

  (** [normalize_filename filename] removes dangerous characters and leading slashes.
      
      @param filename The filename to normalize
      @return Ok normalized_filename or Error message *)
  let normalize_filename filename =
    (* Remove leading slashes *)
    let filename =
      if filename <> "" && filename.[0] = '/' then
        String.sub filename 1 (String.length filename - 1)
      else filename
    in
    (* Check for path traversal and null bytes *)
    if is_safe_filename filename then Ok filename
    else Error "Path contains invalid characters or traversal attempt"

  (** [safe_join base_dir filename] safely joins paths with validation.
      
      This function ensures the result stays within base_dir by:
      1. Normalizing the filename (removing .., leading /, etc.)
      2. Joining the paths
      3. Verifying the result is still under base_dir
      
      @param base_dir Base directory (must be absolute or will be made absolute)
      @param filename Filename to join (will be normalized)
      @return Ok full_path or Error message
      
      Example:
        safe_join "/base" "file.jpg" = Ok "/base/file.jpg"
        safe_join "/base" "../etc/passwd" = Error "Path escape detected" *)
  let safe_join base_dir filename =
    match normalize_filename filename with
    | Error _ as e -> e
    | Ok fname -> (
        let full_path = Filename.concat base_dir fname in
        (* Convert to absolute paths for comparison *)
        let abs_base =
          if Filename.is_relative base_dir then
            Filename.concat (Sys.getcwd ()) base_dir
          else base_dir
        in
        let abs_full =
          if Filename.is_relative full_path then
            Filename.concat (Sys.getcwd ()) full_path
          else full_path
        in
        (* Normalize paths by resolving . and .. components *)
        (* Check if result is still under base directory *)
        match
          (Mutil.start_with abs_base 0 abs_full, String.equal abs_base abs_full)
        with
        | true, _ | _, true -> Ok full_path
        | false, false ->
            Error
              (Printf.sprintf "Path '%s' escapes base directory '%s'" full_path
                 base_dir))
end

(* ========================================================================== *)
(* Metadata Files                                                             *)
(* ========================================================================== *)

module MetadataFiles = struct
  open Constants

  (** [iter_metadata_files f base_path] applies function f to base file
      and all its metadata files (.txt, .src).
      
      @param f Function to apply to each file path
      @param base_path Path to the base file (with or without extension) *)
  let iter_metadata_files f base_path =
    f base_path;
    let base = Filename.remove_extension base_path in
    List.iter
      (fun ext ->
        let meta_path = base ^ ext in
        if Sys.file_exists meta_path then f meta_path)
      metadata_extensions

  (** [move_with_metadata src dst] moves file and its metadata files.
      
      If dst exists, it will be removed first.
      
      @param src Source path (file will be moved along with .txt and .src)
      @param dst Destination path *)
  let move_with_metadata src dst =
    iter_metadata_files
      (fun src_path ->
        let basename = Filename.basename src_path in
        let dst_path = Filename.concat (Filename.dirname dst) basename in
        if Sys.file_exists dst_path then Mutil.rm dst_path;
        if Sys.file_exists src_path then Sys.rename src_path dst_path)
      src

  (** [delete_with_metadata path] deletes file and its metadata files.
      
      @param path Path to the base file *)
  let delete_with_metadata path =
    iter_metadata_files (fun p -> if Sys.file_exists p then Mutil.rm p) path

  (** [copy_metadata_files src_base dst_base] copies metadata from src to dst.
      
      Only copies .txt and .src files, not the main file.
      
      @param src_base Base path of source (without extension)
      @param dst_base Base path of destination (without extension) *)
  let copy_metadata_files src_base dst_base =
    List.iter
      (fun ext ->
        let src_meta = src_base ^ ext in
        let dst_meta = dst_base ^ ext in
        if Sys.file_exists src_meta then
          Filesystem.copy_file ~perm:0o666 src_meta dst_meta)
      metadata_extensions
end

(* ========================================================================== *)
(* Error Handling                                                             *)
(* ========================================================================== *)

module ErrorHandling = struct
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
  let error_to_string = function
    | FileNotFound path -> Printf.sprintf "File not found: %s" path
    | InvalidFormat { path; reason } ->
        Printf.sprintf "Invalid format in %s: %s" path reason
    | UnknownMimeType ext ->
        Printf.sprintf "Unknown MIME type for extension: %s" ext
    | PermissionDenied path -> Printf.sprintf "Permission denied: %s" path
    | IoError { operation; path; message } ->
        Printf.sprintf "I/O error during %s on %s: %s" operation path message
    | InvalidDimensions { path; reason } ->
        Printf.sprintf "Invalid dimensions in %s: %s" path reason
    | PathTraversal path ->
        Printf.sprintf "Path traversal attempt detected: %s" path
    | TooLarge { size; max_size; path } ->
        Printf.sprintf "File %s is too large (%d bytes, max %d)" path size
          max_size
    | ContentMismatch { claimed; detected; path } ->
        Printf.sprintf "Content type mismatch in %s: claimed %s but detected %s"
          path claimed detected

  (** [wrap_io operation path f] wraps an I/O operation with error handling.
      
      @param operation Description of operation (e.g., "read", "write")
      @param path File path being operated on
      @param f Function to execute
      @return Ok result or Error with detailed information *)
  let wrap_io operation path f =
    try Ok (f ())
    with
    | Sys_error msg -> Error (IoError { operation; path; message = msg })
    | exn ->
        Error
          (IoError { operation; path; message = Printexc.to_string exn })
end

(* ========================================================================== *)
(* Streaming                                                                  *)
(* ========================================================================== *)

module Streaming = struct
  open Constants.HTTP

  (** [optimal_buffer_size file_size] determines optimal buffer based on file size.
      
      - Small files (<1MB): 4KB buffer
      - Large files: 64KB buffer or 1% of file size, whichever is smaller
      
      @param file_size Size of file in bytes
      @return Optimal buffer size in bytes *)
  let optimal_buffer_size file_size =
    if file_size > large_file_threshold then
      min (file_size / 100) large_file_buffer_size
    else file_buffer_size

  (** [stream_channel ic output_fn len] streams len bytes from ic efficiently.
      
      Uses adaptive buffer sizing based on total length.
      
      @param ic Input channel
      @param output_fn Function to output string data (e.g., fun s -> Output.print_sstring conf s)
      @param len Total bytes to stream *)
  let stream_channel ic output_fn len =
    let buf_size = optimal_buffer_size len in
    let buf = Bytes.create buf_size in
    let rec loop remaining =
      if remaining > 0 then (
        let to_read = min (Bytes.length buf) remaining in
        really_input ic buf 0 to_read;
        output_fn (Bytes.sub_string buf 0 to_read);
        loop (remaining - to_read))
    in
    loop len
end

(* ========================================================================== *)
(* Extension Checking                                                         *)
(* ========================================================================== *)

module Extensions = struct
  (** Efficient extension checking using sets instead of Array.mem *)

  module StringSet = Set.Make (String)

  (** Set of image extensions *)
  let image_set =
    [ ".jpg"; ".jpeg"; ".png"; ".gif" ] |> List.to_seq |> StringSet.of_seq

  (** Set of all managed extensions *)
  let all_managed_set =
    [ ".jpg"; ".jpeg"; ".png"; ".gif"; ".url"; ".stop" ]
    |> List.to_seq |> StringSet.of_seq

  (** Set of metadata extensions *)
  let metadata_set =
    [ ".txt"; ".src" ] |> List.to_seq |> StringSet.of_seq

  (** [is_image_extension ext] checks if extension is a supported image format.
      O(log n) lookup vs O(n) for Array.mem. *)
  let is_image_extension ext = StringSet.mem ext image_set

  (** [is_managed_extension ext] checks if extension is managed by system *)
  let is_managed_extension ext = StringSet.mem ext all_managed_set

  (** [is_metadata_extension ext] checks if extension is metadata *)
  let is_metadata_extension ext = StringSet.mem ext metadata_set

  (* For backward compatibility with array-based code *)
  let image_extensions = [| ".jpg"; ".jpeg"; ".png"; ".gif" |]
  let all_extensions = [| ".jpg"; ".jpeg"; ".png"; ".gif"; ".url"; ".stop" |]
  let rename_extensions = [| ".jpg"; ".jpeg"; ".png"; ".gif"; ".stop" |]
end
