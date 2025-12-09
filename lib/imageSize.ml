(* Copyright (c) 1998-2007 INRIA *)
(* Copyright (c) 2025 - Refactored with ImageUtil *)

(** Image dimension detection from binary headers.
    This module reads image file headers to extract dimensions
    without loading the full image into memory.
    
    PHASE 1-2 CHANGES:
    - Uses ImageUtil.StringUtil.find_substring (eliminates duplication)
    - Uses ImageUtil.Constants for magic numbers
    - Uses ImageUtil.StringUtil.safe_sub for bounds checking *)

open ImageTypes
open ImageUtil

(* ========================================================================== *)
(* Format detection from binary content                                       *)
(* ========================================================================== *)

(** [detect_format content] detects the image format from binary content
    by examining magic bytes at the start of the file.
    
    @param content Raw binary content (at least first 10 bytes needed)
    @return Some format if recognized, None otherwise *)
let detect_format content =
  let len = String.length content in
  if len < Constants.PNG.magic_size then None
  else if
    len >= Constants.PNG.magic_size
    && String.sub content 0 Constants.PNG.magic_size = Constants.PNG.magic
  then Some PNG
  else if
    len >= Constants.GIF.magic_size
    && String.sub content 0 Constants.GIF.magic_size = Constants.GIF.magic
  then Some GIF
  else if
    len >= Constants.JPEG.min_header_size
    && Char.code content.[0] = Constants.JPEG.magic_byte1
    && Char.code content.[1] = Constants.JPEG.magic_byte2
  then Some JPEG
  else None

(** [detect_format_lenient content] tries harder to find image data
    by searching for magic bytes within the content (handles some
    malformed uploads with spurious headers). *)
let detect_format_lenient content =
  match detect_format content with
  | Some _ as result -> result
  | None ->
      (* Look for JFIF marker for JPEG - using utility function *)
      (match StringUtil.find_substring content Constants.JPEG.jfif_marker with
      | Some i when i >= Constants.JPEG.jfif_offset -> Some JPEG
      | _ -> (
          (* Look for PNG signature *)
          match StringUtil.find_substring content Constants.PNG.magic with
          | Some _ -> Some PNG
          | None -> (
              (* Look for GIF signature *)
              match StringUtil.find_substring content Constants.GIF.magic with
              | Some _ -> Some GIF
              | None -> None)))

(** [extract_content_from_offset content format] extracts clean image data
    starting from the detected format signature. Used to clean up
    malformed uploads. *)
let extract_content_from_offset content format =
  match format with
  | JPEG -> (
      match StringUtil.find_substring content Constants.JPEG.jfif_marker with
      | Some i when i >= Constants.JPEG.jfif_offset -> (
          match
            StringUtil.safe_sub content (i - Constants.JPEG.jfif_offset)
              (String.length content - i + Constants.JPEG.jfif_offset)
          with
          | Ok result -> Some result
          | Error _ -> Some content)
      | _ -> Some content)
  | PNG -> (
      match StringUtil.find_substring content Constants.PNG.magic with
      | Some i -> (
          match
            StringUtil.safe_sub content i (String.length content - i)
          with
          | Ok result -> Some result
          | Error _ -> Some content)
      | None -> Some content)
  | GIF -> (
      match StringUtil.find_substring content Constants.GIF.magic with
      | Some i -> (
          match
            StringUtil.safe_sub content i (String.length content - i)
          with
          | Ok result -> Some result
          | Error _ -> Some content)
      | None -> Some content)

(* ========================================================================== *)
(* Dimension reading from file channels                                       *)
(* ========================================================================== *)

(** [read_png_size ic] reads dimensions from a PNG file.
    PNG stores dimensions at bytes 16-23 as 32-bit big-endian integers. *)
let read_png_size ic =
  try
    let magic = really_input_string ic Constants.PNG.magic_size in
    if magic = Constants.PNG.magic then (
      seek_in ic Constants.PNG.dimensions_offset;
      let w = input_binary_int ic in
      let h = input_binary_int ic in
      Some (w, h))
    else None
  with End_of_file | Sys_error _ -> None

(** [read_gif_size ic] reads dimensions from a GIF file.
    GIF stores dimensions at bytes 6-9 as 16-bit little-endian integers. *)
let read_gif_size ic =
  try
    let magic = really_input_string ic Constants.GIF.magic_size in
    if magic = Constants.GIF.magic then (
      seek_in ic Constants.GIF.dimensions_offset;
      let w =
        let lo = input_byte ic in
        let hi = input_byte ic in
        (hi * 256) + lo
      in
      let h =
        let lo = input_byte ic in
        let hi = input_byte ic in
        (hi * 256) + lo
      in
      Some (w, h))
    else None
  with End_of_file | Sys_error _ -> None

(** [read_jpeg_size ic] reads dimensions from a JPEG file.
    JPEG is more complex - dimensions are in SOF0 or SOF3 markers. *)
let read_jpeg_size ic =
  try
    let magic = really_input_string ic Constants.JPEG.min_header_size in
    (* Check JPEG magic: FF D8 and JFIF or Exif marker *)
    if
      Char.code magic.[0] = Constants.JPEG.magic_byte1
      && Char.code magic.[1] = Constants.JPEG.magic_byte2
      &&
      let marker = String.sub magic Constants.JPEG.jfif_offset 4 in
      marker = Constants.JPEG.jfif_marker
      || marker = Constants.JPEG.exif_marker
    then
      let is_exif =
        String.sub magic Constants.JPEG.jfif_offset 4
        = Constants.JPEG.exif_marker
      in
      (* Skip through markers looking for SOF0 (0xC0) or SOF3 (0xC3) *)
      let rec find_sof found =
        (* Skip to next FF marker *)
        while Char.code (input_char ic) <> Constants.JPEG.magic_byte1 do
          ()
        done;
        (* Skip any padding FF bytes *)
        let rec skip_ff () =
          let ch = input_char ic in
          if Char.code ch = Constants.JPEG.magic_byte1 then skip_ff () else ch
        in
        let marker = skip_ff () in
        let marker_code = Char.code marker in
        if
          marker_code = Constants.JPEG.sof0_marker
          || marker_code = Constants.JPEG.sof3_marker
        then
          (* For Exif, skip first SOF marker *)
          if is_exif && not found then find_sof true
          else (
            (* Skip 3 bytes, then read height and width *)
            for _ = 1 to 3 do
              ignore (input_char ic)
            done;
            let h_hi = input_char ic in
            let h_lo = input_char ic in
            let w_hi = input_char ic in
            let w_lo = input_char ic in
            let w = (Char.code w_hi lsl 8) lor Char.code w_lo in
            let h = (Char.code h_hi lsl 8) lor Char.code h_lo in
            Some (w, h))
        else if marker_code = Constants.JPEG.sos_marker then
          (* Start of scan - give up *)
          None
        else
          (* Skip this marker's data *)
          let len_hi = input_char ic in
          let len_lo = input_char ic in
          let len = (Char.code len_hi lsl 8) lor Char.code len_lo in
          let len = if len >= 32768 then 0 else len in
          for _ = 1 to len - 2 do
            ignore (input_char ic)
          done;
          find_sof found
      in
      find_sof false
    else None
  with End_of_file | Sys_error _ -> None

(* ========================================================================== *)
(* Public API                                                                 *)
(* ========================================================================== *)

(** [size_from_file path] reads image dimensions from a file.
    
    @param path Full path to the image file
    @return Some (width, height) if successful, None otherwise *)
let size_from_file path =
  if path = "" then None
  else
    try
      let ic = Secure.open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () ->
          match String.lowercase_ascii (Filename.extension path) with
          | ".jpg" | ".jpeg" -> read_jpeg_size ic
          | ".png" -> read_png_size ic
          | ".gif" -> read_gif_size ic
          | _ -> None)
    with Sys_error _ -> None

(** [size_from_source source] reads dimensions from an image source.
    Only works for Path sources; returns None for URLs. *)
let size_from_source = function
  | Path path -> size_from_file path
  | Url _ -> None

(** [scale_to_fit ~max_w ~max_h (w, h)] scales dimensions to fit within
    the given bounds while preserving aspect ratio.
    
    @param max_w Maximum width
    @param max_h Maximum height
    @param (w, h) Original dimensions
    @return Scaled (width, height) *)
let scale_to_fit ~max_w ~max_h (w, h) =
  (* First constrain height *)
  let w, h = if h > max_h then (w * max_h / h, max_h) else (w, h) in
  (* Then constrain width *)
  if w > max_w then (max_w, h * max_w / w) else (w, h)

(** [make_image_with_size source] creates an image_with_size record,
    attempting to read dimensions for Path sources. *)
let make_image_with_size source =
  let size = size_from_source source in
  { source; size }
