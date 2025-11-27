(* Copyright (c) 1998-2007 INRIA *)
(* Copyright (c) 2025 - Refactored *)

(** Image dimension detection from binary headers.
    This module reads image file headers to extract dimensions
    without loading the full image into memory. *)

open ImageTypes

(* ========================================================================== *)
(* Format detection from binary content                                       *)
(* ========================================================================== *)

(** [detect_format content] detects the image format from binary content
    by examining magic bytes at the start of the file.
    
    @param content Raw binary content (at least first 10 bytes needed)
    @return Some format if recognized, None otherwise *)
let detect_format content =
  let len = String.length content in
  if len < 4 then None
  else if String.sub content 0 4 = "\137PNG" then Some PNG
  else if String.sub content 0 4 = "GIF8" then Some GIF
  else if
    len >= 10
    && Char.code content.[0] = 0xff
    && Char.code content.[1] = 0xd8
  then
    (* JPEG magic: FF D8 *)
    Some JPEG
  else None

(** [detect_format_lenient content] tries harder to find image data
    by searching for magic bytes within the content (handles some
    malformed uploads with spurious headers). *)
let detect_format_lenient content =
  match detect_format content with
  | Some _ as result -> result
  | None ->
      (* Try to find JFIF marker for JPEG *)
      let rec find_substring s pattern i =
        if i + String.length pattern > String.length s then None
        else if String.sub s i (String.length pattern) = pattern then Some i
        else find_substring s pattern (i + 1)
      in
      (* Look for JFIF marker *)
      (match find_substring content "JFIF" 0 with
      | Some i when i >= 6 -> Some JPEG
      | _ -> (
          (* Look for PNG signature *)
          match find_substring content "\137PNG" 0 with
          | Some _ -> Some PNG
          | None -> (
              (* Look for GIF signature *)
              match find_substring content "GIF8" 0 with
              | Some _ -> Some GIF
              | None -> None)))

(** [extract_content_from_offset content format] extracts clean image data
    starting from the detected format signature. Used to clean up
    malformed uploads. *)
let extract_content_from_offset content format =
  let find_substring s pattern =
    let rec loop i =
      if i + String.length pattern > String.length s then None
      else if String.sub s i (String.length pattern) = pattern then Some i
      else loop (i + 1)
    in
    loop 0
  in
  match format with
  | JPEG -> (
      match find_substring content "JFIF" with
      | Some i when i >= 6 ->
          Some (String.sub content (i - 6) (String.length content - i + 6))
      | _ -> Some content)
  | PNG -> (
      match find_substring content "\137PNG" with
      | Some i -> Some (String.sub content i (String.length content - i))
      | None -> Some content)
  | GIF -> (
      match find_substring content "GIF8" with
      | Some i -> Some (String.sub content i (String.length content - i))
      | None -> Some content)

(* ========================================================================== *)
(* Dimension reading from file channels                                       *)
(* ========================================================================== *)

(** [read_png_size ic] reads dimensions from a PNG file.
    PNG stores dimensions at bytes 16-23 as 32-bit big-endian integers. *)
let read_png_size ic =
  try
    let magic = really_input_string ic 4 in
    if magic = "\137PNG" then (
      seek_in ic 16;
      let w = input_binary_int ic in
      let h = input_binary_int ic in
      Some (w, h))
    else None
  with End_of_file | Sys_error _ -> None

(** [read_gif_size ic] reads dimensions from a GIF file.
    GIF stores dimensions at bytes 6-9 as 16-bit little-endian integers. *)
let read_gif_size ic =
  try
    let magic = really_input_string ic 4 in
    if magic = "GIF8" then (
      seek_in ic 6;
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
    let magic = really_input_string ic 10 in
    (* Check JPEG magic: FF D8 and JFIF or Exif marker *)
    if
      Char.code magic.[0] = 0xff
      && Char.code magic.[1] = 0xd8
      &&
      let marker = String.sub magic 6 4 in
      marker = "JFIF" || marker = "Exif"
    then
      let is_exif = String.sub magic 6 4 = "Exif" in
      (* Skip through markers looking for SOF0 (0xC0) or SOF3 (0xC3) *)
      let rec find_sof found =
        (* Skip to next FF marker *)
        while Char.code (input_char ic) <> 0xff do
          ()
        done;
        (* Skip any padding FF bytes *)
        let rec skip_ff () =
          let ch = input_char ic in
          if Char.code ch = 0xff then skip_ff () else ch
        in
        let marker = skip_ff () in
        let marker_code = Char.code marker in
        if marker_code = 0xc0 || marker_code = 0xc3 then
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
        else if marker_code = 0xda then
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
