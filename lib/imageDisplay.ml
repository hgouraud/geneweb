(* Copyright (c) 1998-2007 INRIA *)
(* Copyright (c) 2025 - Refactored *)

(** HTTP response generation for serving images.
    This module handles the HTTP layer for image delivery:
    - Content-type headers and caching
    - Serving files from disk
    - Redirecting to URLs
    - Placeholder images *)

open Config
open ImageTypes

module Logs = Geneweb_logs.Logs
module Driver = Geneweb_db.Driver

(* ========================================================================== *)
(* Placeholder images                                                         *)
(* ========================================================================== *)

(** [print_placeholder_gendered_portrait conf p size] outputs an HTML img tag
    for a gendered placeholder image *)
let print_placeholder_gendered_portrait conf p size =
  let image, alt =
    match Driver.get_sex p with
    | Def.Male -> ("male.png", "M")
    | Def.Female -> ("female.png", "F")
    | Def.Neuter -> ("sexunknown.png", "?")
  in
  Output.printf conf
    {|<img src="%s/%s" alt="%s" title="sex" width="%d" height="%d">|}
    (Util.images_prefix conf) image alt size size

(* ========================================================================== *)
(* HTTP response helpers                                                      *)
(* ========================================================================== *)

(** [send_content_headers conf ~content_type ~length ~filename] sends HTTP
    headers for file content with caching enabled *)
let send_content_headers conf ~content_type ~length ~filename =
  Output.status conf Def.OK;
  Output.header conf "Content-type: %s" content_type;
  Output.header conf "Content-length: %d" length;
  Output.header conf "Content-disposition: inline; filename=%s"
    (Filename.basename filename);
  (* Cache for one year - images rarely change *)
  Output.header conf "Cache-control: private, max-age=%d" (60 * 60 * 24 * 365);
  Output.flush conf

(** [send_redirect conf url] sends an HTTP redirect to a URL *)
let send_redirect conf url =
  Output.status conf Def.Moved_Temporarily;
  Output.header conf "Location: %s" url;
  Output.flush conf

(* ========================================================================== *)
(* File serving                                                               *)
(* ========================================================================== *)

(** [serve_file conf path] serves a file with appropriate content-type.
    
    @param conf Request configuration
    @param path Full path to the file
    @return Ok () on success, Error message on failure *)
let serve_file conf path =
  let ext = Filename.extension path in
  match ImageTypes.mime_of_extension ext with
  | None ->
      Error
        (Format.sprintf "Could not find mime type from extension for file: %s"
           path)
  | Some content_type -> (
      try
        let ic = Secure.open_in_bin path in
        Fun.protect
          ~finally:(fun () -> close_in ic)
          (fun () ->
            let len = in_channel_length ic in
            send_content_headers conf ~content_type ~length:len ~filename:path;
            (* Stream file in chunks *)
            let buf = Bytes.create 4096 in
            let rec stream remaining =
              if remaining > 0 then (
                let to_read = min (Bytes.length buf) remaining in
                really_input ic buf 0 to_read;
                Output.print_sstring conf (Bytes.sub_string buf 0 to_read);
                stream (remaining - to_read))
            in
            stream len;
            Ok ())
      with Sys_error msg ->
        Logs.syslog `LOG_ERR
          (Format.sprintf "Error serving file %s: %s" path msg);
        Error msg)

(** [serve_url conf url] generates an HTML page that displays an image from URL *)
let serve_url conf url =
  Util.html conf;
  Output.print_sstring conf "<html><head><title>";
  Output.print_sstring conf (Util.transl_nth conf "image/images" 0);
  Output.print_sstring conf "</title></head><body>";
  Output.printf conf {|<img src="%s">|} url;
  Output.print_sstring conf "</body></html>"

(** [serve_source conf source] serves an image from either Path or Url source *)
let serve_source conf = function
  | Path path -> serve_file conf path
  | Url url ->
      serve_url conf url;
      Ok ()

(* ========================================================================== *)
(* Portrait serving                                                           *)
(* ========================================================================== *)

(** [print_portrait conf base p] serves a person's portrait image *)
let print_portrait conf base p =
  match ImageAccess.get_portrait conf base p with
  | Some source -> (
      match serve_source conf source with
      | Ok () -> ()
      | Error _ ->
          Hutil.incorrect_request conf
            ~comment:"print_image_file failed (portrait)")
  | None -> Hutil.incorrect_request conf

(* ========================================================================== *)
(* Blason serving                                                             *)
(* ========================================================================== *)

(** [print_blason conf base p] serves a person's family blason image *)
let print_blason conf base p =
  match ImageAccess.get_blason conf base p ~self:false with
  | Some source -> (
      match serve_source conf source with
      | Ok () -> ()
      | Error _ -> Hutil.incorrect_request conf ~comment:"print_blason failed")
  | None -> Hutil.incorrect_request conf

(* ========================================================================== *)
(* Source/arbitrary image serving                                             *)
(* ========================================================================== *)

(** [print_source conf filename] serves an image from the images directory.
    Respects privacy settings for non-wizard/friend users. *)
let print_source conf filename =
  (* Remove leading slash if present *)
  let filename =
    if filename <> "" && filename.[0] = '/' then
      String.sub filename 1 (String.length filename - 1)
    else filename
  in
  let path = Filename.concat (ImagePath.carrousel_dir conf) filename in
  (* Check access permissions *)
  if
    conf.wizard || conf.friend
    || not (ImageAccess.is_private_path path)
  then
    match serve_file conf path with
    | Ok () -> ()
    | Error _ -> Hutil.incorrect_request conf ~comment:"print_source failed"
  else Hutil.incorrect_request conf ~comment:"access denied to private image"

(* ========================================================================== *)
(* Main entry points                                                          *)
(* ========================================================================== *)

(** [print conf base] main entry point for image requests (m=IM).
    Dispatches based on parameters:
    - s=<filename> : serve source file
    - p=<person> : serve person's portrait *)
let print conf base =
  match Util.p_getenv conf.env "s" with
  | Some filename -> print_source conf filename
  | None -> (
      match Util.find_person_in_env conf base "" with
      | Some p -> print_portrait conf base p
      | None -> Hutil.incorrect_request conf)

(** [print_blason_request conf base] entry point for blason requests (m=IMB).
    Dispatches based on parameters:
    - s=<filename> : serve source file
    - p=<person> : serve person's blason *)
let print_blason_request conf base =
  match Util.p_getenv conf.env "s" with
  | Some filename -> print_source conf filename
  | None -> (
      match Util.find_person_in_env conf base "" with
      | Some p -> print_blason conf base p
      | None -> Hutil.incorrect_request conf)

(** [print_html conf] serves image wrapped in HTML page (for DOCH requests).
    Blocks HTML and PDF files for security. *)
let print_html conf =
  let ext =
    match Util.p_getenv conf.env "s" with
    | Some f -> Filename.extension f
    | None -> ""
  in
  match ext with
  | ".htm" | ".html" | ".pdf" ->
      let title _ = Output.print_sstring conf "Error" in
      Hutil.header conf title;
      Output.print_sstring conf
        "<body><ul><li>DOCH not available for html and pdf.";
      Hutil.trailer conf
  | _ ->
      Util.html conf;
      Output.print_sstring conf "<html><head><title>";
      Output.print_sstring conf (Util.transl_nth conf "image/images" 0);
      Output.print_sstring conf "</title></head><body><img src=\"";
      Output.print_string conf (Util.commd conf);
      (* Rebuild query string with m=IM *)
      Mutil.list_iter_first
        (fun first (k, v) ->
          let v = if k = "m" then Adef.encoded "IM" else v in
          if not first then Output.print_sstring conf "&";
          Output.print_sstring conf k;
          Output.print_sstring conf "=";
          Output.print_string conf v)
        conf.env;
      Output.print_sstring conf "\"></body></html>"

(* ========================================================================== *)
(* Utility functions for other modules                                        *)
(* ========================================================================== *)

(** [print_image_file conf path] serves a file - exported for imageCarrousel *)
let print_image_file conf path = serve_file conf path
