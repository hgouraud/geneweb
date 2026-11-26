open Config
open Def
open Util

let src = Logs.Src.create ~doc:"ImageCarrousel" __MODULE__

module Log = (val Logs.src_log src : Logs.LOG)
module Driver = Geneweb_db.Driver
module Code = Geneweb_http.Code

let rn fname s =
  try if Sys.file_exists fname then Sys.rename fname s
  with Failure _ ->
    Printf.eprintf "Rn failed: %s to %s\n" fname s;
    flush stderr

type image_type = JPEG | GIF | PNG

let extension_of_type = function
  | JPEG -> ".jpg"
  | GIF -> ".gif"
  | PNG -> ".png"

(* Raised after a complete HTTP response has been sent, to cleanly unwind the
   call stack without triggering the server's generic exception handler (which
   would produce a blank "no content sent" page). *)
exception Done

let incorrect conf str =
  Log.err (fun k -> k "incorrect request: %s" str);
  Hutil.incorrect_request conf ~comment:str;
  raise Done

let incorrect_content_type conf base p s =
  Log.err (fun k -> k "incorrect image content type: %s" s);
  let title _ =
    Output.print_sstring conf (Utf8.capitalize (Util.transl conf "error"))
  in
  Hutil.header conf title;
  Output.print_sstring conf "<p>\n<em style=\"font-size:smaller\">";
  Output.printf conf "Error: incorrect image content type: %s" s;
  Output.printf conf "</em>\n</p>\n<ul>\n<li>\n%s</li>\n</ul>\n"
    (Util.referenced_person_title_text conf base p :> string);
  Hutil.trailer conf;
  Output.flush conf;
  raise Done

let error_too_big_image conf base p len max_len =
  Log.err (fun k -> k "image too big: %d bytes (max %d)" len max_len);
  let title _ =
    Output.print_sstring conf (Utf8.capitalize (Util.transl conf "error"))
  in
  Hutil.header ~error:true conf title;
  Output.print_sstring conf "<p><em style=\"font-size:smaller\">";
  Output.printf conf "Error: this image is too big: %d bytes<br>\n" len;
  Output.printf conf "Maximum authorized in this database: %d bytes<br>\n"
    max_len;
  Output.printf conf "</em></p>\n<ul>\n<li>\n%s</li>\n</ul>\n"
    (Util.referenced_person_title_text conf base p :> string);
  Hutil.trailer conf;
  Output.flush conf;
  raise Done

let insert_saved fname =
  let l = String.split_on_char Filename.dir_sep.[0] fname |> List.rev in
  let l = List.rev @@ match l with h :: t -> h :: "saved" :: t | _ -> l in
  String.concat Filename.dir_sep l

let write_file fname content =
  let oc = Secure.open_out_bin fname in
  output_string oc content;
  flush oc;
  close_out oc

let move_file_to_saved ?(sidecars = true) save_dir orig_file =
  try
    Filesystem.create_dir ~parent:true save_dir;
    if not (Sys.file_exists orig_file) then
      raise (Sys_error (Printf.sprintf "File not found: %s" orig_file));
    let saved_file = Filename.concat save_dir (Filename.basename orig_file) in
    if Sys.file_exists saved_file then Mutil.rm saved_file;
    rn orig_file saved_file;
    if sidecars then
      List.iter
        (fun ext ->
          let orig_ext = Filename.remove_extension orig_file ^ ext in
          let saved_ext = Filename.remove_extension saved_file ^ ext in
          if Sys.file_exists orig_ext then rn orig_ext saved_ext)
        [ ".txt"; ".src" ];
    1
  with exn ->
    let bt = Printexc.get_backtrace () in
    Log.err (fun k ->
        k "Error moving file to saved: %s\n%s" (Printexc.to_string exn) bt);
    0

let move_file_to_save dir file =
  move_file_to_saved
    (Filename.concat dir MediaDir.saved)
    (Filename.concat dir (Filename.basename file))

let create_blason_stop conf base p =
  let blason_dir = Layout.portraits conf.layout in
  let blason_stop =
    String.concat Filename.dir_sep
      [ blason_dir; Image.default_image_filename "blasons" base p ^ ".stop" ]
  in
  let oc = open_out blason_stop in
  close_out oc;
  History.record conf base
    (U_Send_image (Util.string_gen_person base (Driver.gen_person_of_person p)))
    "xb";
  blason_stop

(* A new blason of his own replaces the inherited one: the .stop marker,
   which Image.find_img_opt looks up before the image files, must go. *)
let remove_blason_stop conf base p =
  let blason_stop =
    String.concat Filename.dir_sep
      [
        Layout.portraits conf.layout;
        Image.default_image_filename "blasons" base p ^ ".stop";
      ]
  in
  if Sys.file_exists blason_stop then Mutil.rm blason_stop

let move_blason_file conf base src dst =
  let blason_dir = Layout.portraits conf.layout in
  let blason_src =
    String.concat Filename.dir_sep
      [ blason_dir; Image.get_blason_name conf base src ]
  in
  if
    Image.has_blason conf base src true
    && Sys.file_exists blason_src
    && not (Image.has_blason conf base dst true)
  then (
    let blason_dst =
      String.concat Filename.dir_sep
        [
          blason_dir;
          Image.default_image_filename "blasons" base dst
          ^ Filename.extension blason_src;
        ]
    in
    rn blason_src blason_dst;
    History.record conf base
      (U_Send_image
         (Util.string_gen_person base (Driver.gen_person_of_person dst)))
      "mb";
    blason_dst)
  else ""

let normal_image_type s =
  if String.length s > 10 && Char.code s.[0] = 0xff && Char.code s.[1] = 0xd8
  then Some JPEG
  else if String.length s > 4 && String.sub s 0 4 = "\137PNG" then Some PNG
  else if String.length s > 4 && String.sub s 0 4 = "GIF8" then Some GIF
  else None

let string_search s v =
  let rec loop i j =
    if j = String.length v then Some (i - String.length v)
    else if i = String.length s then None
    else if s.[i] = v.[j] then loop (i + 1) (j + 1)
    else loop (i + 1) 0
  in
  loop 0 0

let extract_filename_from_url url =
  try
    let without_protocol =
      match string_search url "://" with
      | Some pos -> String.sub url (pos + 3) (String.length url - pos - 3)
      | None -> url
    in
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

(* get the image type, possibly removing spurious header *)

let image_type s =
  match normal_image_type s with
  | Some t -> Some (t, s)
  | None -> (
      match string_search s "JFIF" with
      | Some i when i > 6 ->
          let s = String.sub s (i - 6) (String.length s - i + 6) in
          Some (JPEG, s)
      | _ -> (
          match string_search s "\137PNG" with
          | Some i ->
              let s = String.sub s i (String.length s - i) in
              Some (PNG, s)
          | _ -> (
              match string_search s "GIF8" with
              | Some i ->
                  let s = String.sub s i (String.length s - i) in
                  Some (GIF, s)
              | None -> None)))

let dump_bad_image conf s =
  match List.assoc_opt "dump_bad_images" conf.base_env with
  | Some "yes" -> (
      try
        (* Where will "bad-image"end up? *)
        let oc = Secure.open_out_bin "bad-image" in
        output_string oc s;
        flush oc;
        close_out oc
      with Sys_error _ -> ())
  | _ -> ()

(* swap files between new and old folder *)
(* [| ".jpg"; ".jpeg"; ".png"; ".gif" |] *)

let swap_files_aux dir file old_file =
  let ext = Filename.extension file in
  let old_ext = Filename.extension old_file in
  let tmp_file = String.concat Filename.dir_sep [ dir; "tempfile.tmp" ] in
  if ext <> old_ext then (
    rn file (Filename.remove_extension old_file ^ ext);
    rn old_file (Filename.remove_extension file ^ old_ext))
  else (
    rn file tmp_file;
    rn old_file file;
    rn tmp_file old_file)

let swap_files file old_file =
  let dir = Filename.dirname file in
  swap_files_aux dir file old_file;
  let txt_file = Filename.remove_extension file ^ ".txt" in
  let old_file = Filename.remove_extension old_file ^ ".txt" in
  swap_files_aux dir txt_file old_file;
  let src_file = Filename.remove_extension file ^ ".src" in
  let old_file = Filename.remove_extension old_file ^ ".src" in
  swap_files_aux dir src_file old_file

let known_extensions = Array.append Image.ext_list_2 Image.ext_list_doc

(* TODO merge with Image.file_without_extension *)
let get_extension conf keydir mode saved fname =
  let t_ext = Filename.extension fname in
  let fname =
    if Array.mem t_ext known_extensions then Filename.remove_extension fname
    else fname
  in
  let dir =
    match mode with
    | "portraits" | "blasons" -> Layout.portraits conf.layout
    | _ -> Filename.concat (Layout.images conf.layout) keydir
  in
  let f =
    if saved then String.concat Filename.dir_sep [ dir; "saved"; fname ]
    else Filename.concat dir fname
  in
  Option.value ~default:"."
    (Mutil.array_find_map
       (fun ext -> if Sys.file_exists (f ^ ext) then Some ext else None)
       known_extensions)

(* ************************************************************************ *)
(*  send, delete, reset and print functions                                 *)
(*                                                                          *)
(* ************************************************************************ *)

(* Moves the portrait (or the blason, by [mode]) of his own of [p] to the
   saved images of [p], with its note and source. The url of the image field
   stays: only MOD_IND changes it. Raises [Sys_error]. *)
let save_portrait_or_blason conf base p mode =
  let stem =
    Filename.concat
      (Layout.portraits conf.layout)
      (Image.default_image_filename mode base p)
  in
  Array.iter
    (fun ext ->
      if Sys.file_exists (stem ^ ext) then (
        let saved = Image.saved_images_folder conf base p in
        Filesystem.create_dir ~parent:true saved;
        let now = Unix.localtime (Unix.time ()) in
        Image.move_with_sidecars stem ext
          (Filename.concat saved (Image.saved_portrait_stem saved mode now))))
    Image.portrait_extensions

let send_portrait ?code conf base p mode ext content =
  let dir = Layout.portraits conf.layout in
  let name = Image.default_image_filename mode base p in
  let written =
    try
      Filesystem.create_dir ~parent:true dir;
      if mode = "blasons" then remove_blason_stop conf base p;
      save_portrait_or_blason conf base p mode;
      if content <> "" then
        write_file (Filename.concat dir (name ^ ext)) content;
      Ok ()
    with (Sys_error _ | Failure _ | Unix.Unix_error _) as e ->
      Error (Printexc.to_string e)
  in
  if written = Ok () then
    History.record conf base
      (U_Send_image
         (Util.string_gen_person base (Driver.gen_person_of_person p)))
      (match code with
      | Some code -> code
      | None -> if mode = "blasons" then "sb" else "si");
  written

(* carrousel *)
let effective_send_c_ok conf base p file file_name =
  let mode =
    try (List.assoc "mode" conf.env :> string) with Not_found -> "portraits"
  in
  let image_url =
    Option.value ~default:"" (Util.p_getenv conf.env "image_url")
  in
  let image_name =
    let user_provided_name =
      Option.value ~default:"" (Util.p_getenv conf.env "image_name")
    in
    if user_provided_name = "" && image_url <> "" then
      extract_filename_from_url image_url
    else if user_provided_name = "" then ""
    else user_provided_name
  in
  let note =
    match Util.p_getenv conf.env "note" with
    | Some v ->
        Util.safe_html
          (Util.only_printable_or_nl (Mutil.strip_all_trailing_spaces v))
    | None -> Adef.safe ""
  in
  let source =
    match Util.p_getenv conf.env "source" with
    | Some v ->
        Util.safe_html
          (Util.only_printable_or_nl (Mutil.strip_all_trailing_spaces v))
    | None -> Adef.safe ""
  in
  let content =
    if mode = "note" || mode = "source" || image_url <> "" then "" else file
  in
  let typ, content =
    if content <> "" then
      match image_type content with
      | None ->
          let ct =
            Option.value ~default:"" (Util.p_getenv conf.env "file_type")
          in
          dump_bad_image conf content;
          incorrect_content_type conf base p ct
      | Some (typ, content) -> (
          match List.assoc_opt "max_images_size" conf.base_env with
          | Some len when String.length content > int_of_string len ->
              error_too_big_image conf base p (String.length content)
                (int_of_string len)
          | _ -> (typ, content))
    else (GIF, content (* we dont care which type, content = "" *))
  in
  if mode = "portraits" || mode = "blasons" then (
    (match send_portrait conf base p mode (extension_of_type typ) content with
    | Ok () -> ()
    | Error e -> incorrect conf ("effective send (portrait/blason): " ^ e));
    file_name)
  else
    let dir =
      Filename.concat
        (Layout.images conf.layout)
        (Image.default_image_filename mode base p)
    in
    Filesystem.create_dir ~parent:true dir;
    let fname = Filename.concat dir file_name in
    if content <> "" && Sys.file_exists fname then
      if move_file_to_save dir fname = 0 then
        incorrect conf "effective send (image)";
    let fname =
      if image_url <> "" then Filename.concat dir image_name ^ ".url" else fname
    in
    if content <> "" then
      try write_file fname content
      with _ ->
        incorrect conf
          (Printf.sprintf "effective send (writing content file %s)" fname)
    else if image_url <> "" then
      try write_file fname image_url
      with _ ->
        incorrect conf
          (Printf.sprintf "effective send (writing .url file %s)" fname)
    else ();
    if note <> Adef.safe "" then
      let fname = Filename.remove_extension fname ^ ".txt" in
      try write_file fname (note :> string)
      with _ ->
        incorrect conf
          (Printf.sprintf "effective send (writing .txt file %s)" fname)
    else ();
    if source <> Adef.safe "" then
      let fname = Filename.remove_extension fname ^ ".src" in
      try write_file fname (source :> string)
      with _ ->
        incorrect conf
          (Printf.sprintf "effective send (writing .txt file %s)" fname)
    else ();
    let changed =
      U_Send_image (Util.string_gen_person base (Driver.gen_person_of_person p))
    in
    History.record conf base changed
      (match mode with
      | "carrousel" ->
          let image = file_name <> "" || image_url <> "" in
          if image && note <> Adef.safe "" && source <> Adef.safe "" then "s3"
          else if image then "sf"
          else if note <> Adef.safe "" then "so"
          else if source <> Adef.safe "" then "ss"
          else "sx"
      | "note" -> "so"
      | "source" -> "ss"
      | _ -> "s?");
    file_name

(* Delete *)
let print_delete_image conf base p =
  let title h =
    transl_nth conf "image/images" 0
    |> transl_decline conf "delete"
    |> Utf8.capitalize_fst |> Output.print_sstring conf;
    if not h then (
      let fn = Driver.p_first_name base p in
      let sn = Driver.p_surname base p in
      let occ = Driver.get_occ p in
      Output.print_sstring conf (Util.transl conf ":");
      Output.print_sstring conf " ";
      Output.print_string conf (Util.escape_html fn);
      Output.print_sstring conf ".";
      Output.print_sstring conf (string_of_int occ);
      Output.print_sstring conf " ";
      Output.print_string conf (Util.escape_html sn))
  in
  Hutil.header conf title;
  Output.printf conf "<form method=\"post\" action=\"%s\">" conf.command;
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded "DEL_IMAGE_OK");
  Util.hidden_input conf "i"
    (Driver.get_iper p |> Driver.Iper.to_string |> Mutil.encode);
  Output.print_sstring conf
    {|<div class="mt-3"><button type="submit" class="btn btn-danger">|};
  transl_nth conf "validate/delete" 1
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf {|</button></div></form>|};
  Hutil.trailer conf

let print_deleted conf base p =
  let title _ =
    transl conf "image deleted"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.header conf title;
  Output.print_sstring conf "<ul><li>";
  Output.print_string conf (referenced_person_text conf base p);
  Output.print_sstring conf "</li></ul>";
  Hutil.trailer conf

let effective_delete_ok conf base p =
  (try save_portrait_or_blason conf base p "portraits"
   with Sys_error e -> incorrect conf ("effective delete ok: " ^ e));
  let changed =
    U_Delete_image (Util.string_gen_person base (Driver.gen_person_of_person p))
  in
  History.record conf base changed "dq";
  print_deleted conf base p

let print_del_ok conf base =
  try
    match Util.find_person_in_env conf base "" with
    | Some p -> effective_delete_ok conf base p
    | None -> incorrect conf "print del ok"
  with Done -> ()

let print_del conf base =
  match Util.find_person_in_env conf base "" with
  | None -> Hutil.incorrect_request conf
  | Some p -> (
      match Image.get_portrait conf base p with
      | Some _ -> print_delete_image conf base p
      | None -> Hutil.incorrect_request conf)

(* Removes the portrait, the blason or an image of the carrousel to the saved
   images of the person; with delete=on, deletes a saved image. *)
let effective_delete_c_ok conf base p =
  let keydir = Image.default_image_filename "portraits" base p in
  let mode =
    try (List.assoc "mode" conf.env :> string) with Not_found -> "portraits"
  in
  let delete =
    try List.assoc "delete" conf.env = Adef.encoded "on"
    with Not_found -> false
  in
  let fname =
    try List.assoc "file_name" conf.env |> Mutil.decode with Not_found -> ""
  in
  let changed =
    U_Delete_image (Util.string_gen_person base (Driver.gen_person_of_person p))
  in
  if mode = "portraits" || mode = "blasons" then (
    if delete then incorrect conf "saved portraits are saved images";
    (try
       if mode = "blasons" && Filename.extension fname = ".stop" then
         remove_blason_stop conf base p
       else save_portrait_or_blason conf base p mode
     with Sys_error e -> incorrect conf ("effective delete c_ok: " ^ e));
    History.record conf base changed (if mode = "blasons" then "db" else "dq");
    fname)
  else
    let dir = Filename.concat (Layout.images conf.layout) keydir in
    Filesystem.create_dir ~parent:true dir;
    let orig_file =
      if delete then
        String.concat Filename.dir_sep [ dir; MediaDir.saved; fname ]
      else Filename.concat dir fname
    in
    let orig_file =
      let ext = Filename.extension orig_file in
      if
        (ext = ".url" || Array.mem ext Image.ext_list_doc)
        && Sys.file_exists orig_file
      then orig_file
      else Filename.remove_extension orig_file |> Image.find_file_without_ext
    in
    let file = Filename.basename orig_file in
    if orig_file = "" then incorrect conf "empty file name"
    else if delete then Mutil.rm orig_file
    else if move_file_to_save dir file = 0 then
      incorrect conf ("effective delete c_ok (" ^ file ^ ")");
    History.record conf base changed (if delete then "ec" else "dc");
    fname

(* The portrait becomes the blason too: a copy of its file, or of the url of
   the image field. Returns the name of the blason, or "error". *)
let effective_copy_portrait_to_blason conf base p =
  let src =
    match Image.src_of_string conf (Driver.sou base (Driver.get_image p)) with
    | `Url u -> Some (".url", u)
    | `Empty | `Path _ | `Src_with_size_info _ -> (
        let stem =
          Filename.concat
            (Layout.portraits conf.layout)
            (Image.default_image_filename "portraits" base p)
        in
        match
          Array.find_opt
            (fun ext -> Sys.file_exists (stem ^ ext))
            Image.portrait_extensions
        with
        | Some ext ->
            Some
              (ext, In_channel.with_open_bin (stem ^ ext) In_channel.input_all)
        | None -> None)
  in
  match src with
  | None -> "error"
  | Some (ext, content) -> (
      match send_portrait ~code:"cb" conf base p "blasons" ext content with
      | Ok () -> Image.default_image_filename "blasons" base p ^ ext
      | Error e ->
          Log.err (fun k -> k "copy portrait to blason: %s" e);
          "error")

(* The portrait becomes the blason: copy it, move the portrait to the saved
   images (unless it is an url of the image field, which only MOD_IND can
   clear) and point the blason notes to the blason (m=FIM, or the url itself
   for an url portrait). Returns the blason file name and the names of the
   updated notes. *)
let effective_move_portrait_to_blason conf base p =
  let image_field_is_url =
    match Image.src_of_string conf (Driver.sou base (Driver.get_image p)) with
    | `Url _ -> true
    | `Empty | `Path _ | `Src_with_size_info _ -> false
  in
  let target =
    match Image.get_portrait conf base p with
    | Some (`Url u) -> BlasonNotes.Url u
    | Some (`Path _) | None -> BlasonNotes.Fim
  in
  let blason = effective_copy_portrait_to_blason conf base p in
  if blason = "error" then (blason, [])
  else (
    if not image_field_is_url then (
      (try save_portrait_or_blason conf base p "portraits"
       with Sys_error e -> incorrect conf ("move portrait to blason: " ^ e));
      History.record conf base
        (U_Delete_image
           (Util.string_gen_person base (Driver.gen_person_of_person p)))
        "dq");
    (blason, BlasonNotes.redirect_portrait_refs conf base p target))

(* The image [file_name] of the carrousel, or of the saved images with
   [saved=on], becomes the portrait or the blason (by [mode]) of [p], the
   previous one going to the saved images. An image of the carrousel is
   copied; a saved one moves, with its note and source, so that taking back a
   former portrait swaps it with the current one. Returns the name of the new
   file, or "error". *)
let effective_image_to conf base p mode =
  let fname = Option.value ~default:"" (Util.p_getenv conf.env "file_name") in
  let saved = Util.p_getenv conf.env "saved" = Some "on" in
  let dir =
    if saved then Image.saved_images_folder conf base p
    else
      Filename.concat
        (Image.carrousel_folder conf)
        (Image.default_image_filename "portraits" base p)
  in
  let ext = String.lowercase_ascii (Filename.extension fname) in
  let ext = if ext = ".jpeg" then ".jpg" else ext in
  let src = Filename.concat dir fname in
  if
    fname = ""
    || Filename.basename fname <> fname
    || (not (Array.mem ext Image.ext_list_1 || ext = ".url"))
    || not (Sys.file_exists src)
  then "error"
  else
    let content = In_channel.with_open_bin src In_channel.input_all in
    let code = if mode = "blasons" then "cd" else "ci" in
    match send_portrait ~code conf base p mode ext content with
    | Error e ->
        Log.err (fun k -> k "image %s to %s: %s" src mode e);
        "error"
    | Ok () ->
        let name = Image.default_image_filename mode base p in
        (if saved then
           try
             Mutil.rm src;
             let dst = Filename.concat (Layout.portraits conf.layout) name in
             List.iter
               (fun side ->
                 let f = Filename.remove_extension src ^ side in
                 if Sys.file_exists f then Sys.rename f (dst ^ side))
               [ ".txt"; ".src" ]
           with Sys_error e ->
             Log.err (fun k -> k "image %s to %s: %s" src mode e));
        name ^ ext

(* An image of the carrousel and its saved version swap. *)
let effective_reset_c_ok conf base p =
  let keydir = Image.default_image_filename "portraits" base p in
  let file_name =
    try List.assoc "file_name" conf.env |> Mutil.decode with Not_found -> ""
  in
  let ext = get_extension conf keydir "carrousel" false file_name in
  let old_ext = get_extension conf keydir "carrousel" true file_name in
  let t_ext = Filename.extension file_name in
  let file_name_no_ext =
    if Array.mem t_ext known_extensions then Filename.remove_extension file_name
    else file_name
  in
  let dir = Filename.concat (Layout.images conf.layout) keydir in
  let file_in_new =
    if ext <> "." then Filename.concat dir (file_name_no_ext ^ ext)
    else Filename.concat dir (file_name_no_ext ^ old_ext)
  in
  let file_in_old =
    if old_ext <> "." then
      String.concat Filename.dir_sep
        [ dir; MediaDir.saved; file_name_no_ext ^ old_ext ]
    else
      String.concat Filename.dir_sep
        [ dir; MediaDir.saved; file_name_no_ext ^ ext ]
  in
  swap_files file_in_new file_in_old;
  let changed =
    U_Send_image (Util.string_gen_person base (Driver.gen_person_of_person p))
  in
  History.record conf base changed "rc";
  file_name

(* ************************************************************************** *)
(*  [Fonc] print : Config.config -> Geneweb_db.Driver.base -> unit                         *)
(* ************************************************************************** *)

(* Carrousel image management with direct HTTP redirects:
   - Process POST action immediately
   - Redirect to main page with success message in URL parameters
   - Single, clean request cycle *)

let print_main_c conf base =
  try
    match Util.p_getenv conf.env "em" with
    | None -> (
        (* Process action and redirect with success message *)
        match Util.p_getenv conf.env "m" with
        | Some m -> (
            match Util.find_person_in_env conf base "" with
            | Some p -> (
                let updated_notes = ref [] in
                let processed_filename =
                  match m with
                  | "SND_IMAGE_OK" ->
                      let mode =
                        try (List.assoc "mode" conf.env :> string)
                        with Not_found -> "portraits"
                      in
                      let file_name =
                        try
                          List.assoc "file_name" conf.env
                          |> Mutil.decode |> String.lowercase_ascii
                        with Not_found -> ""
                      in
                      let file_name =
                        if file_name = "" then
                          try List.assoc "file_name_2" conf.env |> Mutil.decode
                          with Not_found -> ""
                        else file_name
                      in
                      let file_name =
                        (Mutil.decode (Adef.encoded file_name) :> string)
                      in
                      let file =
                        if mode = "note" || mode = "source" then ""
                        else
                          match Util.p_getenv conf.env "image_url" with
                          | Some url when url <> "" -> ""
                          | _ -> (
                              match List.assoc_opt "file" conf.env with
                              | Some file -> (file :> string)
                              | None -> "")
                      in
                      effective_send_c_ok conf base p file file_name
                  | "DEL_IMAGE_C_OK" -> effective_delete_c_ok conf base p
                  | "RESET_IMAGE_C_OK" -> effective_reset_c_ok conf base p
                  | "BLASON_MOVE_TO_ANC" ->
                      if Image.has_blason conf base p true then
                        match Util.p_getenv conf.env "ia" with
                        | Some ia ->
                            let fa =
                              Driver.poi base (Driver.Iper.of_string ia)
                            in
                            Filename.basename (move_blason_file conf base p fa)
                        | None -> ""
                      else ""
                  | "PORTRAIT_TO_BLASON" ->
                      effective_copy_portrait_to_blason conf base p
                  | "PORTRAIT_MOVE_TO_BLASON" ->
                      let blason, notes =
                        effective_move_portrait_to_blason conf base p
                      in
                      updated_notes := notes;
                      Filename.basename blason
                  | "IMAGE_TO_BLASON" ->
                      effective_image_to conf base p "blasons"
                  | "IMAGE_TO_PORTRAIT" ->
                      effective_image_to conf base p "portraits"
                  | "BLASON_STOP" ->
                      let has_blason_self = Image.has_blason conf base p true in
                      let has_blason = Image.has_blason conf base p false in
                      if has_blason && not has_blason_self then
                        Filename.basename (create_blason_stop conf base p)
                      else "error"
                  | _ -> "incorrect request"
                in

                (* HTTP redirect to main page with success message *)
                match processed_filename with
                | "error" -> Hutil.incorrect_request conf
                | "incorrect request" ->
                    Hutil.incorrect_request conf ~comment:"incorrect request"
                | _ ->
                    let mode =
                      try (List.assoc "mode" conf.env :> string)
                      with Not_found -> "portraits"
                    in
                    let display_filename =
                      if Filename.extension processed_filename = "" then
                        (* Pas d'extension, essayer de la récupérer *)
                        let keydir =
                          Image.default_image_filename "portraits" base p
                        in
                        let ext =
                          get_extension conf keydir mode false
                            processed_filename
                        in
                        if ext <> "." then processed_filename ^ ext
                        else processed_filename
                      else processed_filename
                    in
                    let base_url =
                      Printf.sprintf "%sm=MEDIA&%s&em=%s&file_name=%s&mode=%s"
                        (commd conf :> string)
                        (acces conf base p :> string)
                        (Mutil.encode m :> string)
                        (Mutil.encode display_filename :> string)
                        (Mutil.encode mode :> string)
                    in
                    let url_params = ref [] in
                    (try
                       if List.assoc "delete" conf.env = Adef.encoded "on" then
                         url_params := ("delete", "on") :: !url_params
                     with Not_found -> ());
                    (try
                       let fn2 =
                         List.assoc "file_name_2" conf.env |> Mutil.decode
                       in
                       if fn2 <> "" then
                         url_params := ("file_name_2", fn2) :: !url_params
                     with Not_found -> ());
                    if !updated_notes <> [] then
                      url_params :=
                        ("bn", String.concat " " !updated_notes) :: !url_params;
                    let params_string =
                      List.fold_left
                        (fun acc (key, value) ->
                          acc ^ "&" ^ key ^ "=" ^ (Mutil.encode value :> string))
                        "" !url_params
                    in
                    let redirect_url = base_url ^ params_string in
                    Output.status conf Code.Moved_Temporarily;
                    Output.header conf "Location: %s" redirect_url;
                    Output.header conf "Connection: close";
                    Output.flush conf)
            | None -> Hutil.incorrect_request conf ~comment:"missing person")
        | None -> Hutil.incorrect_request conf ~comment:"missing action")
    (* Display main page with success message from URL params *)
    | Some _ -> (
        match Util.find_person_in_env conf base "" with
        | Some p -> Perso.interp_templ "carrousel" conf base p
        | None ->
            Hutil.incorrect_request conf ~comment:"missing person in display")
  with Done -> ()

(* carrousel *)
let print_c ?(saved = false) ?(portrait = true) conf base =
  let mode = if portrait then "portraits" else "blasons" in
  match (Util.p_getenv conf.env "s", Util.find_person_in_env conf base "") with
  | Some f, Some p ->
      let k = Image.default_image_filename "portraits" base p in
      let f = Filename.concat k f in
      ImageDisplay.print_source conf base (if saved then insert_saved f else f)
  | Some f, _ -> ImageDisplay.print_source conf base f
  | _, Some p when not saved -> (
      match
        if mode = "portraits" then Image.get_portrait conf base p
        else Image.get_blason conf base p false
      with
      | Some (`Path f) ->
          Result.fold ~ok:ignore
            ~error:(fun e ->
              Log.err (fun k -> k "%s" e);
              Hutil.incorrect_request conf)
            (ImageDisplay.print_image_file conf f)
      | _ -> Hutil.incorrect_request conf)
  | _, _ -> Hutil.incorrect_request conf
