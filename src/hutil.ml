(* camlp5r ./pa_html.cmo *)
(* $Id: hutil.ml,v 5.11 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

open Config
open Printf

let up_fname conf = "up.png"

let commd_no_params conf =
  conf.command ^ "?" ^
  List.fold_left
    (fun c (k, v) ->
       c ^ (if c = "" then "" else ";") ^ k ^
       (if v = "" then "" else "=" ^ v))
    "" conf.henv

let link_to_referer conf =
  let referer = Util.get_referer conf in
  if referer <> "" then
    let fname = "left.png" in
    let wid_hei =
      match Util.image_size (Util.image_file_name fname) with
        Some (wid, hei) ->
          " width=\"" ^ string_of_int wid ^ "\" height=\"" ^
          string_of_int hei ^ "\""
      | None -> ""
    in
    sprintf
      "<a href=\"%s\"><img src=\"%s/%s\"%s alt=\"&lt;&lt;\" title=\"&lt;&lt;\"%s></a>\n"
      referer (Util.image_prefix conf) fname wid_hei conf.xhs
  else ""

let gen_print_link_to_welcome f conf right_aligned =
  if conf.cancel_links then ()
  else
    let fname = up_fname conf in
    let wid_hei =
      match Util.image_size (Util.image_file_name fname) with
        Some (wid, hei) ->
          " width=\"" ^ string_of_int wid ^ "\" height=\"" ^
          string_of_int hei ^ "\""
      | None -> ""
    in
    if right_aligned then
      Wserver.wprint "<div style=\"float:%s\">\n" conf.right
    else Wserver.wprint "<p>\n";
    f ();
    let str = link_to_referer conf in
    if str = "" then () else Wserver.wprint "%s" str;
    Wserver.wprint "<a href=\"%s\">" (commd_no_params conf);
    Wserver.wprint "<img src=\"%s/%s\"%s alt=\"^^\" title=\"^^\"%s>"
      (Util.image_prefix conf) fname wid_hei conf.xhs;
    Wserver.wprint "</a>\n";
    if right_aligned then Wserver.wprint "</div>\n"
    else Wserver.wprint "</p>\n"

let print_link_to_welcome = gen_print_link_to_welcome (fun () -> ())

let header_without_http conf title =
  Wserver.wprint "%s\n" (Util.doctype conf);
  Wserver.wprint "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n";
  Wserver.wprint "<head>\n";
  Wserver.wprint "  <title>";
  title true;
  Wserver.wprint "</title>\n";
  Wserver.wprint "  <meta name=\"robots\" content=\"none\"%s>\n" conf.xhs;
  Wserver.wprint "  <meta http-equiv=\"Content-Type\" \
                    content=\"text/html; charset=%s\"%s>\n"
    conf.charset conf.xhs;
  Wserver.wprint
    "  <meta http-equiv=\"Content-Style-Type\" content=\"text/css\"%s>\n"
    conf.xhs;
  Wserver.wprint
    "  <link rel=\"shortcut icon\" href=\"%s/favicon_gwd.png\"%s>\n"
    (Util.image_prefix conf) conf.xhs;
  begin match Util.open_templ conf "css" with
    Some ic -> Templ.copy_from_templ conf [] ic
  | None -> ()
  end;
  begin match Util.open_templ conf "js" with
    Some ic -> Templ.copy_from_templ conf [] ic
  | None -> ()
  end;
  Templ.include_hed_trl conf None "hed";
  Wserver.wprint "</head>\n";
  let s =
    try " dir=\"" ^ Hashtbl.find conf.lexicon " !dir" ^ "\"" with
      Not_found -> ""
  in
  let s = s ^ Util.body_prop conf in
  Wserver.wprint "<body%s>" s;
  Wserver.wprint "\n";
  Util.message_to_wizard conf

let header_without_page_title conf title =
  Util.html conf; Util.nl (); header_without_http conf title

let header_link_welcome conf title =
  header_without_page_title conf title;
  print_link_to_welcome conf true;
  Wserver.wprint "<h1>";
  title false;
  Wserver.wprint "</h1>\n"

let header_no_page_title conf title =
  header_without_page_title conf title;
  match Util.p_getenv conf.env "title" with
    None | Some "" -> ()
  | Some x -> Wserver.wprint "<h1>%s</h1>\n" x

let header conf title =
  header_without_page_title conf title;
  Wserver.wprint "<h1>";
  title false;
  Wserver.wprint "</h1>\n"

let red_color = "red"

let rheader conf title =
  header_without_page_title conf title;
  Wserver.wprint "<h1 class=\"error\">";
  title false;
  Wserver.wprint "</h1>\n"

let gen_trailer with_logo conf =
  Templ.include_hed_trl conf None "trl";
  if with_logo then Templ.print_copyright_with_logo conf
  else Templ.print_copyright conf;
  Wserver.wprint "</body>\n</html>\n"

let trailer = gen_trailer true

let incorrect_request conf =
  let title _ =
    Wserver.wprint "%s" (Util.capitale (Util.transl conf "incorrect request"))
  in
  header conf title;
  Wserver.wprint "<p>\n";
  print_link_to_welcome conf false;
  Wserver.wprint "</p>\n";
  trailer conf

let error_cannot_access conf fname =
  let title _ = Wserver.wprint "Error" in
  header conf title;
  Wserver.wprint "<ul>\n";
  Wserver.wprint "<li>\n";
  Wserver.wprint "Cannot access file \"%s.txt\".\n" fname;
  Wserver.wprint "</li>\n";
  Wserver.wprint "</ul>\n";
  trailer conf

let gen_interp header conf base fname ifun env ep =
  let v = !(Templ.template_file) in
  Templ.template_file := fname;
  begin try
    match Templ.input_templ conf fname with
      Some astl ->
        if header then begin Util.html conf; Util.nl () end;
        Templ.interp_ast conf (Some base) ifun env ep astl
    | None -> error_cannot_access conf fname
  with e -> Templ.template_file := v; raise e
  end;
  Templ.template_file := v

let interp_no_header conf base fname ifun env ep =
  gen_interp false conf base fname ifun env ep

let interp conf base fname ifun env ep =
  gen_interp true conf base fname ifun env ep

