(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Gwcomp

let check_magic fname ic =
  let b = really_input_string ic (String.length magic_gwo) in
  if b <> magic_gwo then
    if String.sub magic_gwo 0 4 = String.sub b 0 4 then
      failwith
        ("\"" ^ fname ^ "\" is a GeneWeb object file, but not compatible")
    else
      failwith
        ("\"" ^ fname ^
         "\" is not a GeneWeb object file, or it is a very old version")

let next_family_fun_templ gwo_list fi =
  let ngwo = List.length gwo_list in
  let run =
    if ngwo < 10 || not !(Mutil.verbose) then fun () -> ()
    else if ngwo < 60 then fun () -> Printf.eprintf "."; flush stderr
    else
      let bar_cnt = ref 0 in
      let run () = ProgrBar.run !bar_cnt ngwo; incr bar_cnt in
      ProgrBar.empty := 'o'; ProgrBar.full := '*'; ProgrBar.start (); run
  in
  let ic_opt = ref None in
  let gwo_list = ref gwo_list in
  fun () ->
    let rec loop () =
      let r =
        match !ic_opt with
          Some ic ->
            begin match
              (try Some (input_value ic : gw_syntax) with End_of_file -> None)
            with
              Some fam -> Some fam
            | None -> close_in ic; ic_opt := None; None
            end
        | None -> None
      in
      match r with
        Some fam -> Some fam
      | None ->
          match !gwo_list with
            (x, separate, shift) :: rest ->
              run ();
              gwo_list := rest;
              let ic = open_in_bin x in
              check_magic x ic;
              fi.Db1link.f_curr_src_file <- input_value ic;
              fi.Db1link.f_curr_gwo_file <- x;
              fi.Db1link.f_separate <- separate;
              fi.Db1link.f_shift <- shift;
              Hashtbl.clear fi.Db1link.f_local_names;
              ic_opt := Some ic;
              loop ()
          | [] ->
              if ngwo < 10 || not !(Mutil.verbose) then ()
              else if ngwo < 60 then
                begin Printf.eprintf "\n"; flush stderr end
              else ProgrBar.finish ();
              None
    in
    loop ()

let just_comp = ref false
let out_file = ref (Filename.concat Filename.current_dir_name "a")
let action = ref ""
let copy = ref false

let separate = ref false
let force = ref false
let upd = ref false
let shift = ref 0
let files = ref []

let speclist =
  ["-jc", Arg.Set just_comp, "Only compiling";
   "-o", Arg.String (fun s -> out_file := s),
   "<file> Output database (default: a.gwb)";
   "-action", Arg.String (fun s -> action := s),
   "<str> Backup and reset or update the base";
   "-f", Arg.Set force,
   "sane as -action=backup";
   "-u", Arg.Set upd,
   "same as -action=update";
   "-c", Arg.Set copy,
   "Copy documents/etc/history/forum from backup base to new one";
   "-stats", Arg.Set Db1link.pr_stats, "Print statistics";
   "-nc", Arg.Clear Db1link.do_check, "No consistency check";
   "-cg", Arg.Set Db1link.do_consang, "Compute consanguinity";
   "-sep", Arg.Set separate, " Separate all persons in next file";
   "-sh", Arg.Int (fun x -> shift := x),
   "<int> Shift all persons numbers in next files";
   "-ds", Arg.String (fun s -> Db1link.default_source := s),
   "<str> Set the source field for persons and families without source data";
   "-part", Arg.String (fun s -> Db1link.particules_file := s),
   "<file> Particles file (default = predefined particles)";
   "-mem", Arg.Set Outbase.save_mem, " Save memory, but slower";
   "-nolock", Arg.Set Lock.no_lock_flag, " do not lock database.";
   "-nofail", Arg.Set Gwcomp.no_fail, " no failure in case of error.";
   "-nopicture", Arg.Set Gwcomp.no_picture,
   " do not create associative pictures";
   "-q", Arg.Clear Mutil.verbose, " no verbose";
   "-v", Arg.Set Mutil.verbose, " verbose"]

let anonfun x =
  let sep = !separate in
  if Filename.check_suffix x ".gw" then ()
  else if Filename.check_suffix x ".gwo" then ()
  else raise (Arg.Bad ("Don't know what to do with \"" ^ x ^ "\""));
  separate := false;
  files := (x, sep, !shift) :: !files

let errmsg =
  "Usage: gwc [options] [files]\n\
   where [files] are a list of files:\n  \
   source files end with .gw\n  \
   object files end with .gwo\n\
   and [options] are:"

let main () =
  Mutil.verbose := false;
  Argl.parse speclist anonfun errmsg;
  Secure.set_base_dir (Filename.dirname !out_file);
  if !action = "update" then copy := false ;
  if !force then action := "backup" ;
  if !upd then
    begin
      action := "update" ; copy := false
    end ;
  if !force && !upd then copy := true ;
  let gwo = ref [] in
  List.iter
    (fun (x, separate, shift) ->
       if Filename.check_suffix x ".gw" then
         begin
           begin try Gwcomp.comp_families x with
             e -> Printf.printf "File \"%s\", line %d:\n" x !line_cnt; raise e
           end;
           gwo := (x ^ "o", separate, shift) :: !gwo
         end
       else if Filename.check_suffix x ".gwo" then
         gwo := (x, separate, shift) :: !gwo
       else raise (Arg.Bad ("Don't know what to do with \"" ^ x ^ "\"")))
    (List.rev !files);
  if not !just_comp then
  begin
    let bdir =
      if Filename.check_suffix !out_file ".gwb" then !out_file
      else !out_file ^ ".gwb"
    in
    let bname = Filename.basename (
      if Filename.check_suffix !out_file ".gwb" then 
        Filename.chop_suffix !out_file ".gwb"
      else !out_file)
    in
    let base_bak =
      if Sys.file_exists (Filename.concat bdir "base") then
      begin
        if !action = "none" then
          begin
            Printf.eprintf "The database \"%s\" already exists. \
                    Use option -f or -u to overwrite it."
              !out_file ;
            flush stdout ;
            exit 2
          end
        else if !action = "backup" then
          begin
            let utm = Unix.time () in
            let tm = Unix.localtime utm in
            let date = Printf.sprintf "-%04d-%02d-%02d-%02d-%02d.gwb"
              (tm.Unix.tm_year + 1900) (succ tm.Unix.tm_mon) (tm.Unix.tm_mday)
              tm.Unix.tm_hour tm.Unix.tm_min
            in
            let base_bak = !out_file ^ date in
            Mutil.rn bdir base_bak ;
            Printf.eprintf "Backup %s into %s\n" bdir (!out_file ^ date) ;
            (* create empty base folder structure
            Unix.mkdir (Path.path_from_bname bname).Path.dir_my_base 0o766 ;
            Unix.mkdir (Path.path_from_bname bname).Path.dir_etc_b 0o766 ;
            Unix.mkdir (Path.path_from_bname bname).Path.dir_notes 0o766 ;
            Unix.mkdir (Path.path_from_bname bname).Path.dir_wiznotes 0o766 ;
            Unix.mkdir (Path.path_from_bname bname).Path.dir_cnt 0o766 ;
            Unix.mkdir (Path.path_from_bname bname).Path.dir_lang_b 0o766 ; *)
            base_bak
          end
        else ""
      end
      else ""
    in
    let save_list =
      if !copy && !action = "backup" then
        let file_forum = (Path.path_from_bname bname).Path.file_forum in
        let dir_history = (Path.path_from_bname bname).Path.dir_history in
        let file_history = (Path.path_from_bname bname).Path.file_history in
        let dir_documents = (Path.path_from_bname bname).Path.dir_documents in
        let dir_etc = (Path.path_from_bname bname).Path.dir_etc_b in
        if !Path.reorg then [dir_etc; dir_documents; dir_history; file_forum]
        else [file_history; dir_history; file_forum]
      else []
    in
    if save_list <> [] then
      begin
        let _ = Printf.eprintf "Save list: " in
        List.iter (fun s -> Printf.eprintf ", %s" s) save_list;
        Printf.eprintf "\n"
      end ;
    Lock.control
      (Mutil.lock_file !out_file)
      false
      ~onerror:Lock.print_error_and_exit
      (fun () ->
        let bdir =
          if Filename.check_suffix !out_file ".gwb" then !out_file
          else !out_file ^ ".gwb"
        in
        let next_family_fun = next_family_fun_templ (List.rev !gwo) in
        if Db1link.link next_family_fun bdir then ()
        else
          begin
            Printf.eprintf "*** database not created\n";
            flush stderr;
            exit 2
          end) ;
    Printf.eprintf "Trying save_list (%d)\n" (List.length save_list) ;
    try
      List.iter (fun f ->
          let tmp_f = (Filename.concat base_bak (Filename.basename f)) in
          if Sys.file_exists tmp_f then
            begin
              Mutil.rm_rf f ; Mutil.copy_r tmp_f f
            end
          else () ) save_list
    with _ ->
      begin
        Printf.eprintf "*** database created, but problem with save_list\n" ;
        flush stderr ; exit 22
      end ;
  end
  else ()

let print_exc =
  function
    Failure txt -> Printf.printf "Failed: %s\n" txt; flush stdout; exit 2
  | exc -> Printexc.print raise exc

let _ = try main () with exc -> print_exc exc
