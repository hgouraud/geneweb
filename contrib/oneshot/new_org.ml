(* $Id: new_org.ml,v 1 2018-12-9 hg Exp $ *)

let save = ref false
let verbose = ref false
let yes = ref false
let new_bases_dir = ref ""
let basename = ref ""
let bname = ref ""
(*let new_dir = ref ""*)

let do_one_comm comm err =
  let _ =
    if !verbose then
      Printf.eprintf "Doing %s\n" comm
  in
  if (Sys.command comm) <> 0
  then
    let _ = Printf.eprintf "%s\n" err in
    raise Exit

let one_base cur_dir new_dir basename save =
  Printf.eprintf "Doing %s ... " basename;
  flush stderr;
  if not (Sys.file_exists basename) then
    let _ = Printf.eprintf "base \"%s\" does not exist\n" basename in
    raise Exit
  else ();

  let bname = Filename.basename basename in
  let bname = Filename.remove_extension bname in
  let new_base = Filename.concat new_dir basename in
  if save then
    do_one_comm (Printf.sprintf "mv -f %s %s" new_base (new_base ^ ".save"))
     (Printf.sprintf "Failed to save previous copy of base %s\n" basename)
  else
    do_one_comm (Printf.sprintf "rm -f -R %s" new_base)
      (Printf.sprintf "Failed to destroy old copy of base %s\n" basename);
  do_one_comm (Printf.sprintf "cp -f -R %s %s"
     (Filename.concat cur_dir basename)
     (Filename.concat new_dir basename))
     (Printf.sprintf "Failed to copy full base %s\n" basename);
  (* history has been copied with cp -R *)
  if not (Sys.file_exists (Filename.concat new_base "history_d")) then
      (try Unix.mkdir (Filename.concat new_base "history_d") 0o777
        with Unix.Unix_error (_, _, _) -> ());
  do_one_comm (Printf.sprintf "mv %s %s%s%s"
     (Filename.concat new_base "history")
     (Filename.concat new_base "history_d")
     (Filename.dir_sep) "history.txt")
     (Printf.sprintf "Failed to move history %s\n" basename);
  do_one_comm (Printf.sprintf "mv %s %s"
     (Filename.concat new_base "history_d")
     (Filename.concat new_base "history"))
     (Printf.sprintf "Failed to rename history_d %s\n" basename);
  do_one_comm (Printf.sprintf "mv %s %s"
     (Filename.concat new_base "notes")
     (Filename.concat new_base "notes.txt"))
     (Printf.sprintf "Failed to rename notes %s\n" basename);
  if not (Sys.file_exists (Filename.concat new_base "notes_d")) then
      (try Unix.mkdir (Filename.concat new_base "notes") 0o777
        with Unix.Unix_error (_, _, _) -> ());
  do_one_comm (Printf.sprintf "mv %s %s"
     (Filename.concat new_base "notes_d")
     (Filename.concat new_base "notes"))
     (Printf.sprintf "Failed to rename notes_d %s\n" basename);
  do_one_comm (Printf.sprintf "mv %s %s%s%s"
     (Filename.concat new_base "notes.txt")
     (Filename.concat new_base "notes")
     (Filename.dir_sep) "notes.txt")
     (Printf.sprintf "Failed to rename notes.txt %s\n" basename);

  (try Unix.mkdir (Filename.concat new_base "cnt") 0o777
    with Unix.Unix_error (_, _, _) -> ());
  (try Unix.mkdir (Filename.concat new_base "documents") 0o777
    with Unix.Unix_error (_, _, _) -> ());
  (try Unix.mkdir (Filename.concat new_base "etc") 0o777
    with Unix.Unix_error (_, _, _) -> ());
  (try Unix.mkdir (Filename.concat new_base "etc"; "cnt") 0o777
    with Unix.Unix_error (_, _, _) -> ());
  (try Unix.mkdir (Filename.concat new_base "lang") 0o777
    with Unix.Unix_error (_, _, _) -> ());
  (try Unix.mkdir (Filename.concat new_base "wiznotes") 0o777
    with Unix.Unix_error (_, _, _) -> ());
  let documents = Filename.concat new_base "documents" in
  (try Unix.mkdir (Filename.concat documents "images") 0o777
    with Unix.Unix_error (_, _, _) -> ());
  (try Unix.mkdir (Filename.concat documents "portraits") 0o777
    with Unix.Unix_error (_, _, _) -> ());

  let images = String.concat Filename.dir_sep [cur_dir; "images";] in
  let ls_images = (Array.to_list (Sys.readdir images)) in
  let ls_images =
    List.fold_right (fun f l ->
        if f.[0] <> '.' &&
          ( Filename.extension f = ".jpg" ||
            Filename.extension f = ".gif" ||
            Filename.extension f = ".png" )
        then
          ( f :: l )
        else l) ls_images []
  in
  let len = (List.length ls_images) in
  if len > 0 then
  begin
    let answer =
      if not !yes then
        begin
          print_string
            (Printf.sprintf
              "There are %d jpg, gif or png files at the top level \
               of images/ folder\n" len);
          print_string
            "Do you want to copy them in the document/portrait folder? [Y/n]\n";
          read_line ()
        end
      else "y"
    in
    if answer = "y" || String.length answer = 0 then
    begin
      do_one_comm (Printf.sprintf "cp %s%s%s %s%s 2> /dev/null || :"
        (String.concat Filename.dir_sep [cur_dir; "images";])
        (Filename.dir_sep) ("*.jpg")
        (String.concat Filename.dir_sep [documents; "portraits";])
        (Filename.dir_sep))
        (Printf.sprintf "Failed to copy *.jpg for %s\n" basename);
      do_one_comm (Printf.sprintf "cp %s%s%s %s%s 2> /dev/null || :"
        (String.concat Filename.dir_sep [cur_dir; "images";])
        (Filename.dir_sep) ("*.gif")
        (String.concat Filename.dir_sep [documents; "portraits";])
        (Filename.dir_sep))
        (Printf.sprintf "Failed to copy *.gif for %s\n" basename);
      do_one_comm (Printf.sprintf "cp %s%s%s %s%s 2> /dev/null || :"
        (String.concat Filename.dir_sep [cur_dir; "images";])
        (Filename.dir_sep) ("*.png")
        (String.concat Filename.dir_sep [documents; "portraits";])
        (Filename.dir_sep))
        (Printf.sprintf "Failed to copy *.png for %s\n" basename);
    end;
  end;
  flush stderr;

  let from =
    String.concat Filename.dir_sep [cur_dir; "images"; bname;]
  in
  if Sys.file_exists from then
    do_one_comm (Printf.sprintf "cp %s %s%s"
      (Filename.concat from "*")
      (String.concat Filename.dir_sep [documents; "portraits";])
      (Filename.dir_sep))
      (Printf.sprintf "Failed to copy portraits for %s\n" basename);


  let from =
    String.concat Filename.dir_sep
      [cur_dir; "src"; bname;]
  in
  if Sys.file_exists from then
    do_one_comm (Printf.sprintf "cp -R %s %s%s"
      (Filename.concat from "*.txt")
      (String.concat Filename.dir_sep [documents;])
      (Filename.dir_sep))
      (Printf.sprintf "Failed to copy src/*.txt for %s\n" basename);

  let from =
    String.concat Filename.dir_sep
      [cur_dir; "src"; bname; "images"]
  in
  if Sys.file_exists from then
    do_one_comm (Printf.sprintf "cp -R %s %s%s"
      (Filename.concat from "*")
      (String.concat Filename.dir_sep [documents;])
      (Filename.dir_sep))
      (Printf.sprintf "Failed to copy src/images/* for %s\n" basename);

  let from =
    String.concat Filename.dir_sep
      [cur_dir; "etc"; bname;]
  in
  if Sys.file_exists from then
    do_one_comm (Printf.sprintf "cp -R %s %s%s"
      (Filename.concat from "*")
      (String.concat Filename.dir_sep [new_base; "etc";])
      (Filename.dir_sep))
      (Printf.sprintf "Failed to copy etc for %s\n" basename);

  do_one_comm (Printf.sprintf "cp %s %s"
     (Filename.concat cur_dir (bname ^ ".gwf"))
     (String.concat Filename.dir_sep [new_base; "etc"; bname ^ ".conf"]))
     (Printf.sprintf "Failed to copy .gwf %s\n" basename);

  let from =
    String.concat Filename.dir_sep
      [cur_dir; "lang"; bname;]
  in
  if Sys.file_exists from then
    do_one_comm (Printf.sprintf "cp -R %s %s%s"
      (Filename.concat from "*")
      (String.concat Filename.dir_sep [new_base; "lang";])
      (Filename.dir_sep))
      (Printf.sprintf "Failed to copy lang for %s\n" basename);

  let from =
    String.concat Filename.dir_sep
      [cur_dir; "cnt";]
  in
  if Sys.file_exists from then
    begin
    do_one_comm (Printf.sprintf "cp %s %s%s"
      (Filename.concat from bname ^ "_cnt.lck 2> /dev/null || :")
      (String.concat Filename.dir_sep [new_base; "etc"; "cnt"; "counts.lck"])
      (Filename.dir_sep))
      (Printf.sprintf "Failed to copy counts.lck for %s\n" basename);
    do_one_comm (Printf.sprintf "cp %s %s%s"
      (Filename.concat from bname ^ "_cnt.txt 2> /dev/null || :")
      (String.concat Filename.dir_sep [new_base; "etc"; "cnt"; "counts.txt"])
      (Filename.dir_sep))
      (Printf.sprintf "Failed to copy counts.txt for %s\n" basename);
    do_one_comm (Printf.sprintf "cp %s %s%s"
      (Filename.concat from bname ^ "_f.txt 2> /dev/null || :")
      (String.concat Filename.dir_sep [new_base; "etc"; "cnt"; "friends.log"])
      (Filename.dir_sep))
      (Printf.sprintf "Failed to copy friends.log for %s\n" basename);
    do_one_comm (Printf.sprintf "cp %s %s%s"
      (Filename.concat from bname ^ "_w.txt 2> /dev/null || :")
      (String.concat Filename.dir_sep [new_base; "etc"; "cnt"; "wizards.log"])
      (Filename.dir_sep))
      (Printf.sprintf "Failed to copy wizards.log for %s\n" basename);
    do_one_comm (Printf.sprintf "cp %s %s%s"
      (Filename.concat from bname ^ "_u.txt 2> /dev/null || :")
      (String.concat Filename.dir_sep [new_base; "etc"; "cnt"; "updates.log"])
      (Filename.dir_sep))
      (Printf.sprintf "Failed to copy updates.log for %s\n" basename)
    end;

  Printf.eprintf "done\n";
  flush stderr

let speclist =
  ["-save", Arg.Set save, "rename existing version to xx.save";
   "-verbose", Arg.Set verbose, "print trace of each command";
   "-yes", Arg.Set yes, "answer yes to all interactive queries";
   "-one", Arg.String (fun x -> basename := x), "process designated base"]
let anonfun i = new_bases_dir := i
let usage = "Usage: new_org [-one basename] [-verbose] [-save] [-yes] new_bases_dir"

let main () =
  Arg.parse speclist anonfun usage;
  let cur_dir = Sys.getcwd () in
  let new_dir = Filename.concat (Sys.getcwd ()) !new_bases_dir in
  Printf.eprintf "Starting transporting old bases to %s\n" new_dir;
  if not (Sys.file_exists new_dir) then
    (try Unix.mkdir new_dir 0o777 with Unix.Unix_error (_, _, _) -> ());
  let base_list =
      List.fold_right (fun f1 l ->
        if Filename.extension f1 = ".gwb"
        then
          ( f1 :: l ) else l)
          (Array.to_list (Sys.readdir cur_dir)) []
  in
  if !basename <> "" then
    let basename = if (Filename.extension !basename) = ".gwb"
      then !basename
      else (!basename ^ ".gwb")
    in
    one_base cur_dir new_dir basename !save
  else
    List.iter (fun basename -> one_base cur_dir new_dir basename !save) base_list;
  Printf.eprintf "Finished\n";
  flush stderr

let _ = main ()
