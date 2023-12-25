(* camlp4r *)
(* $Id: public.ml,v 4.26 2007/01/19 09:03:02 deraugla Exp $ *)

open Def
open Gwdb
open Printf


let input_person file =
  let pl = ref [] in
  begin match (try Some (open_in file) with Sys_error _ -> None) with
    Some ic ->
      begin try
        while true do let line = input_line ic in pl := line :: !pl done
      with End_of_file -> ()
      end;
      close_in ic
  | None -> Printf.eprintf "Error while opening file %s\n" file; flush stderr
  end;
  List.rev !pl

let private_everybody bname =
  let base = Gwdb.open_base bname in
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    if get_access p <> Private then
      let p = {(gen_person_of_person p) with access = Private} in
      patch_person base p.key_index p
  done;
  commit_patches base

let private_some bname key =
  let base = Gwdb.open_base bname in
  match Gutil.person_ht_find_all base key with
    [ip] ->
      let p = poi base ip in
      if get_access p <> Private then
        begin let p = {(gen_person_of_person p) with access = Private} in
          patch_person base p.key_index p
        end;
      commit_patches base
  | _ -> Printf.eprintf "Bad key %s\n" key; flush stderr

let private_some_list bname file =
  if Sys.file_exists file then
    let pl = input_person file in List.iter (private_some bname) pl
  else
    begin
      Printf.eprintf "File does not exist : %s\n" file;
      flush stderr;
      exit 2
    end

let list_ind = ref ""
let ind = ref ""
let bname = ref ""
let everybody = ref false

let speclist =
  ["-everybody", Arg.Set everybody, "set flag private to everybody [option
  lente!]";
   "-ind", Arg.String (fun x -> ind := x), "individual key";
   "-list-ind", Arg.String (fun s -> list_ind := s),
   "<file> file to the list of persons"]
let anonfun i = bname := i
let usage = "Usage: private [-everybody] [-ind key] base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  let gcc = Gc.get () in
  gcc.Gc.max_overhead <- 100;
  Gc.set gcc;
  if !everybody then private_everybody !bname
  else if !list_ind = "" then private_some !bname !ind
  else private_some_list !bname !list_ind

let _ = main ()
