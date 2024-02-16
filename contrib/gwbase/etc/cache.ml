(* camlp4r *)
(* $Id: public.ml,v 5.00 2018-07-04 09:03:02 hg Exp $ *)

open Def
open Gwdb
open Printf
open Util


let index = ref 0
let fn = ref ""
let sn = ref ""
let oc = ref 0
let set_true = ref false
let set_false = ref false
let reset_all = ref false
let force = ref false
let list = ref false
let list_tst = ref false
let size = ref false
let trace = ref false
let test = ref false
let bname = ref ""

(* copied from notes.ml, modified conf.bname to bname *)

type cache_person_linked_pages_t = (Def.iper, bool) Hashtbl.t

let (ht_cache_person_linked_pages : cache_person_linked_pages_t) =
  Hashtbl.create 1

let cache_fname_person_linked_pages bname =
  let d_sep = Filename.dir_sep in
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  "." ^ d_sep ^ bname ^ d_sep ^ "cache_person_linked_pages"

let notes_links_fname bname =
  let d_sep = Filename.dir_sep in
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  "." ^ d_sep ^ bname ^ d_sep ^ "notes_links"

let read_cache_person_linked_pages bname =
  let fname = cache_fname_person_linked_pages bname in
  let fname = fname ^ (if !list_tst then "_test" else "") in
  match try Some (Secure.open_in_bin fname) with Sys_error _ -> None with
    Some ic ->
      let ht : cache_person_linked_pages_t = input_value ic in close_in ic; ht
  | None -> ht_cache_person_linked_pages

let write_cache_person_linked_pages bname ht =
  let fname = cache_fname_person_linked_pages bname in
  match try Some (Secure.open_out_bin fname) with Sys_error _ -> None with
    Some oc -> output_value oc ht; close_out oc
  | None -> ()

let patch_cache_person_linked_pages bname ht k v =
  if v then
    begin
      if not !test then
        begin
          Hashtbl.replace ht k v;
          write_cache_person_linked_pages bname ht
        end;
      if !trace then printf "Adding person %d\n" (Adef.int_of_iper k)
    end
  else
    begin
      if not !test then
        begin
          Hashtbl.remove ht k;
          write_cache_person_linked_pages bname ht
        end;
      if !trace then printf "Removing person %d\n" (Adef.int_of_iper k)
    end

let get_someone base i fn sn oc =
  if i >= 0 && i < nb_of_persons base then Some (Adef.iper_of_int !index)
  else if fn <> "" || sn <> "" || oc <> 0 then
    match person_of_key base fn sn oc with
      Some p -> Some p
    | _ -> None
  else None

let has_linked_pages ht ip = try Hashtbl.find ht ip with Not_found -> false

let speclist =
  ["-i", Arg.Int (fun i -> index := i), "Index of person";
   "-fn", Arg.String (fun i -> fn := i), "First name of person";
   "-sn", Arg.String (fun i -> sn := i), "Surname of person";
   "-oc", Arg.Int (fun i -> oc := i), "Occurence of person";
   "-set", Arg.Set set_true, "Set to True";
   "-reset", Arg.Set set_false, "Remove from table";
   "-reset_all", Arg.Set reset_all, "Rebuild table";
   "-force", Arg.Set force, "Force removal of index";
   "-list", Arg.Set list, "List of entries";
   "-list_tst", Arg.Set list_tst, "List of entries of test file";
   "-tr", Arg.Set trace, "Trace actions"; "-tst", Arg.Set test, "Test only";
   "-size", Arg.Set size, "Size of cache table"]

let anonfun i = bname := i
let usage = "Usage: cache [-i #] [-set] [-reset] [-list] [-help] base.\n"

let main () =
  index := -1;
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  let base = Gwdb.open_base !bname in
  printf "Executing cache on %s with -i %d \n\n" !bname !index;
  flush stdout;
  let ht_cache = read_cache_person_linked_pages !bname in
  let _ =
    printf "Cache %s of size %d\n" (cache_fname_person_linked_pages !bname)
      (Hashtbl.length ht_cache)
  in
  let nb_ind = nb_of_persons base in
  if !list || !list_tst = true then
    begin
      printf "Listing cache table\n";
      Hashtbl.iter
        (fun k v ->
           let index = Adef.int_of_iper k in
           if index >= 0 && index < nb_of_persons base then
             let p = poi base k in
             let fn = sou base (get_first_name p) in
             let sn = sou base (get_surname p) in
             let oc = get_occ p in
             let vv = if v then "True" else "False" in
             printf "Person %s.%d %s (i=%d) has linked pages (%s)\n" fn oc sn
               index vv
           else printf "*** Index out of bounds %d ***\n" index)
        ht_cache
    end
  else if !size = true then
    printf "Size of cache table is %d\n" (Hashtbl.length ht_cache)
  else if !reset_all = true then
    begin
      Printf.eprintf "--- cache person linked pages\n";
      flush stderr;
      let fname = notes_links_fname !bname in
      let db = NotesLinks.read_db_from_file fname in
      let ht_key = Hashtbl.create nb_ind in
      List.iter
        (fun (pg, (_, il)) ->
           List.iter (fun (k, _) -> Hashtbl.add ht_key k true) il)
        db;
      ProgrBar.start ();
      let ht_cache = Hashtbl.create 1 in
      for i = 0 to nb_ind - 1 do
        let ip = Adef.iper_of_int i in
        let p = poi base ip in
        let key =
          let fn = Name.lower (sou base (get_first_name p)) in
          let sn = Name.lower (sou base (get_surname p)) in fn, sn, get_occ p
        in
        if Hashtbl.mem ht_key key then Hashtbl.add ht_cache ip true;
        ProgrBar.run i nb_ind
      done;
      ProgrBar.finish ();
      let fname = cache_fname_person_linked_pages !bname in
      let fname = fname ^ (if !test then "_test" else "") in
      match try Some (Secure.open_out_bin fname) with Sys_error _ -> None with
        Some oc -> output_value oc ht_cache; close_out oc
      | None -> ()
    end
  else
    begin match get_someone base !index !fn !sn !oc with
      Some ip ->
        let p = poi base ip in
        let fn = sou base (get_first_name p) in
        let sn = sou base (get_surname p) in
        let oc = get_occ p in
        if has_linked_pages ht_cache ip then
          begin
            printf "Person %s.%d %s (i=%d) has linked pages\n" fn oc sn
              (Adef.int_of_iper ip);
            if !set_false then
              patch_cache_person_linked_pages !bname ht_cache ip false
          end
        else
          begin
            printf "Person %s.%d %s (i=%d) does not have linked pages\n" fn oc
              sn (Adef.int_of_iper ip);
            if !set_true then
              patch_cache_person_linked_pages !bname ht_cache ip true
          end
    | None ->
        if !force && !index >= 0 then
          patch_cache_person_linked_pages !bname ht_cache
            (Adef.iper_of_int !index) false
        else printf "*** Person not found %d ***\n" !index
    end;
  if !test then printf "Test only, nothing changed\n";
  flush stdout

let _ = main ()
