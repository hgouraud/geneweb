open Geneweb
open Def
open Gwdb

let bname = ref ""
let trace = ref false
let fnames = ref false
let places = ref false
let fname_alias = ref false

let cache_fname bname fname =
  let basename =
    if Filename.check_suffix bname ".gwb" then bname
    else bname ^ ".gwb"
  in
  Filename.concat (Util.base_path [] basename) fname

let write_cache_file bname fname list =
  let fname = cache_fname bname fname in
  Printf.eprintf "Write to : %s\n" (fname ^ ".cache");
  begin match try Some (Secure.open_out (fname ^ ".cache"))
    with Sys_error _ -> None with
  | Some oc ->
      begin
        List.iter
          (fun (v, _) -> output_string oc ("<option>" ^ v ^ "\n"))
            list;
        close_out oc;
      end
  | None -> ()
  end

let places_all bname =
  let start = Unix.gettimeofday () in
  let base = Gwdb.open_base bname in
  let ht_size = 2048 in (* FIXME: find the good heuristic *)
  let ht : ('a, 'b) Hashtbl.t = Hashtbl.create ht_size in
  let ht_add istr p =
    let key : 'a = sou base istr in
    match Hashtbl.find_opt ht key with
    | Some _ as prev -> Hashtbl.replace ht key key
    | None -> Hashtbl.add ht key key
  in
  let len = nb_of_persons base in
  ProgrBar.full := '*';
  ProgrBar.start ();
  Printf.eprintf "NBDS for %d ind\n" len;
  let aux b fn p =
    if b then let x = fn p in if not (is_empty_string x) then ht_add x p
  in
  for i = 0 to len - 1 do
    let p = poi base (Adef.iper_of_int i) in
    aux true get_birth_place p ;
    aux true get_baptism_place p ;
    aux true get_death_place p ;
    aux true get_burial_place p ;
    ProgrBar.run i len
  done;
  ProgrBar.finish ();
  let len = nb_of_families base in
  ProgrBar.full := '*';
  ProgrBar.start ();
  Printf.eprintf "Marriages for %d fam\n" len;
  for i = 0 to len - 1 do
    ProgrBar.run i len;
    let fam = foi base (Adef.ifam_of_int i) in
    if not @@ is_deleted_family fam then
    begin
      let pl_ma = get_marriage_place fam in
      if not (is_empty_string pl_ma) then
        let fath = poi base (get_father fam) in
        let moth = poi base (get_mother fam) in
        ht_add pl_ma fath ;
        ht_add pl_ma moth
    end ;
    ProgrBar.run i len
  done ;
  ProgrBar.finish ();
  flush stderr;
  let places_list = Hashtbl.fold (fun k v acc -> (v, 1) :: acc) ht [] in
  let places_list = List.sort (fun (v1, _) (v2, _) ->
    Gutil.alphabetic_utf_8 v1 v2) places_list in
  write_cache_file bname "cache_places" places_list;
  flush stderr;
  let stop = Unix.gettimeofday () in
  Printf.printf "Number of places: %d\n" (List.length places_list);
  Printf.printf "Execution time: %fs\n" (stop -. start);
  flush stderr

let fnames_all bname =
  let start = Unix.gettimeofday () in
  let base = Gwdb.open_base bname in
  let ht = Hashtbl.create 1 in
  let nb_ind = nb_of_persons base in
  Printf.eprintf "Fnames for %d ind\n" nb_ind;
  flush stderr;
  ProgrBar.full := '*';
  ProgrBar.start ();
  for i = 0 to nb_ind - 1 do
    ProgrBar.run i nb_ind;
    let p = poi base (Adef.iper_of_int i) in
    let fn = sou base (get_first_name p) in
    let fna = get_first_names_aliases p in
    let key = fn in
    if not (Hashtbl.mem ht key) then Hashtbl.add ht key (fn, 1)
    else
      let (vv, i) = Hashtbl.find ht key in
      Hashtbl.replace ht key (vv, i + 1);
    if fna <> [] && !fname_alias then
      List.iter (fun fn ->
          let fn = sou base fn in
          let key = fn in
          if not (Hashtbl.mem ht key) then Hashtbl.add ht key (fn, 1)
          else
            let (vv, i) = Hashtbl.find ht key in
            Hashtbl.replace ht key (vv, i + 1)) fna;
    ProgrBar.run i nb_ind
  done;
  ProgrBar.finish ();
  flush stderr;
  let fname_list = Hashtbl.fold (fun k v acc -> v :: acc) ht [] in
  let fname_list = List.sort (fun v1 v2 -> compare v1 v2) fname_list in
  write_cache_file bname "cache_fnames" fname_list;
  flush stderr;
  let stop = Unix.gettimeofday () in
  Printf.printf "Number of first names: %d\n" (Hashtbl.length ht);
  Printf.printf "Execution time: %fs\n" (stop -. start);
  flush stderr
  
let speclist =
  ["-fn", Arg.Set fnames, "produce first names";
   "-pl", Arg.Set places, "produce places";
   "-fna", Arg.Set fname_alias, "add first names aliases"]
   
let anonfun i = bname := i
let usage = "Usage: public [-fn] [-pl] [-fna] base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  if !places then places_all !bname;
  if !fnames then fnames_all !bname

let _ = main ()
