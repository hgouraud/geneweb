open Gwdb
open Def

let bname = ref ""
let trace = ref false
let fnames = ref false
let snames = ref false
let alias = ref false
let pub_names = ref false
let fname_alias = ref false
let sname_alias = ref false
let places = ref false
let estates = ref false
let titles = ref false
let occupations = ref false
let qualifiers = ref false
let all = ref false
let prog = ref false
let width = ref 50

let write_cache_file bname fname list =
  let cache = Filename.concat (!Geneweb.GWPARAM.etc_d bname) "cache" in
  let filename = (bname ^ "_" ^ fname ^ ".cache") in
  let file = Filename.concat cache filename in
  let gz_file = file ^ ".gz" in
  try
    let temp_file = Filename.temp_file "temp_" ".cache" in
    let oc = open_out temp_file in
    List.iter (fun (v, _) -> output_string oc (v ^ "\n")) list;
    close_out oc;
    let gzip_command = Printf.sprintf "gzip -9 -c < %s > %s" temp_file gz_file in
    let status = Unix.system gzip_command in
    Sys.remove temp_file;
    match status with
    | Unix.WEXITED 0 -> () (* Compression successful *)
    | _ -> failwith "Gzip compression failed"
  with exn ->
    Printf.printf "Debug: Exception occurred: %s\n" (Printexc.to_string exn);
    Printf.printf "Debug: Stack trace:\n%s\n" (Printexc.get_backtrace ());
    raise exn

let places_all base bname fname =
  let start = Unix.gettimeofday () in
  let ht_size = 2048 in
  (* FIXME: find the good heuristic *)
  let ht : ('a, 'b) Hashtbl.t = Hashtbl.create ht_size in
  let ht_add istr _p =
    let key : 'a = sou base istr in
    match Hashtbl.find_opt ht key with
    | Some _ -> Hashtbl.replace ht key key
    | None -> Hashtbl.add ht key key
  in
  let len = nb_of_persons base in
  if !prog then (
    Printf.printf "\nplaces\n";
    flush stdout;
    ProgrBar.full := '*';
    ProgrBar.start ());
  let aux b fn p =
    if b then
      let x = fn p in
      if not (is_empty_string x) then ht_add x p
  in

  Collection.iteri
    (fun i ip ->
      let p = poi base ip in
      aux true get_birth_place p;
      aux true get_baptism_place p;
      aux true get_death_place p;
      aux true get_burial_place p;
      if !prog then ProgrBar.run i len else ())
    (Gwdb.ipers base);

  if !prog then ProgrBar.finish ();
  let len = nb_of_families base in
  if !prog then (
    ProgrBar.full := '*';
    ProgrBar.start ());

  Collection.iteri
    (fun i ifam ->
      let fam = foi base ifam in
      let pl_ma = get_marriage_place fam in
      if not (is_empty_string pl_ma) then (
        let fath = poi base (get_father fam) in
        let moth = poi base (get_mother fam) in
        ht_add pl_ma fath;
        ht_add pl_ma moth);
      if !prog then ProgrBar.run i len else ())
    (Gwdb.ifams base);

  if !prog then ProgrBar.finish ();
  flush stderr;
  let places_list = Hashtbl.fold (fun _k v acc -> (v, 1) :: acc) ht [] in
  let places_list =
    List.sort (fun (v1, _) (v2, _) -> Gutil.alphabetic_utf_8 v1 v2) places_list
  in
  write_cache_file bname fname places_list;
  flush stderr;
  let stop = Unix.gettimeofday () in
  let cache = Filename.concat (!Geneweb.GWPARAM.etc_d bname) "cache" in
  let full_name = Filename.concat cache (bname ^ "_" ^ fname ^ ".cache.gz") in
  Format.printf "@[<h>%-*s@ %8d@ %-14s@ %6.2f s@]@," !width full_name
    (List.length places_list) "places" (stop -. start);
  flush stderr

let names_all base bname fname =
  let start = Unix.gettimeofday () in
  let ht = Hashtbl.create 1 in
  let nb_ind = nb_of_persons base in
  flush stderr;
  if !prog then (
    Printf.printf "%s\n" fname;
    flush stdout;
    ProgrBar.full := '*';
    ProgrBar.start ());

  Collection.iteri
    (fun i ip ->
      if !prog then ProgrBar.run i nb_ind;
      let p = poi base ip in
      let nam =
        match fname with
        | "fnames" -> [ get_first_name p ]
        | "snames" -> [ get_surname p ]
        | "alias" -> get_aliases p
        | "occupations" -> [ get_occupation p ]
        | "qualifiers" -> get_qualifiers p
        | "pub_names" -> [ get_public_name p ]
        | "estates" ->
            List.fold_left (fun acc t -> t.t_place :: acc) [] (get_titles p)
        | "titles" ->
            List.fold_left (fun acc t -> t.t_ident :: acc) [] (get_titles p)
        | _ -> []
      in
      List.iter
        (fun nam ->
          let key = sou base nam in
          if not (Hashtbl.mem ht key) then Hashtbl.add ht key (key, 1)
          else
            let vv, i = Hashtbl.find ht key in
            Hashtbl.replace ht key (vv, i + 1))
        nam;

      let fna = if !fname_alias && !fnames then get_first_names_aliases p else [] in
      let sna = if !sname_alias && !snames then get_surnames_aliases p else [] in
      let nam2 =
        if fna <> [] then fna
        else if sna <> [] then sna
        else []
      in
      List.iter
        (fun nam ->
          let key = sou base nam in
          if not (Hashtbl.mem ht key) then Hashtbl.add ht key (key, 1)
          else
            let vv, i = Hashtbl.find ht key in
            Hashtbl.replace ht key (vv, i + 1))
        nam2;
      if !prog then ProgrBar.run i nb_ind else ())
    (Gwdb.ipers base);

  if !prog then ProgrBar.finish ();
  flush stderr;
  let name_list = Hashtbl.fold (fun _k v acc -> v :: acc) ht [] in
  let name_list = List.sort (fun v1 v2 -> compare v1 v2) name_list in
  write_cache_file bname fname name_list;
  flush stderr;
  let stop = Unix.gettimeofday () in
  let cache = Filename.concat (!Geneweb.GWPARAM.etc_d bname) "cache" in
  let full_name = Filename.concat cache (bname ^ "_" ^ fname ^ ".cache.gz") in
  Format.printf "@[<h>%-*s@ %8d@ %-14s@ %6.2f s@]@," !width full_name
    (Hashtbl.length ht) fname (stop -. start)

let speclist =
  [
    ("-bd", Arg.String Secure.set_base_dir, " bases folder");
    ("-fn", Arg.Set fnames, " first names");
    ("-sn", Arg.Set snames, " surnames");
    ("-al", Arg.Set alias, " aliases");
    ("-pu", Arg.Set pub_names, " public names");
    ("-fna", Arg.Set fname_alias, " add first name aliases");
    ("-sna", Arg.Set sname_alias, " add surname aliases");
    ("-qu", Arg.Set qualifiers, " qualifiers");
    ("-pl", Arg.Set places, " places");
    ("-es", Arg.Set estates, " estates");
    ("-ti", Arg.Set titles, " titles");
    ("-oc", Arg.Set occupations, " occupations");
    ("-all", Arg.Set all, " all");
    ("-prog", Arg.Set prog, " show progress bar");
  ]
  |> List.sort compare |> Arg.align

let anonfun i = bname := i

let usage =
  "Usage: cache_files [options] base\n cd bases; before running cache_files."

let main () =
  Secure.set_base_dir ".";
  Arg.parse speclist anonfun usage;
  bname := Filename.remove_extension (Filename.basename !bname);
  if !bname = "" || !bname <> Filename.basename !bname then (
    Arg.usage speclist usage;
    exit 2);
  let base = Gwdb.open_base (!Geneweb.GWPARAM.bpath !bname) in
  Geneweb.GWPARAM.init !bname;
  Geneweb.GWPARAM.init_etc !bname;
  Mutil.mkdir_p (Filename.concat (!Geneweb.GWPARAM.etc_d !bname) "cache");
  Printf.printf "Generating cache(s) compressed with gzip, %s mode\n"
    (if !Geneweb.GWPARAM.reorg then "reorg" else "classic");

  let cache = Filename.concat (!Geneweb.GWPARAM.etc_d !bname) "cache" in
  let full_name = Filename.concat cache (!bname ^ "_occupations.cache.gz") in
  width := String.length full_name + 2;
  Format.printf "@[<v>";
  (* Ouvre une boite verticale *)
  if !places then places_all base !bname "places";
  if !fnames then names_all base !bname "fnames";
  if !snames then names_all base !bname "snames";
  if !alias then names_all base !bname "aliases";
  if !pub_names then names_all base !bname "pub_names";
  if !qualifiers then names_all base !bname "qualifiers";
  if !estates then names_all base !bname "estates";
  if !titles then names_all base !bname "titles";
  if !occupations then names_all base !bname "occupations";
  if !all then (
    fname_alias := true;
    sname_alias := true;
    places_all base !bname "places";
    fnames := true;
    snames := false;
    names_all base !bname "fnames";
    fnames := false;
    snames := true;
    names_all base !bname "snames";
    fnames := false;
    snames := false;
    names_all base !bname "aliases";
    names_all base !bname "pub_names";
    names_all base !bname "estates";
    names_all base !bname "titles";
    names_all base !bname "occupations";
    names_all base !bname "qualifiers");
  Format.printf "@]";
  (* Ferme la boite verticale *)
  flush stderr

let _ = main ()
