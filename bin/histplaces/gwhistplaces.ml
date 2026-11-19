(* Rename a place on the persons (and optionally their families) that a
   given user modified at a given time, as recorded in the history file. *)

module Driver = Geneweb_db.Driver

type key = { fn : string; sn : string; oc : int }

let line_re =
  Str.regexp
    {|^\([0-9-]+ [0-9:]+\) \[\(.*\)\] \([a-z]+\) (\(.*\)\.\([0-9]+\) \(.*\))$|}

(* Keys of the persons named in the history lines starting with [date]
   and written by [user], in order of first appearance. *)
let keys_of_history ~fname ~date ~user =
  let seen = Hashtbl.create 64 in
  let add acc line =
    if not (String.starts_with ~prefix:date line) then acc
    else if not (Str.string_match line_re line 0) then acc
    else if Str.matched_group 2 line <> user then acc
    else
      let k =
        {
          fn = Str.matched_group 4 line;
          oc = int_of_string (Str.matched_group 5 line);
          sn = Str.matched_group 6 line;
        }
      in
      if Hashtbl.mem seen k then acc
      else (
        Hashtbl.replace seen k ();
        k :: acc)
  in
  let ic = open_in fname in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let rec loop acc =
        match input_line ic with
        | line -> loop (add acc line)
        | exception End_of_file -> List.rev acc
      in
      loop [])

let main () =
  let bases_dir = ref "." in
  let date = ref "2026-11-15 08:22" in
  let user = ref "J-F.Class" in
  let from = ref "(Loire-Atlantique)" in
  let into = ref "Ancenis (Loire-Atlantique)" in
  let families = ref false in
  let dry_run = ref false in
  let bname = ref None in
  let speclist =
    [
      ("-bd", Arg.Set_string bases_dir, " bases folder (default .)");
      ( "-date",
        Arg.Set_string date,
        " history time prefix (default \"2026-11-15 08:22\")" );
      ("-user", Arg.Set_string user, " wizard (default J-F.Class)");
      ("-from", Arg.Set_string from, " place to replace");
      ("-to", Arg.Set_string into, " replacement place");
      ("-families", Arg.Set families, " also fix the families of these persons");
      ("-dry-run", Arg.Set dry_run, " list changes, do not commit");
    ]
    |> Arg.align
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION] base" in
  Arg.parse speclist (fun s -> bname := Some s) usage;
  match !bname with
  | None ->
      Arg.usage speclist usage;
      exit 2
  | Some b ->
      Secure.set_base_dir !bases_dir;
      let b = Filename.remove_extension (Filename.basename b) in
      let bpath = Filename.concat !bases_dir b in
      let hist = Filename.concat (bpath ^ ".gwb") "history" in
      let keys = keys_of_history ~fname:hist ~date:!date ~user:!user in
      Printf.printf "%d persons modified by %s at %s\n%!" (List.length keys)
        !user !date;
      let from = !from and into = !into in
      let on_exn exn bt =
        Format.eprintf "%a@." Lock.pp_exception (exn, bt);
        exit 2
      in
      Driver.with_database bpath @@ fun base ->
      Lock.control ~on_exn ~wait:true ~lock_file:(Mutil.lock_file bpath)
      @@ fun () ->
      let into_istr = lazy (Driver.insert_string base into) in
      let count = ref 0 in
      let fix who what istr =
        if String.trim (Driver.sou base istr) = from then (
          incr count;
          Printf.printf "  %s: %s\n" who what;
          Lazy.force into_istr)
        else istr
      in
      let done_fams = Hashtbl.create 16 in
      let fix_family who ifam =
        if not (Hashtbl.mem done_fams ifam) then (
          Hashtbl.replace done_fams ifam ();
          let n0 = !count in
          let gf = Driver.gen_family_of_family (Driver.foi base ifam) in
          let who = who ^ " (family)" in
          let gf =
            {
              gf with
              Def.marriage_place = fix who "marriage" gf.Def.marriage_place;
              fevents =
                List.map
                  (fun e ->
                    {
                      e with
                      Def.efam_place = fix who "fevent" e.Def.efam_place;
                    })
                  gf.Def.fevents;
            }
          in
          if !count > n0 && not !dry_run then Driver.patch_family base ifam gf)
      in
      List.iter
        (fun k ->
          let who = Printf.sprintf "%s.%d %s" k.fn k.oc k.sn in
          match Driver.person_of_key base k.fn k.sn k.oc with
          | None -> Printf.printf "  %s: NOT FOUND (renamed or deleted?)\n" who
          | Some ip ->
              let p = Driver.poi base ip in
              let n0 = !count in
              let gp = Driver.gen_person_of_person p in
              let gp =
                {
                  gp with
                  Def.birth_place = fix who "birth" gp.Def.birth_place;
                  baptism_place = fix who "baptism" gp.Def.baptism_place;
                  death_place = fix who "death" gp.Def.death_place;
                  burial_place = fix who "burial" gp.Def.burial_place;
                  pevents =
                    List.map
                      (fun e ->
                        {
                          e with
                          Def.epers_place = fix who "pevent" e.Def.epers_place;
                        })
                      gp.Def.pevents;
                }
              in
              if !count > n0 && not !dry_run then Driver.patch_person base ip gp;
              if !families then
                Array.iter (fix_family who) (Driver.get_family p))
        keys;
      if !dry_run then Printf.printf "%d places to change (dry run)\n" !count
      else if !count > 0 then (
        Driver.commit_patches base;
        Printf.printf "%d places changed, committed\n" !count)
      else print_endline "No change"

let () = main ()
