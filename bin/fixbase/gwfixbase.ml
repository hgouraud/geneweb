open Geneweb
open Gwdb

let bname = ref ""
let verbosity = ref 2
let fast = ref false
let f_parents = ref false
let f_children = ref false
let p_parents = ref false
let p_families = ref false
let p_NBDS = ref false
let pevents_witnesses = ref false
let fevents_witnesses = ref false
let marriage_divorce = ref false
let index = ref false
let dry_run = ref false
let server = ref "localhost"
let gwd_port = ref 2317

let aux txt
    (fn :
      ?report:(Fixbase.patch -> unit) -> (int -> int -> unit) -> base -> unit)
    ~v1 ~v2 base n cnt =
  let string_of_patch =
    let string_of_p i = Gutil.designation base (poi base i) in
    let string_of_f i =
      let fam = foi base i in
      Printf.sprintf "[%s & %s]"
        (string_of_p @@ get_father fam)
        (string_of_p @@ get_mother fam)
    in
    function
    | Fixbase.Fix_NBDS ip ->
        Printf.sprintf "Fixed pevents for: %s" (string_of_p ip)
    | Fix_AddedUnion ip -> Printf.sprintf "Added union for: %s" (string_of_p ip)
    | Fix_AddedParents ip ->
        Printf.sprintf "Fixed missing parents for: %s" (string_of_p ip)
    | Fix_ParentDeleted ip ->
        Printf.sprintf "Deleted parents for: %s" (string_of_p ip)
    | Fix_AddedChild ifam ->
        Printf.sprintf "Added child in: %s" (string_of_f ifam)
    | Fix_RemovedUnion (ip, ifam) ->
        Printf.sprintf "Removing ifam %s from [%s] unions" (string_of_ifam ifam)
          (string_of_p ip)
    | Fix_RemovedDuplicateUnion (ip, ifam) ->
        Printf.sprintf "Removing duplicate ifam %s from [%s] unions"
          (string_of_ifam ifam) (string_of_p ip)
    | Fix_AddedRelatedFromPevent (ip, ip2) | Fix_AddedRelatedFromFevent (ip, ip2)
      ->
        Printf.sprintf "Added related %s to %s" (string_of_p ip2)
          (string_of_p ip)
    | Fix_MarriageDivorce ifam ->
        Printf.sprintf "Fixed marriage and/or divorce info of %s"
          (string_of_f ifam)
    | Fix_MissingSpouse (ifam, iper) ->
        Printf.sprintf "Fixed missing spouse (%s) in family %s"
          (string_of_p iper) (string_of_f ifam)
  in
  let i' = ref 0 in
  if v1 then (
    print_endline txt;
    flush stdout;
    ProgrBar.start ());
  let progress =
    if v2 then (fun i n ->
      ProgrBar.run i n;
      i' := i)
    else if v1 then ProgrBar.run
    else fun _ _ -> ()
  in
  let report =
    if v2 then
      Some
        (fun s ->
          incr cnt;
          ProgrBar.suspend ();
          print_endline @@ "\t" ^ string_of_patch s;
          flush stdout;
          ProgrBar.restart !i' n)
    else Some (fun _ -> incr cnt)
  in
  fn ?report progress base;
  if v1 then ProgrBar.finish ()

let check_NBDS = aux "Check persons' NBDS" Fixbase.check_NBDS

let check_families_parents =
  aux "Check families' parents" Fixbase.check_families_parents

let check_families_children =
  aux "Check families' children" Fixbase.check_families_children

let check_persons_parents =
  aux "Check persons' parents" Fixbase.check_persons_parents

let check_persons_families =
  aux "Check persons' families" Fixbase.check_persons_families

let check_pevents_witnesses =
  aux "Check persons' events witnesses" Fixbase.check_pevents_witnesses

let check_fevents_witnesses =
  aux "Check family events witnesses" Fixbase.check_fevents_witnesses

let fix_marriage_divorce =
  aux "Fix families' marriage and divorce" Fixbase.fix_marriage_divorce

let check ~dry_run ~verbosity ~fast ~f_parents ~f_children ~p_parents
    ~p_families ~p_NBDS ~pevents_witnesses ~fevents_witnesses ~marriage_divorce
    bname =
  let v1 = !verbosity >= 1 in
  let v2 = !verbosity >= 2 in
  if not v1 then Mutil.verbose := false;
  let fast = !fast in
  let base = Gwdb.open_base bname in
  let fix = ref 0 in
  let nb_fam = nb_of_families base in
  let nb_ind = nb_of_persons base in
  if fast then (
    load_strings_array base;
    load_persons_array base);
  if !f_parents then check_families_parents ~v1 ~v2 base nb_fam fix;
  if !f_children then check_families_children ~v1 ~v2 base nb_fam fix;
  if !p_parents then check_persons_parents ~v1 ~v2 base nb_ind fix;
  if !p_NBDS then check_NBDS ~v1 ~v2 base nb_ind fix;
  if !p_families then check_persons_families ~v1 ~v2 base nb_ind fix;
  if !pevents_witnesses then check_pevents_witnesses ~v1 ~v2 base nb_ind fix;
  if !fevents_witnesses then check_fevents_witnesses ~v1 ~v2 base nb_fam fix;
  if !marriage_divorce then fix_marriage_divorce ~v1 ~v2 base nb_fam fix;
  if fast then (
    clear_strings_array base;
    clear_persons_array base);
  if not !dry_run then (
    if !fix <> 0 then (
      Gwdb.commit_patches base;
      if v1 then (
        Printf.printf "%n changes commited\n" !fix;
        flush stdout))
    else if v1 then (
      Printf.printf "No change\n";
      flush stdout);
    if v1 then (
      Printf.printf "Rebuilding the indexes..\n";
      flush stdout);
    Gwdb.sync base)
  else Printf.printf "No commits\n";
  if v1 then (
    Printf.printf "Done\n";
    flush stdout)

(**/**)

let speclist =
  [
    ("-dry-run", Arg.Set dry_run, " do not commit changes (only print)");
    ("-q", Arg.Unit (fun () -> verbosity := 1), " quiet mode");
    ("-qq", Arg.Unit (fun () -> verbosity := 0), " very quiet mode");
    ("-fast", Arg.Set fast, " fast mode. Needs more memory.");
    ("-families-parents", Arg.Set f_parents, " missing doc");
    ("-families-children", Arg.Set f_children, " missing doc");
    ("-persons-NBDS", Arg.Set p_parents, " missing doc");
    ("-persons-parents", Arg.Set p_parents, " missing doc");
    ("-persons-families", Arg.Set p_families, " missing doc");
    ("-pevents-witnesses", Arg.Set pevents_witnesses, " missing doc");
    ("-fevents-witnesses", Arg.Set fevents_witnesses, " missing doc");
    ("-marriage-divorce", Arg.Set marriage_divorce, " missing doc");
    ( "-index",
      Arg.Set index,
      " rebuild index. It is automatically enable by any other option." );
    ("-server", Arg.String (fun s -> server := s), " missing doc");
    ("-gwd_p", Arg.Int (fun x -> gwd_port := x), " missing doc");
  ]
  |> List.sort compare |> Arg.align

let anonfun i = bname := i
let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION] base"

let main () =
  Arg.parse speclist anonfun usage;
  Secure.set_base_dir (Filename.dirname !bname);
  if !bname = "" then (
    Arg.usage speclist usage;
    exit 2);
  Lock.control (Mutil.lock_file !bname) false ~onerror:Lock.print_try_again
  @@ fun () ->
  if
    !f_parents || !f_children || !p_parents || !p_families || !pevents_witnesses
    || !fevents_witnesses || !marriage_divorce || !p_NBDS || !index
  then ()
  else (
    f_parents := true;
    f_children := true;
    p_parents := true;
    p_families := true;
    pevents_witnesses := true;
    fevents_witnesses := true;
    marriage_divorce := true;
    p_NBDS := true);
  check ~dry_run ~fast ~verbosity ~f_parents ~f_children ~p_NBDS ~p_parents
    ~p_families ~pevents_witnesses ~fevents_witnesses ~marriage_divorce !bname

let _ = main ()
