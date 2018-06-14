(* camlp4r *)
(* $Id: public.ml,v 4.26 2007-01-19 09:03:02 deraugla Exp $ *)

open Def;
open Gwdb;
open Printf;

value year_of p =
  match
    (Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
     get_death p, CheckItem.date_of_death (get_death p))
  with
  [ (_, _, NotDead, _) -> None
  | (Some (Dgreg d _), _, _, _) -> Some d.year
  | (_, Some (Dgreg d _), _, _) -> Some d.year
  | (_, _, _, Some (Dgreg d _)) -> Some d.year
  | _ -> None ]
;

value most_recent_year_of p =
  match
    (Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
     get_death p, CheckItem.date_of_death (get_death p))
  with
  [ (_, _, NotDead, _) -> None
  | (_, _, _, Some (Dgreg d _)) -> Some d.year
  | (_, Some (Dgreg d _), _, _) -> Some d.year
  | (Some (Dgreg d _), _, _, _) -> Some d.year
  | _ -> None ]
;

value is_old lim_year p =
  match year_of p with
  [ Some y -> y < lim_year
  | None -> False ]
;

value nb_gen_by_century = 3;

value nb_desc_gen lim_year p =
  match most_recent_year_of p with
  [ Some year -> (lim_year - year) * nb_gen_by_century / 100
  | None -> 0 ]
;

value changes = ref 0;
value trace = ref False;
value execute = ref True;
value ascend = ref False;
value descend = ref False;
value age_death = ref 80;
value age_sp = ref 20;

value today = ref 2018;
value lim_year = ref 0;
value lim_b = ref 120;
value lim_d = ref 20;
value lim_m = ref 100;
value ind = ref "";
value bname = ref "";
value everybody = ref False;

(*
type death =
  [ NotDead
  | Death of death_reason and cdate
  | DeadYoung
  | DeadDontKnowWhen
  | DontKnowIfDead
  | OfCourseDead ]
;
*)

value get_dates base p =
  let (reason, d, d2) =
    match
      (Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
      get_death p, CheckItem.date_of_death (get_death p))
    with
    [ (_, _, NotDead, _) -> ("not dead", 0, 0)
    | (_, Some (Dgreg d _), _, _) -> ("baptized on", d.year, d.year+lim_b.val)
    | (Some (Dgreg d _), _, _, _) -> ("born on", d.year, d.year+lim_b.val)
    | (_, _, (Death reasn d1), Some (Dgreg d _)) -> ("dead on", d.year, (d.year+lim_d.val))
    | (_, _, DeadYoung, _) -> ("dead young", 0, -1)
    | (_, _, OfCourseDead, _) -> ("of course dead", 0, -2)
    | (_, _, DeadDontKnowWhen, _) -> ("dead, but dont know when", 0, -3)
    | (_, _, DontKnowIfDead, _) -> ("dont know if dead", 0, -4)
    | (_, _, (Death _ _), _) -> ("dead, no reason (d), no date", 0, -5)]
  in
  (reason, d, d2)
;

value mark_old base scanned old p =
  if not scanned.(Adef.int_of_iper (get_key_index p)) then do {
    let (reason, y, old_p) = get_dates base p in
    if old_p > today.val then 
      old.(Adef.int_of_iper (get_key_index p)) := 1
    else
      old.(Adef.int_of_iper (get_key_index p)) := old_p;
    for i = 0 to Array.length (get_family p) - 1 do {
      let ifam = (get_family p).(i) in
      let fam = foi base ifam in
      let m_date = 
        match Adef.od_of_codate (get_marriage fam) with
        [ Some (Dgreg d _) -> Some (d.year+lim_m.val)
        | _ -> None ]
      in
      let isp = Gutil.spouse (get_key_index p) fam in
      let sp = poi base isp in do {
        match m_date with
        [ Some d -> 
            if d < today.val then do {
              old.(Adef.int_of_iper (get_key_index p)) := d-lim_m.val;
              old.(Adef.int_of_iper (get_key_index sp)) := d-lim_m.val;
            }
            else ()
        | None -> ()];
        if not scanned.(Adef.int_of_iper isp) then do {
          let (reason_sp, y_sp, old_sp) = get_dates base sp in
          scanned.(Adef.int_of_iper (get_key_index sp)) := True;
          if old_sp > lim_year.val then (* mark spouse itself as old *)
            old.(Adef.int_of_iper (get_key_index sp)) := 1
          else
            old.(Adef.int_of_iper (get_key_index sp)) := old_p;
        }
        else ();
      }
    }
  }
  else ()
;

value rec mark_ancestors base scanned p =
  if not scanned.(Adef.int_of_iper (get_key_index p)) then do {
    scanned.(Adef.int_of_iper (get_key_index p)) := True;
    if not (is_quest_string (get_first_name p)) &&
       not (is_quest_string (get_surname p))
    then do {
      if trace.val then do {
        let (reason, d, date) = get_dates base p in
          printf "Anc: %s, %s %d\n" (Gutil.designation base p) reason d; flush stdout; 
        } else ();
      let p = {(gen_person_of_person p) with access = Public} in
      if execute.val then patch_person base p.key_index p else ();
      incr changes;
    }
    else ();
    if ascend.val then 
      match get_parents p with
      [ Some ifam ->
          let cpl = foi base ifam in
          do {
            mark_ancestors base scanned (poi base (get_father cpl));
            mark_ancestors base scanned (poi base (get_mother cpl));
          }
      | None -> () ]
    else ();
  }
  else ()
;

value public_everybody bname =
  let base = Gwdb.open_base bname in
  do {
    for i = 0 to nb_of_persons base - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      if get_access p <> Public then
        let p = {(gen_person_of_person p) with access = Public} in
        if execute.val then patch_person base p.key_index p else ()
      else ();
    };
    commit_patches base;
  }
;

value cnt = ref 0;

value public_all bname lim_year =
  let base = Gwdb.open_base bname in
  let () = load_ascends_array base in
  let () = load_couples_array base in
  let old = Array.make (nb_of_persons base) 0 in
  do {
    let scanned = Array.make (nb_of_persons base) False in
    for i = 0 to nb_of_persons base - 1 do {
      if not scanned.(i) then do {
        let p = poi base (Adef.iper_of_int i) in
        mark_old base scanned old p 
      }
      else ();
    };
    cnt.val := 0;
    for i = 0 to nb_of_persons base - 1 do {
      if old.(i) > 1 then incr cnt else ();
    };
    let scanned = Array.make (nb_of_persons base) False in
    for i = 0 to nb_of_persons base - 1 do {
      if old.(i) > 1 then do {
        let p = poi base (Adef.iper_of_int i) in
        mark_ancestors base scanned p
      }
      else ();
    };
    if changes.val > 0 then commit_patches base else ();
  }
;

value public_some bname lim_year key =
  let base = Gwdb.open_base bname in
  match Gutil.person_ht_find_all base key with
  [ [ip] ->
      let p = poi base ip in
      let scanned = Array.make (nb_of_persons base) False in
      let () = load_ascends_array base in
      let () = load_couples_array base in
      do {
        mark_ancestors base scanned p;
        if changes.val > 0 then commit_patches base else ();
      }
  | _ ->
      do {
        Printf.eprintf "Bad key %s\n" key;
        flush stderr;
        exit 2
      } ]
;


value speclist =
  [("-lb", Arg.Int (fun i -> lim_b.val := i),
    "limit birth (default = " ^ string_of_int lim_b.val ^ ")");
   ("-ld", Arg.Int (fun i -> lim_d.val := i),
    "limit death (default = " ^ string_of_int lim_d.val ^ ")");
   ("-lm", Arg.Int (fun i -> lim_m.val := i),
    "limit marriage (default = " ^ string_of_int lim_m.val ^ ")");
   ("-everybody", Arg.Set everybody, "set flag public to everybody");
   ("-ind", Arg.String (fun x -> ind.val := x), "individual key");
   ("-tr", Arg.Set trace, "trace changed persons");
   ("-ma", Arg.Set ascend, "mark ascendants");
   ("-tst", Arg.Clear execute, "do not perform changes (test only)")]
;
value anonfun i = bname.val := i;
value usage = "Usage: public [-y #] [-a #] [-everybody] [-ind key] 
  [-ma] [-md] [-tr] [-tst] base.\n
  Public if born < y or dead < y+a";

value main () =
  do {
    Arg.parse speclist anonfun usage;
    if bname.val = "" then do { Arg.usage speclist usage; exit 2; } else ();
    let gcc = Gc.get () in
    gcc.Gc.max_overhead := 100;
    Gc.set gcc;
    lim_year.val := today.val-lim_b.val;
    if everybody.val then public_everybody bname.val
    else if ind.val = "" then public_all bname.val lim_year.val
    else public_some bname.val lim_year.val ind.val;
    printf "Set %d persons to old\n" cnt.val; flush stdout;
    printf "Changed %d persons\n" changes.val; flush stdout;
  }
;

main ();
