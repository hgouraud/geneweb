(* camlp4r *)
(* $Id: public.ml,v 5.00 2018-07-04 09:03:02 hg Exp $ *)

open Def;
open Gwdb;
open Printf;
open Gutil;
open NotesLinks;

value add_in_db db who (list_nt, list_ind) =
  let db = List.remove_assoc who db in
  if list_nt = [] && list_ind = [] then db
  else [(who, (list_nt, list_ind)) :: db]
;

value char_dir_sep = ':';

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
value trace_old = ref False;
value execute = ref True;
value ascend = ref True;
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
value testhg = ref False;
value testhg1 = ref False;
value set_friends = ref False;
value marriages = ref False;
value accents = ref False;
value cnt = ref 0;
value rgpd_files = ref ".";
value pr_nldb = ref False;
value nldb_kind = ref "";
value count_notes = ref False;
value list_notes = ref False;
value details = ref False;

value nb_ift = ref 0;
value nb_pub = ref 0;
value nb_ami = ref 0;
value nb_amm = ref 0;
value nb_prv = ref 0;
value nb_oth = ref 0;
value nbfo_ift = ref 0;
value nbfo_pub = ref 0;
value nbfo_ami = ref 0;
value nbfo_amm = ref 0;
value nbfo_prv = ref 0;
value nbfo_oth = ref 0;
value nbfn_ift = ref 0;
value nbfn_pub = ref 0;
value nbfn_ami = ref 0;
value nbfn_amm = ref 0;
value nbfn_prv = ref 0;
value nbfn_oth = ref 0;

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

value rindex s c =
  pos (String.length s - 1) where rec pos i =
    if i < 0 then None else if s.[i] = c then Some i else pos (i - 1)
;

value tr c1 c2 s =
  match rindex s c1 with
  [ Some _ ->
      String.init (String.length s) convert_char
        where convert_char i =
          if s.[i] = c1 then c2 else s.[i]
  | None -> s ]
;

(*
    let nldb () =
      let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
      let fname = Filename.concat bdir "notes_links" in
      let db = NotesLinks.read_db_from_file fname in
      let db = Notes.merge_possible_aliases conf db in
      Vnldb db
    in
*)

value get_b_dates base p =
  let (reason, d, d2) =
    match
      (Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
      get_death p, CheckItem.date_of_death (get_death p))
    with
    [ (_, Some (Dgreg d _), _, _) -> ("baptized in", d.year, d.year+lim_b.val)
    | (Some (Dgreg d _), _, _, _) -> ("born in", d.year, d.year+lim_b.val)
    | (_, _, _, _) -> ("unknown", 0, 0) ]
  in
  (reason, d, d2)
;

value get_d_dates base p =
  let (reason, d, d2) =
    match
      (Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
      get_death p, CheckItem.date_of_death (get_death p))
    with
    [ (_, _, NotDead, _) -> ("not dead", 0, 0)
    | (_, _, (Death reasn d1), Some (Dgreg d _)) -> ("dead in", d.year, (d.year+lim_d.val))
    | (_, _, DeadYoung, _) -> ("dead young", 0, -1)
    | (_, _, OfCourseDead, _) -> ("of course dead", 0, -2)
    | (_, _, DeadDontKnowWhen, _) -> ("dead, but dont know when", 0, -3)
    | (_, _, DontKnowIfDead, _) -> ("dont know if dead", 0, -4)
    | (_, _, (Death _ _), _) -> ("dead, no reason (d), no date", 0, -5) ]
  in
  (reason, d, d2)
;

value mark_old base scanned old p =
  if not scanned.(Adef.int_of_iper (get_key_index p)) &&
    old.(Adef.int_of_iper (get_key_index p)) = 0 &&
    ((get_access p) <> Public )
  then do {
    let (reason, y, old_p) = get_b_dates base p in
    if old_p > today.val || y = 0 then 
      old.(Adef.int_of_iper (get_key_index p)) := 1
    else do {
      (* birth date + lim_b > today *)
      old.(Adef.int_of_iper (get_key_index p)) := y;
      if trace_old.val then do {
        printf "Old (birth): %s, %s %d\n" (Gutil.designation base p) reason y;
        flush stdout
      } else ();
    };
    let (reason, y, old_p) = get_d_dates base p in
    if old_p > today.val || y = 0 then 
      old.(Adef.int_of_iper (get_key_index p)) := 1
    else do {
      (* death date + lim_d > today *)
      old.(Adef.int_of_iper (get_key_index p)) := y;
      if trace_old.val then do {
        printf "Old (death): %s, %s %d\n" (Gutil.designation base p) reason y;
        flush stdout
      } else ();
    };
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
              (* marriage date + lim_m < today *)
              if trace_old.val then do {
                printf "Old (marriage): %s, in %d\n" (Gutil.designation base p) (d-lim_m.val);
                printf "Old (marriage): %s, in %d\n" (Gutil.designation base sp) (d-lim_m.val);
                flush stdout;
              } else ();
              old.(Adef.int_of_iper (get_key_index p)) := d-lim_m.val;
              old.(Adef.int_of_iper (get_key_index sp)) := d-lim_m.val;
            }
            else ()
        | None -> ()];
        if not scanned.(Adef.int_of_iper isp) &&
          old.(Adef.int_of_iper (get_key_index sp)) = 0
        then do {
          scanned.(Adef.int_of_iper (get_key_index sp)) := True;
          let (reason_sp, y_sp, old_sp) = get_b_dates base sp in
          if old_sp > today.val then (* mark spouse itself as old *)
            old.(Adef.int_of_iper (get_key_index sp)) := 1
          else
            old.(Adef.int_of_iper (get_key_index sp)) := y_sp;
          let (reason_sp, y_sp, old_sp) = get_d_dates base sp in
          if old_sp > today.val then (* mark spouse itself as old *)
            old.(Adef.int_of_iper (get_key_index sp)) := 1
          else
            old.(Adef.int_of_iper (get_key_index sp)) := y_sp;
        }
        else ();
      }
    }
  }
  else ()
;

value rec mark_ancestors base scanned old p =
  if not scanned.(Adef.int_of_iper (get_key_index p)) then do {
    scanned.(Adef.int_of_iper (get_key_index p)) := True;
    if not (is_quest_string (get_first_name p)) &&
       not (is_quest_string (get_surname p))
    then 
      (* dont change access if Private *)
      if ((get_access p) <> Public && (get_access p) <> Private) then do {
        if trace.val  then do {
          let (reason, d, date) = get_b_dates base p in
          printf "Anc: %s, %s %d\n" (Gutil.designation base p) reason d;
          flush stdout; 
          }
        else ();
        let gp = {(gen_person_of_person p) with access = Public} in
        if execute.val then do {
          patch_person base gp.key_index gp;
          incr changes;
        }
        else ();
        incr cnt;
      }
      else ()
    else ();
    if ascend.val then 
      match get_parents p with
      [ Some ifam ->
          let cpl = foi base ifam in
          do {
            if old.(Adef.int_of_iper (get_father cpl)) > 1 then
              mark_ancestors base scanned old (poi base (get_father cpl)) 
            else ();
            if old.(Adef.int_of_iper (get_mother cpl)) > 1 then
              mark_ancestors base scanned old (poi base (get_mother cpl))
            else ();
          }
      | None -> () ]
    else ();
  }
  else ()
;

value public_everybody old base bname =
  let _ = printf "Public_everybody: %s\n" bname in
  let _ = flush stderr in
  do {
    for i = 0 to nb_of_persons base - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      if get_access p <> Public then do {
        incr cnt;
        let gp = {(gen_person_of_person p) with access = Public} in
        if execute.val then do {
          incr changes;
          patch_person base gp.key_index gp
        }
        else ()
      }
      else ();
    };
    if changes.val > 0 then do {
      Gwdb.commit_patches base;
      printf "Patches applied\n"; flush stdout;
      }
    else ();
  }
;

value test_public old base bname =
  let _ = printf "Test_public: %s\n" bname in
  let _ = flush stderr in
  let () = load_ascends_array base in
  let () = load_couples_array base in
  do {
    cnt.val := 0;
    for i = 0 to nb_of_persons base - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      let (reason, bd, bd2) = 
        match
          (Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
          get_death p, CheckItem.date_of_death (get_death p))
        with
        [ (Some (Dgreg d _), _, NotDead, _) -> ("born in", d.year, d.year+lim_b.val)
        | (_, Some (Dgreg d _),  NotDead, _) -> ("baptized in", d.year, d.year+lim_b.val)
        | (Some (Dgreg d _), _, DontKnowIfDead, _) -> ("born in", d.year, d.year+lim_b.val)
        | (_, Some (Dgreg d _),  DontKnowIfDead, _) -> ("baptized in", d.year, d.year+lim_b.val)
        | (_, _, _, _) -> ("other", 0, 0) ]
      in
      if bd2 > today.val && (get_access p) = Public then do {
        incr cnt;
        printf "Public: %s, %s: %d (%d)\n" (Gutil.designation base p) reason bd bd2;
        let gp = {(gen_person_of_person p) with access = IfTitles} in
        if execute.val then do { 
          patch_person base gp.key_index gp;
          incr changes;
        }
        else ();
      }
      else ();
    };
    if cnt.val > 0 then
      printf "Nb of persone: %d\n" cnt.val
    else ();
    if changes.val > 0 then do {
      Gwdb.commit_patches base;
      printf "Patches applied\n"; flush stdout;
    }
    else ();
  }
;

value test_dead_child old base bname =
  let _ = printf "Test_dead_child: %s\n" bname in
  let _ = flush stderr in
  let () = load_ascends_array base in
  let () = load_couples_array base in
  do {
    let scanned = Array.make (nb_of_persons base) False in
    for i = 0 to nb_of_persons base - 1 do {
      (*
      let p = poi base (Adef.iper_of_int i) in
      let _ = printf "Pers: %s\n" (Gutil.designation base p) in
      let _ = flush stdout in
      *)
      if not scanned.(i) then do {
        let p = poi base (Adef.iper_of_int i) in
        mark_old base scanned old p 
      }
      else ();
    };
    cnt.val := 0;
    for i = 0 to nb_of_persons base - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      let (pdreason, dd, dd2) = get_d_dates base p in
      (* dd2 <> 0 -> dead *)
      match get_parents p with
      [ Some ifam ->
          let cpl = foi base ifam in
          do {
            let fa = poi base (get_father cpl) in
            let (fbreason, fbd, fbd2) = get_b_dates base fa in
            let (fdreason, fdd, fdd2) = get_d_dates base fa in
            let f_not_old = not (fbd2 < today.val || (fdd2 <> 0 && fdd2 < today.val) ) in
            if dd2 <> 0 && f_not_old && (get_access fa = Public) then do {
              incr cnt;
              printf "Father of: %s, %s: %d; born: %d, dead: %d\n" (Gutil.designation base p)
                pdreason dd fbd fdd;
              let gp = {(gen_person_of_person fa) with access = IfTitles} in
              if execute.val then do {
                patch_person base gp.key_index gp;
                incr changes;
              }
              else ()
            }
            else ();
            let mo = poi base (get_mother cpl) in
            let (mbreason, mbd, mbd2) = get_b_dates base mo in
            let (mdreason, mdd, mdd2) = get_d_dates base mo in
            let mdd2 = if mdd2 = 0 then today.val+1 else mdd2 in
            let m_not_old = not (mbd2 < today.val || mdd2 < today.val ) in
            if dd2 <> 0 && m_not_old && (get_access mo = Public) then do {
              incr cnt;
              printf "Mother of: %s, %s: %d; born: %d, dead: %d\n" (Gutil.designation base p)
                pdreason dd mbd mdd;
              let gp = {(gen_person_of_person mo) with access = IfTitles} in
              if execute.val then patch_person base gp.key_index gp else ();
              incr changes;
            }
            else ();
          }
      | None -> () ]
    };
    if changes.val > 0 then do {
      Gwdb.commit_patches base;
      printf "Patches applied\n"; flush stdout;
    }
    else ();
   if cnt.val > 0 then
      printf "Nb of persone: %d\n" cnt.val
    else ();
  }
;

value public_all old base bname lim_year =
  let _ = printf "Public_all: %s, with lim_year: %d\n" bname lim_year in
  let _ = flush stderr in
  let () = load_ascends_array base in
  let () = load_couples_array base in
  do {
    let scanned = Array.make (nb_of_persons base) False in
    for i = 0 to nb_of_persons base - 1 do {
      (*
      let p = poi base (Adef.iper_of_int i) in
      let _ = printf "Pers: %s\n" (Gutil.designation base p) in
      let _ = flush stdout in
      *)
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
        mark_ancestors base scanned old p
      }
      else ();
    };
    if execute.val && changes.val > 0 then do {
      Gwdb.commit_patches base;
      printf "Patches applied\n"; flush stdout;
    }
    else ();
  }
;

value set_friend base p =
  let old_access = get_access p in
  let old_as = if old_access = IfTitles then "IfTitles"
    else if old_access = Public then "Public"
    else if old_access = Friend then "Friend"
    else if old_access = Friend_m then "Friend_m"
    else if old_access = Private then "Private"
    else "Other"
  in
  let fns = tr ' ' '_' (sou base (get_first_name p)) in
  let sns = tr ' ' '_' (sou base (get_surname p)) in
  let ocs = string_of_int (get_occ p) in
  let new_access =
    let d_sep = Filename.dir_sep in
    let rgpd_file = 
       rgpd_files.val ^ d_sep ^ fns ^ "." ^ ocs ^ "." ^ sns
    in
      (* if one of the files exist, set the Friend or Friend_m value *)
    if Sys.file_exists (rgpd_file ^ "-et-mineurs.pdf") then Friend_m
    else if Sys.file_exists (rgpd_file ^ ".pdf") then Friend
      (* if none of the file exist and person was Friend, then it becomes Private *)
    else if old_access = Friend || old_access = Friend_m then Private
      (* otherwise keep thee current value *)
    else old_access
  in
  let new_as = if old_access = IfTitles then "IfTitles"
    else if new_access = Public then "Public"
    else if new_access = Friend then "Friend"
    else if new_access = Friend_m then "Friend_m"
    else if new_access = Private then "Private"
    else "Other"
  in
  let tst = if execute.val=True then "" else "(?)" in
  do {
    if old_access = IfTitles then incr nb_ift
    else if old_access = Public then incr nb_pub
    else if old_access = Friend then incr nb_ami
    else if old_access = Friend_m then incr nb_amm
    else if old_access = Private then incr nb_prv
    else incr nb_oth;
    if new_access = Friend || new_access = Friend_m ||
      ( new_access = Private && (old_access = Friend || old_access = Friend_m )) then do {
      if old_access = IfTitles then incr nbfo_ift
      else if old_access = Public then incr nbfo_pub
      else if old_access = Friend then incr nbfo_ami
      else if old_access = Friend_m then incr nbfo_amm
      else if old_access = Private then incr nbfo_prv
      else incr nbfo_oth;
      if new_access = IfTitles then incr nbfn_ift
      else if new_access = Public then incr nbfn_pub
      else if new_access = Friend then incr nbfn_ami
      else if new_access = Friend_m then incr nbfn_amm
      else if new_access = Private then incr nbfn_prv
      else incr nbfn_oth;
      printf "Status: %s.%s.%s, %s %s -> %s \n" fns ocs sns old_as tst new_as; flush stdout;
    }
    else ();
    incr cnt;
    let gp = {(gen_person_of_person p) with access = new_access} in
    if execute.val && new_access <> old_access then do { 
      patch_person base gp.key_index gp;
      incr changes;
    }
    else ();
  }
;

value set_friend_all base bname =
  let _ =printf "Set_friend_all: %s, rgpd: %s\n" bname rgpd_files.val in
  let _ = flush stderr in
  let () = load_ascends_array base in
  let () = load_couples_array base in
  do {
    cnt.val := 0;
    for i = 0 to nb_of_persons base - 1 do {
      set_friend base (poi base (Adef.iper_of_int i))
    };
    if changes.val > 0 then do {
      Gwdb.commit_patches base;
      printf "Patches applied\n"; flush stdout;
    }
    else ();
    printf "Totals: IfTitle %d, Public %d, Friend %d, Friend_m %d, Private %d, Other %d\n"
      nb_ift.val nb_pub.val nb_ami.val nb_amm.val nb_prv.val nb_oth.val;
    printf "Total Friends: IfTitle %d -> %d, Public %d -> %d\n"
      nbfo_ift.val nbfn_ift.val nbfo_pub.val nbfn_pub.val;
    printf "               Friend %d -> %d, Friend_m %d -> %d\n"
      nbfo_ami.val nbfn_ami.val nbfo_amm.val nbfn_amm.val;
    printf "               Private %d -> %d, Other %d -> %d\n"
      nbfo_prv.val nbfn_prv.val nbfo_oth.val nbfn_oth.val;
      flush stdout;
  }
;

value public_some old base bname lim_year key =
  let _ = printf "Set_public_some: %s, for: %s with lim_year: %d\n"
    bname key lim_year in 
  let _ = flush stderr in
  match Gutil.person_ht_find_all base key with
  [ [ip] ->
      let p = poi base ip in
      let _ = printf "Pers: %s\n" (Gutil.designation base p) in
      let _ = flush stdout in
      let scanned = Array.make (nb_of_persons base) False in
      let () = load_ascends_array base in
      let () = load_couples_array base in
      do {
        mark_ancestors base scanned old p;
        if changes.val > 0 then do {
          Gwdb.commit_patches base;
          printf "Patches applied\n"; flush stdout;
        }
        else ();
        
      }
  | _ ->
      do {
        Printf.eprintf "Bad key %s\n" key;
        flush stderr;
        exit 2
      } ]
;

value compare_date d1 d2 =
  match (d1, d2) with
  [ (Dgreg dmy1 _, Dgreg dmy2 _) ->
      match Pervasives.compare dmy1.year dmy2.year with
      [ 0 ->
          match Pervasives.compare dmy1.month dmy2.month with
          [ 0 -> Pervasives.compare dmy1.day dmy2.day
          | x -> x ]
      | x -> x]
  | (Dgreg dmy1 _, Dtext _) -> 1
  | (Dtext _, Dgreg dmy2 _) -> -1
  | (Dtext _, Dtext _) -> 0 ]
;

value check_marriages_order base warning p = do {
  let b = Array.copy (get_family p) in
  (* Astuce : on construire un tableau identique à la famille dans *)
  (* lequel on remplace toutes les dates inconnues par la dernière *)
  (* date maximale que l'on ait vu.                                *)
  (* Exemple : Ma (mariage sans date), et M3 après M1              *)
  (* ordre initial Ma M5 Mb M3 M1 ... devient Ma M1 M3 M5 Mb       *)
  let (_, a) =
    Array.fold_left
      (fun (max_date, tab) ifam ->
        let fam = foi base ifam in
        let date =
          match Adef.od_of_codate (get_marriage fam) with
          [ Some d -> Some d
          | None -> max_date ]
        in
        let max_date =
          match (date, max_date) with
          [ (Some d1, Some d2) ->
              if compare_date d1 d2 = 1 then Some d1
              else Some d2
          | (Some d1, None) -> Some d1
          | _ -> max_date ]
        in
        (max_date, Array.append tab [| (ifam, date) |]))
      (None, [| |]) (get_family p)
  in
  Array.stable_sort
    (fun (f1, d1) (f2, d2) ->
      match (d1, d2) with
      [ (Some d1, Some d2) -> compare_date d1 d2
      | _ -> 0 ] )
    a;
  let a = Array.map (fun (f, _) -> f) a in
  if a <> b then do {
    incr cnt;
    printf "Changed order of marriages of %s\n" (Gutil.designation base p)
  }
  else ();
  if a <> b && execute.val then do {
    warning (ChangedOrderOfMarriages p b a);
    incr changes;
    flush stdout;
    let rec loop i fam =
      if i = Array.length fam then ()
      else do { fam.(i) := a.(i); loop (i + 1) fam }
    in loop 0 (get_family p);
  }
  else ()
};


value check_marriages base bname = do {
  printf "Checking order of marriages for %s\n" bname;
  flush stdout;
  let wl = ref [] in
  let warning w = wl.val := [w :: wl.val] in
  for i = 0 to nb_of_persons base - 1 do {
    let p = poi base (Adef.iper_of_int i) in
    check_marriages_order base warning p
    (* On teste les deux conjoints! cela pose t'il un problème?
       pour le commit_patches plus tard?? *)
  };
  List.iter
  (fun
    [ ChangedOrderOfMarriages p _ after ->
         patch_union base (get_key_index p) {family = after}
    | _ -> () ])
  wl.val;
  if execute.val then Gwdb.commit_patches base else ();
  printf "Done checking order of marriages\n";
  flush stdout;
};

value find_same_name_viet base p =
  let f = p_first_name base p in
  let s = p_surname base p in
  let ipl = person_ht_find_all base (f ^ " " ^ s) in
  let f = Name.strip_lower f in
  let s = Name.strip_lower s in
  let pl =
    List.fold_left
      (fun pl ip ->
         let p = poi base ip in
         if Name.strip_lower (p_first_name base p) = f &&
            Name.strip_lower (p_surname base p) = s then
           [p :: pl]
         else pl)
      [] ipl
  in
  List.sort (fun p1 p2 -> compare (get_occ p1) (get_occ p2)) pl
;

value check_accents base bname = do {
  printf "Checking accent duplicates for %s\n" bname;
  flush stdout;
  ProgrBar.full.val := '*';
  ProgrBar.start ();
  let nb_ind = nb_of_persons base in
  for i = 0 to nb_ind - 1 do {
    let p = poi base (Adef.iper_of_int i) in
    let f = sou base (Gwdb.get_first_name p) in 
    let s = sou base (Gwdb.get_surname p) in 
    let o = Gwdb.get_occ p in
    match find_same_name_viet base p with
    [ [_] -> ()
    | pl ->
        List.iter
          (fun p ->
            let ip = Adef.int_of_iper (get_key_index p) in
            let f0 = sou base (Gwdb.get_first_name p) in
            let f1 = Name.strip_lower_viet (sou base (Gwdb.get_first_name p)) in
            let s0 = sou base (Gwdb.get_surname p) in
            let s1 = Name.strip_lower_viet (sou base (Gwdb.get_surname p)) in
            let o1 = Gwdb.get_occ p in
            if (f <> "?" && s <> "?") && i <> ip && o = o1 then do {
              printf "Conflict between (%d %s.%d %s)\n" i f o s;
              printf "and (%d %s.%d %s)\n" ip f0 o1 s0 } else ()
            ) pl ];
    ProgrBar.run i nb_ind
  };
  ProgrBar.finish ();
  printf "Done checking accent duplicates\n";
  flush stdout;
};

value print_nldb bname = do {
  let base = Gwdb.open_base bname in
  let bdir = bname ^ ".gwb" in
  let fname = Filename.concat bdir "notes_links" in
  let db = NotesLinks.read_db_from_file fname in
  (*  else [(who, (list_nt, list_ind)) :: db] *)
  let ht = Hashtbl.create 2000 in
  List.iter (
    fun (who, (list_nt, list_ind)) -> do {
      let (kind, str) =
        match who with
        [ NotesLinks.PgInd iper -> ("Person", (Gutil.designation base (poi base iper)))
        | NotesLinks.PgFam  ifam -> ("Family", (string_of_int (Adef.int_of_ifam ifam)))
        | NotesLinks.PgNotes -> ("Notes", "Main note")
        | NotesLinks.PgMisc str -> ("Misc", str)
        | NotesLinks.PgWizard str -> ("Wizard", str)]
      in
      if kind = "Misc" || kind = "Notes" then
        if not (Hashtbl.mem ht str) then
          Hashtbl.add ht str (str, 1)
        else do {
          let (s, i) = Hashtbl.find ht str in
          Hashtbl.replace ht str (s, i + 1)
        } else ();
      if nldb_kind.val <> "" && nldb_kind.val = kind ||
        nldb_kind.val = "Misc" && List.length list_nt > 0 ||
        nldb_kind.val =  "" then do {
        if details.val then printf "%s: %s \n" kind str else ();
        List.iter (fun nt -> do {
          if details.val then printf "  note: %s\n" nt else ();
          let str = nt in
          if not (Hashtbl.mem ht str) then
            Hashtbl.add ht str (str, 1)
          else do {
            let (s, i) = Hashtbl.find ht str in
            Hashtbl.replace ht str (s, i + 1)
          }}
        ) list_nt;
        if nldb_kind.val <> "Misc" && details.val then
          List.iter (fun ((fn, sn, oc), _link) -> printf "  ind: %s %s %d\n" fn sn oc) list_ind
        else ()}
      else ()
    }
  ) db;
  let list = Hashtbl.fold (fun _s (s, c) l -> [(s, c) :: l]) ht [] in
  let list = List.sort (fun (s1, c1) (s2, c2) -> Gutil.alphabetic_utf_8 s1 s2) list in
  
  printf "\n";
  if count_notes.val then
    printf "Nbr of notes files: %d\n" (List.length list)
  else ();
  if list_notes.val then do {
    printf "Nbr of notes files: %d\n" (List.length list);
    List.iter (fun (s, c) -> printf "Note: %s (%d)\n" s c) list;
    printf "Nbr of notes files: %d\n" (List.length list)}
  else ()
};

value speclist =
  [("-lb", Arg.Int (fun i -> lim_b.val := i),
    "limit birth (default = " ^ string_of_int lim_b.val ^ ")");
   ("-ld", Arg.Int (fun i -> lim_d.val := i),
    "limit death (default = " ^ string_of_int lim_d.val ^ ")");
   ("-lm", Arg.Int (fun i -> lim_m.val := i),
    "limit marriage (default = " ^ string_of_int lim_m.val ^ ")");
   ("-everybody", Arg.Set everybody, "set flag public to everybody");
   ("-testhg", Arg.Set testhg, "test for dead child and still young");
   ("-testhg1", Arg.Set testhg1, "test for born < 120 and public");
   ("-set_fr", Arg.Set set_friends, "set friends");
   ("-ind", Arg.String (fun x -> ind.val := x), "individual key");
   ("-tr", Arg.Set trace, "trace changed persons");
   ("-tro", Arg.Set trace_old, "trace set to old");
   ("-ma_no", Arg.Clear ascend, "do not mark ascendants");
   ("-rgpd", Arg.String (fun x -> rgpd_files.val := x), "Set RGPD folder");
   ("-tst", Arg.Clear execute, "do not perform changes (test only)");
   ("-pr_nldb", Arg.Set pr_nldb, "print nldb");
   ("-nldb_kind", Arg.String (fun x -> nldb_kind.val := x), "filter nldb kind");
   ("-details", Arg.Set details, "give details");
   ("-count", Arg.Set count_notes, "count nbr of notes");
   ("-list", Arg.Set list_notes, "list notes");
   ("-marriages", Arg.Set marriages, "check order of marriages");
   ("-accents", Arg.Set accents, "check possible duplicates because of accents")
   ]
;
value anonfun i = bname.val := i;
value usage = "Usage: toolhg [-lb #] [-ld #] [-lm #] 
      [-everybody] [-testhg] [-testhg1] [-set_fr] [-ind] 
      [-tro] [-ma_tro] [-rgpd] [-tst] [-marriages] base.\n";
  
value main () =
  do {
    Arg.parse speclist anonfun usage;
    if bname.val = "" then do { Arg.usage speclist usage; exit 2; } else ();
    printf "Executing toolhg today (%d) on %s with -lb %d -ld %d -lm %d\n\n" 
      today.val bname.val lim_b.val lim_d.val lim_m.val;
    flush stdout;
    let gcc = Gc.get () in
    gcc.Gc.max_overhead := 100;
    Gc.set gcc;
    lim_year.val := today.val-lim_b.val;
    let base = Gwdb.open_base bname.val in
    let old = Array.make (nb_of_persons base) 0 in
    if testhg.val then test_dead_child old base bname.val
    else if marriages.val then check_marriages base bname.val
    else if accents.val then check_accents base bname.val
    else if testhg1.val then test_public old base bname.val
    else if set_friends.val then set_friend_all base bname.val
    else if everybody.val then public_everybody old base bname.val
    else if ind.val <> "" then public_some old base bname.val lim_year.val ind.val
    else if pr_nldb.val then print_nldb bname.val
    else public_all old base bname.val lim_year.val;
    if trace_old.val then do {
      printf "Set %d persons to old\n" cnt.val; flush stdout;
    } else ();
    let mar = if marriages.val then "marriage order for " else "" in
    printf "Changed %s%d (%d) persons\n" mar changes.val cnt.val; flush stdout;
  }
;

main ();
