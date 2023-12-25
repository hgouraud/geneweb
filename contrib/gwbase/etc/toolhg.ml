(* camlp4r *)
(* $Id: public.ml,v 5.00 2018-07-04 09:03:02 hg Exp $ *)

open Def
open Gwdb
open Printf
open Gutil
open NotesLinks

let add_in_db db who (list_nt, list_ind) =
  let db = List.remove_assoc who db in
  if list_nt = [] && list_ind = [] then db
  else (who, (list_nt, list_ind)) :: db

let char_dir_sep = ':'

let year_of p =
  match
    Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
    get_death p, CheckItem.date_of_death (get_death p)
  with
    _, _, NotDead, _ -> None
  | Some (Dgreg (d, _)), _, _, _ -> Some d.year
  | _, Some (Dgreg (d, _)), _, _ -> Some d.year
  | _, _, _, Some (Dgreg (d, _)) -> Some d.year
  | _ -> None

let most_recent_year_of p =
  match
    Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
    get_death p, CheckItem.date_of_death (get_death p)
  with
    _, _, NotDead, _ -> None
  | _, _, _, Some (Dgreg (d, _)) -> Some d.year
  | _, Some (Dgreg (d, _)), _, _ -> Some d.year
  | Some (Dgreg (d, _)), _, _, _ -> Some d.year
  | _ -> None

let is_old lim_year p =
  match year_of p with
    Some y -> y < lim_year
  | None -> false

let nb_gen_by_century = 3

let nb_desc_gen lim_year p =
  match most_recent_year_of p with
    Some year -> (lim_year - year) * nb_gen_by_century / 100
  | None -> 0

let changes = ref 0
let trace = ref false
let trace_old = ref false
let execute = ref true
let ascend = ref true
let age_death = ref 80
let age_sp = ref 20

let today = ref 2018
let lim_year = ref 0
let lim_b = ref 120
let lim_d = ref 20
let lim_m = ref 100
let ind = ref ""
let bname = ref ""
let everybody = ref false
let testhg = ref false
let testhg1 = ref false
let set_friends = ref false
let marriages = ref false
let cnt = ref 0
let rgpd_files = ref "."
let pr_nldb = ref false
let nldb_kind = ref ""
let count_notes = ref false
let list_notes = ref false
let details = ref false

let nb_ift = ref 0
let nb_pub = ref 0
let nb_ami = ref 0
let nb_amm = ref 0
let nb_prv = ref 0
let nb_oth = ref 0
let nbfo_ift = ref 0
let nbfo_pub = ref 0
let nbfo_ami = ref 0
let nbfo_amm = ref 0
let nbfo_prv = ref 0
let nbfo_oth = ref 0
let nbfn_ift = ref 0
let nbfn_pub = ref 0
let nbfn_ami = ref 0
let nbfn_amm = ref 0
let nbfn_prv = ref 0
let nbfn_oth = ref 0

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

let rindex s c =
  let rec pos i =
    if i < 0 then None else if s.[i] = c then Some i else pos (i - 1)
  in
  pos (String.length s - 1)

let tr c1 c2 s =
  match rindex s c1 with
    Some _ ->
      let convert_char i = if s.[i] = c1 then c2 else s.[i] in
      String.init (String.length s) convert_char
  | None -> s

(*
    let nldb () =
      let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
      let fname = Filename.concat bdir "notes_links" in
      let db = NotesLinks.read_db_from_file fname in
      let db = Notes.merge_possible_aliases conf db in
      Vnldb db
    in
*)

let get_b_dates base p =
  let (reason, d, d2) =
    match
      Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
      get_death p, CheckItem.date_of_death (get_death p)
    with
      _, Some (Dgreg (d, _)), _, _ -> "baptized in", d.year, d.year + !lim_b
    | Some (Dgreg (d, _)), _, _, _ -> "born in", d.year, d.year + !lim_b
    | _, _, _, _ -> "unknown", 0, 0
  in
  reason, d, d2

let get_d_dates base p =
  let (reason, d, d2) =
    match
      Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
      get_death p, CheckItem.date_of_death (get_death p)
    with
      _, _, NotDead, _ -> "not dead", 0, 0
    | _, _, Death (reasn, d1), Some (Dgreg (d, _)) ->
        "dead in", d.year, d.year + !lim_d
    | _, _, DeadYoung, _ -> "dead young", 0, -1
    | _, _, OfCourseDead, _ -> "of course dead", 0, -2
    | _, _, DeadDontKnowWhen, _ -> "dead, but dont know when", 0, -3
    | _, _, DontKnowIfDead, _ -> "dont know if dead", 0, -4
    | _, _, Death (_, _), _ -> "dead, no reason (d), no date", 0, -5
  in
  reason, d, d2

let mark_old base scanned old p =
  if not scanned.(Adef.int_of_iper (get_key_index p)) &&
     old.(Adef.int_of_iper (get_key_index p)) = 0 && get_access p <> Public
  then
    let (reason, y, old_p) = get_b_dates base p in
    if old_p > !today || y = 0 then
      old.(Adef.int_of_iper (get_key_index p)) <- 1
    else
      begin
        (* birth date + lim_b > today *)
        old.(Adef.int_of_iper (get_key_index p)) <- y;
        if !trace_old then
          begin
            printf "Old (birth): %s, %s %d\n" (Gutil.designation base p)
              reason y;
            flush stdout
          end
      end;
    let (reason, y, old_p) = get_d_dates base p in
    if old_p > !today || y = 0 then
      old.(Adef.int_of_iper (get_key_index p)) <- 1
    else
      begin
        (* death date + lim_d > today *)
        old.(Adef.int_of_iper (get_key_index p)) <- y;
        if !trace_old then
          begin
            printf "Old (death): %s, %s %d\n" (Gutil.designation base p)
              reason y;
            flush stdout
          end
      end;
    for i = 0 to Array.length (get_family p) - 1 do
      let ifam = (get_family p).(i) in
      let fam = foi base ifam in
      let m_date =
        match Adef.od_of_codate (get_marriage fam) with
          Some (Dgreg (d, _)) -> Some (d.year + !lim_m)
        | _ -> None
      in
      let isp = Gutil.spouse (get_key_index p) fam in
      let sp = poi base isp in
      begin match m_date with
        Some d ->
          if d < !today then
            begin
              (* marriage date + lim_m < today *)
              if !trace_old then
                begin
                  printf "Old (marriage): %s, in %d\n"
                    (Gutil.designation base p) (d - !lim_m);
                  printf "Old (marriage): %s, in %d\n"
                    (Gutil.designation base sp) (d - !lim_m);
                  flush stdout
                end;
              old.(Adef.int_of_iper (get_key_index p)) <- d - !lim_m;
              old.(Adef.int_of_iper (get_key_index sp)) <- d - !lim_m
            end
      | None -> ()
      end;
      if not scanned.(Adef.int_of_iper isp) &&
         old.(Adef.int_of_iper (get_key_index sp)) = 0
      then
        begin
          scanned.(Adef.int_of_iper (get_key_index sp)) <- true;
          let (reason_sp, y_sp, old_sp) = get_b_dates base sp in
          if old_sp > !today then
            old.(Adef.int_of_iper (get_key_index sp)) <- 1
          else old.(Adef.int_of_iper (get_key_index sp)) <- y_sp;
          let (reason_sp, y_sp, old_sp) = get_d_dates base sp in
          if old_sp > !today then
            old.(Adef.int_of_iper (get_key_index sp)) <- 1
          else old.(Adef.int_of_iper (get_key_index sp)) <- y_sp
        end
    done

let rec mark_ancestors base scanned old p =
  if not scanned.(Adef.int_of_iper (get_key_index p)) then
    begin
      scanned.(Adef.int_of_iper (get_key_index p)) <- true;
      if not (is_quest_string (get_first_name p)) &&
         not (is_quest_string (get_surname p))
      then
        if get_access p <> Public && get_access p <> Private then
          begin
            if !trace then
              begin let (reason, d, date) = get_b_dates base p in
                printf "Anc: %s, %s %d\n" (Gutil.designation base p) reason d;
                flush stdout
              end;
            let gp = {(gen_person_of_person p) with access = Public} in
            if !execute then
              begin patch_person base gp.key_index gp; incr changes end;
            incr cnt
          end;
      if !ascend then
        match get_parents p with
          Some ifam ->
            let cpl = foi base ifam in
            if old.(Adef.int_of_iper (get_father cpl)) > 1 then
              mark_ancestors base scanned old (poi base (get_father cpl));
            if old.(Adef.int_of_iper (get_mother cpl)) > 1 then
              mark_ancestors base scanned old (poi base (get_mother cpl))
        | None -> ()
    end

let public_everybody old base bname =
  let _ = printf "Public_everybody: %s\n" bname in
  let _ = flush stderr in
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    if get_access p <> Public then
      begin
        incr cnt;
        let gp = {(gen_person_of_person p) with access = Public} in
        if !execute then
          begin incr changes; patch_person base gp.key_index gp end
      end
  done;
  if !changes > 0 then
    begin
      Gwdb.commit_patches base;
      printf "Patches applied\n";
      flush stdout
    end

let test_public old base bname =
  let _ = printf "Test_public: %s\n" bname in
  let _ = flush stderr in
  let () = load_ascends_array base in
  let () = load_couples_array base in
  cnt := 0;
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    let (reason, bd, bd2) =
      match
        Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
        get_death p, CheckItem.date_of_death (get_death p)
      with
        Some (Dgreg (d, _)), _, NotDead, _ ->
          "born in", d.year, d.year + !lim_b
      | _, Some (Dgreg (d, _)), NotDead, _ ->
          "baptized in", d.year, d.year + !lim_b
      | Some (Dgreg (d, _)), _, DontKnowIfDead, _ ->
          "born in", d.year, d.year + !lim_b
      | _, Some (Dgreg (d, _)), DontKnowIfDead, _ ->
          "baptized in", d.year, d.year + !lim_b
      | _, _, _, _ -> "other", 0, 0
    in
    if bd2 > !today && get_access p = Public then
      begin
        incr cnt;
        printf "Public: %s, %s: %d (%d)\n" (Gutil.designation base p) reason
          bd bd2;
        let gp = {(gen_person_of_person p) with access = IfTitles} in
        if !execute then
          begin patch_person base gp.key_index gp; incr changes end
      end
  done;
  if !cnt > 0 then printf "Nb of persone: %d\n" !cnt;
  if !changes > 0 then
    begin
      Gwdb.commit_patches base;
      printf "Patches applied\n";
      flush stdout
    end

let test_dead_child old base bname =
  let _ = printf "Test_dead_child: %s\n" bname in
  let _ = flush stderr in
  let () = load_ascends_array base in
  let () = load_couples_array base in
  let scanned = Array.make (nb_of_persons base) false in
  for i = 0 to nb_of_persons base - 1 do
    if not scanned.(i) then
      let p = poi base (Adef.iper_of_int i) in mark_old base scanned old p
  done;
  cnt := 0;
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    let (pdreason, dd, dd2) = get_d_dates base p in
    (* dd2 <> 0 -> dead *)
    match get_parents p with
      Some ifam ->
        let cpl = foi base ifam in
        let fa = poi base (get_father cpl) in
        let (fbreason, fbd, fbd2) = get_b_dates base fa in
        let (fdreason, fdd, fdd2) = get_d_dates base fa in
        let f_not_old = not (fbd2 < !today || fdd2 <> 0 && fdd2 < !today) in
        if dd2 <> 0 && f_not_old && get_access fa = Public then
          begin
            incr cnt;
            printf "Father of: %s, %s: %d; born: %d, dead: %d\n"
              (Gutil.designation base p) pdreason dd fbd fdd;
            let gp = {(gen_person_of_person fa) with access = IfTitles} in
            if !execute then
              begin patch_person base gp.key_index gp; incr changes end
          end;
        let mo = poi base (get_mother cpl) in
        let (mbreason, mbd, mbd2) = get_b_dates base mo in
        let (mdreason, mdd, mdd2) = get_d_dates base mo in
        let mdd2 = if mdd2 = 0 then !today + 1 else mdd2 in
        let m_not_old = not (mbd2 < !today || mdd2 < !today) in
        if dd2 <> 0 && m_not_old && get_access mo = Public then
          begin
            incr cnt;
            printf "Mother of: %s, %s: %d; born: %d, dead: %d\n"
              (Gutil.designation base p) pdreason dd mbd mdd;
            let gp = {(gen_person_of_person mo) with access = IfTitles} in
            if !execute then patch_person base gp.key_index gp; incr changes
          end
    | None -> ()
  done;
  if !changes > 0 then
    begin
      Gwdb.commit_patches base;
      printf "Patches applied\n";
      flush stdout
    end;
  if !cnt > 0 then printf "Nb of persone: %d\n" !cnt

let public_all old base bname lim_year =
  let _ = printf "Public_all: %s, with lim_year: %d\n" bname lim_year in
  let _ = flush stderr in
  let () = load_ascends_array base in
  let () = load_couples_array base in
  let scanned = Array.make (nb_of_persons base) false in
  for i = 0 to nb_of_persons base - 1 do
    if not scanned.(i) then
      let p = poi base (Adef.iper_of_int i) in mark_old base scanned old p
  done;
  cnt := 0;
  for i = 0 to nb_of_persons base - 1 do if old.(i) > 1 then incr cnt done;
  let scanned = Array.make (nb_of_persons base) false in
  for i = 0 to nb_of_persons base - 1 do
    if old.(i) > 1 then
      let p = poi base (Adef.iper_of_int i) in
      mark_ancestors base scanned old p
  done;
  if !execute && !changes > 0 then
    begin
      Gwdb.commit_patches base;
      printf "Patches applied\n";
      flush stdout
    end

let set_friend base p =
  let old_access = get_access p in
  let old_as =
    if old_access = IfTitles then "IfTitles"
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
    let rgpd_file = !rgpd_files ^ d_sep ^ fns ^ "." ^ ocs ^ "." ^ sns in
    (* if one of the files exist, set the Friend or Friend_m value *)
    if Sys.file_exists (rgpd_file ^ "-et-mineurs.pdf") then Friend_m
    else if Sys.file_exists (rgpd_file ^ ".pdf") then Friend
    else if old_access = Friend || old_access = Friend_m then Private
    else old_access
  in
  let new_as =
    if old_access = IfTitles then "IfTitles"
    else if new_access = Public then "Public"
    else if new_access = Friend then "Friend"
    else if new_access = Friend_m then "Friend_m"
    else if new_access = Private then "Private"
    else "Other"
  in
  let tst = if !execute = true then "" else "(?)" in
  if old_access = IfTitles then incr nb_ift
  else if old_access = Public then incr nb_pub
  else if old_access = Friend then incr nb_ami
  else if old_access = Friend_m then incr nb_amm
  else if old_access = Private then incr nb_prv
  else incr nb_oth;
  if new_access = Friend || new_access = Friend_m ||
     new_access = Private && (old_access = Friend || old_access = Friend_m)
  then
    begin
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
      printf "Status: %s.%s.%s, %s %s -> %s \n" fns ocs sns old_as tst new_as;
      flush stdout
    end;
  incr cnt;
  let gp = {(gen_person_of_person p) with access = new_access} in
  if !execute && new_access <> old_access then
    begin patch_person base gp.key_index gp; incr changes end

let set_friend_all base bname =
  let _ = printf "Set_friend_all: %s, rgpd: %s\n" bname !rgpd_files in
  let _ = flush stderr in
  let () = load_ascends_array base in
  let () = load_couples_array base in
  cnt := 0;
  for i = 0 to nb_of_persons base - 1 do
    set_friend base (poi base (Adef.iper_of_int i))
  done;
  if !changes > 0 then
    begin
      Gwdb.commit_patches base;
      printf "Patches applied\n";
      flush stdout
    end;
  printf
    "Totals: IfTitle %d, Public %d, Friend %d, Friend_m %d, Private %d, Other %d\n"
    !nb_ift !nb_pub !nb_ami !nb_amm !nb_prv !nb_oth;
  printf "Total Friends: IfTitle %d -> %d, Public %d -> %d\n" !nbfo_ift
    !nbfn_ift !nbfo_pub !nbfn_pub;
  printf "               Friend %d -> %d, Friend_m %d -> %d\n" !nbfo_ami
    !nbfn_ami !nbfo_amm !nbfn_amm;
  printf "               Private %d -> %d, Other %d -> %d\n" !nbfo_prv
    !nbfn_prv !nbfo_oth !nbfn_oth;
  flush stdout

let public_some old base bname lim_year key =
  let _ =
    printf "Set_public_some: %s, for: %s with lim_year: %d\n" bname key
      lim_year
  in
  let _ = flush stderr in
  match Gutil.person_ht_find_all base key with
    [ip] ->
      let p = poi base ip in
      let _ = printf "Pers: %s\n" (Gutil.designation base p) in
      let _ = flush stdout in
      let scanned = Array.make (nb_of_persons base) false in
      let () = load_ascends_array base in
      let () = load_couples_array base in
      mark_ancestors base scanned old p;
      if !changes > 0 then
        begin
          Gwdb.commit_patches base;
          printf "Patches applied\n";
          flush stdout
        end
  | _ -> Printf.eprintf "Bad key %s\n" key; flush stderr; exit 2

let compare_date d1 d2 =
  match d1, d2 with
    Dgreg (dmy1, _), Dgreg (dmy2, _) ->
      begin match Pervasives.compare dmy1.year dmy2.year with
        0 ->
          begin match Pervasives.compare dmy1.month dmy2.month with
            0 -> Pervasives.compare dmy1.day dmy2.day
          | x -> x
          end
      | x -> x
      end
  | Dgreg (dmy1, _), Dtext _ -> 1
  | Dtext _, Dgreg (dmy2, _) -> -1
  | Dtext _, Dtext _ -> 0

let check_marriages_order base warning p =
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
             Some d -> Some d
           | None -> max_date
         in
         let max_date =
           match date, max_date with
             Some d1, Some d2 ->
               if compare_date d1 d2 = 1 then Some d1 else Some d2
           | Some d1, None -> Some d1
           | _ -> max_date
         in
         max_date, Array.append tab [| ifam, date |])
      (None, [| |]) (get_family p)
  in
  Array.stable_sort
    (fun (f1, d1) (f2, d2) ->
       match d1, d2 with
         Some d1, Some d2 -> compare_date d1 d2
       | _ -> 0)
    a;
  let a = Array.map (fun (f, _) -> f) a in
  if a <> b then
    begin
      incr cnt;
      printf "Changed order of marriages of %s\n" (Gutil.designation base p)
    end;
  if a <> b && !execute then
    begin
      warning (ChangedOrderOfMarriages (p, b, a));
      incr changes;
      flush stdout;
      let rec loop i fam =
        if i = Array.length fam then ()
        else begin fam.(i) <- a.(i); loop (i + 1) fam end
      in
      loop 0 (get_family p)
    end


let check_marriages base bname =
  printf "Checking order of marriages for %s\n" bname;
  flush stdout;
  let wl = ref [] in
  let warning w = wl := w :: !wl in
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    check_marriages_order base warning p
  done;
  List.iter
    (function
       ChangedOrderOfMarriages (p, _, after) ->
         patch_union base (get_key_index p) {family = after}
     | _ -> ())
    !wl;
  if !execute then Gwdb.commit_patches base;
  printf "Done checking order of marriages\n";
  flush stdout

let print_nldb bname =
  let base = Gwdb.open_base bname in
  let bdir = bname ^ ".gwb" in
  let fname = Filename.concat bdir "notes_links" in
  let db = NotesLinks.read_db_from_file fname in
  (*  else [(who, (list_nt, list_ind)) :: db] *)
  let ht = Hashtbl.create 2000 in
  List.iter
    (fun (who, (list_nt, list_ind)) ->
       let (kind, str) =
         match who with
           NotesLinks.PgInd iper ->
             "Person", Gutil.designation base (poi base iper)
         | NotesLinks.PgFam ifam ->
             "Family", string_of_int (Adef.int_of_ifam ifam)
         | NotesLinks.PgNotes -> "Notes", "Main note"
         | NotesLinks.PgMisc str -> "Misc", str
         | NotesLinks.PgWizard str -> "Wizard", str
       in
       if kind = "Misc" || kind = "Notes" then
         if not (Hashtbl.mem ht str) then Hashtbl.add ht str (str, 1)
         else
           begin let (s, i) = Hashtbl.find ht str in
             Hashtbl.replace ht str (s, i + 1)
           end;
       if !nldb_kind <> "" && !nldb_kind = kind ||
          !nldb_kind = "Misc" && List.length list_nt > 0 || !nldb_kind = ""
       then
         begin
           if !details then printf "%s: %s \n" kind str;
           List.iter
             (fun nt ->
                if !details then printf "  note: %s\n" nt;
                let str = nt in
                if not (Hashtbl.mem ht str) then Hashtbl.add ht str (str, 1)
                else
                  let (s, i) = Hashtbl.find ht str in
                  Hashtbl.replace ht str (s, i + 1))
             list_nt;
           if !nldb_kind <> "Misc" && !details then
             List.iter
               (fun ((fn, sn, oc), _link) ->
                  printf "  ind: %s %s %d\n" fn sn oc)
               list_ind
         end)
    db;
  let list = Hashtbl.fold (fun _s (s, c) l -> (s, c) :: l) ht [] in
  let list =
    List.sort (fun (s1, c1) (s2, c2) -> Gutil.alphabetic_utf_8 s1 s2) list
  in
  printf "\n";
  if !count_notes then printf "Nbr of notes files: %d\n" (List.length list);
  if !list_notes then
    begin
      printf "Nbr of notes files: %d\n" (List.length list);
      List.iter (fun (s, c) -> printf "Note: %s (%d)\n" s c) list;
      printf "Nbr of notes files: %d\n" (List.length list)
    end

let speclist =
  ["-lb", Arg.Int (fun i -> lim_b := i),
   "limit birth (default = " ^ string_of_int !lim_b ^ ")";
   "-ld", Arg.Int (fun i -> lim_d := i),
   "limit death (default = " ^ string_of_int !lim_d ^ ")";
   "-lm", Arg.Int (fun i -> lim_m := i),
   "limit marriage (default = " ^ string_of_int !lim_m ^ ")";
   "-everybody", Arg.Set everybody, "set flag public to everybody";
   "-testhg", Arg.Set testhg, "test for dead child and still young";
   "-testhg1", Arg.Set testhg1, "test for born < 120 and public";
   "-set_fr", Arg.Set set_friends, "set friends";
   "-ind", Arg.String (fun x -> ind := x), "individual key";
   "-tr", Arg.Set trace, "trace changed persons";
   "-tro", Arg.Set trace_old, "trace set to old";
   "-ma_no", Arg.Clear ascend, "do not mark ascendants";
   "-rgpd", Arg.String (fun x -> rgpd_files := x), "Set RGPD folder";
   "-tst", Arg.Clear execute, "do not perform changes (test only)";
   "-pr_nldb", Arg.Set pr_nldb, "print nldb";
   "-nldb_kind", Arg.String (fun x -> nldb_kind := x), "filter nldb kind";
   "-details", Arg.Set details, "give details";
   "-count", Arg.Set count_notes, "count nbr of notes";
   "-list", Arg.Set list_notes, "list notes";
   "-marriages", Arg.Set marriages, "check order of marriages"]
let anonfun i = bname := i
let usage =
  "Usage: toolhg [-lb #] [-ld #] [-lm #] 
      [-everybody] [-testhg] [-testhg1] [-set_fr] [-ind] 
      [-tro] [-ma_tro] [-rgpd] [-tst] [-marriages] base.\n"
  
let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  printf "Executing toolhg today (%d) on %s with -lb %d -ld %d -lm %d\n\n"
    !today !bname !lim_b !lim_d !lim_m;
  flush stdout;
  let gcc = Gc.get () in
  gcc.Gc.max_overhead <- 100;
  Gc.set gcc;
  lim_year := !today - !lim_b;
  let base = Gwdb.open_base !bname in
  let old = Array.make (nb_of_persons base) 0 in
  if !testhg then test_dead_child old base !bname
  else if !marriages then check_marriages base !bname
  else if !testhg1 then test_public old base !bname
  else if !set_friends then set_friend_all base !bname
  else if !everybody then public_everybody old base !bname
  else if !ind <> "" then public_some old base !bname !lim_year !ind
  else if !pr_nldb then print_nldb !bname
  else public_all old base !bname !lim_year;
  if !trace_old then
    begin printf "Set %d persons to old\n" !cnt; flush stdout end;
  let mar = if !marriages then "marriage order for " else "" in
  printf "Changed %s%d (%d) persons\n" mar !changes !cnt; flush stdout

let _ = main ()
