(* gwreplay: replay onto a base rebuilt from an older .gw dump the changes
   recorded by history_diff since a date.

   Stage 1 (always): inventory of the [history] log and the [history_d]
   records of the source base (the base that received the changes).
   Stage 2 (-target): plan of what would be done on the target base (the
   base rebuilt from the dump); nothing is written (-dry-run, the default).
   -target -verify: checks that the state recorded at the end of the window
   (-until) is present in the target base; read-only.
   Stage 3 (-target -write): the plan is applied to the target base, then
   committed. The source base is never written. Work on a copy of the
   target, and run gwfixbase and consang on it afterwards. *)

open Geneweb
open Def
module Driver = Geneweb_db.Driver
module Iper = Driver.Iper
module Ifam = Driver.Ifam

type file_status = Clean | Truncated of int
type pkey = { fn : string; sn : string; occ : int }

let string_of_pkey k = Printf.sprintf "%s.%d %s" k.fn k.occ k.sn
let pkey_equal a b = a.fn = b.fn && a.sn = b.sn && a.occ = b.occ

let pkey_of_gen (p : (_, _, string) gen_person) =
  { fn = p.first_name; sn = p.surname; occ = p.occ }

(* Same person key, ignoring case and accents as GeneWeb does. *)
let same_key a b =
  a.occ = b.occ
  && Name.lower a.fn = Name.lower b.fn
  && Name.lower a.sn = Name.lower b.sn

let key_of p = string_of_pkey (pkey_of_gen p)

type person_entry = {
  file : string;
  nb_in_window : int;
  first_date : string;
  last : HistoryDiff.gen_record;
  dump_key : pkey;
      (** key at the time of the dump: last record before the window, or the
          first record of the window (history_diff writes the old state first
          when a person is modified for the first time). *)
  status : file_status;
  current_key : pkey;
      (** key in the last record of the file, whatever its date: the key the
          person should have in the source base now *)
}

type family_entry = {
  fam_date : string;
  fam : (Iper.t, Ifam.t, string) gen_family;
  children : Iper.t array;
  spouses : Iper.t list;
}

let date_of (r : HistoryDiff.gen_record) = Adef.as_string r.HistoryDiff.date

(* Read every record of one history file. A record that cannot be read
   (truncated by a full disk, or written in an older format) stops the
   reading of that file; it is reported, not fatal. *)
let read_history_file fname =
  let ic = open_in_bin fname in
  let len = in_channel_length ic in
  let rec loop acc =
    if pos_in ic >= len then (List.rev acc, Clean)
    else
      match (input_value ic : HistoryDiff.gen_record) with
      | r -> loop (r :: acc)
      | exception (End_of_file | Failure _) ->
          (List.rev acc, Truncated (List.length acc))
  in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> loop [])

let rec walk dir acc =
  Array.fold_left
    (fun acc name ->
      let path = Filename.concat dir name in
      if Sys.is_directory path then walk path acc else path :: acc)
    acc (Sys.readdir dir)

let duplicates arr =
  let seen = Iper.Table.create 8 in
  Array.fold_left
    (fun acc ip ->
      if Iper.Table.mem seen ip then
        if List.exists (Iper.equal ip) acc then acc else ip :: acc
      else (
        Iper.Table.add seen ip ();
        acc))
    [] arr

let family_duplicates (f : (Iper.t, Ifam.t, string) gen_family) =
  let from_events =
    List.fold_left
      (fun acc e ->
        List.rev_append (duplicates (Array.map fst e.efam_witnesses)) acc)
      [] f.fevents
  in
  List.rev_append (duplicates f.witnesses) from_events

let person_duplicates (p : (Iper.t, Iper.t, string) gen_person) =
  List.fold_left
    (fun acc e ->
      List.rev_append (duplicates (Array.map fst e.epers_witnesses)) acc)
    [] p.pevents

(* Every iper a replayed record points to, with the places it is referenced
   from (used to explain unresolved references). *)
let referenced_ipers ~fam_label persons families =
  let refs = Iper.Table.create 1024 in
  let add why ip =
    if not (Iper.is_dummy ip) then
      let l = Option.value ~default:[] (Iper.Table.find_opt refs ip) in
      Iper.Table.replace refs ip (why :: l)
  in
  List.iter
    (fun e ->
      let p = e.last.HistoryDiff.gen_p in
      let k = key_of p in
      List.iter
        (fun r ->
          Option.iter (add ("relation parent of " ^ k)) r.r_fath;
          Option.iter (add ("relation parent of " ^ k)) r.r_moth)
        p.rparents;
      List.iter (add ("listed in related of " ^ k)) p.related;
      List.iter
        (fun ev ->
          Array.iter
            (fun (ip, _) -> add ("witness of a personal event of " ^ k) ip)
            ev.epers_witnesses)
        p.pevents)
    persons;
  Ifam.Table.iter
    (fun ifam fe ->
      let f = fam_label ifam fe in
      Array.iter (add ("witness of " ^ f)) fe.fam.witnesses;
      List.iter
        (fun ev ->
          Array.iter
            (fun (ip, _) -> add ("witness of a family event of " ^ f) ip)
            ev.efam_witnesses)
        fe.fam.fevents;
      Array.iter (add ("child of " ^ f)) fe.children;
      List.iter (add ("spouse in " ^ f)) fe.spouses)
    families;
  refs

type log_line = { time : string; user : string; action : string; key : string }

let parse_log_line line =
  if String.length line < 23 then None
  else
    let time = String.sub line 0 19 in
    let user, i =
      match (line.[20], String.index_from_opt line 20 ']') with
      | '[', Some j -> (String.sub line 21 (j - 21), j + 2)
      | _ -> ("", 20)
    in
    if i + 2 > String.length line then None
    else
      let action = String.sub line i 2 in
      let key =
        if i + 3 <= String.length line then
          String.sub line (i + 3) (String.length line - i - 3)
        else ""
      in
      Some { time; user; action; key }

(* "First names.occ Surname": the occurrence number is the first ".N "
   (digits followed by a space); first names may themselves contain dots. *)
let parse_key s =
  let len = String.length s in
  let rec digits j =
    if j < len && s.[j] >= '0' && s.[j] <= '9' then digits (j + 1) else j
  in
  let rec find i =
    if i >= len then None
    else if s.[i] = '.' then
      let j = digits (i + 1) in
      if j > i + 1 && j < len && s.[j] = ' ' then
        Some
          {
            fn = String.sub s 0 i;
            occ = int_of_string (String.sub s (i + 1) (j - i - 1));
            sn = String.sub s (j + 1) (len - j - 1);
          }
      else find (i + 1)
    else find (i + 1)
  in
  find 0

let read_log fname since until =
  if not (Sys.file_exists fname) then []
  else
    let ic = open_in fname in
    let rec loop acc =
      match input_line ic with
      | line -> (
          match parse_log_line line with
          | Some l
            when String.compare l.time since >= 0
                 && String.compare l.time until <= 0 ->
              loop (l :: acc)
          | Some _ | None -> loop acc)
      | exception End_of_file -> List.rev acc
    in
    Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> loop [])

let with_out dir name f =
  let oc = open_out (Filename.concat dir name) in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> f oc)

let plural n s = if n > 1 then s else ""

(* ---------------------------------------------------------------------- *)
(* Stage 2: plan on the target base                                        *)

type person_action =
  | P_modify of Iper.t
  | P_rename of Iper.t  (** found under its dump key, key changed since *)
  | P_create

type target_ref =
  | Existing of Iper.t
  | New  (** created by this replay *)
  | Missing  (** neither in the target base nor created by this replay *)
  | Unknown  (** no key known for the source iper *)

type family_action = F_modify of Ifam.t | F_create | F_blocked of string

let string_of_target_ref r =
  match r with
  | Existing ip -> "target iper " ^ Iper.to_string ip
  | New -> "created by the replay"
  | Missing -> "MISSING in target"
  | Unknown -> "UNKNOWN key"

(* ---------------------------------------------------------------------- *)
(* Stage 3: write onto the target base                                     *)

let dedupe_by fst_of l =
  let seen = Iper.Table.create 8 in
  List.filter
    (fun x ->
      let ip = fst_of x in
      if Iper.Table.mem seen ip then false
      else (
        Iper.Table.add seen ip ();
        true))
    l

let apply ~target ~out_dir ~assume_yes ~tb ~find ~keys ~couples ~p_actions
    ~f_actions =
  let log_oc = open_out (Filename.concat out_dir "apply.log") in
  Fun.protect ~finally:(fun () -> close_out log_oc) @@ fun () ->
  let logf fmt =
    Printf.kfprintf
      (fun oc ->
        output_char oc '\n';
        flush oc)
      log_oc fmt
  in
  let lock_file = Mutil.lock_file target in
  let on_exn exn bt =
    Format.eprintf "%a@." Lock.pp_exception (exn, bt);
    exit 2
  in
  Lock.control ~on_exn ~wait:false ~lock_file @@ fun () ->
  let istr s = Driver.insert_string tb s in
  let key src =
    match Iper.Table.find_opt keys src with
    | Some k -> string_of_pkey k
    | None -> "?" ^ Iper.to_string src
  in
  (* source iper -> target iper *)
  let tmap = Iper.Table.create 1024 in
  let resolve src =
    match Iper.Table.find_opt tmap src with
    | Some ip -> Some ip
    | None -> (
        match Iper.Table.find_opt keys src with
        | None -> None
        | Some k -> (
            match find k with
            | Some ip ->
                Iper.Table.replace tmap src ip;
                Some ip
            | None -> None))
  in
  (* 1. persons to create: bare records first, so that every reference made
     by the other records can be resolved in the next passes. *)
  let n_created = ref 0 in
  List.iter
    (fun (e, a) ->
      let p = e.last.HistoryDiff.gen_p in
      match a with
      | P_modify ip | P_rename ip -> Iper.Table.replace tmap p.key_index ip
      | P_create ->
          let np =
            {
              (Driver.no_person Iper.dummy) with
              first_name = istr p.first_name;
              surname = istr p.surname;
              occ = p.occ;
              sex = p.sex;
            }
          in
          let ip =
            Driver.insert_person_with_union_and_ascendants tb np
              Driver.no_ascend Driver.no_union
          in
          Iper.Table.replace tmap p.key_index ip;
          incr n_created;
          logf "person %s: created as target iper %s" (key_of p)
            (Iper.to_string ip))
    p_actions;
  let witnesses ctx arr =
    Array.to_list arr
    |> List.filter_map (fun (ip, wk) ->
        match resolve ip with
        | Some t -> Some (t, wk)
        | None ->
            logf "%s: witness %s dropped (not in target)" ctx (key ip);
            None)
    |> dedupe_by fst |> Array.of_list
  in
  let persons_of ctx role arr =
    Array.to_list arr
    |> List.filter_map (fun ip ->
        match resolve ip with
        | Some t -> Some t
        | None ->
            logf "%s: %s %s dropped (not in target)" ctx role (key ip);
            None)
    |> dedupe_by (fun x -> x)
  in
  (* Renamed persons: the pages linking to their old key are collected
     before anything is written, as gwd does before saving a person. *)
  let conf = Config.{ empty with wizard = true; bname = Driver.bname tb } in
  let renames =
    match
      List.filter_map
        (fun (_, a) ->
          match a with
          | P_rename tip -> Some tip
          | P_modify _ | P_create -> None)
        p_actions
    with
    | [] -> []
    | tips ->
        let db = Notes.merge_possible_aliases conf (Driver.read_nldb tb) in
        List.map
          (fun tip ->
            let old_key =
              Util.make_key tb (Driver.gen_person_of_person (Driver.poi tb tip))
            in
            (tip, old_key, Notes.links_to_ind conf tb db old_key None))
          tips
  in
  (* 2. persons: full record, references translated to target ipers *)
  let n_persons = ref 0 in
  List.iter
    (fun (e, _) ->
      let p = e.last.HistoryDiff.gen_p in
      let ctx = "person " ^ key_of p in
      match Iper.Table.find_opt tmap p.key_index with
      | None -> logf "%s: skipped, no target iper" ctx
      | Some tip ->
          let rparents =
            List.filter_map
              (fun r ->
                let tr = Option.map (fun ip -> (ip, resolve ip)) in
                match (tr r.r_fath, tr r.r_moth) with
                | (None | Some (_, None)), (None | Some (_, None)) ->
                    logf "%s: relation dropped (parents not in target)" ctx;
                    None
                | f, m ->
                    Some
                      {
                        r with
                        r_fath = Option.bind f snd;
                        r_moth = Option.bind m snd;
                      })
              p.rparents
          in
          let pevents =
            List.map
              (fun ev ->
                { ev with epers_witnesses = witnesses ctx ev.epers_witnesses })
              p.pevents
          in
          let related =
            let current = Driver.get_related (Driver.poi tb tip) in
            List.rev_append
              (persons_of ctx "related" (Array.of_list p.related))
              current
            |> List.rev
            |> dedupe_by (fun x -> x)
          in
          let np = { p with key_index = tip; rparents; pevents; related } in
          let np = Futil.map_person_ps (fun ip -> ip) istr np in
          let np = { np with key_index = tip } in
          Driver.patch_person tb tip np;
          Notes.update_notes_links_person conf tb np;
          incr n_persons;
          logf "%s: written to target iper %s" ctx (Iper.to_string tip))
    p_actions;
  (* 3. families *)
  let set_parents ctx tifam child =
    let cp = Driver.poi tb child in
    match Driver.get_parents cp with
    | Some x when Ifam.equal x tifam -> ()
    | old ->
        (match old with
        | Some other ->
            let d = Driver.gen_descend_of_family (Driver.foi tb other) in
            Driver.patch_descend tb other
              {
                children =
                  Array.of_list
                    (List.filter
                       (fun c -> not (Iper.equal c child))
                       (Array.to_list d.children));
              };
            logf "%s: child %s moved from family %s" ctx (Iper.to_string child)
              (Ifam.to_string other)
        | None -> ());
        Driver.patch_ascend tb child
          { parents = Some tifam; consang = Adef.no_consang }
  in
  let n_fam_mod = ref 0 and n_fam_new = ref 0 and n_fam_skip = ref 0 in
  List.iter
    (fun (ifam, fe, a, _) ->
      let ctx = "family " ^ Ifam.to_string ifam in
      match a with
      | F_blocked why ->
          incr n_fam_skip;
          logf "%s: skipped, %s" ctx why
      | F_modify _ | F_create -> (
          match Ifam.Table.find_opt couples ifam with
          | None ->
              incr n_fam_skip;
              logf "%s: skipped, couple unknown" ctx
          | Some (fs, ms) -> (
              match (resolve fs, resolve ms) with
              | Some tf, Some tm ->
                  let children =
                    Array.of_list (persons_of ctx "child" fe.children)
                  in
                  let fevents =
                    List.map
                      (fun ev ->
                        {
                          ev with
                          efam_witnesses = witnesses ctx ev.efam_witnesses;
                        })
                      fe.fam.fevents
                  in
                  let fwit =
                    Array.of_list (persons_of ctx "witness" fe.fam.witnesses)
                  in
                  let f = { fe.fam with witnesses = fwit; fevents } in
                  let f =
                    Futil.map_family_ps (fun ip -> ip) (fun i -> i) istr f
                  in
                  let tifam =
                    match a with
                    | F_modify ti ->
                        let old = Driver.get_children (Driver.foi tb ti) in
                        Array.iter
                          (fun c ->
                            if not (Array.exists (Iper.equal c) children) then
                              match Driver.get_parents (Driver.poi tb c) with
                              | Some x when Ifam.equal x ti ->
                                  Driver.patch_ascend tb c
                                    {
                                      parents = None;
                                      consang = Adef.no_consang;
                                    };
                                  logf "%s: child %s removed" ctx
                                    (Iper.to_string c)
                              | Some _ | None -> ())
                          old;
                        incr n_fam_mod;
                        ti
                    | F_create | F_blocked _ ->
                        let ti =
                          Driver.insert_family_with_couple_and_descendants tb
                            (Driver.no_family Ifam.dummy)
                            Driver.no_couple Driver.no_descend
                        in
                        Driver.patch_couple tb ti (Adef.couple tf tm);
                        List.iter
                          (fun sp ->
                            let u = Driver.get_family (Driver.poi tb sp) in
                            if not (Array.exists (Ifam.equal ti) u) then
                              Driver.patch_union tb sp
                                { family = Array.append u [| ti |] })
                          [ tf; tm ];
                        incr n_fam_new;
                        ti
                  in
                  let f = { f with fam_index = tifam } in
                  Driver.patch_family tb tifam f;
                  Notes.update_notes_links_family conf tb f;
                  Driver.patch_descend tb tifam { children };
                  Array.iter (set_parents ctx tifam) children;
                  logf "%s: written to target family %s (%d children)" ctx
                    (Ifam.to_string tifam) (Array.length children)
              | None, (Some _ | None) | Some _, None ->
                  incr n_fam_skip;
                  logf "%s: skipped, a spouse is not in target" ctx)))
    f_actions;
  (* 4. links to renamed persons, in notes of persons, families, misc
     notes and wizard notes; asked for confirmation unless -yes *)
  let to_rename =
    List.filter_map
      (fun (tip, old_key, pgl) ->
        let p = Driver.gen_person_of_person (Driver.poi tb tip) in
        if old_key = Util.make_key tb p then None
        else
          Some
            ( old_key,
              (Driver.sou tb p.first_name, Driver.sou tb p.surname, p.occ),
              pgl ))
      renames
  in
  let n_renamed = ref 0 in
  (match to_rename with
  | [] -> ()
  | l ->
      let n = List.length l in
      Printf.printf "\n%d person%s renamed by the replay:\n" n (plural n "s");
      List.iter
        (fun ((ofn, osn, oocc), (nfn, nsn, nocc), pgl) ->
          Printf.printf "  %s.%d %s -> %s.%d %s (%d linking page%s)\n" ofn oocc
            osn nfn nocc nsn (List.length pgl)
            (plural (List.length pgl) "s"))
        l;
      let proceed =
        assume_yes
        ||
        (Printf.printf "Rewrite the links to these %d person%s? [Y/n] %!" n
           (plural n "s");
         match String.trim (input_line stdin) with
         | "" | "y" | "Y" | "yes" | "o" | "O" | "oui" -> true
         | _ -> false
         | exception End_of_file ->
             print_endline "\n(no answer: links not rewritten; use -yes)";
             false)
      in
      if proceed then
        List.iter
          (fun ( ((ofn, osn, oocc) as old_key),
                 ((nfn, nsn, nocc) as new_key),
                 pgl ) ->
            Notes.update_ind_key conf tb pgl old_key new_key;
            incr n_renamed;
            logf "rename %s.%d %s -> %s.%d %s: %d linking pages rewritten" ofn
              oocc osn nfn nocc nsn (List.length pgl))
          l
      else
        List.iter
          (fun ((ofn, osn, oocc), (nfn, nsn, nocc), _) ->
            logf "rename %s.%d %s -> %s.%d %s: links NOT rewritten (declined)"
              ofn oocc osn nfn nocc nsn)
          l);
  Driver.commit_patches tb;
  let msg =
    Printf.sprintf
      "Written to %s: %d persons (%d created, %d renamed with their links \
       updated), %d families modified, %d created, %d skipped. Details in %s."
      target !n_persons !n_created !n_renamed !n_fam_mod !n_fam_new !n_fam_skip
      (Filename.concat out_dir "apply.log")
  in
  logf "%s" msg;
  print_endline msg;
  print_endline
    "Next: run gwfixbase (related links, witnesses) and consang on the target."

(* ---------------------------------------------------------------------- *)
(* -verify: is the state recorded at the end of the window in the target?  *)

type check_status =
  | Ok_same
  | Differs of (string * string option) list
      (** field name, and "expected -> actual" for text fields *)
  | Absent of string

let dedupe_pairs arr =
  let seen = Hashtbl.create 8 in
  Array.of_list
    (List.filter
       (fun (ip, _) ->
         if Hashtbl.mem seen ip then false
         else (
           Hashtbl.add seen ip ();
           true))
       (Array.to_list arr))

let dedupe_arr arr =
  let seen = Hashtbl.create 8 in
  Array.of_list
    (List.filter
       (fun ip ->
         if Hashtbl.mem seen ip then false
         else (
           Hashtbl.add seen ip ();
           true))
       (Array.to_list arr))

let short s =
  let s = String.map (fun c -> if c = '\n' || c = '\t' then ' ' else c) s in
  if String.length s <= 60 then s else String.sub s 0 57 ^ "..."

(* A text field: show both values when they differ. *)
let txt name a b =
  if a = b then None
  else Some (name, Some (Printf.sprintf "%S -> %S" (short a) (short b)))

let txts name a b =
  if a = b then None
  else
    Some
      ( name,
        Some
          (Printf.sprintf "[%s] -> [%s]"
             (short (String.concat "; " a))
             (short (String.concat "; " b))) )

(* Any other field: name only. *)
let fld name same = if same then None else Some (name, None)

(* Person fields compared (related and key_index are not: related is
   rebuilt by gwfixbase, key_index is the base's own numbering). *)
let person_diffs (a : (_, Iper.t option, string) gen_person)
    (b : (_, Iper.t option, string) gen_person) =
  let pev (p : (_, Iper.t option, string) gen_person) =
    List.map
      (fun e -> { e with epers_witnesses = dedupe_pairs e.epers_witnesses })
      p.pevents
  in
  List.filter_map
    (fun x -> x)
    [
      txt "first_name" a.first_name b.first_name;
      txt "surname" a.surname b.surname;
      fld "occ" (a.occ = b.occ);
      (let str x =
         match x with Male -> "male" | Female -> "female" | Neuter -> "neuter"
       in
       if a.sex = b.sex then None
       else Some ("sex", Some (str a.sex ^ " -> " ^ str b.sex)));
      fld "access" (a.access = b.access);
      txt "image" a.image b.image;
      txt "public_name" a.public_name b.public_name;
      txts "qualifiers" a.qualifiers b.qualifiers;
      txts "aliases" a.aliases b.aliases;
      txts "first_names_aliases" a.first_names_aliases b.first_names_aliases;
      txts "surnames_aliases" a.surnames_aliases b.surnames_aliases;
      fld "titles" (a.titles = b.titles);
      fld "rparents" (a.rparents = b.rparents);
      txt "occupation" a.occupation b.occupation;
      fld "birth" (a.birth = b.birth);
      txt "birth_place" a.birth_place b.birth_place;
      txt "birth_note" a.birth_note b.birth_note;
      txt "birth_src" a.birth_src b.birth_src;
      fld "baptism" (a.baptism = b.baptism);
      txt "baptism_place" a.baptism_place b.baptism_place;
      txt "baptism_note" a.baptism_note b.baptism_note;
      txt "baptism_src" a.baptism_src b.baptism_src;
      fld "death" (a.death = b.death);
      txt "death_place" a.death_place b.death_place;
      txt "death_note" a.death_note b.death_note;
      txt "death_src" a.death_src b.death_src;
      fld "burial" (a.burial = b.burial);
      txt "burial_place" a.burial_place b.burial_place;
      txt "burial_note" a.burial_note b.burial_note;
      txt "burial_src" a.burial_src b.burial_src;
      fld "pevents" (pev a = pev b);
      txt "notes" a.notes b.notes;
      txt "psources" a.psources b.psources;
    ]

(* Family fields compared (origin_file and fam_index are not). *)
let family_diffs (a : (Iper.t option, _, string) gen_family)
    (b : (Iper.t option, _, string) gen_family) =
  let fev (f : (Iper.t option, _, string) gen_family) =
    List.map
      (fun e -> { e with efam_witnesses = dedupe_pairs e.efam_witnesses })
      f.fevents
  in
  List.filter_map
    (fun x -> x)
    [
      fld "marriage" (a.marriage = b.marriage);
      txt "marriage_place" a.marriage_place b.marriage_place;
      txt "marriage_note" a.marriage_note b.marriage_note;
      txt "marriage_src" a.marriage_src b.marriage_src;
      fld "witnesses" (dedupe_arr a.witnesses = dedupe_arr b.witnesses);
      fld "relation" (a.relation = b.relation);
      fld "divorce" (a.divorce = b.divorce);
      fld "fevents" (fev a = fev b);
      txt "comment" a.comment b.comment;
      txt "fsources" a.fsources b.fsources;
    ]

(* [same_base]: the target is the source base itself; ipers are then
   compared directly instead of through person keys. *)
let check ~target ~out_dir ~verbose ~hide_ok ~same_base ~tb ~keys ~couples
    ~persons ~fams =
  let find k = Driver.person_of_key tb k.fn k.sn k.occ in
  let norm s = Mutil.normalize_utf_8 s in
  let nb_p = Driver.nb_of_persons tb and nb_f = Driver.nb_of_families tb in
  let in_range n i = i >= 0 && i < n in
  let iper_ok ip = in_range nb_p (int_of_string (Iper.to_string ip)) in
  let ifam_ok ifam = in_range nb_f (int_of_string (Ifam.to_string ifam)) in
  let tcache = Iper.Table.create 1024 in
  (* On the source base an iper is trusted only if it still designates the
     person the history knows under it; otherwise (base renumbered since the
     record was written) the person is looked up by key. *)
  let key_at ip =
    let p = Driver.poi tb ip in
    {
      fn = Driver.p_first_name tb p;
      sn = Driver.p_surname tb p;
      occ = Driver.get_occ p;
    }
  in
  let resolve src =
    if same_base then
      match Iper.Table.find_opt keys src with
      | Some k when iper_ok src && same_key (key_at src) k -> Some src
      | Some k -> find k
      | None -> if iper_ok src then Some src else None
    else
      match Iper.Table.find_opt tcache src with
      | Some r -> r
      | None ->
          let r =
            match Iper.Table.find_opt keys src with
            | Some k -> find k
            | None -> None
          in
          Iper.Table.replace tcache src r;
          r
  in
  let key src =
    match Iper.Table.find_opt keys src with
    | Some k -> string_of_pkey k
    | None -> "?" ^ Iper.to_string src
  in
  let unresolved = ref 0 in
  let resolve_counted src =
    let r = resolve src in
    (match r with None -> incr unresolved | Some _ -> ());
    r
  in
  let p_results =
    List.map
      (fun e ->
        let p = e.last.HistoryDiff.gen_p in
        let target_ip =
          if
            same_base && iper_ok p.key_index
            && same_key (key_at p.key_index) e.current_key
          then Some p.key_index
          else
            match find e.current_key with
            | Some ip -> Some ip
            | None -> find (pkey_of_gen p)
        in
        let deleted tip =
          let tp = Driver.poi tb tip in
          let empty i =
            match Driver.sou tb i with "" | "?" -> true | _ -> false
          in
          empty (Driver.get_first_name tp) && empty (Driver.get_surname tp)
        in
        let status =
          match target_ip with
          | None -> Absent "person not found under its key"
          | Some tip when deleted tip ->
              Absent "deleted after its last recorded change (dp in the log)"
          | Some tip -> (
              let expected = Futil.map_person_ps resolve_counted norm p in
              let actual =
                Util.string_gen_person tb
                  (Driver.gen_person_of_person (Driver.poi tb tip))
                |> Futil.map_person_ps (fun ip -> Some ip) norm
              in
              match person_diffs expected actual with
              | [] -> Ok_same
              | l -> Differs l)
        in
        (key_of p, date_of e.last, status))
      persons
  in
  let f_results =
    List.map
      (fun (ifam, fe) ->
        let label =
          match Ifam.Table.find_opt couples ifam with
          | Some (f, m) -> key f ^ " + " ^ key m
          | None -> "family " ^ Ifam.to_string ifam
        in
        let compare_with ti =
          let tfam = Driver.foi tb ti in
          let expected =
            Futil.map_family_ps resolve_counted (fun i -> i) norm fe.fam
          in
          let actual =
            Util.string_gen_family tb (Driver.gen_family_of_family tfam)
            |> Futil.map_family_ps (fun ip -> Some ip) (fun i -> i) norm
          in
          let diffs = family_diffs expected actual in
          let sort l = List.sort compare l in
          let exp_children =
            sort (Array.to_list (Array.map resolve_counted fe.children))
          in
          let act_children =
            sort
              (List.map
                 (fun ip -> Some ip)
                 (Array.to_list (Driver.get_children tfam)))
          in
          let diffs =
            if exp_children = act_children then diffs
            else ("children", None) :: diffs
          in
          match diffs with [] -> Ok_same | l -> Differs l
        in
        let status =
          let same_couple ti =
            match Ifam.Table.find_opt couples ifam with
            | Some (fs, ms) -> (
                let c = Driver.foi tb ti in
                match (resolve fs, resolve ms) with
                | Some tf, Some tm ->
                    Iper.equal (Driver.get_father c) tf
                    && Iper.equal (Driver.get_mother c) tm
                | (None | Some _), _ -> false)
            | None -> true
          in
          if
            same_base && ifam_ok ifam
            && Ifam.is_dummy (Driver.get_ifam (Driver.foi tb ifam))
          then Absent "deleted after its last recorded change (df in the log)"
          else if same_base && ifam_ok ifam && same_couple ifam then
            compare_with ifam
          else
            match Ifam.Table.find_opt couples ifam with
            | None -> Absent "couple unknown (rerun with -resolve)"
            | Some (fs, ms) -> (
                match (resolve fs, resolve ms) with
                | Some tf, Some tm -> (
                    match
                      Array.find_opt
                        (fun i ->
                          Iper.equal (Driver.get_mother (Driver.foi tb i)) tm)
                        (Driver.get_family (Driver.poi tb tf))
                    with
                    | None -> Absent "no family for this couple"
                    | Some ti -> compare_with ti)
                | None, (Some _ | None) ->
                    Absent ("father " ^ key fs ^ " not found")
                | Some _, None -> Absent ("mother " ^ key ms ^ " not found"))
        in
        (label, fe.fam_date, status))
      fams
  in
  let string_of_status st =
    match st with
    | Ok_same -> "OK"
    | Differs l -> "DIFFERS: " ^ String.concat ", " (List.map fst l)
    | Absent why -> "MISSING: " ^ why
  in
  let details st =
    match st with
    | Differs l ->
        List.filter_map (fun (n, d) -> Option.map (fun d -> n ^ ": " ^ d) d) l
    | Ok_same | Absent _ -> []
  in
  with_out out_dir "verify.tsv" (fun oc ->
      Printf.fprintf oc "kind\twhat\tlast_change\tstatus\tdetails\n";
      List.iter
        (fun (kind, l) ->
          List.iter
            (fun (k, d, st) ->
              match st with
              | Ok_same when hide_ok -> ()
              | Ok_same | Differs _ | Absent _ ->
                  Printf.fprintf oc "%s\t%s\t%s\t%s\t%s\n" kind k d
                    (string_of_status st)
                    (String.concat " | " (details st)))
            l)
        [ ("person", p_results); ("family", f_results) ]);
  let tally l =
    List.fold_left
      (fun (ok, df, ab) (_, _, st) ->
        match st with
        | Ok_same -> (ok + 1, df, ab)
        | Differs _ -> (ok, df + 1, ab)
        | Absent _ -> (ok, df, ab + 1))
      (0, 0, 0) l
  in
  let pok, pdf, pab = tally p_results and fok, fdf, fab = tally f_results in
  let summary oc =
    Printf.fprintf oc "\nVerification of %s%s:\n" target
      (if same_base then " (same base as the source)" else "");
    Printf.fprintf oc "persons: %d identical, %d different, %d missing\n" pok
      pdf pab;
    Printf.fprintf oc "families: %d identical, %d different, %d missing\n" fok
      fdf fab;
    if !unresolved > 0 then
      Printf.fprintf oc
        "references with no known key: %d (they show as differences; rerun \
         with -resolve)\n"
        !unresolved;
    if pdf + pab + fdf + fab = 0 then
      Printf.fprintf oc "All changes of the window are present.\n"
  in
  with_out out_dir "verify_summary.txt" summary;
  summary stdout;
  let show title l =
    match
      List.filter
        (fun (_, _, st) ->
          match st with Ok_same -> false | Differs _ | Absent _ -> true)
        l
    with
    | [] -> ()
    | bad ->
        Printf.printf "\n%s not matching:\n" title;
        List.iter
          (fun (k, d, st) ->
            Printf.printf "  %s  %s: %s\n" d k (string_of_status st);
            List.iter (Printf.printf "      %s\n") (details st))
          bad
  in
  show "Persons" p_results;
  show "Families" f_results;
  if verbose && not hide_ok then (
    Printf.printf "\nAll checked records:\n";
    List.iter
      (fun (k, d, st) ->
        Printf.printf "  person %s  %s: %s\n" d k (string_of_status st))
      p_results;
    List.iter
      (fun (k, d, st) ->
        Printf.printf "  family %s  %s: %s\n" d k (string_of_status st))
      f_results)

let plan ~target ~out_dir ~verbose ~write ~assume_yes ~persons ~fams ~couples
    ~keys ~log =
  Driver.with_database target @@ fun tb ->
  let find k = Driver.person_of_key tb k.fn k.sn k.occ in
  (* persons *)
  let by_src = Iper.Table.create 256 in
  let p_actions =
    List.map
      (fun e ->
        let p = e.last.HistoryDiff.gen_p in
        let latest = pkey_of_gen p in
        let action =
          match find e.dump_key with
          | Some ip ->
              if pkey_equal e.dump_key latest then P_modify ip else P_rename ip
          | None -> (
              match find latest with Some ip -> P_modify ip | None -> P_create)
        in
        Iper.Table.replace by_src p.key_index action;
        (e, action))
      persons
  in
  let target_of src_ip =
    match Iper.Table.find_opt by_src src_ip with
    | Some (P_modify ip | P_rename ip) -> Existing ip
    | Some P_create -> New
    | None -> (
        match Iper.Table.find_opt keys src_ip with
        | None -> Unknown
        | Some k -> (
            match find k with Some ip -> Existing ip | None -> Missing))
  in
  let key src_ip =
    match Iper.Table.find_opt keys src_ip with
    | Some k -> string_of_pkey k
    | None -> "?" ^ Iper.to_string src_ip
  in
  (* families *)
  let f_actions =
    List.map
      (fun (ifam, fe) ->
        let action =
          match Ifam.Table.find_opt couples ifam with
          | None -> F_blocked "couple unknown (rerun with -resolve)"
          | Some (fath, moth) -> (
              match (target_of fath, target_of moth) with
              | Existing f, Existing m -> (
                  let fp = Driver.poi tb f in
                  match
                    Array.find_opt
                      (fun i ->
                        Iper.equal (Driver.get_mother (Driver.foi tb i)) m)
                      (Driver.get_family fp)
                  with
                  | Some i -> F_modify i
                  | None -> F_create)
              | (Existing _ | New), (Existing _ | New) -> F_create
              | (Missing | Unknown), (Existing _ | New | Missing | Unknown) ->
                  F_blocked ("father " ^ key fath ^ " not in target")
              | (Existing _ | New), (Missing | Unknown) ->
                  F_blocked ("mother " ^ key moth ^ " not in target"))
        in
        let problems =
          let check role ip acc =
            match target_of ip with
            | Existing _ | New -> acc
            | (Missing | Unknown) as r ->
                Printf.sprintf "%s %s: %s" role (key ip)
                  (string_of_target_ref r)
                :: acc
          in
          let acc = Array.fold_right (check "child") fe.children [] in
          let acc = Array.fold_right (check "witness") fe.fam.witnesses acc in
          List.fold_right
            (fun ev acc ->
              Array.fold_right
                (fun (ip, _) acc -> check "event witness" ip acc)
                ev.efam_witnesses acc)
            fe.fam.fevents acc
        in
        (ifam, fe, action, problems))
      fams
  in
  (* references from changed persons *)
  let p_problems =
    List.filter_map
      (fun (e, _) ->
        let p = e.last.HistoryDiff.gen_p in
        let check role ip acc =
          match target_of ip with
          | Existing _ | New -> acc
          | (Missing | Unknown) as r ->
              Printf.sprintf "%s %s: %s" role (key ip) (string_of_target_ref r)
              :: acc
        in
        let acc =
          List.fold_right
            (fun r acc ->
              let acc =
                match r.r_fath with
                | Some ip -> check "relation parent" ip acc
                | None -> acc
              in
              match r.r_moth with
              | Some ip -> check "relation parent" ip acc
              | None -> acc)
            p.rparents []
        in
        let acc =
          List.fold_right
            (fun ev acc ->
              Array.fold_right
                (fun (ip, _) acc -> check "event witness" ip acc)
                ev.epers_witnesses acc)
            p.pevents acc
        in
        match acc with [] -> None | l -> Some (key_of p, l))
      p_actions
  in
  (* deletions and merges from the log: not in history_d *)
  let manual =
    List.filter_map
      (fun l ->
        match l.action with
        | "dp" -> (
            match parse_key l.key with
            | Some k -> (
                match find k with
                | Some ip ->
                    Some
                      ( l,
                        "delete person (target iper " ^ Iper.to_string ip ^ ")"
                      )
                | None -> Some (l, "deleted person, already absent from target")
                )
            | None -> Some (l, "deleted person, key not parsed"))
        | "df" -> Some (l, "delete family of this person: to do by hand")
        | "fp" -> Some (l, "merge of persons: to check by hand")
        | "ff" -> Some (l, "merge of families: to check by hand")
        | _ -> None)
      log
  in
  (* reports *)
  let count f l = List.length (List.filter f l) in
  let n_mod =
    count
      (fun (_, a) ->
        match a with P_modify _ -> true | P_rename _ | P_create -> false)
      p_actions
  in
  let n_ren =
    count
      (fun (_, a) ->
        match a with P_rename _ -> true | P_modify _ | P_create -> false)
      p_actions
  in
  let n_new =
    count
      (fun (_, a) ->
        match a with P_create -> true | P_modify _ | P_rename _ -> false)
      p_actions
  in
  let f_mod =
    count
      (fun (_, _, a, _) ->
        match a with F_modify _ -> true | F_create | F_blocked _ -> false)
      f_actions
  in
  let f_new =
    count
      (fun (_, _, a, _) ->
        match a with F_create -> true | F_modify _ | F_blocked _ -> false)
      f_actions
  in
  let f_blk =
    count
      (fun (_, _, a, _) ->
        match a with F_blocked _ -> true | F_modify _ | F_create -> false)
      f_actions
  in
  let string_of_paction a =
    match a with
    | P_modify ip -> "modify target iper " ^ Iper.to_string ip
    | P_rename ip -> "modify and rename target iper " ^ Iper.to_string ip
    | P_create -> "create"
  in
  let string_of_faction a =
    match a with
    | F_modify i -> "modify target family " ^ Ifam.to_string i
    | F_create -> "create"
    | F_blocked why -> "BLOCKED: " ^ why
  in
  let couple_label ifam =
    match Ifam.Table.find_opt couples ifam with
    | Some (f, m) -> key f ^ " + " ^ key m
    | None -> "?"
  in
  with_out out_dir "plan.tsv" (fun oc ->
      Printf.fprintf oc "kind\tsource\tdump_key\tkey\taction\tproblems\n";
      List.iter
        (fun (e, a) ->
          let p = e.last.HistoryDiff.gen_p in
          let probs =
            match List.assoc_opt (key_of p) p_problems with
            | Some l -> String.concat " | " l
            | None -> ""
          in
          Printf.fprintf oc "person\t%s\t%s\t%s\t%s\t%s\n"
            (Iper.to_string p.key_index)
            (string_of_pkey e.dump_key)
            (key_of p) (string_of_paction a) probs)
        p_actions;
      List.iter
        (fun (ifam, _, a, probs) ->
          Printf.fprintf oc "family\t%s\t\t%s\t%s\t%s\n" (Ifam.to_string ifam)
            (couple_label ifam) (string_of_faction a)
            (String.concat " | " probs))
        f_actions;
      List.iter
        (fun (l, what) ->
          Printf.fprintf oc "log\t%s %s\t\t%s\t%s\t\n" l.time l.action l.key
            what)
        manual);
  let summary oc =
    Printf.fprintf oc "\nPlan on %s%s:\n" target
      (if write then "" else " (dry run, nothing written)");
    Printf.fprintf oc
      "persons: %d to modify, %d to modify and rename, %d to create\n" n_mod
      n_ren n_new;
    Printf.fprintf oc "families: %d to modify, %d to create, %d blocked\n" f_mod
      f_new f_blk;
    Printf.fprintf oc
      "records with references missing in target: %d persons, %d families\n"
      (List.length p_problems)
      (count (fun (_, _, _, pr) -> pr <> []) f_actions);
    Printf.fprintf oc "log actions to handle by hand (deletions, merges): %d\n"
      (List.length manual)
  in
  with_out out_dir "plan_summary.txt" summary;
  summary stdout;
  if verbose then (
    Printf.printf "\nPersons:\n";
    List.iter
      (fun (e, a) ->
        let p = e.last.HistoryDiff.gen_p in
        let renamed =
          if pkey_equal e.dump_key (pkey_of_gen p) then ""
          else " (was " ^ string_of_pkey e.dump_key ^ ")"
        in
        Printf.printf "  %s%s: %s\n" (key_of p) renamed (string_of_paction a);
        match List.assoc_opt (key_of p) p_problems with
        | Some l -> List.iter (Printf.printf "      %s\n") l
        | None -> ())
      p_actions;
    Printf.printf "\nFamilies:\n";
    List.iter
      (fun (ifam, _, a, probs) ->
        Printf.printf "  family %s (%s): %s\n" (Ifam.to_string ifam)
          (couple_label ifam) (string_of_faction a);
        List.iter (Printf.printf "      %s\n") probs)
      f_actions;
    if manual <> [] then (
      Printf.printf "\nFrom the history log, to handle by hand:\n";
      List.iter
        (fun (l, what) ->
          Printf.printf "  %s [%s] %s %s: %s\n" l.time l.user l.action l.key
            what)
        manual));
  if write then
    apply ~target ~out_dir ~assume_yes ~tb ~find ~keys ~couples ~p_actions
      ~f_actions

(* ---------------------------------------------------------------------- *)
(* Stage 1: inventory                                                       *)

let run ~bname ~history_d ~log_file ~since ~until ~out_dir ~resolve_in_base
    ~verbose ~target ~write ~verify ~assume_yes ~hide_ok =
  (try Unix.mkdir out_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let files = walk history_d [] in
  let keys = Iper.Table.create 4096 in
  let key_dates = Iper.Table.create 4096 in
  let sexes = Iper.Table.create 4096 in
  let persons = ref [] in
  let truncated = ref [] in
  let families = Ifam.Table.create 1024 in
  List.iter
    (fun file ->
      let records, status =
        try read_history_file file
        with Sys_error e ->
          Printf.eprintf "cannot read %s: %s\n%!" file e;
          ([], Truncated 0)
      in
      (match status with
      | Truncated n -> truncated := (file, n) :: !truncated
      | Clean -> ());
      (* An iper may designate different persons in records written before
         and after a renumbering of the base: keep the most recent record. *)
      List.iter
        (fun (r : HistoryDiff.gen_record) ->
          let p = r.HistoryDiff.gen_p in
          let d = date_of r in
          match Iper.Table.find_opt key_dates p.key_index with
          | Some d0 when String.compare d0 d > 0 -> ()
          | Some _ | None ->
              Iper.Table.replace key_dates p.key_index d;
              Iper.Table.replace keys p.key_index (pkey_of_gen p);
              Iper.Table.replace sexes p.key_index p.sex)
        records;
      let current_key =
        match List.rev records with
        | r :: _ -> pkey_of_gen r.HistoryDiff.gen_p
        | [] -> { fn = ""; sn = ""; occ = 0 }
      in
      let before, window =
        List.filter (fun r -> String.compare (date_of r) until <= 0) records
        |> List.partition (fun r -> String.compare (date_of r) since < 0)
      in
      match (List.rev window, window) with
      | [], _ | _, [] -> ()
      | last :: _, first :: _ ->
          let dump_key =
            match List.rev before with
            | b :: _ -> pkey_of_gen b.HistoryDiff.gen_p
            | [] -> pkey_of_gen first.HistoryDiff.gen_p
          in
          persons :=
            {
              file;
              nb_in_window = List.length window;
              first_date = date_of first;
              last;
              dump_key;
              status;
              current_key;
            }
            :: !persons;
          List.iter
            (fun (r : HistoryDiff.gen_record) ->
              let d = date_of r in
              let me = r.HistoryDiff.gen_p.key_index in
              List.iter2
                (fun (f : (Iper.t, Ifam.t, string) gen_family) children ->
                  match Ifam.Table.find_opt families f.fam_index with
                  | None ->
                      Ifam.Table.replace families f.fam_index
                        { fam_date = d; fam = f; children; spouses = [ me ] }
                  | Some fe ->
                      let spouses =
                        if List.exists (Iper.equal me) fe.spouses then
                          fe.spouses
                        else me :: fe.spouses
                      in
                      if String.compare d fe.fam_date >= 0 then
                        Ifam.Table.replace families f.fam_index
                          { fam_date = d; fam = f; children; spouses }
                      else
                        Ifam.Table.replace families f.fam_index
                          { fe with spouses })
                r.HistoryDiff.gen_f r.HistoryDiff.gen_c)
            window)
    files;
  let persons =
    List.sort
      (fun a b -> String.compare (date_of a.last) (date_of b.last))
      !persons
  in
  if not (Sys.file_exists log_file) then
    Printf.eprintf "warning: history log %s not found (use -history)\n%!"
      log_file;
  let log = read_log log_file since until in
  let key ip =
    match Iper.Table.find_opt keys ip with
    | Some k -> string_of_pkey k
    | None -> "?" ^ Iper.to_string ip
  in
  let fam_label ifam fe =
    Printf.sprintf "family %s (%s)" (Ifam.to_string ifam)
      (String.concat " + " (List.map key fe.spouses))
  in
  let fams =
    Ifam.Table.fold (fun ifam fe acc -> (ifam, fe) :: acc) families []
    |> List.sort (fun (_, a) (_, b) -> String.compare a.fam_date b.fam_date)
  in
  (* Couples (father, mother) of the changed families: from the history when
     both spouses were edited, from the source base otherwise. *)
  let couples = Ifam.Table.create 256 in
  List.iter
    (fun (ifam, fe) ->
      match fe.spouses with
      | [ a; b ] -> (
          match Iper.Table.find_opt sexes a with
          | Some Female -> Ifam.Table.replace couples ifam (b, a)
          | Some (Male | Neuter) | None -> Ifam.Table.replace couples ifam (a, b)
          )
      | _ -> ())
    fams;
  let refs = referenced_ipers ~fam_label persons families in
  let unresolved () =
    Iper.Table.fold
      (fun ip _ acc -> if Iper.Table.mem keys ip then acc else ip :: acc)
      refs []
  in
  (* Lookups in the (damaged) source base, one record at a time, flushed
     before each read: if a record crashes the process, the last line of
     base_lookup.tsv names it. *)
  let still_unresolved, misaligned =
    if not resolve_in_base then
      ( List.map
          (fun ip ->
            ( ip,
              "never edited since history_d was started, and not looked up in \
               the base (rerun with -resolve)" ))
          (unresolved ()),
        [] )
    else
      with_out out_dir "base_lookup.tsv" @@ fun oc ->
      Driver.with_database bname @@ fun base ->
      let nb = Driver.nb_of_persons base in
      let nbf = Driver.nb_of_families base in
      let lookup_person ip =
        if Iper.Table.mem keys ip then true
        else (
          Printf.fprintf oc "person\t%s\t%!" (Iper.to_string ip);
          let i = int_of_string (Iper.to_string ip) in
          if i < 0 || i >= nb then (
            Printf.fprintf oc "OUT_OF_RANGE\n%!";
            false)
          else
            let p = Driver.poi base ip in
            let k =
              {
                fn = Driver.p_first_name base p;
                sn = Driver.p_surname base p;
                occ = Driver.get_occ p;
              }
            in
            Printf.fprintf oc "%s\n%!" (string_of_pkey k);
            Iper.Table.replace keys ip k;
            true)
      in
      List.iter
        (fun (ifam, _) ->
          if not (Ifam.Table.mem couples ifam) then (
            Printf.fprintf oc "family\t%s\t%!" (Ifam.to_string ifam);
            let i = int_of_string (Ifam.to_string ifam) in
            if i < 0 || i >= nbf then Printf.fprintf oc "OUT_OF_RANGE\n%!"
            else
              let fam = Driver.foi base ifam in
              let f = Driver.get_father fam and m = Driver.get_mother fam in
              Printf.fprintf oc "%s + %s\n%!" (Iper.to_string f)
                (Iper.to_string m);
              Ifam.Table.replace couples ifam (f, m);
              ignore (lookup_person f : bool);
              ignore (lookup_person m : bool)))
        fams;
      let still =
        List.filter_map
          (fun ip ->
            if lookup_person ip then None
            else
              Some
                ( ip,
                  Printf.sprintf
                    "beyond the end of the base (%d persons): dangling \
                     reference, this person does not exist"
                    nb ))
          (unresolved ())
      in
      (* Numbering check: the iper of each changed person must still
         designate that person in the source base. If not, the base was
         renumbered (rebuilt from a .gw) after the record was written. *)
      let misaligned =
        List.filter_map
          (fun e ->
            let ip = e.last.HistoryDiff.gen_p.key_index in
            Printf.fprintf oc "check\t%s\t%!" (Iper.to_string ip);
            let i = int_of_string (Iper.to_string ip) in
            if i < 0 || i >= nb then (
              Printf.fprintf oc "OUT_OF_RANGE\n%!";
              Some (e, "beyond the end of the base"))
            else
              let p = Driver.poi base ip in
              let k =
                {
                  fn = Driver.p_first_name base p;
                  sn = Driver.p_surname base p;
                  occ = Driver.get_occ p;
                }
              in
              Printf.fprintf oc "%s\n%!" (string_of_pkey k);
              if same_key k e.current_key then None
              else Some (e, "now designates " ^ string_of_pkey k))
          persons
      in
      (still, misaligned)
  in
  (* Latest misaligned record, and the first aligned record after it: the
     renumbering happened in between. *)
  let renumbered_between =
    match misaligned with
    | [] -> None
    | _ :: _ ->
        let last_bad =
          List.fold_left
            (fun acc (e, _) -> max acc (date_of e.last))
            "" misaligned
        in
        let is_bad e =
          List.exists (fun (e', _) -> e'.file = e.file) misaligned
        in
        let first_good =
          List.fold_left
            (fun acc e ->
              let d = date_of e.last in
              if is_bad e || String.compare d last_bad <= 0 then acc
              else
                match acc with
                | Some a when String.compare a d <= 0 -> acc
                | Some _ | None -> Some d)
            None persons
        in
        Some (last_bad, first_good)
  in
  (* Families whose couple is empty in the source base were deleted there
     after their last recorded change: they are left out of every report. *)
  let deleted_fams, fams =
    List.partition
      (fun (ifam, _) ->
        match Ifam.Table.find_opt couples ifam with
        | Some (f, m) -> Iper.is_dummy f && Iper.is_dummy m
        | None -> false)
      fams
  in
  (* reports *)
  with_out out_dir "persons.tsv" (fun oc ->
      Printf.fprintf oc
        "iper\tkey\tdump_key\tnb_records\tfirst\tlast\twizard\tdup_witnesses\ttruncated\tfile\n";
      List.iter
        (fun e ->
          let r = e.last in
          let p = r.HistoryDiff.gen_p in
          let dups = person_duplicates p in
          let trunc =
            match e.status with Clean -> "" | Truncated n -> string_of_int n
          in
          Printf.fprintf oc "%s\t%s\t%s\t%d\t%s\t%s\t%s\t%s\t%s\t%s\n"
            (Iper.to_string p.key_index)
            (key_of p)
            (string_of_pkey e.dump_key)
            e.nb_in_window e.first_date (date_of r)
            (Adef.as_string r.HistoryDiff.wizard)
            (String.concat "," (List.map key dups))
            trunc e.file)
        persons);
  with_out out_dir "families.tsv" (fun oc ->
      Printf.fprintf oc
        "ifam\tlast\tspouses\tnb_children\tnb_witnesses\tdup_witnesses\n";
      List.iter
        (fun (ifam, fe) ->
          let dups = family_duplicates fe.fam in
          Printf.fprintf oc "%s\t%s\t%s\t%d\t%d\t%s\n" (Ifam.to_string ifam)
            fe.fam_date
            (String.concat " + " (List.map key fe.spouses))
            (Array.length fe.children)
            (Array.length fe.fam.witnesses)
            (String.concat "," (List.map key dups)))
        fams);
  with_out out_dir "log.tsv" (fun oc ->
      Printf.fprintf oc "time\tuser\taction\tkey\n";
      List.iter
        (fun l ->
          Printf.fprintf oc "%s\t%s\t%s\t%s\n" l.time l.user l.action l.key)
        log);
  let referenced_from ip =
    List.rev (Option.value ~default:[] (Iper.Table.find_opt refs ip))
  in
  with_out out_dir "unresolved.tsv" (fun oc ->
      Printf.fprintf oc "iper\treason\treferenced_from\n";
      List.iter
        (fun (ip, why) ->
          Printf.fprintf oc "%s\t%s\t%s\n" (Iper.to_string ip) why
            (String.concat " | " (referenced_from ip)))
        still_unresolved);
  with_out out_dir "misaligned.tsv" (fun oc ->
      Printf.fprintf oc "iper\trecorded_key\tlast_change\tproblem\n";
      List.iter
        (fun (e, why) ->
          Printf.fprintf oc "%s\t%s\t%s\t%s\n"
            (Iper.to_string e.last.HistoryDiff.gen_p.key_index)
            (string_of_pkey e.current_key)
            (date_of e.last) why)
        misaligned);
  with_out out_dir "truncated.tsv" (fun oc ->
      Printf.fprintf oc "file\treadable_records\n";
      List.iter (fun (f, n) -> Printf.fprintf oc "%s\t%d\n" f n) !truncated);
  let count_actions =
    let h = Hashtbl.create 16 in
    List.iter
      (fun l ->
        let n = Option.value ~default:0 (Hashtbl.find_opt h l.action) in
        Hashtbl.replace h l.action (n + 1))
      log;
    Hashtbl.fold (fun a n acc -> (a, n) :: acc) h []
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  let fam_one_spouse =
    List.length (List.filter (fun (_, fe) -> List.length fe.spouses < 2) fams)
  in
  let fam_dups =
    List.length
      (List.filter (fun (_, fe) -> family_duplicates fe.fam <> []) fams)
  in
  let summary oc =
    Printf.fprintf oc "window: %s .. %s\n" since until;
    Printf.fprintf oc "history_d files scanned: %d\n" (List.length files);
    Printf.fprintf oc "persons changed in window: %d\n" (List.length persons);
    Printf.fprintf oc "families changed in window: %d\n" (List.length fams);
    if deleted_fams <> [] then
      Printf.fprintf oc
        "  families deleted since in the source base (left out): %d\n"
        (List.length deleted_fams);
    Printf.fprintf oc "  with only one spouse in history: %d\n" fam_one_spouse;
    Printf.fprintf oc "  with duplicate witnesses: %d\n" fam_dups;
    Printf.fprintf oc "referenced persons: %d\n" (Iper.Table.length refs);
    Printf.fprintf oc "  not resolved to a key: %d\n"
      (List.length still_unresolved);
    Printf.fprintf oc "truncated history files: %d\n" (List.length !truncated);
    if resolve_in_base then (
      Printf.fprintf oc
        "changed persons whose iper now designates someone else: %d\n"
        (List.length misaligned);
      match renumbered_between with
      | None -> ()
      | Some (bad, good) ->
          Printf.fprintf oc
            "  the base was renumbered after %s%s: records before that use \
             another numbering; rerun with -since %s\n"
            bad
            (match good with Some g -> " and before " ^ g | None -> "")
            (match good with Some g -> "\"" ^ g ^ "\"" | None -> "LATER"))
    else
      Printf.fprintf oc
        "numbering of the source base not checked (use -resolve)\n";
    Printf.fprintf oc "history log lines in window: %d\n" (List.length log);
    List.iter (fun (a, n) -> Printf.fprintf oc "  %s: %d\n" a n) count_actions
  in
  with_out out_dir "summary.txt" summary;
  summary stdout;
  if verbose then (
    Printf.printf "\nPersons changed in window (%d):\n" (List.length persons);
    List.iter
      (fun e ->
        let r = e.last in
        let p = r.HistoryDiff.gen_p in
        Printf.printf "  %s  %-14s %s (iper %s), %d edit%s%s%s\n" (date_of r)
          ("[" ^ Adef.as_string r.HistoryDiff.wizard ^ "]")
          (key_of p)
          (Iper.to_string p.key_index)
          e.nb_in_window
          (plural e.nb_in_window "s")
          (match person_duplicates p with
          | [] -> ""
          | l -> ", duplicate witnesses: " ^ String.concat ", " (List.map key l))
          (match e.status with
          | Clean -> ""
          | Truncated _ -> ", history file truncated"))
      persons;
    Printf.printf "\nFamilies changed in window (%d):\n" (List.length fams);
    List.iter
      (fun (ifam, fe) ->
        let nc = Array.length fe.children in
        let nw = Array.length fe.fam.witnesses in
        Printf.printf "  %s  %s: %d child%s, %d witness%s%s%s\n" fe.fam_date
          (fam_label ifam fe) nc (plural nc "ren") nw (plural nw "es")
          (if List.length fe.spouses < 2 then
             ", only one spouse in history (other spouse to look up)"
           else "")
          (match family_duplicates fe.fam with
          | [] -> ""
          | l -> ", duplicate witnesses: " ^ String.concat ", " (List.map key l)))
      fams;
    (match misaligned with
    | [] -> ()
    | l ->
        Printf.printf
          "\nChanged persons whose iper now designates someone else (%d):\n"
          (List.length l);
        List.iter
          (fun (e, why) ->
            Printf.printf "  %s  %s (iper %s): %s\n" (date_of e.last)
              (string_of_pkey e.current_key)
              (Iper.to_string e.last.HistoryDiff.gen_p.key_index)
              why)
          l);
    Printf.printf "\nReferences not resolved to a key (%d):\n"
      (List.length still_unresolved);
    (match still_unresolved with
    | [] -> ()
    | _ :: _ ->
        Printf.printf
          "  (a changed record points to these ipers, but no person key is \
           known for them)\n");
    List.iter
      (fun (ip, why) ->
        Printf.printf "  iper %s: %s\n" (Iper.to_string ip) why;
        List.iter (Printf.printf "      %s\n") (referenced_from ip))
      still_unresolved);
  if write && misaligned <> [] then (
    Printf.eprintf
      "error: %d changed persons were recorded under another numbering of the \
       base (see misaligned.tsv). Writing would attach their data to the wrong \
       persons. Rerun with a -since after the renumbering.\n"
      (List.length misaligned);
    exit 2);
  if write && not resolve_in_base then
    Printf.eprintf
      "warning: numbering of the source base not checked; -resolve is \
       recommended with -write\n\
       %!";
  match target with
  | None -> ()
  | Some target ->
      if verify then
        Driver.with_database target @@ fun tb ->
        check ~target ~out_dir ~verbose ~hide_ok ~same_base:(target = bname) ~tb
          ~keys ~couples ~persons ~fams
      else
        plan ~target ~out_dir ~verbose ~write ~assume_yes ~persons ~fams
          ~couples ~keys ~log

let () =
  let bname = ref "" in
  let bases_dir = ref "" in
  let since = ref "" in
  let history_d = ref "" in
  let log_file = ref "" in
  let out_dir = ref "gwreplay_report" in
  let resolve_in_base = ref false in
  let verbose = ref false in
  let target = ref "" in
  let dry_run = ref false in
  let write = ref false in
  let verify = ref false in
  let assume_yes = ref false in
  let hide_ok = ref false in
  let until = ref "" in
  let speclist =
    [
      ( "-bd",
        Arg.Set_string bases_dir,
        "DIR bases folder (default: current folder)" );
      ( "-since",
        Arg.Set_string since,
        "\"YYYY-MM-DD[ HH:MM:SS]\" start of the window" );
      ( "-history_d",
        Arg.Set_string history_d,
        "DIR history_d folder (default: <bd>/BASE.gwb/history_d)" );
      ( "-history",
        Arg.Set_string log_file,
        "FILE history log (default: <bd>/BASE.gwb/history)" );
      ( "-o",
        Arg.Set_string out_dir,
        "DIR report folder (default: gwreplay_report)" );
      ( "-resolve",
        Arg.Set resolve_in_base,
        " also resolve unknown ipers and couples by reading the source base" );
      ( "-target",
        Arg.Set_string target,
        "BASE base rebuilt from the dump, to replay onto (in <bd> if relative)"
      );
      ( "-dry-run",
        Arg.Set dry_run,
        " with -target: show the plan, write nothing (default)" );
      ( "-until",
        Arg.Set_string until,
        "\"YYYY-MM-DD[ HH:MM:SS]\" end of the window (default: no end)" );
      ( "-ok",
        Arg.Set hide_ok,
        " with -verify: leave out records found OK (verify.tsv and listings)" );
      ( "-verify",
        Arg.Set verify,
        " with -target: check that the state recorded at the end of the window \
         is present in the target base (read-only)" );
      ( "-yes",
        Arg.Set assume_yes,
        " with -write: do not ask before rewriting links to renamed persons" );
      ( "-write",
        Arg.Set write,
        " with -target: apply the plan to the target base (work on a copy!)" );
      ( "-verbose",
        Arg.Set verbose,
        " list changed persons and families, explain unresolved references" );
    ]
    |> Arg.align
  in
  let usage =
    "Usage: gwreplay -since DATE [-until DATE] [-bd DIR] [-resolve] [-target \
     BASE [-dry-run|-write|-verify]] [options] SOURCE_BASE"
  in
  Arg.parse speclist (fun s -> bname := s) usage;
  if !bname = "" || !since = "" then (
    Arg.usage speclist usage;
    exit 2);
  let base_path b =
    let b = if Filename.check_suffix b ".gwb" then b else b ^ ".gwb" in
    let b =
      if !bases_dir <> "" && Filename.is_relative b then
        Filename.concat !bases_dir b
      else b
    in
    if not (Sys.file_exists b && Sys.is_directory b) then (
      Printf.eprintf "error: base %s not found\n" b;
      exit 2);
    b
  in
  let bname = base_path !bname in
  let target =
    match !target with
    | "" -> None
    | t ->
        if (!dry_run && !write) || (!verify && !write) then (
          Printf.eprintf "error: -write excludes -dry-run and -verify\n";
          exit 2);
        let t = base_path t in
        if !write && t = bname then (
          Printf.eprintf "error: the target must differ from the source base\n";
          exit 2);
        Some t
  in
  (* The database layer only opens files under authorized bases folders. *)
  Secure.set_base_dir (Filename.dirname bname);
  Option.iter (fun t -> Secure.set_base_dir (Filename.dirname t)) target;
  (* Paths used by the notes code (notes_d, wiznotes, links database). *)
  GWPARAM.init ();
  let since =
    if String.length !since = 10 then !since ^ " 00:00:00" else !since
  in
  let until =
    match !until with
    | "" -> "9999-12-31 23:59:59"
    | u -> if String.length u = 10 then u ^ " 23:59:59" else u
  in
  if !verify && Option.is_none target then (
    Printf.eprintf "error: -verify needs -target BASE\n";
    exit 2);
  let history_d =
    if !history_d = "" then Filename.concat bname "history_d" else !history_d
  in
  let log_file =
    if !log_file = "" then Filename.concat bname "history" else !log_file
  in
  run ~bname ~history_d ~log_file ~since ~until ~out_dir:!out_dir
    ~resolve_in_base:!resolve_in_base ~verbose:!verbose ~target ~write:!write
    ~verify:!verify ~assume_yes:!assume_yes ~hide_ok:!hide_ok
