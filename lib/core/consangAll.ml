(* Copyright (c) 1998-2007 INRIA *)

module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection
module Gutil = Geneweb_db.Gutil

let relationship base tab ip1 ip2 =
  fst (Consang.relationship_and_links base tab false ip1 ip2)

let trace verbosity cnt max_cnt =
  if verbosity >= 2 then (
    Printf.eprintf "%9d\008\008\008\008\008\008\008\008\008" cnt;
    flush stderr)
  else if verbosity >= 1 then ProgrBar.run (max_cnt - cnt) max_cnt

let consang_array base =
  let patched = ref false in
  let fget i = Driver.get_parents @@ Driver.poi base i in
  let cget i = Driver.get_consang @@ Driver.poi base i in
  let cset i v =
    patched := true;
    Driver.patch_ascend base i
      Def.
        { (Driver.gen_ascend_of_person @@ Driver.poi base i) with consang = v }
  in
  (fget, cget, cset, patched)

(* ---------------------------------------------------------------------------
   Ancestor fingerprint filter (192-bit Bloom-like filter)

   Each person accumulates a fingerprint = union of all ancestors' fingerprints.
   If fingerprint(father) AND fingerprint(mother) = 0, the parents share no
   known ancestor with certainty => consanguinity = 0, skip relationship().

   Three independent 63-bit words give ~192 effective bits, greatly reducing
   false positives compared to a single 64-bit word.

   Note: patch_ascend accumulates in an in-memory hashtable (pending.h_ascend)
   and commit_patches flushes everything to disk at once — so calling cset
   per person is fine and no additional write buffer is needed.
   --------------------------------------------------------------------------- *)

type fingerprint = { fp0 : int; fp1 : int; fp2 : int }

let fp_zero = { fp0 = 0; fp1 = 0; fp2 = 0 }

let fp_union a b =
  { fp0 = a.fp0 lor b.fp0; fp1 = a.fp1 lor b.fp1; fp2 = a.fp2 lor b.fp2 }

let fp_intersects a b =
  a.fp0 land b.fp0 <> 0 || a.fp1 land b.fp1 <> 0 || a.fp2 land b.fp2 <> 0

(* One bit per person in each of the three words, chosen independently *)
let fp_of_iper i =
  let h0 = Hashtbl.hash i in
  let h1 = Hashtbl.hash (i, 1) in
  let h2 = Hashtbl.hash (i, 2) in
  {
    fp0 = 1 lsl (h0 land 62);
    fp1 = 1 lsl (h1 land 62);
    fp2 = 1 lsl (h2 land 62);
  }

(** Build ancestor fingerprints for all persons. [sorted] must be in topological
    order (ancestors first) so that when we process a person, their parents'
    fingerprints are already set. *)
let compute_fingerprints base fget sorted =
  let fp_tab = Driver.iper_marker (Driver.ipers base) fp_zero in
  List.iter
    (fun i ->
      let own = fp_of_iper i in
      let inherited =
        match fget i with
        | None -> fp_zero
        | Some ifam ->
            let cpl = Driver.foi base ifam in
            let ifath = Driver.get_father cpl in
            let imoth = Driver.get_mother cpl in
            fp_union
              (Collection.Marker.get fp_tab ifath)
              (Collection.Marker.get fp_tab imoth)
      in
      Collection.Marker.set fp_tab i (fp_union own inherited))
    sorted;
  fp_tab

(** Compute consanguinity for all persons in the database. Persons are processed
    in topological order (ancestors first), so a single pass suffices — no
    multi-pass while loop needed.

    Two optimisations:
    - Per-family cache: siblings sharing the same parents reuse the
      already-computed consanguinity (avoids redundant relationship() calls).
    - Ancestor fingerprint filter: if the 192-bit fingerprints of father and
      mother have no common bit, they certainly share no known ancestor and
      consanguinity is 0 without calling the expensive relationship().

    Note on writes: patch_ascend accumulates patches in memory; commit_patches
    flushes everything to disk in one shot at the end. No extra buffering
    needed. *)
let compute ?(verbosity = 2) base =
  Driver.load_ascends_array base;
  Driver.load_couples_array base;
  let fget, cget, cset, patched = consang_array base in
  (try
     let tab =
       let ts = Consang.topological_sort base Driver.poi in
       Consang.make_relationship_info base ts
     in
     let ts = tab.Consang.tstab in
     let persons = Driver.ipers base in
     let max_cnt = Driver.nb_of_persons base in
     let cnt = ref max_cnt in
     if verbosity >= 1 then Printf.eprintf "To do: %d persons\n" max_cnt;
     if max_cnt <> 0 then
       if verbosity >= 2 then (
         Printf.eprintf "Computing consanguinity...";
         flush stderr)
       else if verbosity >= 1 then ProgrBar.start ();
     (* Sort persons in decreasing topological order: ancestors first.
        Topological sort guarantees: i is ancestor of j => ts[i] > ts[j].
        Processing in decreasing order ensures parents are always computed
        before their children, so a single pass is sufficient. *)
     let sorted =
       Collection.fold (fun acc i -> i :: acc) [] persons
       |> List.sort (fun a b ->
           compare (Collection.Marker.get ts b) (Collection.Marker.get ts a))
     in
     (* Build fingerprints after sorting, so ancestors are processed first *)
     let fp_tab = compute_fingerprints base fget sorted in
     (* Per-family cache: relationship() is expensive; siblings sharing the
        same parents must not trigger a recomputation for each child. *)
     let family_cache : (Driver.ifam, Adef.fix) Hashtbl.t =
       Hashtbl.create (Driver.nb_of_families base)
     in
     let most = ref None in
     List.iter
       (fun i ->
         trace verbosity !cnt max_cnt;
         decr cnt;
         match fget i with
         | None ->
             (* Person has no parents: consanguinity is 0 *)
             cset i (Adef.fix_of_float 0.0)
         | Some ifam ->
             let cg =
               match Hashtbl.find_opt family_cache ifam with
               | Some cg ->
                   (* Siblings: reuse already computed family consanguinity *)
                   cg
               | None ->
                   let cpl = Driver.foi base ifam in
                   let ifath = Driver.get_father cpl in
                   let imoth = Driver.get_mother cpl in
                   let consang =
                     (* Fingerprint filter: if no common ancestor bit,
                        consanguinity is certainly 0 — skip relationship() *)
                     if
                       not
                         (fp_intersects
                            (Collection.Marker.get fp_tab ifath)
                            (Collection.Marker.get fp_tab imoth))
                     then 0.0
                     else relationship base tab ifath imoth
                   in
                   let cg = Adef.fix_of_float consang in
                   Hashtbl.add family_cache ifam cg;
                   cg
             in
             cset i cg;
             if verbosity >= 2 then
               if match !most with Some m -> cg > cget m | None -> true then (
                 Printf.eprintf "\nMax consanguinity %g for %s... "
                   (Adef.float_of_fix cg)
                   (Gutil.designation base (Driver.poi base i));
                 flush stderr;
                 most := Some i))
       sorted;
     if max_cnt <> 0 then
       if verbosity >= 2 then (
         Printf.eprintf " done   \n";
         flush stderr)
       else if verbosity >= 1 then ProgrBar.finish ()
   with Sys.Break when verbosity > 0 ->
     Printf.eprintf "\n";
     flush stderr;
     ());
  if !patched then Driver.commit_patches base;
  !patched
