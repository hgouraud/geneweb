(* $Id: fnameCache.ml,v 7.00 2019-02-12 09:58:44 hg Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config

type cache_fname_t = (string, (string * int)) Hashtbl.t

let (ht_cache_fname : cache_fname_t) =
  Hashtbl.create 1

let cache_fname conf =
  let bname =
    if Filename.check_suffix conf.bname ".gwb" then conf.bname
    else conf.bname ^ ".gwb"
  in
  Filename.concat (Util.base_path [] bname) "cache_fname"

let read_cache_fname conf =
  let fname = cache_fname conf in
  match try Some (Secure.open_in_bin fname) with Sys_error _ -> None with
  | Some ic ->
      begin
        let ht : cache_fname_t = input_value ic in
        close_in ic; ht
      end
  | None -> ht_cache_fname

let write_cache_fname conf ht_cache_fname =
  let ht_cache = read_cache_fname conf in
  let () =
    Hashtbl.iter
      (fun k v ->
         if not (Hashtbl.mem ht_cache_fname k) then
           Hashtbl.add ht_cache_fname k v
         else ())
      ht_cache
  in
  let fname = cache_fname conf in
  begin match try Some (Secure.open_out_bin fname) with Sys_error _ -> None with
  | Some oc ->
      begin
        output_value oc ht_cache_fname;
        close_out oc
      end
  | None -> ()
  end;
  begin match try Some (Secure.open_out (fname ^ ".cach")) with Sys_error _ -> None with
  | Some oc ->
      begin
        Hashtbl.iter
          (fun _k (v, _) -> output_string oc ("<option>" ^ v ^ "\n"))
            ht_cache_fname;
        close_out oc;
      Printf.eprintf "done\n";
      end
  | None -> ()
  end

(* keep a ref count for v.
  ok = "" -> add new entry
  v = "" -> suppress entry
  if count reaches 0, suppress entry *)
let patch_cache_fname conf ok k v merge =
  let ht_cache_fname = read_cache_fname conf in
  if v <> "" then
    begin
    if not (Hashtbl.mem ht_cache_fname k) then
      Hashtbl.add ht_cache_fname k (v, 1)
    else
      if not merge then
        begin
          let (vv, i) = Hashtbl.find ht_cache_fname k in
          Hashtbl.replace ht_cache_fname k (vv, i + 1)
        end
      else ()
    end
  else
    begin
    if not (Hashtbl.mem ht_cache_fname k) then
      Printf.eprintf "Inconsistent cache_fname\n"
    else
      begin
        let (vv, i) = Hashtbl.find ht_cache_fname k in
        if i = 1 then
          Hashtbl.remove ht_cache_fname k
        else
          Hashtbl.replace ht_cache_fname k (vv, i - 1)
      end
    end;
  if ok <> "" then
    begin
      if not (Hashtbl.mem ht_cache_fname ok) then
        Printf.eprintf "Inconsistent cache_fname: %s\n" ok
      else
        begin
          let (vv, i) = Hashtbl.find ht_cache_fname ok in
          if i = 1 then
            Hashtbl.remove ht_cache_fname ok
          else
            Hashtbl.replace ht_cache_fname ok (vv, i - 1)
        end
    end
  else ();
  let fname = cache_fname conf in
  match try Some (Secure.open_out_bin fname) with Sys_error _ -> None with
  | Some oc ->
      begin
        output_value oc ht_cache_fname;
        close_out oc
      end
  | None -> ()
