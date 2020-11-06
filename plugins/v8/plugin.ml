open Geneweb
open Config
open Jingoo
open Jg_types
open Gwxjg

let w_default_env assets conf base models =
  let asset =
    func_arg1_no_kw begin function
      | Tstr s -> Tstr (Filename.concat assets s)
      | x -> failwith_type_error_1 "asset" x
    end
  in
  ("M", models)
  :: ("asset", asset)
  :: Data.default_env conf base

let interp assets conf file models =
  Gwdlib.JgInterp.render ~dir:(Filename.concat assets "templates") ~conf ~file ~models ;
  true

let asearch assets conf base =
  let models = w_default_env assets conf base Tnull in
  interp assets conf "SEARCH_ADVANCED.html.jingoo" models

let itree_aux assets conf base root =
  let root = Gwxjg.Data.unsafe_mk_person conf base root in
  let models =
    w_default_env assets conf base begin Tpat begin function
        | "root" -> root
        | _ -> raise Not_found
      end
    end
  in
  interp assets conf "ITREE.html.jingoo" models

let itree assets conf base =
  match Util.find_person_in_env conf base "" with
  | Some p ->
    itree_aux assets conf base p
  | None -> assert false

let ssearch assets conf base =
  let v = Util.decode_varenv @@ List.assoc "v" conf.env in
  let l = Name.split_sname v in
  let rec loop acc = function
    | [] -> []
    | hd :: tl -> let acc = hd :: acc in (acc, tl) :: loop acc tl
  in
  let l = loop [] l in
  let l = List.rev_append l @@ List.rev_map (fun (a, b) -> (b, a)) l in
  let l = List.sort_uniq compare l in
  let l =
    List.map begin fun (fn, sn) ->
      let conf =
        { conf with env =
                      ("first_name", String.concat " " fn)
                      :: ("surname", String.concat " " sn)
                      :: conf.env }
      in
      fst @@ AdvSearchOk.advanced_search conf base max_int
    end l
  in
  let l = List.flatten l in
  match List.sort_uniq compare l with
  | [ p ] -> itree_aux assets conf base p
  | l ->
    let l = Tlist (List.map (Data.unsafe_mk_person conf base) l) in
    let models =
      w_default_env assets conf base begin Tpat begin function
          | "result" -> l
          | _ -> raise Not_found
        end end
    in
    interp assets conf "SEARCH_RESULT.html.jingoo" models

let home assets conf base =
  match Util.find_person_in_env conf base "" with
  | Some p ->
    let root = Gwxjg.Data.unsafe_mk_person conf base p in
    let models =
      w_default_env assets conf base begin Tpat begin function
          | "root" -> root
          | _ -> raise Not_found
        end
      end
    in
    interp assets conf "IND.html.jingoo" models
  | None ->
    let models = w_default_env assets conf base Tnull in
    interp assets conf "HOME.html.jingoo" models

let mod_fam assets conf base =
  if not conf.wizard then false
  else match Util.p_getenv conf.env "i" with
    | Some i ->
      let root =
        match Util.find_person_in_env conf base "p" with
        | Some p -> Gwxjg.Data.unsafe_mk_person conf base p
        | None -> Tnull
      in
      let ifam = Gwdb.ifam_of_string i in
      let sfam = UpdateFam.string_family_of conf base ifam in
      let digest = Tstr (Update.digest_family sfam) in
      let models =
        w_default_env assets conf base begin Tpat begin function
            | "digest" -> digest
            | "family" -> Gwxjg.Data.get_n_mk_family conf base ifam @@ Gwdb.foi base ifam
            | "root" -> root
            | _ -> raise Not_found
          end
        end
      in
      interp assets conf "MOD_FAM.html.jingoo" models
    | _ -> false

(* let warning assets conf base =
 *   let ht = Hashtbl.create 1024 in
 *   Check.check_base base ignore (fun x -> Hashtbl.replace ht x ()) ignore ;
 *   let warnings =
 *     Hashtbl.fold begin fun w () acc ->
 *       Gwxjg.Data.mk_warning conf base w :: acc
 *     end ht []
 *   in
 *   let models =
 *     let warnings = Tlist warnings in
 *     w_default_env assets conf base begin Tpat begin function
 *         | "warnings" -> warnings
 *         | _ -> raise Not_found
 *       end end
 *   in
 *   interp assets conf "WARNINGS.html.jingoo" models *)

let ns = "v8"

let () =
  Gwdlib.GwdPlugin.register ~ns "" home
; Gwdlib.GwdPlugin.register ~ns "SEARCH_ADVANCED" asearch
; Gwdlib.GwdPlugin.register ~ns "SEARCH_SIMPLE" ssearch
; Gwdlib.GwdPlugin.register ~ns "MOD_FAM" mod_fam
(* ; Gwdlib.GwdPlugin.register ~ns "WARNINGS" warning *)
; Gwdlib.GwdPlugin.register ~ns "ITREE" itree
