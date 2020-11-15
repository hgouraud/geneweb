open Geneweb
open Config
open Jingoo
open Jg_types
open Gwxjg

let getenv_list fn prefix conf =
  let rec loop acc i =
    match Util.p_getenv conf.env (prefix ^ string_of_int i) with
    | Some k -> loop (fn k :: acc) (i + 1)
    | None -> acc
  in
  loop [] 0

let w_updated_conf fn =
  fun assets conf base ->
  let conf =
    if List.mem_assoc "theme" conf.env
    then { conf with henv = ("theme", List.assoc "theme" conf.env) :: conf.henv }
    else conf
  in
  let env = List.filter (fun (k, _) -> not (List.mem_assoc k conf.henv || List.mem_assoc k conf.senv)) conf.env in
  let conf = { conf with env } in
  fn assets conf base

let w_default_env assets conf base models =
  let asset =
    func_arg1_no_kw begin function
      | Tstr s -> Tstr (Filename.concat assets s)
      | x -> failwith_type_error_1 "asset" x
    end
  in
  let enabled_plugin =
    func_arg1_no_kw begin function
      | Tstr s -> Tbool (List.mem s !Gwdlib.GwdPlugin.registered
                         && match List.assoc_opt "plugins" conf.Config.base_env with
                         | Some list -> List.mem s @@ String.split_on_char ',' list
                         | None -> false)
      | x -> failwith_type_error_1 "asset" x
    end
  in
  ("M", models)
  :: ("asset", asset)
  :: ("enabled_plugin", enabled_plugin)
  :: Data.default_env conf base

let interp assets _conf file models =
  let env =
    { Jg_types.autoescape = false
    ; template_dirs = [ Filename.concat assets "templates" ]
    ; filters = []
    ; extensions = []
    ; strict_mode = true
    }
  in
  let output x = Wserver.print_string @@ Jg_runtime.string_of_tvalue x in
  let ctx =
    let models = fun x -> List.assoc x models in
    Jg_interp.init_context ~env ~models ~output ()
  in
  Jg_interp.from_file ~env ~ctx ~models ~output file ;
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

let shortest_path assets conf base p =
  let fexcl = getenv_list Gwdb.ifam_of_string "ef" conf in
  let root =
    match List.assoc_opt "ei" conf.env with
    | Some i -> Gwdb.iper_of_string i
    | None -> match Util.find_person_in_env conf base "1" with
      | Some root -> (Gwdb.get_iper root)
      | None -> assert false
  in
  match Relation.get_shortest_path_relation conf base (Gwdb.get_iper p) root fexcl with
  | None ->
    let models =
      let target = Data.unsafe_mk_person conf base p in
      let root = Data.get_n_mk_person conf base root in
      let ifams = Tlist (List.map (fun i -> Data.get_n_mk_family conf base i @@ Gwdb.foi base i) fexcl) in
      w_default_env assets conf base @@
      Tpat begin function
        | "excluded" -> ifams
        | "root" -> root
        | "target" -> target
        | _ -> raise Not_found
      end
    in
    interp assets conf "PATH_ERROR.html.jingoo" models

  | Some (path, ifam) ->
    let path =
      Tlist begin List.rev_map begin fun (i, r) ->
          let p = Data.get_n_mk_person conf base i in
          let r = match r with
            | Relation.Self -> Tstr "Self"
            | Relation.Parent -> Tstr "Parent"
            | Relation.Sibling -> Tstr "Sibling"
            | Relation.HalfSibling -> Tstr "HalfSibling"
            | Relation.Mate -> Tstr "Mate"
            | Relation.Child -> Tstr "Child"
          in
          Tpat begin function "person" -> p | "relation" -> r | _ -> raise Not_found end
        end path end
    in
    let models =
      let ifam = Tstr (Gwdb.string_of_ifam ifam) in
      w_default_env assets conf base @@
      Tpat begin function
        | "path" -> path
        | "ifam" -> ifam
        | _ -> raise Not_found
      end
    in
    interp assets conf "PATH.html.jingoo" models

let a assets conf base =
  let jg_mod left right =
    match left, right with
    | _, Tint 0 -> failwith "jg_mod:zero division error"
    | Tint x1, Tint x2 -> Tint(x1 mod x2)
    | _, _ -> failwith_type_error_2 "jg_mod" left right
  in
  let jg_mod = Jg_types.func_arg2_no_kw jg_mod in
  match Util.find_person_in_env conf base "" with
  | None -> assert false
  | Some p ->
    let root = Gwxjg.Data.unsafe_mk_person conf base p in
    let models =
      w_default_env assets conf base begin Tpat begin function
          | "root" -> root
          | "jg_mod" -> jg_mod
          | _ -> raise Not_found
        end
      end
    in
    interp assets conf "H_TREE.html.jingoo" models

let cal assets conf base =
  let models = w_default_env assets conf base Tnull in
  interp assets conf "CAL.html.jingoo" models

let home assets conf base =
  match Util.find_person_in_env conf base "" with
  | Some p ->
    if List.assoc_opt "et" conf.env = Some "S"
    && List.assoc_opt "em" conf.env = Some "R"
    then shortest_path assets conf base p
    else
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

let warning assets conf base =
  let ht = Hashtbl.create 1024 in
  Check.check_base base ignore (fun x -> Hashtbl.replace ht x ()) ignore ;
  let warnings =
    Hashtbl.fold begin fun w () acc ->
      Gwxjg.Data.mk_warning conf base w :: acc
    end ht []
  in
  let models =
    let warnings = Tlist warnings in
    w_default_env assets conf base begin Tpat begin function
        | "warnings" -> warnings
        | _ -> raise Not_found
      end end
  in
  interp assets conf "WARNINGS.html.jingoo" models

let ns = "v8"

let () =
  Gwdlib.GwdPlugin.register ~ns
    [ "", w_updated_conf home
    ; "A", w_updated_conf a
    ; "CAL", w_updated_conf cal
    ; "MOD_FAM", w_updated_conf mod_fam
    ; "SEARCH_ADVANCED", w_updated_conf asearch
    ; "SEARCH_SIMPLE", w_updated_conf ssearch
    ; "WARNINGS", w_updated_conf warning
    ; "ITREE", w_updated_conf itree
    ]
