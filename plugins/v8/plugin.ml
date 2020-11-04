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
  Gwdlib.JgInterp.render ~dir:(Filename.concat assets "templates") ~conf ~file ~models

let asearch assets conf base =
  let models = w_default_env assets conf base Tnull in
  interp assets conf "SEARCH_ADVANCED.html.jingoo" models

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
  let l = List.sort_uniq compare l in
  let l = Tlist (List.map (Data.unsafe_mk_person conf base) l) in
  let models =
    w_default_env assets conf base begin Tpat begin function
        | "result" -> l
        | _ -> raise Not_found
      end end
  in
  interp assets conf "SEARCH_RESULT.html.jingoo" models

let home assets conf base =
  let models = w_default_env assets conf base Tnull in
  interp assets conf "HOME.html.jingoo" models  

let ns = "v8"

let () =
  Gwdlib.GwdPlugin.register ~ns "" home
; Gwdlib.GwdPlugin.register ~ns "SEARCH_ADVANCED" asearch
; Gwdlib.GwdPlugin.register ~ns "SEARCH_SIMPLE" ssearch
