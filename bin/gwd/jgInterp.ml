open Geneweb
open Jingoo

let render ?(dir = Util.search_in_lang_path "etc") ~conf ~file ~models =
  let env =
    { Jg_types.autoescape = false
    ; template_dirs = [ dir ]
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
  Jg_interp.from_file ~env ~ctx ~models ~output file
