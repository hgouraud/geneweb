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

let hello assets conf base =
  let models = w_default_env assets conf base Tnull in
  interp assets conf "welcome.html.jingoo" models

let ns = "v8"

let () =
  Gwdlib.GwdPlugin.register ~ns "HELLO" (fun s -> hello s)
