open Geneweb

let ht
  : (string, (string * (Config.config -> Gwdb.base -> bool))) Hashtbl.t
  = Hashtbl.create 0

let assets = ref ""

let register ~ns m fn = let fn = fn !assets in Hashtbl.add ht m (ns, fn)
let get = Hashtbl.find_opt ht
