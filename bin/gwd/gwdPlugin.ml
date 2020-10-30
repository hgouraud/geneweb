open Geneweb

let ht
  : (string, (string * (Config.config -> Gwdb.base -> unit))) Hashtbl.t
  = Hashtbl.create 0

let register ~namespace ~mode fn = Hashtbl.add ht mode (namespace, fn)
let get = Hashtbl.find_opt ht
