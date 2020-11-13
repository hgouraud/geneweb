open Geneweb
open Config
open Def
open Gwdb
open TemplAst
open Util


let () =
  Gwdlib.GwdPlugin.register ~ns:"fanchart" [ "A", fun assets conf base ->
      match Util.find_person_in_env conf base "" with
      | None -> false
      | Some p ->
        if p_getenv conf.env "t" = Some  "FC"
        then (Perso.interp_templ "fanchart" conf base p ; true)
        else false
    ]
