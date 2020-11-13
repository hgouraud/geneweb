open Geneweb
open Config
open Def
open Gwdb
open TemplAst
open Util

let print_ascend conf base p =
  if p_getenv conf.env "t" = Some  "FC"
  then (Perso.interp_templ "fanchart" conf base p ; true)
  else false
