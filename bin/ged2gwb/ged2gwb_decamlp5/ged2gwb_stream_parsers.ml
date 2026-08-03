(* ================================================================== *)
(* Remplacement des stream-parsers camlp5 (syntaxe `parser [< >]`)     *)
(* par des fonctions récursives explicites sur Stream.peek/junk.       *)
(*                                                                     *)
(* Ceci supprime le DERNIER usage de la syntaxe `parser` (extension    *)
(* camlp5 pa_op) dans ged2gwb.  Après ça, plus aucun `parser`/`[< >]`/ *)
(* `EXTEND` : le fichier compile en OCaml pur, sans `-pp camlp5`.      *)
(* `Stream` reste utilisé (Stream.of_channel/of_string/peek/junk/count)*)
(* et provient désormais de la bibliothèque `camlp-streams` — qui      *)
(* n'est PAS camlp5 (pas de préprocesseur), juste le module Stream     *)
(* sorti de la stdlib.  Pour t'en débarrasser aussi, voir la note Seq  *)
(* en bas.                                                             *)
(*                                                                     *)
(* Règle de désucrage (identique à ce que camlp5 produit) :            *)
(*   - on `peek` le PREMIER caractère de chaque alternative ;          *)
(*   - s'il correspond, on `junk` (branche engagée) puis on continue ; *)
(*   - la branche `[< >]` (vide) = cas par défaut sans consommation ;  *)
(*   - une fonction SANS branche vide (line_start) lève Stream.Failure *)
(*     quand rien ne correspond, exactement comme avant.               *)
(* ================================================================== *)

(* --- Remplace ged2gwb.ml lignes 69-104 --- *)

let rec skip_eol strm =
  match Stream.peek strm with
  | Some ('\010' | '\013') -> Stream.junk strm; skip_eol strm
  | _ -> ()

let rec get_to_eoln len strm =
  match Stream.peek strm with
  | Some ('\010' | '\013') -> Stream.junk strm; skip_eol strm; Buff.get len
  | Some '\t' -> Stream.junk strm; get_to_eoln (Buff.store len ' ') strm
  | Some c -> Stream.junk strm; get_to_eoln (Buff.store len c) strm
  | None -> Buff.get len

let rec skip_to_eoln strm =
  match Stream.peek strm with
  | Some ('\010' | '\013') -> Stream.junk strm; skip_eol strm
  | Some _ -> Stream.junk strm; skip_to_eoln strm
  | None -> ()

let eol_chars = ['\010'; '\013']

let rec get_ident len strm =
  match Stream.peek strm with
  | Some (' ' | '\t') -> Stream.junk strm; Buff.get len
  | Some c when not (List.mem c eol_chars) ->
      Stream.junk strm; get_ident (Buff.store len c) strm
  | _ -> Buff.get len

let skip_space strm =
  match Stream.peek strm with
  | Some (' ' | '\t') -> Stream.junk strm
  | _ -> ()

(* Pas de branche vide dans l'original : Stream.Failure si le caractère
   n'est ni ' ' ni [num].  C'est ce qui permet à get_lev_list de savoir
   qu'un niveau ne correspond pas et de s'arrêter proprement. *)
let rec line_start num strm =
  match Stream.peek strm with
  | Some ' ' -> Stream.junk strm; line_start num strm
  | Some x when x = num -> Stream.junk strm
  | _ -> raise Stream.Failure

(* --- Remplace ged2gwb.ml lignes 257-280 --- *)

let rec get_lev n strm =
  line_start n strm;
  skip_space strm;
  let r1 = get_ident 0 strm in
  let (rlab, rval, rcont, l) =
    if String.length r1 > 0 && r1.[0] = '@' then parse_address n r1 strm
    else parse_text n r1 strm
  in
  { rlab;
    rval = utf8_of_string rval;
    rcont = utf8_of_string rcont;
    rsons = List.rev l;
    rpos = !line_cnt;
    rused = false }

and parse_address n r1 strm =
  let r2 = get_ident 0 strm in
  let r3 = get_to_eoln 0 strm in
  let l = get_lev_list [] (Char.chr (Char.code n + 1)) strm in
  (r2, r1, r3, l)

and parse_text n r1 strm =
  let r2 = get_to_eoln 0 strm in
  let l = get_lev_list [] (Char.chr (Char.code n + 1)) strm in
  (r1, r2, "", l)

(* `[< x = get_lev n; s >] -> ... | [< >] -> l`
   get_lev est le PREMIER composant : un Stream.Failure (venu de
   line_start quand le niveau ne correspond pas) = "pas de match" ->
   branche vide -> on rend [l].  Un Stream.Error (consommation puis
   échec) se propage, comme avant. *)
and get_lev_list l n strm =
  match (try Some (get_lev n strm) with Stream.Failure -> None) with
  | Some x -> get_lev_list (x :: l) n strm
  | None -> l

(* --- Remplace ged2gwb.ml lignes 321-365 --- *)

let rec skip_spaces strm =
  match Stream.peek strm with
  | Some ' ' -> Stream.junk strm; skip_spaces strm
  | _ -> ()

let rec ident_slash len strm =
  match Stream.peek strm with
  | Some '/' -> Stream.junk strm; Buff.get len
  | Some '\t' -> Stream.junk strm; ident_slash (Buff.store len ' ') strm
  | Some c -> Stream.junk strm; ident_slash (Buff.store len c) strm
  | None -> Buff.get len

(* strip_spaces (l. 351) reste inchangé, défini juste avant parse_name. *)

let parse_name strm =
  skip_spaces strm;
  let invert =
    match Stream.peek strm with
    | Some '/' -> Stream.junk strm; true
    | _ -> false
  in
  let f = ident_slash 0 strm in
  skip_spaces strm;
  let s = ident_slash 0 strm in
  let (f, s) = if invert then (s, f) else (f, s) in
  let f = strip_spaces f in
  let s = strip_spaces s in
  ((if f = "" then "x" else f), (if s = "" then "?" else s))

(* ------------------------------------------------------------------ *)
(* NOTE — supprimer aussi camlp-streams (optionnel)                    *)
(*                                                                     *)
(* Ces fonctions n'utilisent plus que Stream.peek/junk.  Pour éliminer *)
(* la dépendance `camlp-streams`, remplacer Stream par un lecteur      *)
(* maison à une case d'avance (peek/junk sur un in_channel + un        *)
(* buffer de 1 caractère), ou par Seq.t.  Attention : Stream est       *)
(* IMPÉRATIF et partagé (get_lev_list rappelle get_lev sur le MÊME     *)
(* flux avec effet de bord) ; un Seq fonctionnel demanderait de        *)
(* threader l'état de lecture en retour de chaque fonction.  Un petit  *)
(* type mutable `{ ic; mutable ahead : char option }` avec `peek`/     *)
(* `junk` est le remplacement le plus direct et garde ces fonctions    *)
(* quasi identiques (juste `Stream.peek`->`R.peek`, `Stream.junk`->    *)
(* `R.junk`).  Les points d'entrée Stream.of_channel/of_string         *)
(* (l. 956, 965, 1142, 1647, 2701...) deviennent R.of_channel/of_string*)
(* et Stream.count (l. 2663) -> un compteur dans le lecteur.           *)
(* ------------------------------------------------------------------ *)
