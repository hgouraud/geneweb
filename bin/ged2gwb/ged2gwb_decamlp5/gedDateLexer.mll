(* Lexeur ocamllex remplaçant [lexing_date] + [number]/[ident]/[text] et le
   Token/Plexing de camlp5.  Il produit directement les tokens de
   gedDateParser.mly.

   Rappels de comportement de l'ancien lexeur camlp5 (lignes 389-412) :
     - INT  = suite de chiffres
     - ID   = suite de [A-Z]  (l'entrée est déjà passée en majuscules)
     - TEXT = "(" ... ")" avec parenthèses imbriquées conservées telles quelles
     - '.'  et tout autre caractère isolé -> token "vide" ("", c)
     - espaces/tab/CR ignorés
     - fin de flux -> EOI
   Les mots-clés (BEF, AFT, mois, calendriers, ...) étaient de simples ID dont
   la *valeur* était testée par la grammaire.  Ici on les classe en tokens
   dédiés (table ci-dessous) pour garder une grammaire LR propre ; les ID
   génériques ne servent plus qu'aux chiffres romains et aux mots inconnus. *)

{
open GedDateParser

let keyword_table : (string, GedDateParser.token) Hashtbl.t = Hashtbl.create 97
let () =
  List.iter (fun (k, t) -> Hashtbl.replace keyword_table k t)
    [ (* intervalles *)
      "BEF", BEF; "AFT", AFT; "BET", BET; "AND", AND; "TO", TO; "FROM", FROM;
      (* précisions *)
      "ABT", ABT; "ENV", ENV; "EST", EST;
      (* calendriers *)
      "DGREGORIAN", DGREGORIAN; "DJULIAN", DJULIAN;
      "DFRENCH", DFRENCH; "DHEBREW", DHEBREW;
      "R", R; "AN", AN; "BCE", BCE;
      (* mois grégoriens *)
      "JAN", JAN; "FEB", FEB; "MAR", MAR; "APR", APR; "MAY", MAY; "JUN", JUN;
      "JUL", JUL; "AUG", AUG; "SEP", SEP; "OCT", OCT; "NOV", NOV; "DEC", DEC;
      (* mois républicains *)
      "VEND", VEND; "BRUM", BRUM; "FRIM", FRIM; "NIVO", NIVO; "PLUV", PLUV;
      "VENT", VENT; "GERM", GERM; "FLOR", FLOR; "PRAI", PRAI; "MESS", MESS;
      "THER", THER; "FRUC", FRUC; "COMP", COMP;
      (* mois hébraïques *)
      "TSH", TSH; "CSH", CSH; "KSL", KSL; "TVT", TVT; "SHV", SHV; "ADR", ADR;
      "ADS", ADS; "NSN", NSN; "IYR", IYR; "SVN", SVN; "TMZ", TMZ; "AAV", AAV;
      "ELL", ELL ]

let word_token w =
  match Hashtbl.find_opt keyword_table w with Some t -> t | None -> ID w
}

rule token = parse
  | [' ' '\t' '\r']+        { token lexbuf }
  | ['0'-'9']+ as n         { INT n }
  (* Décision 1 : on replie l'ère "B.C."/"B.C" en un seul token BCE.  L'ancien
     grammaire l'analysait en 4 tokens (ID "B"; "."; ID "C"; "."?), ce qui
     obligeait à garder "B" et "C" comme identifiants — en collision avec les
     chiffres romains "C"=100.  Le repli supprime la collision sans changer la
     sémantique (BCE et B.C. signifient tous deux "année négative"). *)
  | "B.C." | "B.C"          { BCE }
  | ['A'-'Z']+ as w         { word_token w }
  | '('                     { TEXT (text (Buffer.create 32) 0 lexbuf) }
  | '@'                     { AT }
  | '#'                     { HASH }
  | '.'                     { DOT }
  | '/'                     { SLASH }
  | '-'                     { MINUS }
  | eof                     { EOI }
  (* tout autre caractère : token non reconnu par la grammaire -> Parser.Error,
     qui déclenche le repli Dtext, exactement comme l'ancien Stream.Error. *)
  | _                       { OTHER }

(* Contenu d'un "( ... )" avec parenthèses imbriquées conservées, comme
   l'ancien [text].  [depth] = niveau d'imbrication courant. *)
and text buf depth = parse
  | ')'    { if depth = 0 then Buffer.contents buf
             else (Buffer.add_char buf ')'; text buf (depth - 1) lexbuf) }
  | '('    { Buffer.add_char buf '('; text buf (depth + 1) lexbuf }
  | eof    { Buffer.contents buf }   (* texte non terminé : on rend ce qu'on a *)
  | _ as c { Buffer.add_char buf c; text buf depth lexbuf }
