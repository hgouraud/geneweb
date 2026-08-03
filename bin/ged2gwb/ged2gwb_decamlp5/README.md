# Sortie de camlp5 pour ged2gwb (GeneWeb)

ged2gwb est le dernier module GeneWeb à dépendre de camlp5. Ce dossier contient
le remplacement complet de ses deux usages de camlp5 :

1. la grammaire de dates GEDCOM (`EXTEND … END` + `Grammar.Entry`) → **menhir + ocamllex** ;
2. les stream-parsers de lignes GEDCOM (syntaxe `parser [< >]`) → **fonctions OCaml pures** sur `Stream`.

Après application, ged2gwb ne contient plus aucun `parser` / `[< >]` / `EXTEND` :
il compile en OCaml pur, sans `-pp camlp5`.

## Fichiers

| Fichier                        | Rôle |
|--------------------------------|------|
| `gedDateLexer.mll`             | Lexeur ocamllex : remplace `lexing_date`/`number`/`ident`/`text` + la couche `Token`/`Plexing`. |
| `gedDateParser.mly`            | Grammaire menhir : remplace le bloc `EXTEND … END` (entrées `date_value`, `date_value_recover`, `date_interval`). |
| `gedDateAux.ml`                | Helpers extraits de ged2gwb (`make_date`, `recover_date`, `mk_yearint`, `is_roman_int`, `to_int`, exception `Not_a_date`, type `range`, état mutable des dates). Nécessaire pour casser le cycle parseur ↔ ged2gwb. |
| `ged2gwb_stream_parsers.ml`    | Réécriture des 11 fonctions `parser [< >]` (skip_eol, get_to_eoln, get_ident, line_start, get_lev, get_lev_list, parse_name, …) en récursion explicite `Stream.peek`/`junk`. |
| `ged2gwb_glue.ml`              | Ce qu'il faut supprimer dans ged2gwb + nouvelles versions de `date_of_field` et `decode_date_interval`. |
| `dune.snippet`                 | Stanzas `ocamllex`/`menhir` à ajouter ; retrait de camlp5 des libraries. |

## Étapes d'intégration

1. **Ajouter** `gedDateLexer.mll`, `gedDateParser.mly`, `gedDateAux.ml` au répertoire de ged2gwb.
2. **Compléter `gedDateAux.ml`** : recopier le vrai corps de `check_month` (ged2gwb l. 296, actuellement un PLACEHOLDER) et vérifier `Left/Right` (viennent de `Def`).
3. **Dans ged2gwb.ml, supprimer** :
   - stream-parsers de dates : `lexing_date`, `number`, `ident`, `text` (l. 389-412) ;
   - `make_date_lexing`, `tparse`, `using_token`, `date_lexer` (l. 414-439) ;
   - `date_g`, `date_value`, `date_interval`, `date_value_recover` (l. 446-449) ;
   - `roman_int` via `Grammar.Entry.of_parser` (l. 458-462) ;
   - le bloc `EXTEND … END` complet (l. 522-697) ;
   - `make_date`, `recover_date`, `is_roman_int`, type `range`, `check_month`,
     `month_number_dates`, `no_negative_dates`, `date_str` (déplacés dans `gedDateAux`).
4. **Dans ged2gwb.ml, remplacer** les 11 fonctions `parser [< >]` (l. 69-104, 257-280, 321-365) par le contenu de `ged2gwb_stream_parsers.ml`.
5. **Repréfixer** les usages restants : `!no_negative_dates` → `!GedDateAux.no_negative_dates`, `month_number_dates` / `MonthDayDates` / … → `GedDateAux.…`.
6. **Remplacer** `date_of_field` (l. 704-714) et `decode_date_interval` (l. ~1141) par les versions de `ged2gwb_glue.ml`.
7. **Build** : appliquer `dune.snippet` (ajouter `ocamllex`/`menhir`, retirer `camlp5.*`).
8. **Vérifier** : `menhir --explain gedDateParser.mly` puis inspecter `gedDateParser.conflicts` — les conflits attendus sont bénins (voir plus bas).

## Décisions de portage (choix ordonné camlp5 → LR)

1. **Ère `B.C.`** repliée en un seul token `BCE` par le lexeur (supprime la collision `C` = romain 100).
2. **`date_range` inliné** dans `date_or_text`, avec un `date_top` sans `AFT/BEF` : les deux branches commençaient par `AFT/BEF`, choix tranché par ordre en camlp5, impossible en LR sans conflit.
3. **`date_fren` ramené à 2 règles** (le `year_fren` partageait le préfixe `int`).
4. **`roman_int`** gardé en `ID` + prédicat `is_roman_int`, fidèle au `Grammar.Entry.of_parser` d'origine.

## Réserves

- **Non compilé** : écrit à la main (pas de toolchain OCaml/menhir disponible lors de la rédaction). À builder dans l'arbre GeneWeb.
- **Conflits menhir attendus** : quelques shift/reduce & reduce/reduce dans les
  séquences `ioption`/`list` de `date_greg`/`date_fren`/`date_hebr`, tous
  résolus vers un `make_date` identique (ex. « année seule » captée comme jour
  ou comme année → même date). À confirmer via `--explain`.
- **Reste `camlp-streams`** (module `Stream`, hors préprocesseur, ≠ camlp5).
  Optionnel : le remplacer par un lecteur mutable `{ ic; mutable ahead }` — voir
  la note en bas de `ged2gwb_stream_parsers.ml`.
