# Localisation d'une base

Toutes les bases GeneWeb d'un serveur sont regroupées dans un même dossier `bases/` (chemin indiqué à `gwd` via l'option `-bd`, voir [Le serveur gwd](gwd.md)). Chaque base y occupe son propre sous-dossier `nombase.gwb/`, accompagné de son fichier de configuration `nombase.gwf` (voir [Le fichier .gwf](base-gwf.md)) placé à côté, pas dedans.

> Source : page [bases](https://geneweb.tuxfamily.org/wiki/bases) du wiki officiel.

## Structure interne d'une base (`nombase.gwb/`)

| Élément | Rôle |
|---|---|
| `lang/` | Compléments locaux au lexique de langue (chargés avec `-add_lexicon` au démarrage de `gwd`) |
| `etc/template.txt` | Variante personnalisée d'un fichier de template, utilisée en priorité sur le fichier générique de `gw/etc/` (voir [Templates](../divers/templates.md)) |
| `etc/modules/module_xx.txt` | Variantes personnalisées de fichiers modules |
| `cache/mybase_fnames_cache.gz` | Liste des prénoms utilisée par le navigateur pour l'auto-complétion des formulaires |
| `cache/template.txt` | Variante personnelle du fichier de template, mise en cache |
| `cache/particles.txt` (ou `mybase-particles.txt`) | Liste des particules nobiliaires (voir [Signification des divers champs](../magicien/creation-champs.md)) propre à cette base |
| `hed.txt` / `trl.txt` | En-tête et pied de page spécifiques à la base (voir [Personnalisation](../divers/personnalisation.md)) |
| `history/` | Historique des modifications (activé par le paramètre `history` du `.gwf`) |
| `history_d/` | Dossier des fichiers de différences, un sous-dossier par lettre (peut être déplacé via `history_path=` dans la configuration) |
| `notes` | Note principale de la base, affichée sur la page d'accueil (voir [Le format .gw](../divers/format-gw.md#note-de-base-notes-db)) |
| `notes_d/` | Dossier de toutes les notes et pages liées |
| `notes_d/<nom>.txt` | Une page étendue (« linked page »), invoquée depuis une note via `[[[nom/texte]]]` (voir [Le format wiki](../divers/format-wiki.md)) |
| `lex_utf8.txt` | Fichier de lexique principal (voir [Internationalisation, lexiques](../divers/i18n-lexiques.md)) |
| `lex_additionnal.txt` | Fichier de lexique additionnel, utilisé par `gwd -add_lexicon` |
| `src/` | Fichiers sources associés à la base : images (invoquées par `m=IMH`) et fichiers texte (invoqués par `m=SRC`) |

Les noms de fichiers en italique dans la documentation originale sont ceux choisis librement par l'utilisateur (par exemple `mybase`, `mybase2`, `myimage.jpg`).

## Bases multiples et sélection via l'URL

Un même serveur `gwd` peut héberger un nombre quelconque de bases indépendantes dans son dossier `bases/`. La base concernée par une requête est sélectionnée par son nom dans l'URL, par exemple `http://serveur:2317/nombase`.

## Traductions par fichier texte

Tout fichier texte affiché via les commandes `m=SRC&v=fichier` ou `m=DOC&s=fichier.txt` passe par le moteur de traduction et d'expansion de macros de GeneWeb (`%s`, `%v`, `%i`, `%k`...). Il est aussi possible de fournir une traduction complète et indépendante d'un tel fichier en suffixant son nom par `_lg` (où `lg` est le code à deux lettres de la langue voulue) : GeneWeb sélectionne alors ce fichier alternatif entier plutôt que d'appliquer sa traduction terme à terme.

## Archivage simplifié (7.1-exp)

Une version expérimentale de GeneWeb 7.1 permettrait de simplifier l'archivage d'une base en regroupant tous les fichiers nécessaires sous un même dossier. <!-- TODO : préciser une fois cette fonctionnalité disponible sur une version stable. -->
