# Cycle de vie d'une base

Au-delà de la création initiale (voir [Mise en place et paramétrisation d'une base](base.md)), plusieurs opérations de maintenance courantes accompagnent la vie d'une base GeneWeb : sauvegarde, nettoyage, renommage, archivage, ou encore fusion/division de bases entières.

> **Ne pas confondre avec la fusion de doublons.** Ce chapitre couvre les opérations à l'échelle d'une base entière. La fusion de deux fiches en double au sein d'une même base est un sujet distinct, traité dans [Suppression, fusion](../magicien/suppression-fusion.md).

<!--
TODO — sources à consulter (timeout au moment de la rédaction, à réessayer) : pages du wiki officiel
[Save](https://geneweb.tuxfamily.org/wiki/save/fr), [Recover](https://geneweb.tuxfamily.org/wiki/recover/fr),
[Clean](https://geneweb.tuxfamily.org/wiki/clean/fr), [Rename](https://geneweb.tuxfamily.org/wiki/rename/fr),
[Archive](https://geneweb.tuxfamily.org/wiki/archive/fr), [Divide](https://geneweb.tuxfamily.org/wiki/divide/fr),
[Merge](https://geneweb.tuxfamily.org/wiki/merge/fr) (fusion de deux bases entières, distinct de la fusion de fiches).
-->

## Sauvegarde

> Source : page [save](https://geneweb.tuxfamily.org/wiki/save) du wiki officiel. Les mêmes opérations sont accessibles en ligne de commande ou depuis l'interface web [gwsetup](gestion.md).

Les supports de stockage ne sont jamais fiables à 100 % : conservez plusieurs copies sur des supports distincts. Il peut aussi être prudent de garder des versions intermédiaires d'une base, pour pouvoir revenir en arrière en cas de problème sérieux après une modification. Enfin, le format `.gw` reste le moyen le plus sûr et le plus propre d'échanger une base entre utilisateurs de GeneWeb (le GEDCOM est possible mais sans garantie de compatibilité à 100 %).

- **Mécanisme simple** : copier tel quel le dossier `toto.gwb`.
- **Meilleur mécanisme** : produire un fichier texte `toto.gw` au format `.gw` (voir [Le format .gw](../divers/format-gw.md)), avec [`gwu`](gestion-import-export.md#gwu) :
  ```bash
  gwu toto > toto.gw
  # ou, équivalent, avec l'option -o :
  gwu toto -o toto.gw
  ```
  Le nom du fichier est libre, à condition de garder l'extension `.gw`. Pour une archive sûre, multipliez les copies de ce fichier sur plusieurs supports.

### Avantages du format .gw pour l'archivage

- **Simplicité** : un seul fichier là où le dossier de base en contient plusieurs (les notes de forum et l'historique de modifications ne sont toutefois pas conservés dans un fichier `.gw`).
- **Compacité** : un fichier `.gw` source est nettement plus léger que le dossier de base correspondant.
- **Compatibilité ascendante** : le format `.gw` reste compatible avec les futures versions de GeneWeb.
- **Lisibilité** : le format `.gw` est lisible par un humain, et permet une édition globale ou une détection de différences (`diff base-v1.gw base-v2.gw` sous Unix/Linux/macOS).

### Restaurer une base à partir d'un fichier .gw

```bash
gwc toto.gw -o toto
```

### Extraction partielle de données

`gwu` permet aussi de n'extraire qu'une partie d'une base :

| Option | Effet |
|---|---|
| `-o <fichier>` | Nom du fichier de sortie (sinon, sortie sur la sortie standard) |
| `-odir <dossier>` | Crée un fichier par personne/famille extraite dans ce dossier, plutôt qu'un fichier unique (voir aussi [Diviser une base fusionnée](#diviser-une-base-fusionnée-en-ses-sources-dorigine) ci-dessous) |
| `-a "<prénom>" [numéro] "<patronyme>"` | Extrait les ascendants de la personne désignée |
| `-d` | Extrait les descendants de la personne désignée, ainsi que leurs conjoints et les parents de ces conjoints (sauf si `-nsp` est activé, pour ce dernier point) |
| `-ad` | Extrait les ascendants de la personne désignée et leurs descendants, conjoints compris, mais sans les parents de ces conjoints ; les descendants de la personne désignée elle-même sont inclus |
| `-s` | Extrait les personnes portant un patronyme donné, avec leurs conjoints et les parents de ces conjoints (sauf si `-nsp` est activé) |
| `-sep_only_file <fichier>` | Avec `-sep` (voir [Fusionner deux bases](#fusionner-ou-diviser-deux-bases-entières)), ne sépare que les groupes de ce fichier |
| `-sep_limit <nombre>` | Avec `-sep`, seuil associé à la séparation |

**Règles de combinaison** : utiliser à la fois `-a` et `-d` calcule leur **intersection** (personnes qui sont à la fois ascendantes et descendantes d'un point de vue de la requête). Utiliser plusieurs options `-s` calcule leur **union** (toutes les personnes correspondant à au moins un des patronymes donnés).

<!-- TODO : à vérifier — les pages liées associées aux personnes extraites devraient faire partie de l'extraction, mais ce point n'est pas confirmé avec certitude par la source. -->

### Points de vigilance (bases GeneWeb 7)

Certaines informations « orphelines » ne sont pas sauvegardées dans le fichier `.gw` produit par `gwu` :

- **Personnes isolées** (sans famille, ni témoin, ni relation avec quiconque) — un cas qui peut survenir, par exemple, pour une personne ayant été témoin d'un événement depuis supprimé. GeneWeb 7 propose un paramètre supplémentaire **`-isolated`** pour les archiver malgré tout.
- **Une page liée** (créée via la syntaxe [wikitext](../divers/format-wiki.md) ou la commande `m=NOTES;f=nom_page`) n'est pas sauvegardée si elle n'est référencée ni dans les notes d'une personne, ni dans une autre page liée.
- Si `gwu` s'arrête avec l'erreur `Uncaught exception: Invalid_argument("index out of bounds")`, ajoutez le paramètre **`-mem`** (rencontré avec la version 5.02 sous Ubuntu).

## Restauration (recover)

Décrit comment récupérer une base à partir des fichiers source après une évolution de version de GeneWeb.

> Source : page [recover](https://geneweb.tuxfamily.org/wiki/recover) du wiki officiel.

- **Mise à jour mineure** (pas de changement du format de base) : il suffit de copier/coller le dossier `nombase.gwb` de l'ancienne version vers la nouvelle (idéalement installée dans un dossier séparé, par précaution), puis d'ouvrir la base avec la nouvelle version pour vérifier qu'elle est bien acceptée. Si la base provient à l'origine d'un fichier GEDCOM avec peu ou pas de modifications depuis, on peut aussi choisir de refaire l'import GEDCOM avec la nouvelle version — l'outil `ged2gwb` (voir [Import / export](gestion-import-export.md)) s'améliorant de version en version, de nouvelles balises GEDCOM peuvent y être mieux prises en charge.
- **Mise à jour majeure** : un cycle complet de sauvegarde/restauration (voir [Sauvegarde](#sauvegarde) ci-dessus) est nécessaire. Consultez le fichier `CHANGES` du dépôt pour savoir si une évolution donnée impose ce cycle complet.
## Nettoyage (clean)

> Source : page [clean](https://geneweb.tuxfamily.org/wiki/clean) du wiki officiel.

Deux problèmes s'accumulent naturellement dans une base au fil des modifications :
- après ajout ou modification de familles, l'affichage de la consanguinité disparaît ou devient incorrect ;
- après des suppressions, l'espace correspondant n'est pas récupéré (le nombre total de personnes affiché ne diminue jamais, même après suppression).

**Premier niveau de nettoyage** : relancer [`consang`](gestion-maintenance.md#consang), qui recalcule la consanguinité pour toute la base et améliore le temps d'accès. Le paramètre `wizard_just_friend` du [fichier .gwf](base-gwf.md) est utile à activer pendant cette opération, pour empêcher temporairement toute mise à jour concurrente de la base par un autre magicien.

**Nettoyage plus profond** : une séquence `gwu` puis `gwc` recrée entièrement la base à partir de zéro.
```bash
gwu ancienne_base -o fichier.gw
gwc fichier.gw -o nouvelle_base
```
Bénéfice secondaire : cette séquence produit un fichier au format `.gw`, la solution d'archivage recommandée (voir [Installation](installation.md)). **Attention** : toute personne isolée (n'appartenant à aucune famille, et n'étant ni témoin ni lié par une relation à quiconque) disparaît de la base au cours de cette opération — voir [Maintenance](gestion-maintenance.md#connex) pour repérer ces cas au préalable via `connex`. Il est recommandé de nommer la nouvelle base différemment de l'ancienne, pour éviter toute perte de données si une étape échoue en cours de route.

## Renommage (rename)

<!-- TODO : mécanisme de redirection automatique pour les anciens liens/signets, déjà mentionné via le paramètre `renamed` dans Le fichier .gwf -->

## Archivage (archive)

Pour archiver une distribution sur un support amovible : outre les fichiers de base habituels, veillez à conserver l'exécutable `gwd` avec son fichier `.arg` associé (voir [Le serveur gwd](gwd.md)), et à vérifier que la propriété exécutable de ces fichiers est bien préservée après copie (en particulier `gwd.exe` sous Windows).

<!-- TODO : reste du contenu de la page [archive](https://geneweb.tuxfamily.org/wiki/archive), au-delà du point ci-dessus. -->

## Fusionner ou diviser deux bases entières

Cas d'usage : regrouper deux généalogies distinctes en une seule base, ou au contraire isoler une branche dans sa propre base.

> Source : page [merge/fr](https://geneweb.tuxfamily.org/wiki/merge/fr) du wiki officiel.

La fusion de deux bases se déroule en plusieurs temps :

1. **Extraire** chacune des deux bases au format `.gw` avec `gwu` (voir [Import / export](gestion-import-export.md)).
2. **Concaténer** les deux fichiers `.gw`, puis les recompiler ensemble avec `gwc`. Si des personnes portent le même nom et le même numéro d'occurrence dans les deux bases d'origine, `gwc` signale des erreurs de conflit — l'option `gwc -sep` décale automatiquement les numéros d'occurrence pour éviter ces conflits.
3. **Nettoyer les doublons** : si l'option `-sep` a été utilisée, une même personne réelle peut désormais exister sous deux numéros différents dans la base fusionnée. Ouvrez la nouvelle base dans votre navigateur et appliquez la [fusion de fiches](../magicien/suppression-fusion.md) autant de fois que nécessaire.

> **À savoir** : une fusion (comme une suppression) ne récupère pas la place occupée par les personnes ou familles supprimées — la page d'accueil continue d'indiquer le nombre de personnes de départ, pas le nombre réel après nettoyage des doublons.

Pour identifier de quelle base d'origine provient un individu au sein d'une famille de la base fusionnée, ajoutez `;opt=from` à l'URL de sa fiche.

### Diviser une base fusionnée en ses sources d'origine

> Source : page [Divide/fr](https://geneweb.tuxfamily.org/wiki/Divide/fr) du wiki officiel.

Lors d'une fusion (voir ci-dessus), GeneWeb conserve la trace de l'origine des informations et peut ensuite les séparer à nouveau pour recréer les fichiers source d'origine. Si trois fichiers `toto.gw`, `dupont.gw` et `titi.gw` ont été fusionnés ainsi :
```bash
gwc toto.gw -sep dupont.gw -sep titi.gw -o basefusionnee
```

Pour recréer les trois fichiers source dans un dossier `outdir` (à créer au préalable avec `mkdir outdir` s'il n'existe pas) :
```bash
gwu basefusionnee -odir outdir
```

Si la base a été modifiée après la fusion, les fichiers ainsi régénérés reflètent ces modifications et ne sont donc plus identiques aux fichiers d'origine. Si `gwu` affiche du texte source à l'écran plutôt que de l'écrire dans un fichier du dossier, c'est le signe que des personnes ont été ajoutées après la fusion sans que GeneWeb sache dans quel fichier d'origine les classer (des personnes non reliées à une source connue).

## Mise à jour distante automatisée

Le wiki officiel fournit un modèle de script shell (page [remote-base](https://geneweb.tuxfamily.org/wiki/remote-base)) permettant d'automatiser la mise à jour d'une base à partir d'un fichier `.gw` envoyé sur le serveur (par exemple depuis un poste de travail distant, via `scp` ou un mécanisme équivalent — le transfert lui-même n'est pas couvert par ce script). Le script, exécuté côté serveur après réception du fichier :

1. sauvegarde la base existante en la renommant avec un horodatage (`BASE-AAAA-MM-JJ-HH:MM:SS.gwb`) plutôt que de l'écraser directement ;
2. reconstruit la base à partir du fichier `.gw` reçu, avec `gwc -f -o` ;
3. met à jour l'index des pages liées avec `update_nldb` (voir [Maintenance](gestion-import-export.md)) ;
4. réapplique l'historique existant à la nouvelle base, si l'historique est activé (voir `history` dans [Le fichier .gwf](base-gwf.md)).

Une page [remote-files](https://geneweb.tuxfamily.org/wiki/remote-files) propose un mécanisme équivalent pour les dossiers d'images et de fichiers sources d'une base — confirmé par la page [CGI](https://geneweb.tuxfamily.org/wiki/CGI) du wiki, qui précise que la procédure sauvegarde aussi les dossiers `images` et `src` de la base avec le même horodatage, et exécute côté serveur un script généralement nommé `remote.sh`. Ces sauvegardes s'accumulant avec le temps, un nettoyage périodique est recommandé.

Une variante de ce script, pensée spécifiquement pour un **hébergement mutualisé** (page [base-upld](https://geneweb.tuxfamily.org/wiki/base-upld) du wiki officiel), s'appuie sur `scp`/`ssh` avec authentification par paire de clés plutôt que par mot de passe en clair, et s'invoque `update-base <nom_de_base> [images|src]`.

## Voir aussi

Le paramètre d'URL `dag=on` (arbres ascendants/descendants, voir [Affichage](../utilisateur/affichage.md#représentation-en-graphe-dagsvg)) permet de fusionner visuellement les ancêtres communs (« implexes ») d'un arbre, un sujet différent mais qui utilise un vocabulaire de « fusion » proche — à ne pas confondre avec les fusions de fiches ou de bases traitées dans ce chapitre.
