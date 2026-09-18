# Import / export : gwu, gwb2ged, gwc, ged2gwb

> Sources : pages [gwc](https://geneweb.tuxfamily.org/wiki/gwc) et [man/fr](https://geneweb.tuxfamily.org/wiki/man/fr) du wiki officiel.

## gwc

Lit un fichier au format `.gw` pour créer (ou recréer) une base GeneWeb (voir [Le format .gw](../divers/format-gw.md)).

- Lancé sans aucun paramètre, `gwc` crée une base vide nommée `a` (dossier `a.gwb`), dans laquelle on peut ensuite saisir familles et personnes.
- L'option `-o <nom>` définit le nom de la base créée. **Attention** : la base créée doit se trouver sur la même partition/le même volume que le dossier courant, sinon `gwc` échoue avec l'erreur `Uncaught exception: Sys_error("Invalid cross-device link")`.
- Une variante `gwc2` existe également.
- **Mode `reorg`** *(prévu pour la version 7.1 ou 7.2)* : dans ce mode, toutes les données auxiliaires d'une base (configuration, compteurs, portraits, images, fichiers sources) sont regroupées à l'intérieur du dossier `mybase.gwb` lui-même (voir [Localisation d'une base](base-localisation.md)) plutôt que dispersées — ce qui permettra une sauvegarde complète par simple archive zip du dossier. `gwd` 7.1 devrait pouvoir reconnaître automatiquement les deux organisations (classique et `reorg`) selon le paramètre `-reorg` utilisé lors de la création avec `gwc`.

## gwu

Extrait le contenu d'une base `.gwb` pour créer un fichier `.gw` (voir [Le format .gw](../divers/format-gw.md)) — l'opération inverse de `gwc`. Recommandé pour l'archivage ou l'échange d'une base (voir [Installation](installation.md)).

## ged2gwb

Lit un fichier au format GEDCOM (`.ged`) pour créer une base GeneWeb.

Options rencontrées :
- **`-no_pit`** (*no public if titles*) : ne considère pas automatiquement comme publiques les personnes titrées (comportement par défaut sans cette option — voir [Signification des divers champs](../magicien/creation-champs.md#accès-confidentialité)).
- **`-tnd`** (*try negative dates*) : force des dates négatives en cas d'incohérence détectée (ex. naissance après décès).
- **`-no_nd`** (*no negative dates*) : n'interprète pas une année précédée d'un signe moins comme une année négative.
- **`-udi x-y`** (*undefined death interval*) : définit l'intervalle utilisé quand le statut décès est indéfini — avant `x` ans, la personne est considérée vivante ; après `y` ans, décédée ; entre les deux, statut « ne sait pas ». Par défaut, `x`=80 et `y`=120 (voir la logique équivalente décrite dans [Convention date → MOD_IND](../magicien/navigation-maj-dates.md#choix-automatique-du-statut-décèsvivant)).
- **`-uin`** (*untreated in notes*) : place dans le champ notes toute balise GEDCOM que `ged2gwb` ne sait pas traiter, plutôt que de la perdre.
- **`-ds`** (*default source*) : définit une source par défaut pour les personnes et familles sans donnée de source.
- **`-dates_dm`** / **`-dates_md`** : interprète les dates numériques ambiguës comme jour/mois/année ou mois/jour/année respectivement.
- **`-charset [ANSEL|ASCII|MSDOS]`** : force un décodage de caractères donné, prioritaire sur celui éventuellement précisé dans le fichier GEDCOM lui-même.
- **`-nopicture`** : n'importe pas les images du fichier GEDCOM/`.gw`.

## gwb2ged

Extrait le contenu d'une base GeneWeb pour créer un fichier au format GEDCOM (`.ged`).

> **Limite connue** : `gwb2ged` ne dispose pas (encore) de l'option `-ad` disponible avec `gwu`. En cas de besoin, la méthode de contournement consiste à extraire d'abord avec `gwu`, recréer une base à partir du résultat, puis faire l'extraction GEDCOM complète avec `gwb2ged` sur cette base recréée.

> La compatibilité GEDCOM n'est pas garantie dans les deux sens : toute information que GeneWeb ne sait pas interpréter est conservée telle quelle dans une note plutôt que perdue (voir [Le format .gw](../divers/format-gw.md)).
