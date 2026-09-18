# Le format .gw

Le format source `.gw` est utilisé par GeneWeb pour créer une base généalogique (via la commande `gwc`). GeneWeb peut aussi produire un fichier `.gw` à partir d'une base existante, pour l'archivage ou l'échange.

C'est un format texte, à la fois lisible par un humain et manipulable par un programme (édition possible avec n'importe quel éditeur de texte classique). C'est la forme la plus adaptée pour l'archivage d'une base et pour l'échange entre utilisateurs de GeneWeb. GeneWeb sait aussi produire et lire du GEDCOM, mais sans garantie de compatibilité totale (aucune information n'est perdue pour autant : ce que GeneWeb ne sait pas interpréter dans un GEDCOM est conservé tel quel dans une note texte).

> Source : page [.gw format](https://geneweb.tuxfamily.org/wiki/GWformat) du wiki officiel du projet.

## gwplus et rétrocompatibilité

Depuis la version 7.00, le format `.gw` a été étendu pour gérer les événements personnels et familiaux détaillés — cette extension est désignée **gwplus**. Un fichier `.gw` ne garde pas trace de la version de GeneWeb avec laquelle il a été créé.

> **Attention** : les versions de GeneWeb antérieures à 7.00 ne savent pas lire les extensions gwplus. Pour limiter ce problème, la commande `gwu` propose une option `-old_gw` qui reporte dans le champ notes toute information incompatible avec l'ancien format, plutôt que de la perdre.

## Convention de notation utilisée ci-dessous

- Le texte destiné à apparaître **tel quel** est en **gras**.
- Les mots en *italique* sont remplacés par leur valeur effective (leur initiale est en majuscule dans les exemples, ex. `HusbandLastName`).
- Les paramètres entre `[crochets]` sont optionnels.
- Des paramètres séparés par `|` sont des alternatives : un seul choix est possible.

## Structure générale d'un fichier .gw

```
[encoding: utf-8]   # optionnel, redéfinit l'encodage en utf-8 (sinon iso-8859-1 par défaut)
[gwplus]             # optionnel, indique que le fichier suit le format gwplus plutôt que gw classique

un nombre quelconque de blocs parmi :

Famille              # jeton de début : fam
Note personnelle     # jeton de début : notes
Relations            # jeton de début : rel
Événements personnels # jeton de début : pevt (spécifique à gwplus)
Note de base         # jeton de début : notes-db (un seul par base)
Page étendue         # jeton de début : page-ext
Note de magicien      # jeton de début : wizard-note
```

## Bloc famille (`fam`)

L'information structurante principale d'une base GeneWeb est la « famille » : un mari, une épouse, et leurs enfants (la vérification du sexe des conjoints peut être désactivée). Les informations personnelles (date de naissance, etc.) sont rattachées aux enfants.

**Cas n° 1 — les deux conjoints sont déjà mentionnés comme enfant d'une autre famille :**

```
fam NomMari Prénom[.Numéro] +[DateMariage]
  [#sep | - DateDivorce] [#nm | #eng] [#noment]
  [#mp LieuMariage] [#ms SourceMariage]
NomFemme Prénom[.Numéro]           # tous les arguments fam sur une seule ligne
[wit[ m| f|]: Témoin (format Personne, voir plus bas)]  # plusieurs témoins possibles
[src Source de la famille]
[comm Commentaire libre sur la famille]
   ÉvénementFamilial (plusieurs possibles)
end fevt]
- [h | f | ] Personne   # voir la section Informations personnelles ci-dessous
```

- Si les conjoints ne sont pas mariés, insérer le tag `#nm` (relation) ou `#eng` (fiançailles).
- Si les conjoints sont séparés, utiliser `#sep` ; s'ils sont divorcés, utiliser `-` suivi de la date de divorce (optionnelle).
- Si le patronyme d'un enfant est omis, celui du père est utilisé.
- Toutes les informations du tag `fam` doivent être sur une seule ligne ; les tags `wit`, `src` et `comm` suivent sur des lignes séparées, et sont optionnels.
- Si le sexe d'un enfant est inconnu, ne mettre ni `h` ni `f` après le tiret.

Si la famille n'a pas d'enfant, les tags `beg`/`end` peuvent être omis :
```
fam HEYDENREICH Gaspard +1719 TRESCH Rosine_Catherine
```

**Cas n° 2 — un ou les deux conjoints ne sont pas déjà répertoriés comme enfant d'une famille :** on saisit directement leurs informations personnelles, comme pour un enfant.

```
fam Corno John 1935 #bp Soisy 1997 + Rempp Zabeth
```

S'il n'y a aucune information personnelle associée à une personne, indiquer `0` après son nom/prénom (utilisé comme date de naissance par défaut, et signale que cette personne n'est définie nulle part ailleurs). Si le conjoint est inconnu, indiquer deux points d'interrogation séparés par un espace :
```
fam Diemer Patrick 0 + Heidenreich Sylvie 0
fam Doe John 0 + ? ?
```

## Événements familiaux (`fevt`, spécifique à gwplus)

```
NomÉvénementFamilial [DateÉvénement] [#p LieuÉvénement] [#s SourceÉvénement]
[wit[ m| f|]: Personne]
[note Texte libre (pas de "_" !) jusqu'au prochain événement ou fin de bloc]
```

Codes disponibles :

| Code | Signification |
|---|---|
| `#marr` | Mariage |
| `#nmar` | Sans mariage |
| `#nmen` | Sans mention |
| `#enga` | Fiançailles |
| `#div` | Divorce |
| `#sep` | Séparation |
| `#anul` | Annulation |
| `#marb` | Bans de mariage |
| `#marc` | Contrat de mariage |
| `#marl` | Licence de mariage |
| `#resi` | Résidence |

Certaines informations peuvent apparaître à la fois dans le tag `fam` (ex. `#mp` = lieu de mariage) et dans un sous-bloc `fevt` (`#p` = lieu de l'événement) : en cas de fichier édité à la main, l'information du sous-bloc `fevt` prend le pas sur celle de la ligne `fam` (donc si le sous-bloc `fevt` est vide, l'information de la ligne `fam` sera perdue). Le `+` dans la ligne `fam` marque simplement le début d'une relation, pas nécessairement un mariage : les tags `#nm` (ligne `fam`) ou `#nmar` (sous-bloc `fevt`) indiquent une relation sans mariage ; en leur absence, un mariage est supposé. Le tag `#noment`/`#nmen` crée un événement de relation sans nom.

## Informations personnelles

Le minimum pour une information personnelle est le patronyme, le prénom, un numéro d'occurrence optionnel, et une date de naissance :
```
Corno Yann 0
```
Pour un enfant, la date de naissance n'est pas obligatoire. En résumé, elle n'est requise que si la personne est un parent, ou si elle a une date de décès (dans ce cas, si la date de naissance est inconnue, utiliser `0`).

Format complet d'une personne :

```
NomPatronyme Prénom[.Numéro]
  [(NomPublic)] [#nick Qualificatif]
  [{PrénomAlias}] [#salias PatronymeAlias] [#alias Alias]
  [[Titre (voir plus bas)]] [#apubl | #apriv]
  [#image CheminFichierImage]
  [#occu Occupation] [#src SourcePersonne]
DateNaissance [#bp LieuNaissance] [#bs SourceNaissance]
  [!DateBaptême] [#pp LieuBaptême] [#ps SourceBaptême]
[#od] [DateDécès] [#dp LieuDécès] [#ds SourceDécès]
  [#buri | #crem [DateInhumation]] [#rp LieuInhumation] [#rs SourceInhumation]
```

- Si l'on ne sait pas si une personne est encore vivante, utiliser `?` comme date de décès.
- Si la personne est manifestement décédée (par exemple née il y a plus de 150 ans), utiliser le tag `#od`.
- Si la personne est morte en bas âge, utiliser le tag `#mj`.
- `#apubl` et `#apriv` contrôlent l'accès (public ou privé — magiciens/amis) ; en leur absence, la règle liée aux titres de noblesse s'applique (voir [Signification des divers champs](../magicien/creation-champs.md#accès-confidentialité)).

Le type de décès peut être précisé par un préfixe :

| Type | Caractère | Exemple |
|---|---|---|
| Tué | `k` | `k10/5/1990` |
| Assassiné | `m` | `m10/5/1990` |
| Exécuté | `e` | `e10/5/1990` |
| Disparu | `s` | `s10/5/1990` |

## Titres

Bien qu'ils fassent partie des informations personnelles, les titres sont décrits ici séparément pour plus de clarté. Ils sont notés entre crochets `[ ]` (qui font ici partie intégrante de la syntaxe) ; plusieurs titres s'enchaînent avec une nouvelle paire de crochets chacun :

```
[NomTitre:Titre:LieuTitre:DateDébut:DateFin:Nième]
```

Chaque élément est séparé par `:` ; laisser un élément vide si l'information n'est pas disponible. Le titre principal est désigné par `*` comme `NomTitre`.

## Événements personnels (`pevt`, spécifique à gwplus)

```
pevt NomPatronyme Prénom[.Numéro]
  ÉvénementPersonnel (plusieurs possibles)
end pevt
```

Chaque événement suit la structure :
```
NomÉvénementPersonnel [Date] [#p Lieu] [#s Source]
  [wit [m|f| ]: Personne]
  [note Texte libre (pas de "_" !) jusqu'au prochain événement ou fin de bloc]
```

Codes disponibles (liste complète) :

`#birt` naissance · `#bapt` baptême · `#deat` décès · `#buri` inhumation · `#crem` crémation · `#acco` accomplissement · `#acqu` acquisition · `#adhe` adhésion · `#awar` décoration · `#bapl` baptême LDS · `#barm` bar-mitzvah · `#basm` bat-mitzvah · `#bles` bénédiction · `#cens` recensement · `#chgn` changement de nom · `#circ` circoncision · `#conf` confirmation · `#conl` confirmation LDS · `#degr` diplôme · `#demm` démobilisation militaire · `#dist` distinction · `#dotl` dotation LDS · `#educ` éducation · `#elec` élection · `#emig` émigration · `#endl` dotation · `#exco` excommunication · `#fcom` première communion · `#flkl` lien familial LDS · `#fune` funérailles · `#grad` remise de diplôme · `#hosp` hospitalisation · `#illn` maladie · `#immi` immigration · `#lpas` liste de passagers · `#mdis` distinction militaire · `#mobm` mobilisation militaire · `#mpro` promotion militaire · `#mser` service militaire · `#natu` naturalisation · `#occu` occupation · `#ordn` ordination · `#prop` propriété · `#resi` résidence · `#reti` retraite · `#slgc`/`#slgp`/`#slgs` scellement LDS (enfant/parent/conjoint) · `#vteb` vente de bien · `#will` testament

## Notes personnelles (`notes`)

```
notes NomPatronyme Prénom[.Numéro]
Texte libre (les balises HTML sont acceptées — voir l'option `-tags` de `gwd` et le fichier `tags.txt`)
end notes
```

## Relations (`rel`)

```
rel NomPatronyme Prénom[.Numéro]
- adop: PèreAdoptif + MèreAdoptive
- adop fath: PèreAdoptif
- adop moth: MèreAdoptive
- reco: PèreReconnaissant + MèreReconnaissante
- reco fath: PèreReconnaissant
- reco moth: MèreReconnaissante
- cand: PèreCandidatAdoption + MèreCandidateAdoption
- cand fath: PèreCandidatAdoption
- cand moth: MèreCandidateAdoption
- godp: Parrain + Marraine
- godp fath: Parrain
- godp moth: Marraine
- fost: PèreNourricier + MèreNourricière
- fost fath: PèreNourricier
- fost moth: MèreNourricière
```
(seul un sous-ensemble de ces marqueurs est utilisé en pratique sur une relation donnée)

## Note de base (`notes-db`)

C'est le texte affiché quand on clique sur le lien « Note de présentation » de la page d'accueil — un seul par base.

```
notes-db
  Texte libre commençant par deux espaces (HTML et syntaxe wiki acceptés)
end notes-db
```

## Pages étendues (`page-ext`)

Les pages étendues sont référencées depuis les notes via la syntaxe wiki `[[[NomDePage/texte affiché]]]` (voir [Le format wiki](format-wiki.md)) ; la même syntaxe peut être utilisée à l'intérieur d'une page étendue elle-même, pour créer des liens croisés entre pages.

```
# page étendue "NomPage" utilisée par :
#  - person "PrénomPersonne[.Numéro] NomPersonne"   # une ligne par utilisation (deux espaces avant le tiret)
#  - extended page "NomPage"                          # ou par une autre page étendue
page-ext NomPage
  Texte libre, deux espaces en début de chaque ligne.
  Peut contenir des formats HTML et wiki.
end page-ext
```

## Note de magicien (`wizard-note`)

```
wizard-note NomMagicien
  horodatage    # date de création de la page, en secondes depuis le 1er janvier 1970 (epoch Unix)
  Texte libre au format wiki décrivant le magicien et son activité.
  Chaque ligne commence par deux espaces.
end wizard-note
```

Chaque magicien peut disposer d'une page décrivant son activité et son parcours, qu'il peut éditer lui-même. Si le paramètre `authorized_wizards_notes` du fichier `.gwf` (voir [Le fichier .gwf](../geneweb/base-gwf.md)) est positionné à `yes`, la page d'accueil propose un lien vers une page listant toutes les notes de magicien actives, triées par ordre alphabétique.

## Format des dates

Les dates suivent le standard européen `jj/mm/aaaa`, `aaaa`, ou `mm/aaaa`. Une date obligatoire mais inconnue se note `0`.

Des préfixes permettent de préciser le degré de certitude :

| Type | Caractère | Exemple |
|---|---|---|
| Environ | `~` | `~10/5/1990` |
| Peut-être | `?` | `?10/5/1990` |
| Avant | `<` | `<10/5/1990` |
| Après | `>` | `>10/5/1990` |
| Ou | `\|` | `10/5/1990\|1991` |
| Entre | `..` | `10/5/1990..1991` |

Le calendrier est grégorien par défaut. Pour le calendrier julien, ajouter un `J` en fin de date ; `F` pour le calendrier républicain ; `H` pour le calendrier hébraïque. Exemple : `10/9/5750H`.

## Chaînes de caractères

Les espaces à l'intérieur d'une chaîne sont remplacés par un underscore `_` (sauf dans les notes et certains commentaires) : `Marie_Julienne`. Les majuscules ASCII suivent la norme ISO-8859-1 (sauf mention `[encoding: utf-8]` en tête de fichier).

## Référencer une personne plusieurs fois

Une même personne peut être référencée à plusieurs endroits d'un fichier `.gw` : comme parent, comme enfant, ou dans une relation (témoin, parrain...). Le patronyme, le prénom et le numéro d'occurrence (si nécessaire) doivent correspondre exactement pour que GeneWeb établisse le rapprochement.

## Utilitaires associés

Voir [Import / export : gwu, gwb2ged, gwc, ged2gwb](../geneweb/gestion-import-export.md) pour la compilation et décompilation d'un fichier `.gw`.
