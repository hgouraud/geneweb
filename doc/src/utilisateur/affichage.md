# Affichage : pages, arbres, relations

## Types de pages

- **Fiche individu** : informations personnelles, liens familiaux, image le cas échéant.
- **Fiche famille** : informations sur l'union et liste des enfants.
- **Arbre ascendant / descendant** : représentation graphique sur plusieurs générations.
- **Page de relations** : chemin(s) de parenté entre deux individus de la base.

## Arbres ascendants et descendants

GeneWeb peut afficher un arbre sur un nombre de générations paramétrable, dans plusieurs styles (texte, tableau HTML, ou représentation graphique). Le nombre maximal de générations affichées peut être limité par la configuration de la base (voir les variables `max_anc_level` / `max_desc_level` mentionnées dans [Templates](../divers/templates.md#variables-de-configuration-bvar)).

GeneWeb 7.00 a introduit un **arbre ascendant compact**, réduisant la surface nécessaire pour afficher jusqu'à 5 générations d'ancêtres depuis un individu.

> **Précision** : une présentation *encore plus* compacte a été proposée sur le wiki officiel (page [Alternative ancestor chart](https://geneweb.tuxfamily.org/wiki/Alternative_ancestor_chart)), mais il s'agit d'une suggestion de contributeur non implémentée à ce jour — pas d'une fonctionnalité disponible dans GeneWeb. Seul l'arbre compact de la version 7.00 mentionné ci-dessus est confirmé comme existant réellement.

### Construire un arbre personnalisé par l'URL

> Source : page [trees](https://geneweb.tuxfamily.org/wiki/trees) du wiki officiel.

Au-delà des options du menu Ascendants/Descendants (listes ou arbres, avec ou sans conjoints, avec ou sans images), il est possible de construire un arbre sur mesure directement par les paramètres de l'URL :

- **`si=<numéro_sosa>;`** (répété, indices consécutifs à partir de 1) : inclut dans l'arbre l'ascendant portant ce numéro Sosa (voir [Navigation](navigation.md#numérotation-sosa)).
- **`im=off;`** : masque les images (affichées par défaut).
- **`sp=off;`** : masque les conjoints (affichés par défaut).
- **`i<n>=<index>;`** et **`t<n>=<texte>;`** (avec `n` un numéro de génération) : associe à la personne d'index `<index>` à cette génération un texte affiché en gras à côté de son nom — utile par exemple pour mettre en évidence un chemin de parenté particulier (parrainage, témoignage à un événement) au sein d'un arbre d'ascendance commune.

Le moteur de template `templm` (voir [Templates](../divers/templates.md)) propose des fonctions équivalentes sous les rubriques « Ascendants », « Frères et sœurs », « Descendants ».

> **Variante syntaxique rencontrée** : certains exemples de la documentation utilisent `image=on;`/`spouse=on;` plutôt que `im=off;`/`sp=off;` pour les mêmes réglages — les deux formes semblent coexister selon la version ou la page concernée, à vérifier au cas par cas. Un second gabarit d'arbre descendant existe également, sous la commande `m=D&t=TV` (à côté du gabarit classique `m=D&t=V`).

> **Limite connue** : la commande `m=RLM` (chemins de relation entre deux personnes) ne propose actuellement aucune option pour n'explorer que les chemins passant par des liens d'alliance (mariage) plutôt que par la filiation directe.

## Représentation en graphe (DAG/SVG)

Au-delà de l'arbre classique, GeneWeb peut représenter les relations familiales sous forme de graphe orienté (DAG), utile notamment pour visualiser correctement les cas de consanguinité ou de familles recomposées, où un simple arbre en couches ne suffit pas à représenter fidèlement tous les liens.

Sur un arbre ascendant, le paramètre d'URL `dag=on` fusionne visuellement les ancêtres communs (« implexes ») du graphe : il n'y a alors plus de chemins redondants affichés séparément pour un même ancêtre atteint par plusieurs branches.

<!-- TODO : décrire précisément l'accès à cette vue depuis l'interface utilisateur (bouton ou uniquement paramètre d'URL ?), et ses autres options d'affichage (par ex. le rendu des liens de conjoints en arc, distinct des liens de filiation) -->

## Calcul de parenté et de consanguinité

GeneWeb intègre un calculateur de parenté capable de déterminer, entre deux individus quelconques de la base, le ou les chemins de parenté qui les relient — y compris sur des bases comportant plusieurs millions d'individus. Le même mécanisme sert au calcul du taux de consanguinité d'une personne (probabilité que ses deux lignées parentales se recroisent).

<!-- TODO : décrire le point d'entrée dans l'interface pour lancer un calcul de relation entre deux personnes précises, et la lecture du résultat affiché -->

## Statistiques et widgets d'accueil

La page d'accueil d'une base peut afficher des éléments complémentaires : anniversaires du jour, naissances ou décès récents, pyramide des âges pour une année donnée, etc.

<!-- TODO : préciser lesquels de ces éléments sont activés par défaut vs configurables via le .gwf -->

## Personnalisation de l'affichage

Le contenu et la mise en page de ces différentes vues sont pilotés par les templates du serveur (voir [Templates](../divers/templates.md)) : ce sont eux qui déterminent, par exemple, quelles informations apparaissent sur la fiche individu et dans quel ordre.
