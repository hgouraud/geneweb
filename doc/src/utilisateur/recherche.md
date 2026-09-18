# Recherche

## Les cinq lignes de saisie de la page d'accueil

Au-delà des boutons cliquables (dont la fonction est généralement explicite), la page d'accueil d'une base propose cinq lignes de saisie pour la recherche. La première accepte, au choix : un numéro Sosa (voir [Navigation](navigation.md#numérotation-sosa)), un nom public, un alias, ou un prénom et nom complets — ce qui suffit dans la grande majorité des cas.

> Source : page [Manual](https://geneweb.tuxfamily.org/wiki/manual) du wiki officiel. <!-- TODO : détailler les quatre autres lignes de saisie une fois la page consultée intégralement. -->

## Recherche simple depuis la page d'accueil

La page d'accueil d'une base propose un champ de recherche par nom. La recherche fonctionne par la « clé » de la personne — prénom et patronyme — sans tenir compte des accents ni de la casse : chercher « therese dupont » retrouve aussi bien « Thérèse Dupont ».

Sur un patronyme composé ou à particule, la recherche fonctionne par préfixe : si la base contient à la fois des « Gautier » et des « Gautier Sauvagnac », chercher « gautier » ne retrouve que les premiers (correspondance exacte), tandis que chercher « gautiera » ou « gautier de » propose un choix entre les deux patronymes.

> Source : page [search](https://geneweb.tuxfamily.org/wiki/search) du wiki officiel.

Quand plusieurs personnes partagent le même prénom et le même patronyme, GeneWeb propose une liste de choix plutôt que d'ouvrir directement une fiche. Il est possible de cibler directement une personne précise en insérant son numéro d'occurrence entre un point et le prénom et le nom :

```
Jean .4 Dupont
```

retrouve directement la personne portant le numéro d'occurrence 4 (voir [Création d'individu / famille](../magicien/creation.md#la-clé-dune-personne-prénom-patronyme-numéro) pour la notion de clé et de numéro d'occurrence).

## Recherche par qualificatif

Une personne dotée d'un [qualificatif](../magicien/creation-champs.md#nom-public-et-qualificatif) peut être retrouvée par la combinaison « prénom qualificatif » plutôt que « prénom patronyme ». Ainsi, *Louis VI* (dont le nom public est *Louis VI*) reste accessible via « Louis le Gros », « Louis VI le Gros », ou encore par son nom public seul. Le qualificatif n'est pas traité comme un patronyme dans la recherche : « le Gros » ne fonctionne pas comme un nom de famille à part entière.

> Source : page [update](https://geneweb.tuxfamily.org/wiki/update) du wiki officiel.

## Recherche avancée

<!--
TODO :
- Recherche par lieu (naissance, décès, mariage) : formulaire dédié et opérateurs disponibles
- Recherche par plage de dates ou par événement
- Recherche approximative / phonétique sur les noms
- Recherche par occupation, par titre de noblesse
-->

## Recherche des personnes de même prénom/nom dans toute la base

L'interface permet de lister toutes les personnes portant un prénom ou un patronyme donné (utile notamment pour repérer des doublons avant une fusion — voir [Suppression, fusion](../magicien/suppression-fusion.md)).

<!-- TODO : préciser le point d'entrée exact dans l'interface (menu, lien direct) pour ce type de listage -->

## Astuce pour l'administrateur de base

En tant que magicien, une astuce de recherche par clé permet de retrouver rapidement une personne précise sans naviguer par les liens familiaux, ce qui est notamment utile lors de la préparation d'une fusion de doublons.
