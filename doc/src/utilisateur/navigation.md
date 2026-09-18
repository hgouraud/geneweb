# Navigation

## Navigation entre fiches

Depuis une fiche personnelle, des liens permettent de circuler dans la base :
- vers les parents (père, mère)
- vers le ou les conjoints, et pour chacun, vers la famille correspondante
- vers chaque enfant
- vers les frères et sœurs

## Confidentialité rencontrée en navigation

Selon le niveau d'accès (visiteur, ami, magicien — voir [Sécurité et contrôle d'accès](../divers/securite.md)), certaines informations peuvent être masquées pour les personnes considérées comme potentiellement vivantes (dates de naissance, de mariage, etc.), sauf si la personne est marquée comme publique par son propriétaire de base (par exemple en raison d'un titre de noblesse — voir [Signification des divers champs](../magicien/creation-champs.md#accès-confidentialité)).

## Numérotation Sosa

GeneWeb peut numéroter les ascendants d'une personne de référence selon la numérotation Sosa-Stradonitz (la personne de référence porte le numéro 1, son père le numéro 2, sa mère le numéro 3, et ainsi de suite en doublant à chaque génération). Cette numérotation permet de se repérer rapidement dans un grand nombre de générations ascendantes et de comparer facilement deux ascendances.

<!-- TODO : préciser comment définir/changer la personne de référence Sosa depuis l'interface -->

## Historique et signets

<!--
TODO :
- Existe-t-il un historique de navigation propre à GeneWeb (au-delà de celui du navigateur) ?
- Existe-t-il un mécanisme de signets/favoris ?
-->

## Liens permanents (permalinks)

<!-- TODO : décrire le système de permalien une fois vérifié en pratique (URL stable pointant vers un individu indépendamment des renumérotations internes de la base) -->

## Construction manuelle d'URL

Il est possible de construire directement une URL pointant vers une action ou une vue précise (par exemple afficher un arbre ascendant sur un nombre de générations donné), en utilisant les paramètres de commande décrits dans [Templates](../divers/templates.md#commandes-durl-m). Utile notamment pour créer des liens personnalisés depuis une note.
