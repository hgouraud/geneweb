# Création d'individu / famille

## Les deux notions internes : personnes et familles

Une base GeneWeb repose sur deux types d'entités :
- **Une personne** porte les informations individuelles : prénom, patronyme, dates et lieux (naissance, baptême, décès), titres de noblesse, notes, etc.
- **Une famille** représente une union simple : un père, une mère, leurs enfants communs, ainsi que la date et le lieu du mariage (et du divorce, le cas échéant). Une personne mariée plusieurs fois appartient donc à plusieurs familles distinctes, une par union.

Toute modification de la base se traduit par la création, la modification ou la suppression de personnes et/ou de familles.

## La clé d'une personne : prénom, patronyme, numéro

Chaque personne est repérée de façon unique par la combinaison **prénom + patronyme + numéro d'occurrence**. Le numéro sert uniquement à distinguer deux personnes qui porteraient le même prénom et le même patronyme (par exemple deux "Jean Dupont" dans la même base) — il n'a aucune valeur en lui-même et n'a pas besoin de suivre un ordre particulier. Quand il n'est pas précisé, il vaut 0 par défaut.

Ce numéro n'apparaît que dans les formulaires de mise à jour, jamais dans les pages de consultation normales. Il ne doit pas être confondu avec le numéro d'un titre de noblesse (« Louis XIV » n'a pas nécessairement le numéro d'occurrence 14, même s'il est pratique de les faire correspondre quand c'est possible).

Deux messages d'erreur classiques découlent directement de cette notion de clé :
- **« Personne déjà existante »** : la combinaison prénom/patronyme/numéro que vous tentez de créer est déjà prise par quelqu'un d'autre dans la base.
- **« Personne inconnue »** : vous tentez de relier une personne (comme parent ou enfant) à une combinaison prénom/patronyme/numéro qui n'existe pas dans la base.

## Accéder aux formulaires de mise à jour

Deux points d'entrée principaux :
- Depuis la page d'accueil, le lien **Ajouter une famille** (utile pour démarrer une base vide).
- Depuis une fiche personnelle, le lien **Mise à jour**, qui propose un menu dont les options varient selon la situation familiale de la personne (mariée ou non, avec ou sans parents, avec ou sans image, etc.) — voir [Navigation et mise à jour](navigation-maj.md).

Dans tous les formulaires, **aucune modification n'est appliquée avant validation explicite** (bouton "Ok" en fin de formulaire) : on peut toujours se rétracter en abandonnant simplement la navigation.

## « Relier » ou « Créer » ?

Dans les champs désignant un parent ou un enfant, un choix apparaît entre deux modes :
- **Relier** : la personne existe déjà dans la base. La recherche se fait par clé (accents et majuscules ignorés) et le système vérifie la cohérence (sexe du parent, existence effective).
- **Créer** : la personne n'existe pas encore. Elle sera créée avec l'orthographe exacte saisie dans le formulaire — ici, accents et majuscules comptent, puisqu'ils définissent l'orthographe réelle de la personne créée.

## Pièges fréquents à éviter

- **Modifier le prénom ou le nom d'un enfant** : passez par **Modifier** la fiche de l'enfant lui-même, jamais par **Modifier/Famille**. Renommer un enfant depuis le formulaire famille casse le lien avec ses parents plutôt que de renommer la personne.
- **Ajouter un enfant à une famille existante** : passez par **Modifier/Famille** depuis la fiche d'un des parents. N'utilisez pas **Ajouter/Famille**, qui crée une toute nouvelle union (un nouveau conjoint), ni **Ajouter/Parents**, qui sert à remonter d'une génération.

Voir aussi [Signification des divers champs](creation-champs.md) pour le détail de chaque zone du formulaire, et [Suppression, fusion](suppression-fusion.md) pour la fusion de doublons.
