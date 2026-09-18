# Introduction

Ce manuel documente **GeneWeb** ([geneweb/master](https://github.com/geneweb/geneweb)), un logiciel libre de généalogie fonctionnant comme serveur web.

Il s'adresse à trois publics différents, correspondant aux trois grandes parties de ce manuel :

- **Administrateur du serveur et des bases** : installation du logiciel, lancement et configuration de `gwd`/`gwsetup`, création et paramétrisation des bases, outils en ligne de commande (`gwc`, `gwu`, `ged2gwb`...), cycle de vie d'une base (sauvegarde, nettoyage, fusion/division de bases entières). Public : administrateur système / hébergeur du serveur.
- **Contributeur au contenu de la base** : saisie, modification, suppression, fusion de personnes ou de familles au sein d'une base déjà installée — via l'interface web, avec les droits de magicien. Public : généalogiste alimentant le contenu d'une base, sans nécessairement en administrer l'infrastructure.
- **Utilisateur** (ami ou visiteur) : recherche, navigation, consultation des arbres et relations. Public : personne consultant une base, avec ou sans mot de passe d'accès étendu.

Une section **Divers** regroupe les sujets transverses (formats de fichiers, sécurité, personnalisation, internationalisation).

> **Périmètre de cette documentation**
> Ce manuel documente le comportement de la branche `master` du dépôt officiel [geneweb/geneweb](https://github.com/geneweb/geneweb). Les forks et versions dérivées peuvent présenter des différences non couvertes ici.

## Comment contribuer à cette documentation

<!-- TODO: expliquer le processus (PR sur doc/src/, build local avec mdbook serve, convention de nommage des fichiers) -->
