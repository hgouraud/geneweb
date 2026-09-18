# Outils : dictionnaires, correction orthographique

La correction orthographique est citée comme une fonctionnalité native de GeneWeb dans la présentation générale du projet, au même titre que le calcul de parenté/consanguinité ou la gestion des titres de noblesse. Le détail concret de son fonctionnement dans l'interface reste à documenter précisément.

> **Ne pas confondre avec les outils Geneanet.** Geneanet (qui utilise GeneWeb comme moteur) propose ses propres « dictionnaires de saisie » pour les métiers et les lieux, ainsi qu'une auto-complétion — mais ce sont des fonctionnalités ajoutées par Geneanet à sa propre interface de saisie (dite « saisie standard »), distinctes de l'interface native de `geneweb/master`. Cette page doit décrire le comportement du logiciel `geneweb/master` lui-même, pas les outils spécifiques de Geneanet.

> **Piste de clarification trouvée depuis** : le sommaire de la page [manual/fr](https://geneweb.tuxfamily.org/wiki/manual/fr) du wiki officiel liste, parmi les chapitres du manuel, « Modifier le dictionnaire des textes GeneWeb » juste avant les chapitres sur le fichier `.gwf` et les mots de passe. Cet intitulé pourrait en fait désigner le **lexique** de l'interface (voir [Internationalisation, lexiques](../divers/i18n-lexiques.md)) plutôt qu'une fonctionnalité de correction orthographique distincte — à vérifier avant de conclure que cette page documente un sujet réellement séparé.

<!--
TODO, en vérifiant directement sur une instance geneweb/master locale :
- La correction orthographique porte-t-elle sur les noms de lieux, les prénoms, les professions, ou autre chose ? Ou bien ce chapitre du manuel officiel renvoie-t-il simplement vers le lexique (voir remarque ci-dessus) ?
- Existe-t-il un dictionnaire éditable par le magicien de la base, distinct du lexique, ou est-ce un mécanisme figé ?
- Y a-t-il une auto-complétion native (sans dépendre d'un module tiers) sur les champs lieu/profession dans les formulaires de mise à jour ?
- Si un mécanisme d'autocomplétion existe côté serveur (WebSocket JSON-RPC), préciser s'il s'agit d'une fonctionnalité de base ou d'un ajout spécifique à un fork/plugin — ne documenter ici que ce qui est présent nativement sur master.
-->
