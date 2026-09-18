# Installation

Cette section décrit les différentes méthodes pour installer et lancer un serveur GeneWeb à partir de la branche `master` du [dépôt officiel](https://github.com/geneweb/geneweb).

> **Attention avant toute mise à jour**
> Il est fortement recommandé d'exporter vos bases au format `.gw` avant d'installer une nouvelle version de GeneWeb (voir [Le format .gw](../divers/format-gw.md)), le format interne des bases pouvant évoluer d'une version à l'autre.

## Vue d'ensemble des méthodes

| Méthode | Cas d'usage recommandé | Détails |
|---|---|---|
| **Binaires précompilés** | La plupart des utilisateurs — aucune dépendance à installer | [Binaires et sources](installation-binaires-sources.md) |
| **Compilation depuis les sources** | Développeurs, contributeurs, plateformes non couvertes par les binaires | [Binaires et sources](installation-binaires-sources.md) |
| **Docker** | Déploiement serveur, environnement isolé et reproductible | [Docker](installation-docker.md) |

Les instructions spécifiques à chaque système sont détaillées dans les pages suivantes :
- [Windows](installation-win.md)
- [Unix / Linux](installation-unix.md)
- [macOS](installation-mac.md)

## Essayer sans installer

Pour un premier essai sans rien installer localement :
- une [démo en ligne](https://demo.geneweb.tuxfamily.org/gw7/) de GeneWeb 7 est proposée par le projet ;
- GeneWeb peut aussi être exécuté directement dans un notebook [Google Colab](https://github.com/geneweb/geneweb/blob/master/geneweb_colab.ipynb) fourni dans le dépôt.
