# Maintenance : update_nldb, consang, connex, cache_files

> Source : page [man/fr](https://geneweb.tuxfamily.org/wiki/man/fr) du wiki officiel.

## update_nldb

Crée (ou met à jour) l'index des pages liées présentes dans les notes via la syntaxe `[[[nom_page/texte affiché]]]` (voir [Le format wiki](../divers/format-wiki.md)). C'est cet index qui permet à une fiche individuelle d'afficher un lien « pages liées » vers toutes les notes et pages étendues qui la mentionnent.

Usage :
```bash
./update_nldb <nom_de_base>
```

La liste des pages liées d'une base peut aussi être obtenue directement via la commande d'URL `m=MISC_NOTES`.

## consang

Calcule le taux de consanguinité de chaque individu d'une base, et améliore accessoirement le temps d'accès à la base au passage.

> Source : page [consang](https://geneweb.tuxfamily.org/wiki/consang) du wiki officiel.

Usage :
```bash
./consang -i [options] <nom_de_base>
```

L'option **`-i`** (reconstruction des index) est **obligatoire** : sans elle, les index ne sont pas recalculés et la base ne fonctionnera pas correctement par la suite. *(Avertissement du wiki lui-même : cette option ne serait plus supportée depuis GeneWeb 7.0.0 — à vérifier sur `master` actuel avant de s'y fier.)*

| Option | Effet |
|---|---|
| `-q` | Mode silencieux |
| `-scratch` | Recalcul intégral, en repartant de zéro |
| `-mem` | Économise la mémoire, au prix d'une exécution plus lente lors de la réécriture de la base |
| `-nolock` | Ne verrouille pas la base |

Le temps d'exécution dépend de la taille de la base (de quelques secondes à quelques minutes), avec une barre de progression. La base reste consultable pendant le calcul, mais **toute mise à jour est bloquée** — une tentative de modification depuis une autre fenêtre affiche un message invitant à réessayer plus tard. Le traitement peut être interrompu à tout moment par `Ctrl+C` : il reprendra alors où il s'était arrêté au prochain lancement, sauf si des familles ont été modifiées entre-temps (dans ce cas, il repart de zéro).

Il est recommandé de relancer `consang` de temps à autre après un usage intensif de mise à jour — voir [Nettoyage](cycle-vie-base.md#nettoyage-clean) pour la procédure de nettoyage complète, qui s'appuie sur cet outil.

L'affichage du taux de consanguinité (sous forme de pourcentage) est contrôlé par la variable `show_consang` du [fichier .gwf](base-gwf.md).

## connex

Fournit des informations sur les composantes connexes d'une base (regroupements d'individus reliés entre eux), et permet leur suppression — utile pour repérer et éliminer des branches isolées. Disponible uniquement en ligne de commande.

> **Disponibilité incertaine** : d'après le wiki officiel lui-même, cet outil « peut ou non être inclus dans votre distribution » — à vérifier sur votre installation avant de s'y fier.

<!-- TODO : options précises et exemple de commande, non trouvés dans les sources consultées jusqu'ici. -->

## cache_files

<!-- TODO : cet exécutable n'a pas été confirmé dans les listes de fichiers du dossier gw/ consultées jusqu'ici (voir Binaires et sources) — vérifier s'il s'agit toujours d'un outil distinct sur master actuel, ou d'un mécanisme de cache intégré à gwd sans exécutable séparé. -->
