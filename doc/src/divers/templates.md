# Templates

GeneWeb génère ses pages HTML (et d'autres formats, comme du LaTeX pour des outils tiers) à partir de fichiers de template situés dans le dossier `etc/` de la distribution (par exemple `perso.txt` pour la fiche individu). Ces fichiers combinent du HTML classique avec un petit langage de programmation propre à GeneWeb, permettant de conditionner l'affichage selon le contenu de la base.

> Référence complète : la page [Expert](https://geneweb.tuxfamily.org/wiki/Expert) du wiki du projet documente plusieurs centaines de commandes de template. Cette page-ci n'en donne qu'une introduction ; consultez le wiki (ou, en dernier recours, le code source `templ.ml`/`perso.ml`) pour la liste exhaustive.

## Structures de contrôle de base

Le langage de template propose notamment :

- **Condition** : `%if;(condition) ... %elseif;(...) ... %else; ... %end;`
- **Définition de macro locale** : `%define;nom(paramètres) ... %end;`, appelée ensuite avec `%apply;nom(valeurs)`
- **Liaison de variable locale** : `%let;nom;valeur%in; ...`
- **Boucle sur une liste** : `%foreach; ... %end;`
- **Boucle bornée** : `%for;compteur;début;fin; ... %end;`
- **Espacement / mise en forme** : `%nl;` (retour à la ligne), `%nn;` (pas de retour à la ligne), `%sp;` (espace), `%sq;` (guillemet)

Les conditions et expressions supportent les opérateurs arithmétiques et logiques usuels (`+ - * / %`, comparaisons, `and` / `or` / `not`, `is_substr`, `in`).

> **Avertissement du projet lui-même** : toutes les commandes ne sont pas disponibles sur toutes les pages. Il n'existe pas de table exhaustive fiable de "quelle commande est disponible où" — l'exploration par essai/erreur, ou la lecture du code source, reste souvent nécessaire.

## Exemple illustratif

Extrait simplifié inspiré d'un test de condition sur le sexe d'un individu, tel qu'on peut le rencontrer dans `perso.txt` :

```
%if;(sex = 0 or sex = 2)
  Affichage adapté au cas "homme ou sexe inconnu"
%else;
  Affichage adapté au cas "femme"
%end;
```

## Variables de configuration (`bvar`)

Toute variable ajoutée dans le fichier `.gwf` d'une base (voir [Le fichier .gwf](../geneweb/base-gwf.md)) devient accessible dans les templates via le préfixe `bvar.` :

```
# dans le .gwf :
new_var=some-text

# dans un template :
%if;(bvar.new_var = "some-text")
  ... action spécifique ...
%end;
```

De nombreuses options de comportement du serveur passent par ce mécanisme plutôt que par une option `gwd` dédiée — la page Expert du wiki en liste un grand nombre (affichage de la consanguinité, gestion du forum, chemins d'images personnalisés, etc.).

## Variables d'environnement (`evar`)

Les paramètres présents dans l'URL de la requête sont accessibles dans les templates via `%evar.nom;` (ou la forme courte `%e.nom;` depuis la version 7.1). Par exemple, avec une URL contenant `?lang=fr&p=jean&n=dupont`, un template peut tester la langue demandée ou récupérer le prénom passé en paramètre.

## Variables personnelles dans les notes

Une variable définie dans le `.gwf` sous la forme `var_xxx=valeur` peut être insérée dans une note ou un template via la macro `%vxxx;`. C'est notamment utilisé pour factoriser la signature d'une source répétée dans plusieurs notes (nom complet, éventuellement lien vers une page).

## Personnaliser l'apparence

Où placer des templates personnalisés, comment en sélectionner un alternatif via le `.gwf`, et la personnalisation par CSS/en-tête/pied de page plutôt que par template : voir [Personnalisation (CSS, en-tête, pied de page)](personnalisation.md), qui regroupe ces sujets transverses à l'apparence du site.

## Sortie non-HTML

Le résultat produit par un template n'est généralement pas du HTML : il peut s'agir d'un autre format, produit par un mécanisme de template distinct appelé **Templm**. Certaines pages restent néanmoins figées, produites directement par le code source OCaml de `gwd` lui-même et non personnalisables via un template.

## Système de modules (v7)

Depuis la version 7, la fiche individuelle peut être personnalisée par un système de modules plutôt qu'en réécrivant un template entier : chaque section de la fiche est associée à une lettre (de `a` à `z`) dans le fichier `.gwf`, sous la forme `perso_module_<lettre>=<nom_module>`. Exemple extrait du fichier de référence `a.gwf` :

```
perso_module_i=individu
perso_module_p=parents
perso_module_f=fratrie
```

## Recherche des fichiers de template

Si un fichier de template personnalisé n'est pas trouvé pour une base, GeneWeb parcourt successivement les templates listés dans la variable `template` du `.gwf` — il est donc possible de ne personnaliser qu'un seul fichier et de laisser GeneWeb retrouver les autres dans les dossiers par défaut.

## Macros

Les fichiers de GeneWeb ne sont pas du HTML pur : ils contiennent des variables, historiquement appelées **macros**, sous forme de séquences commençant par `%` suivies d'une lettre (par exemple `%s` ou `%x`). Elles sont remplacées automatiquement par des valeurs qui dépendent du contexte : nom de la base, langue courante, nombre de personnes dans la base, etc.

> Ne pas confondre avec les variables entre crochets `[texte à traduire]`, qui sont traduites par GeneWeb à partir du lexique (voir [Internationalisation, lexiques](i18n-lexiques.md)) — mécanisme différent des macros `%`.

Un exemple notable : la macro `%s` permet de construire des URL portables, qui fonctionnent quel que soit le serveur (local ou distant) sur lequel la base est déplacée — utile en particulier pour les liens vers des images (voir [Portraits, blasons, images, carrousel](../magicien/images-carrousel.md)).

> **Deux contextes d'utilisation, très différents** : dans un fichier appelé par la commande `m=SRC` (voir [Commandes d'URL](#commandes-durl-m) ci-dessous), **toutes** les macros sont actives. Mais dans une note (personnelle, liée à un événement, ou page liée — voir [Notes diverses](../magicien/creation-notes.md)), **seules deux macros fonctionnent : `%s` et une variante de `%v`**. Dans ce second contexte, `%vtexte;` est remplacée par le contenu de la variable `bvar.var_texte` (voir aussi [Variables personnelles dans les notes](#variables-personnelles-dans-les-notes) ci-dessous), avec en prime une expansion des variables d'environnement système `${VARIABLE}` si `expand_env=yes` est présent dans le `.gwf`. Toute autre macro placée dans une note (`%b`, `%n`, `%f`...) ne sera tout simplement pas interprétée — c'est une confusion fréquente, y compris signalée par des utilisateurs sur le dépôt GitHub du projet.

Un tableau récapitulatif de toutes les macros disponibles dans le contexte des commandes `m=SRC` peut être généré automatiquement en insérant un petit bout de code HTML/GeneWeb dans la note d'un individu de la base (voir la page [Macros](https://geneweb.tuxfamily.org/wiki/macros/fr) du wiki officiel pour le code exact et la table de référence [Macro-table](https://geneweb.tuxfamily.org/wiki/Macro-table/fr)).

> **Emplacement du fichier inclus par `%r`** : le fichier `nom.txt` inclus doit se trouver au même endroit que le fichier qui l'invoque — par exemple dans `gw/etc/` s'il est associé à l'affichage standard de `gwd` (comme `perso.txt`), ou dans `bases/src/<base>/` s'il est invoqué depuis un fichier lui-même appelé via `m=SRC;v=fichier`.

## Commandes d'URL (`m=...`)

Les actions de GeneWeb (affichage d'une fiche, ajout, modification, fusion, etc.) sont déclenchées par un paramètre `m=` dans l'URL, par exemple `m=MOD_IND` pour modifier un individu. La liste de ces commandes est également documentée sur la page Expert du wiki ; leur usage direct (construction manuelle d'URL) est surtout utile pour créer des liens personnalisés depuis une note ou un template.

Une requête complète prend la forme `http://serveur:port/base?requête`, où `requête` est la somme des paramètres ajoutés après le `?`. Le séparateur entre paramètres successifs peut être indifféremment `;` ou `&` — les deux fonctionnent, y compris mélangés dans la même URL.

## Autres paramètres d'URL utiles

| Paramètre | Effet |
|---|---|
| `dag=on` | Sur un arbre ascendant/descendant, fusionne les ancêtres communs (implexes) — voir [Affichage](../utilisateur/affichage.md#représentation-en-graphe-dagsvg) |
| `opt=from` | Indique de quel fichier source provient un individu au sein d'une base fusionnée (visible aux magiciens uniquement) — voir [Cycle de vie d'une base](../geneweb/cycle-vie-base.md) |
| `opt=misc` | Affiche en fin de page toutes les combinaisons prénom/nom/alias valides pour désigner la personne |
| `cgl=on` | « Cancel GeneWeb links » : désactive les liens hypertexte GeneWeb sur la page, utile pour l'impression ou un copier-coller vers un autre document |
