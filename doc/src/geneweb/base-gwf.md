# Le fichier .gwf

Le fichier de configuration d'une base GeneWeb est un simple fichier texte contenant des variables (une par ligne, sous la forme `clé=valeur`). Il doit porter le même nom que la base, avec l'extension `.gwf`, et se trouver à la racine du dossier contenant la base (donc à côté du dossier `nombase.gwb`, pas dedans).

> Source : page [Configuration](https://geneweb.tuxfamily.org/wiki/configuration/fr) du wiki officiel du projet.

## Fichier de référence

Le dépôt fournit un fichier d'exemple, [`etc/a.gwf`](https://github.com/geneweb/geneweb/blob/master/etc/a.gwf), qui contient une configuration-type listant toutes les options possibles (en anglais, mais c'est la référence la plus à jour). Pour configurer une base nommée `dupont`, on copie ce fichier à côté de `dupont.gwb` sous le nom `dupont.gwf`, puis on édite les options voulues.

Si la base est créée via [`gwsetup`](../geneweb/gestion.md), une configuration type plus restreinte est proposée automatiquement :

```
use_cdn=yes
access_by_key=yes
disable_forum=yes
hide_private_names=no
use_restrict=no
show_consang=yes
display_sosa=yes
place_surname_link_to_ind=yes
max_anc_level=8
max_anc_tree=7
max_desc_level=12
max_desc_tree=4
max_cousins=2000
max_cousins_level=5
latest_event=20
template=*
long_date=no
counter=no
full_siblings=yes
hide_advanced_request=no
```

L'interface `gwsetup` permet de modifier certaines de ces variables directement depuis le navigateur, sans éditer le fichier à la main. Le template [`templm`](https://geneweb.tuxfamily.org/wiki/templm/fr) propose plusieurs paramètres supplémentaires spécifiques.

## Principales variables

- **`body_prop`** (remplacé par **`css_prop`** depuis GeneWeb 6.00) : configure les couleurs de fond, motif de fond, couleur du texte et des liens. La syntaxe est celle acceptée dans l'attribut du tag HTML `<body>`, avant le `>`. S'applique à toutes les pages de la base, pas seulement à l'accueil. Exemple :
  ```
  body_prop=background=monfond.jpg vlink=yellow
  ```
- **`friend_passwd`** : mot de passe « ami », limite globalement la visibilité des données sur les personnes potentiellement vivantes (voir `private_years` dans `a.gwf`, valeur par défaut 150 ans). **`friend_passwd_file`** permet une gestion par visiteur : si renseigné, seules les personnes ayant saisi le bon mot de passe depuis la page d'accueil ont accès à ces données.
- **`wizard_passwd`** : mot de passe « magicien », limite globalement le droit de faire des modifications. **`wizard_passwd_file`** permet là aussi une gestion par utilisateur.
- **`wizard_just_friend`** : mettre à `yes` interdit temporairement toute mise à jour de la base (utile pendant un [nettoyage](https://geneweb.tuxfamily.org/wiki/clean/fr) de base, par exemple), `no` pour rétablir l'autorisation normale.
- **`default_lang`** : langue par défaut de la page d'accueil (sans cette variable, c'est le français). Valeurs possibles : `af`, `bg`, `br`, `cs`, `ca`, `de`, `da`, `en`, `eo`, `es`, `et`, `fi`, `fr`, `he`, `is`, `it`, `lv`, `nl`, `no`, `pl`, `pt`, `pt-br`, `ro`, `ru`, `sl`, `sv`, `zh`. Quelle que soit la langue par défaut, chaque visiteur peut en choisir une autre depuis le drapeau en haut de la page d'accueil.
- **`can_send_image`** : autorise (`yes`) ou non (`no`, valeur par défaut) les magiciens à envoyer et supprimer des images lors des mises à jour (voir [Portraits, blasons, images, carrousel](../magicien/images-carrousel.md)). Attention : les images envoyées sont stockées sur le serveur et peuvent consommer beaucoup d'espace disque.
- **`renamed`** : signale qu'une base a été renommée, pour rediriger automatiquement les anciens signets/liens vers le nouveau nom plutôt que d'afficher une erreur.
- **`history`** : active l'archivage d'un historique des modifications, stocké dans `nombase.gwb/history` ; un lien apparaît alors sur la page d'accueil.
- **`history_diff`** : stocke un historique spécifique à chaque individu.
- **`authorized_wizards_notes`** : mettre à `yes` pour proposer, sur la page d'accueil, un lien vers la liste de toutes les [notes de magicien](../divers/format-gw.md#note-de-magicien-wizard-note) actives de la base, triées par ordre alphabétique.

<!--
TODO : cette liste n'est qu'un sous-ensemble commenté par le wiki. Pour une liste exhaustive et à jour, se référer directement au fichier etc/a.gwf du dépôt (en anglais) et la reprendre ici de façon structurée, en particulier les variables liées à l'affichage (max_anc_level, max_desc_level, show_consang, display_sosa...) déjà mentionnées ailleurs dans ce manuel via les bvar de template (voir Templates > Variables de configuration).
-->

## Voir aussi

- [Templates > Variables de configuration (`bvar`)](../divers/templates.md#variables-de-configuration-bvar) : toute variable de ce fichier devient accessible dans les templates via le préfixe `bvar.`.
- [Sécurité et contrôle d'accès](../divers/securite.md) : pour la gestion des mots de passe magicien/ami en ligne de commande plutôt que dans ce fichier.
