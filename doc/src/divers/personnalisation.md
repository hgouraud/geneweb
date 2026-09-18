# Personnalisation (CSS, en-tête, pied de page)

Au-delà des [templates](templates.md) qui définissent la structure et le contenu des pages, GeneWeb permet de personnaliser leur habillage visuel par des moyens plus simples, sans toucher à la logique des templates eux-mêmes.

> Sources : pages [CSS](https://geneweb.tuxfamily.org/wiki/CSS) et [UI](https://geneweb.tuxfamily.org/wiki/UI) du wiki officiel. Reste à consulter : [Header](https://geneweb.tuxfamily.org/wiki/header/fr).

## Feuilles de style CSS

La mise en page de GeneWeb repose sur CSS. La feuille de style par défaut est `etc/css.css`. Pour utiliser sa propre feuille de style, deux étapes :

1. Éditer le fichier `.gwf` de la base (voir [Le fichier .gwf](../geneweb/base-gwf.md)) pour y renseigner la variable `css` avec le nom de votre feuille de style.
2. Enregistrer le fichier `.css` correspondant dans le dossier `gw/css/`.

<!-- TODO : détail de la variable css_prop (couleurs/fond via l'attribut HTML body, déjà évoquée dans Le fichier .gwf sous son ancien nom body_prop) par opposition à css (feuille de style complète) -- clarifier la distinction exacte entre les deux mécanismes. -->

## En-tête et pied de page

Les fichiers `hed.txt` et `trl.txt`, à la racine d'une base (voir [Localisation d'une base](../geneweb/base-localisation.md)), permettent de personnaliser l'en-tête et le pied de page spécifiquement pour cette base.

<!-- TODO : variables disponibles dans ces fichiers, syntaxe exacte -->

## Autres réglages d'interface

Depuis la page d'accueil, le visiteur peut changer à tout moment :

- **Le template utilisé** : soit via un bouton visible (pas nécessairement affiché avant d'accéder à une fiche personnelle), soit directement dans l'URL avec `templ=nom_du_template;`. Ce choix reste actif pendant toute la navigation dans la base.
- **La langue de l'interface** : soit via un bouton (9 langues fréquentes proposées par défaut depuis GeneWeb 7.00 ; un jeu de drapeaux de pays dans les versions 5.02 et antérieures), soit directement dans l'URL avec `lang=xx;` (code ISO 639-1 à deux lettres).

Tous les textes de l'interface passant par la traduction (à quelques rares exceptions) sont regroupés dans le lexique — voir [Internationalisation, lexiques](i18n-lexiques.md).

## Voir aussi

Personnaliser [le lexique et les déclinaisons](i18n-lexiques.md) relève de la même logique générale de personnalisation, mais est traité dans son propre chapitre plutôt qu'ici.
