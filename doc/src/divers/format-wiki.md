# Le format wiki

Les notes (individuelles, familiales, ou notes générales de la base) utilisent une syntaxe de type wiki plutôt que du HTML brut — GeneWeb l'appelle *Wikitext*, une version simplifiée de la syntaxe MediaWiki. Une barre d'outils au-dessus des zones de texte permet d'insérer cette syntaxe sans avoir à la mémoriser.

> Source : page [Wikitext](https://geneweb.tuxfamily.org/wiki/wikitext) du wiki officiel du projet.

## Titre de page

Pour une page étendue (voir plus bas), le tag `TITLE=` doit être placé avant le texte, en tout début de page.

## Titres et sous-titres

```
= Titre =
== Sous-titre ==
```

À partir de quatre titres dans une même page, une table des matières est générée automatiquement (avec des options pour la masquer, la repositionner, ou en afficher une version courte).

Quand la modification d'une note est autorisée, GeneWeb ouvre la fenêtre d'édition section par section, avec trois flèches (gauche, haut, droite) en haut à gauche pour naviguer entre les sections.

## Mise en forme du texte

- **Gras et italique** : s'obtiennent avec des guillemets simples, comme sur MediaWiki — `'''''gras et italique'''''`.
- **Style de mise en valeur propre à GeneWeb** (gras vert par défaut) : accolades — `{texte}`.
- **Indentation** : deux-points en début de ligne, répétés selon le niveau souhaité (`:premier niveau`, `::deuxième niveau`).
- **Saut de ligne forcé** : une ligne vide, ou la balise HTML `<br />`.
- **Texte préformaté** : des lignes commençant par une espace sont affichées telles quelles, à condition qu'il y en ait au moins deux et qu'une ligne vide les entoure.

## Listes à puces

```
* un
* deux
* trois
** trois virgule un
** trois virgule deux
```

On peut combiner `*` et `:` pour gérer l'indentation dans une liste à puces sur plusieurs paragraphes.

## Liens internes

### Vers une page (étendue)

Le nom de la page entre **triples** crochets :
```
[[[NomDePage]]]
```
Pour afficher un texte différent du nom de la page, ajouter un `/` suivi du texte à afficher :
```
[[[NomDePage/texte affiché]]]
```
Si la page n'existe pas encore, le lien apparaît en rouge ; cliquer dessus ouvre une page vide à remplir. On peut organiser les pages en dossiers en préfixant le nom de fichier par un nom de dossier.

### Vers la page d'un magicien

```
[[w:NomMagicien]]
```
ou, avec un texte affiché différent :
```
[[w:NomMagicien/Nom complet affiché]]
```

### Vers un individu

Contrairement aux liens vers une page, un lien vers un individu utilise des **doubles** crochets, avec quatre parties séparées par `/` : prénom, patronyme, numéro d'occurrence, texte affiché.
```
[[prénom/patronyme/numéro/texte affiché]]
```
Exemple :
```
[[Maria/Baxter/0/Maria Baxter]]
```
Si le numéro et le texte affiché sont omis, GeneWeb affiche par défaut « Prénom Patronyme ».

GeneWeb maintient un index des notes et pages où un individu donné est mentionné, consultable depuis sa fiche via le lien **pages liées** (commande `m=LINKED;p=prénom;n=nom`). L'outil `update_nldb` (voir [Maintenance](../geneweb/gestion-maintenance.md)) met à jour cet index de références croisées.

## Lien retour depuis la fiche individuelle (pages étendues)

Quand une page contient des liens vers des individus, GeneWeb peut ajouter automatiquement un lien de retour vers cette page, à différents endroits de la fiche de la personne, selon un tag placé en tête de la page étendue :

| Tag | Emplacement du lien retour |
|---|---|
| `HEAD=` | sous le nom |
| `OCCU=` | sous l'occupation |
| `DEATH=` | à la fin de la date et du lieu de décès, entre parenthèses |
| `BIBLIO=` | après les sources, sous un sous-titre bibliographie |
| `BNOTE=` | au début des notes |
| `NOTE=` | à la fin des notes |

Dans le lien vers l'individu, le texte entre `;` et la fermeture des crochets (ou entre accolades `{texte}` s'il y en a) devient le texte affiché pour ce lien retour ; le caractère `*` y est remplacé par le contenu du tag correspondant. Si rien n'est mis après le `;`, aucun lien retour n'apparaît pour cet individu — utile par exemple pour garder, dans la page, un lien vers un magicien sans que cela génère un lien retour visible sur sa fiche.

Exemple (extrait) :
```
TITLE=Liste des anciens de telle promotion

[[Jean/Dupont;]] Responsable de cette page<br>
[[Nicolas/Martin/Martin, Nicolas;* 1942]], ingénieur, écrivain, 1942<br>
```

## Caractères spéciaux

Pour écrire littéralement un caractère qui a un sens syntaxique, le faire précéder de `%` :

| Saisie | Résultat |
|---|---|
| `%'` | `'` |
| `%{` / `%}` | `{` / `}` |
| `%[` / `%]` | `[` / `]` |
| `%%` | `%` |

## HTML et variables dans les notes

Les notes ne sont pas du texte purement passif : elles peuvent contenir des balises HTML ainsi que certaines macros interprétées par GeneWeb au moment de l'affichage. Voir [Templates](templates.md#macros) pour le mécanisme général des macros — mais dans le contexte précis d'une note, **seules deux macros sont actives : `%s` et une variante de `%v`** ; toute autre macro (`%b`, `%n`, `%f`...) y est simplement ignorée, contrairement à un fichier appelé par `m=SRC` où toutes les macros fonctionnent.

Les balises HTML autorisées dans les notes sont contrôlées par l'option `gwd -allowed_tags <fichier>`, qui pointe vers un fichier listant une balise autorisée par ligne (voir aussi [`tags.txt`](../geneweb/installation-binaires-sources.md#contenu-détaillé-du-dossier-gw)). <!-- TODO : lister le contenu par défaut de ce fichier sur master (balises autorisées telles que livrées avec la distribution). -->

> Source : page [tags](https://geneweb.tuxfamily.org/wiki/tags) du wiki officiel.

Une balise saisie dans une note mais absente de `tags.txt` n'est **pas supprimée** : elle est échappée et s'affiche littéralement dans le navigateur (par exemple `<funnytag>` saisi devient visible tel quel, plutôt que d'être interprété ou disparaître silencieusement). Une balise non-HTML de ce type peut porter du texte entre son crochet ouvrant et son crochet fermant — utile si la page produite par GeneWeb est ensuite retraitée par un programme externe plutôt que simplement affichée dans un navigateur.
