# Internationalisation, lexiques

GeneWeb est traduit dans de nombreuses langues (français, anglais, allemand, espagnol, italien, et bien d'autres). Les textes de l'interface ne sont pas codés en dur : ils sont chargés depuis des fichiers de lexique.

> Sources : pages [Lexicon](https://geneweb.tuxfamily.org/wiki/lexicon) et [Declension](https://geneweb.tuxfamily.org/wiki/declension) du wiki officiel.

## Sélection de la langue

Plusieurs mécanismes déterminent la langue affichée :

- **Langue par défaut du serveur** : option `gwd -lang <code>` (par défaut `en`)
- **Langue du navigateur** : option `gwd -blang`, qui utilise la langue déclarée par le navigateur du visiteur si elle est disponible
- **Langue explicite dans l'URL** : paramètre `?lang=<code>` de la requête

<!-- TODO : confirmer l'ordre de priorité exact sur master actuel — un comportement surprenant avec -blang a été signalé (issue GitHub #1678, la langue ne changeait pas correctement au clic sur un lien de langue) ; à vérifier avant de documenter cet ordre comme garanti. -->

## Structure des fichiers de lexique

Le lexique est un fichier texte qui définit la traduction, dans plusieurs langues, des termes utilisés par l'interface. Chaque entrée suit ce schéma :

```
terme à traduire
l1: traduction dans la langue 1
l2: traduction dans la langue 2
autre terme à traduire
l1: ...
```

Depuis la version 7, le fichier principal s'appelle `lex_utf8.txt` et doit être encodé en **UTF-8** (les versions 5 et antérieures utilisaient un fichier `lexicon.txt` encodé en Latin-1 — attention si vous éditez un vieux fichier). Un fichier additionnel `lex_additionnal.txt`, placé dans le même dossier `lang/`, permet d'ajouter des entrées sans modifier le fichier principal.

Pour étendre le lexique avec ses propres termes (dans le cadre d'un template personnalisé, par exemple), on fournit un fichier supplémentaire suivant la même syntaxe, via l'option `gwd -add_lexicon <fichier>`. Ce fichier doit être placé dans l'un des dossiers `lang` reconnus (`bases/lang` ou `gw/lang`).

> **À partir de la version 7.1** : réorganisation des fichiers de ressources (templates et lexiques). Les lexiques sont désormais placés dans `plugins/v7/assets/lex`, et tous les fichiers de ce dossier sont automatiquement ajoutés au lexique.

## Ajouter ou compléter une traduction

La langue de l'interface se choisit depuis la page d'accueil, ou en ajoutant `lang=xx` dans l'URL.

Pour contribuer une traduction :
- Un dossier du dépôt GitHub GeneWeb contient des fichiers `lex_ll.txt` (où `ll` est le code à deux lettres de la langue). Leur contenu est explicite ; une PR avec de nouvelles entrées (ou tout autre moyen de transmission) sera intégrée au dépôt principal.
- Un jeu de fichiers équivalent existe pour l'outil `gwsetup`.
- Si le fichier de votre langue n'existe pas encore dans ces dossiers, signalez-le par une issue GitHub.

> Seuls l'anglais et le français sont bien maintenus à ce jour ; toute contribution de traduction est bienvenue (voir la page [Contribute](https://geneweb.tuxfamily.org/wiki/Contribute) du wiki officiel pour la liste des contributeurs déjà impliqués par langue).

## Déclinaisons

Certaines langues déclinent certains mots (cas grammatical) selon le contexte de la phrase générée — une simple traduction mot à mot ne suffit pas. GeneWeb permet de saisir, pour un prénom ou un patronyme sujet à déclinaison, plusieurs formes en une seule fois.

Dans le lexique d'une langue à déclinaison, les cas sont désignés par des lettres entre deux `:` (par exemple en tchèque, `:a:` pour l'accusatif et `:g:` pour le génitif). Dans le champ prénom/patronyme du formulaire, on saisit la forme nominative suivie des autres formes nécessaires :

- Un suffixe commençant par `+` signifie « à partir de la forme nominative, ajouter » : `Adler:g:+a` signifie que le génitif d'« Adler » est « Adlera ».
- Un suffixe commençant par `-` signifie « à partir de la forme nominative, retrancher une lettre à la fin » (`--` pour deux lettres, etc.).
- Exemple combinant les deux : `Vladana:a:-u:g:-y`.

À l'affichage d'une fiche, c'est la forme nominative qui est utilisée par défaut ; les autres formes ne servent que dans les expressions complètes où la langue l'exige. Il n'existe pas de documentation exhaustive des cas spécifiques à chaque langue — le wiki renvoie vers des sites linguistiques généraux et les exemples déjà présents dans le lexique.

## Lien avec cette documentation

Le glossaire de ce manuel ([Glossaire](../glossaire.md)) devrait, à terme, rester cohérent avec la terminologie officielle utilisée dans le lexique français de l'interface — pour éviter qu'un même concept soit nommé différemment dans l'interface et dans le manuel.
