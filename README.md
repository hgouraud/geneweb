# Documentation GeneWeb (doc/)

Ce dossier contient le manuel utilisateur de GeneWeb, écrit en Markdown et
buildé avec [mdBook](https://rust-lang.github.io/mdBook/) en HTML statique.

## Build local

```bash
cargo install mdbook      # une seule fois
cd doc
mdbook build               # génère doc/book/ (HTML statique)
mdbook serve                # ou : serveur local avec rechargement automatique
```

Le résultat statique (`doc/book/`) peut être :
- publié sur GitHub Pages,
- copié dans le tarball de distribution (`make dist` ou équivalent),
- consulté directement en ouvrant `doc/book/index.html` dans un navigateur.

## Structure

```
geneweb-doc/
├── doc/
│   ├── book.toml           # configuration mdBook (+ préprocesseur gettext)
│   ├── po/                 # traductions (gettext) — voir section Traduction
│   └── src/
│       ├── SUMMARY.md       # table des matières (structure du manuel)
│       ├── README.md        # page d'introduction
│       ├── demarrage-rapide.md
│       ├── glossaire.md
│       ├── geneweb/         # administration serveur (installation, bases, gwsetup)
│       ├── magicien/        # administration de base (fiches, images, fusion...)
│       ├── utilisateur/     # consultation (recherche, navigation, affichage)
│       └── divers/          # sujets transverses (.gw, wiki, sécurité, templates, i18n)
├── scripts/i18n.sh          # aide-mémoire des commandes de traduction
├── TRANSLATION-GLOSSARY.md  # glossaire FR→EN de la terminologie GeneWeb
└── .github/workflows/docs.yml  # build + publication fr + traductions
```

## Traduction (anglais et autres langues)

Le français est la langue source, écrite directement dans `doc/src/*.md`.
Les traductions se font via [Gettext](https://www.gnu.org/software/gettext/)
et le plugin [mdbook-i18n-helpers](https://github.com/google/mdbook-i18n-helpers) :
chaque texte traduisible du français est extrait dans un fichier modèle
`doc/po/messages.pot`, puis traduit dans un fichier `doc/po/<langue>.po`
(ex. `doc/po/en.po`).

**Prérequis (une fois) :**
```bash
cargo install mdbook-i18n-helpers
brew install gettext && brew link gettext --force   # macOS ; sous Linux : apt install gettext
```

**À chaque modification du français**, mettre à jour les traductions
existantes (les traducteurs relisent ensuite les entrées marquées « fuzzy »,
qui gardent temporairement l'ancienne traduction) :
```bash
./scripts/i18n.sh update
./scripts/i18n.sh check     # combien de messages restent à traduire/relire ?
```

**Démarrer une nouvelle langue :**
```bash
./scripts/i18n.sh new en
# éditer doc/po/en.po
./scripts/i18n.sh build en   # génère doc/book/en/
./scripts/i18n.sh serve en   # prévisualisation locale
```

Voir `TRANSLATION-GLOSSARY.md` pour la terminologie GeneWeb à respecter
(« Magicien » → « Wizard », pas « Magician », etc.) — à consulter avant
toute traduction pour garder une terminologie cohérente entre les pages.

Le fichier `.github/workflows/docs.yml` automatise ce pipeline : à chaque
push, il régénère le `.pot`, fusionne les `.po` existants, signale (sans
bloquer) le nombre de messages non traduits, puis publie le français et
chaque traduction disponible sur GitHub Pages.

## Sélecteur de langue sur le site publié

`doc/theme/language-picker.js` et `.css` ajoutent un petit sélecteur de
langue en haut de chaque page une fois le site publié. Il suppose la
structure produite par `.github/workflows/docs.yml` :

```
site/          → français (racine)
site/en/       → anglais
site/<lang>/   → une future langue
```

Le script calcule lui-même l'URL équivalente dans l'autre langue en
ajoutant/retirant le préfixe `/en/` — pas de configuration supplémentaire
nécessaire, il suffit que les pages traduites vivent au même chemin relatif
que leur original français.

> **Limite en local** : ce sélecteur ne fonctionne correctement qu'une fois
> le site publié avec cette structure de dossiers (un seul serveur, deux
> sous-chemins). En local, `mdbook serve` et `./scripts/i18n.sh serve en`
> tournent sur des ports séparés (souvent tous les deux sur `:3000`, l'un
> après l'autre) — le sélecteur y pointera vers une URL qui n'existe pas
> tel quel. Pour tester le sélecteur lui-même, le plus simple est de builder
> les deux langues puis de les servir ensemble avec un serveur statique
> unique, par exemple :
> ```bash
> ./scripts/i18n.sh build en
> mdbook build            # régénère doc/book/ (français) sans écraser book/en/
> cd book && python3 -m http.server 8000
> # ouvrir http://localhost:8000/ (fr) et http://localhost:8000/en/ (anglais)
> ```

## État actuel

Squelette initial : chaque fichier contient un titre et des commentaires
`<!-- TODO -->` indiquant le contenu à rédiger. Rechercher `TODO` dans
`doc/src/` pour la liste complète des sections à compléter :

```bash
grep -rn "TODO" doc/src/
```

## Contribuer

1. Éditer les fichiers `.md` concernés dans `doc/src/`.
2. Si une nouvelle page est ajoutée, l'enregistrer dans `doc/src/SUMMARY.md`.
3. Vérifier le rendu avec `mdbook serve` avant de proposer une PR.
