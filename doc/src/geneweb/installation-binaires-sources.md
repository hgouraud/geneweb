# Binaires et sources

## Télécharger les binaires précompilés

La méthode la plus simple consiste à télécharger une archive précompilée depuis la [page des releases GitHub](https://github.com/geneweb/geneweb/releases/latest), pour Linux, macOS ou Windows.

1. Téléchargez l'archive correspondant à votre système.
2. Extrayez-la à l'emplacement de votre choix (le dossier peut être renommé sans problème).
3. Le contenu de la distribution se présente ainsi (l'exemple ci-dessous correspond à une distribution GNU/Linux ; les autres systèmes sont très similaires) :

```
distribution/
├── bases/              # dossier des bases de données
├── gw/                  # exécutables et fichiers listés en détail ci-dessous
├── images/
├── lang/                # fichiers de lexique / traductions
├── plugins/
├── etc/
├── gwd.sh               # script de lancement du serveur principal (Unix)
├── gwsetup.sh           # script de lancement de l'interface d'administration (Unix)
├── geneweb.command      # lancement en double-clic (macOS)
├── START.htm            # lancement en double-clic (Windows)
├── CHANGES.txt
├── LICENSE.txt
└── README.txt
```

### Contenu détaillé du dossier `gw/`

> Source : page [distribution](https://geneweb.tuxfamily.org/wiki/distribution) du wiki officiel.

| Fichier | Rôle |
|---|---|
| `connex` | Outil de calcul de connexité — voir [Maintenance](gestion-import-export.md) |
| `consang` | Outil de calcul de consanguinité |
| `ged2gwb`, `ged2gwb2`, `gwb2ged` | Création/extraction de base au format GEDCOM |
| `gwc`, `gwc2` | Création d'une base à partir d'un fichier `.gw` |
| `gwu` | Extraction d'une base vers un fichier `.gw` |
| `gwd` | Le serveur GeneWeb principal — voir [Le serveur gwd](gwd.md) |
| `gwd.arg` | Paramètres d'exécution statiques pour `gwd` |
| `gwd.log` | Traces d'exécution de `gwd` (selon ce qui est précisé au lancement) |
| `gwd.xlc` | **Liste noire** excluant l'accès à des domaines ou utilisateurs spécifiques |
| `gwsetup` | Serveur auxiliaire de gestion des bases |
| `only.txt` | Restriction d'accès à `gwsetup` par adresse IP (une seule adresse, `127.0.0.1` par défaut — voir [Sécurité et contrôle d'accès](../divers/securite.md)) |
| `tags.txt` | Liste des balises HTML autorisées (voir [Le format wiki](../divers/format-wiki.md)) |
| `update_nldb` | Mise à jour des liens internes entre notes et fiches individuelles |
| `geneweb` | Lance `gwsetup`, `gwd`, et ouvre la page `START.html` (variante `.command` sur macOS) |

<!-- TODO : le wiki mentionne aussi gwdiff, gwfixbase, gwgc, gwrepl sans description ("…") — à documenter si leur usage est découvert. -->

Voir les pages [Windows](installation-win.md), [Unix / Linux](installation-unix.md) et [macOS](installation-mac.md) pour le premier lancement propre à chaque système.

## Compiler depuis les sources

Recommandé pour les développeurs, les contributeurs, ou si aucun binaire précompilé ne correspond à votre plateforme.

### Prérequis

- [OCaml](https://ocaml.org) 4.10 ou supérieur
- [opam](https://opam.ocaml.org), le gestionnaire de paquets OCaml

### Étapes de compilation

```bash
git clone https://github.com/geneweb/geneweb
cd geneweb

# Installer les dépendances déclarées par les fichiers .opam du projet
opam install . --deps-only

# Configurer le projet (voir les options ci-dessous)
ocaml ./configure.ml

# Compiler et générer une distribution complète (équivalent au contenu d'une release)
make distrib
```

`make distrib` produit un dossier de distribution avec la même structure que les archives précompilées (voir ci-dessus), prêt à être déplacé et lancé comme n'importe quelle release.

Pour connaître les options de configuration disponibles (par exemple activer l'API, choisir le format de base de données, etc.) :

```bash
ocaml ./configure.ml --help
```

> **Note pour les contributeurs**
> Le projet a changé de système de configuration au fil du temps (auparavant un script shell `configure`, aujourd'hui `configure.ml`). Assurez-vous de suivre les instructions du `README.md` de la version exacte de `master` que vous clonez, celles-ci pouvant évoluer.

<!-- TODO : détailler les options les plus utiles de `configure.ml --help` (ex. --api, choix GWDB) une fois passées en revue avec un contributeur du projet -->
