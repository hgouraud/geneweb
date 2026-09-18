# Gestion d'une base (outils gwsetup)

`gwsetup` est le serveur auxiliaire dédié à la création et à la gestion des bases, via une interface web séparée de `gwd` (voir [Le serveur gwd](gwd.md)) — accessible par défaut sur le port 2316 (page d'accueil `http://localhost:2316/`), à ne jamais exposer publiquement (voir [Sécurité et contrôle d'accès](../divers/securite.md)).

## Premier lancement selon la plateforme

- **Windows** : double-cliquer sur `gwsetup.bat`, disponible dans le dossier d'installation de GeneWeb. Une fenêtre s'ouvre, proposant un choix de langue et indiquant l'adresse à laquelle `gwsetup` est disponible — garder cette fenêtre ouverte tant qu'on interagit avec `gwsetup` (elle peut être réduite, pas fermée).
- **Linux, installation via un paquet RPM** : `gwsetup` est probablement déjà lancé automatiquement ; utiliser l'adresse indiquée au moment de l'installation.

## Options de lancement

| Option | Effet |
|---|---|
| `-p <port>` | Port à utiliser (par défaut 2316) |
| `-lang <code>` | Langue par défaut |
| `-daemon` | Utilisation comme démon Unix |
| `-only <fichier>` | Chemin vers le fichier contenant l'adresse IP autorisée à administrer (`only.txt`, une seule adresse, `127.0.0.1` par défaut) |
| `-log <fichier>` | Redirige les traces vers ce fichier |
| `-gd <dossier>` | Chemin du dossier partagé GeneWeb |
| `-bindir <chaîne>` | Dossier des binaires (par défaut : valeur de `-gd`) |

> **Point important, source de confusion fréquente** : contrairement à `gwd` (option `-bd`, voir [Le serveur gwd](gwd.md)), `gwsetup` **n'a pas d'option équivalente pour préciser où se trouvent les bases** — il les cherche dans le **dossier courant** au moment de son lancement. Il faut donc systématiquement se placer (`cd`) dans le dossier des bases avant de lancer `gwsetup`, par exemple :
> ```bash
> export BIN_DIR="dossier où se trouvent gwd et gwsetup"
> export BASE_DIR="dossier où vous voulez garder vos bases"
> cd "$BASE_DIR"
> "$BIN_DIR/gwsetup" -gd "$BIN_DIR" -bindir "$BIN_DIR"
> ```

<!-- TODO : vue d'ensemble des fonctions disponibles depuis l'interface web elle-même (création de base, import GEDCOM, etc.) -->

