# Le serveur gwd

`gwd` est le processus serveur principal de GeneWeb : c'est lui qui répond aux requêtes des visiteurs sur le port configuré (2317 par défaut) et génère les pages à la volée. Cette page couvre ses options générales de démarrage — pour les options liées à la sécurité et au contrôle d'accès (mots de passe, restriction IP...), voir [Sécurité et contrôle d'accès](../divers/securite.md).

> Sources : pages [gwd/fr](https://geneweb.tuxfamily.org/wiki/gwd/fr), [Gwd-start](https://geneweb.tuxfamily.org/wiki/Gwd-start) et [CGI](https://geneweb.tuxfamily.org/wiki/CGI) du wiki officiel.

## Options générales connues (à vérifier/compléter)

| Option | Effet |
|---|---|
| `-a <adresse>` | Adresse spécifique à écouter (par défaut : toutes les adresses de la machine) |
| `-add_lexicon <fichier>` | Ajoute ce fichier comme lexique supplémentaire (voir [Internationalisation, lexiques](../divers/i18n-lexiques.md)) |
| `-allowed_tags <fichier>` | Fichier listant, une par ligne, les balises HTML autorisées dans les notes (voir [Le format wiki](../divers/format-wiki.md)) |
| `-auth <fichier>` | Fichier d'autorisation restreignant l'accès (lignes `utilisateur:mot_de_passe` — voir [Sécurité et contrôle d'accès](../divers/securite.md)) |
| `-bd <dossier>` | Chemin vers les bases GeneWeb |
| `-blang` | Sélectionne la langue déclarée par le navigateur du visiteur, si disponible |
| `-cache_langs` | Langues de lexique à mettre en cache |
| `-cgi` | Force le mode CGI (voir [Mode CGI](installation-cgi.md)) |
| `-dd <dossier>` | Chemin vers le dossier de documentation |
| `-hd <dossier>` | Chemin vers le dossier `gw/` de la distribution (lexique, templates, exécutables) — option centrale, utilisée dans tous les scripts de lancement réels rencontrés |
| `-images_url <url>` | URL pour les images de GeneWeb (par défaut, gwd les sert lui-même) |
| `-images_dir <chemin>` | Variante de l'option précédente avec un chemin de dossier relatif |
| `-log <fichier>` | Redirige les traces de connexion vers ce fichier (voir [Sécurité et contrôle d'accès](../divers/securite.md)) |
| `-p <numéro>` | Port à utiliser (par défaut 2317 ; > 1024 pour un utilisateur normal) |
| `-setup_link` | Affiche un lien vers `gwsetup` en bas de chaque page |
| `-wd <dossier>` | Chemin pour les communications par socket (Windows) et le compteur d'accès |

> **Remarque** : les chemins fournis aux options `-allowed_tags`, `-auth` et `-log` peuvent être déplacés et renommés ; ils peuvent être absolus ou relatifs selon votre configuration et la façon dont `gwd` est lancé. Les fichiers fournis à `-add_lexicon` doivent, eux, obligatoirement être placés dans l'un des dossiers `lang`.

## Exemple de lancement réel

Extrait du script [Gwd-start](https://geneweb.tuxfamily.org/wiki/Gwd-start) fourni par le projet :

```bash
"$BIN_DIR/$VERS/gw/gwd" \
  -allowed_tags "$BASE_DIR/tags.txt" \
  -hd "$BIN_DIR/$VERS/gw" \
  -bd "$BASE_DIR" \
  -robot_xcl 1000,1 \
  > "$BIN_DIR/$VERS/gw/gwd.log" 2>&1 &
```

`gwsetup` est lancé juste avant, avec ses propres options (`-gd`, `-lang`) — voir [Gestion d'une base](gestion.md) pour le détail, notamment le piège classique concernant l'emplacement des bases.

## Fichier d'options (`gwd.arg`)

Plutôt que de répéter les options en ligne de commande, on peut les regrouper dans un fichier `gwd.arg` placé dans le dossier `gw/` (voir [Binaires et sources](installation-binaires-sources.md#contenu-détaillé-du-dossier-gw)) : chaque option et sa valeur occupent deux lignes séparées. Exemple :

```
-cgi
-bd
~/bases
-hd
./
-robot_xcl
19,60
-allowed_tags
./tags.txt
```

<!--
TODO : compléter avec toutes les options actuelles de `gwd -help` sur master (liste ci-dessus non exhaustive), structurer par catégorie (réseau, chemins, comportement).
-->

## Évolution prévue (v7.1)

`gwc` devrait proposer un paramètre `-reorg` (voir [Import / export](gestion-import-export.md#gwc)), et la version 7.1 de `gwd` devrait reconnaître automatiquement les deux organisations de dossier de base (classique et `reorg`).

## Démarrage du serveur

Voir les scripts `gwd.sh` (Unix/macOS) ou le lancement via `START.htm`/`geneweb.command` mentionnés dans les pages d'[installation](installation.md) selon la plateforme.
