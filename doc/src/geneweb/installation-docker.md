# Installation avec Docker

Le dépôt `geneweb/geneweb` contient un dossier [`docker/`](https://github.com/geneweb/geneweb/tree/master/docker) maintenu à jour dans `master`, permettant de construire une image à partir des sources.

> **À vérifier / compléter**
> Au moment de la rédaction, aucune image officielle publiée par le projet sur Docker Hub n'a été identifiée avec certitude — seul le `Dockerfile` du dépôt a été repéré. Plusieurs images **communautaires non officielles** existent par ailleurs (`jeffernz/geneweb`, `momoz/geneweb`, etc.), généralement basées sur d'anciennes versions de GeneWeb. Cette page doit être complétée et vérifiée directement à partir du contenu actuel de `docker/` avant publication — idéalement par quelqu'un ayant testé le build en conditions réelles.

## Construire l'image depuis les sources

```bash
git clone https://github.com/geneweb/geneweb
cd geneweb
docker build -t geneweb -f docker/Dockerfile .
```

<!-- TODO : vérifier le chemin exact du Dockerfile et les arguments de build disponibles dans docker/ sur master actuel -->

## Lancer le conteneur

<!--
TODO, une fois le Dockerfile vérifié :
- Ports à exposer (2317 pour gwd, 2316 pour gwsetup, éventuellement 2322 pour l'API)
- Volume à monter pour la persistance des bases (bases/)
- Variables d'environnement disponibles (langue, fuseau horaire, etc.)
- Exemple docker-compose.yml complet
-->

## Alternative : images communautaires

Si vous cherchez une solution prête à l'emploi plutôt qu'un build depuis les sources, plusieurs images non officielles maintenues par des membres de la communauté existent sur Docker Hub (`jeffernz/geneweb`, `momoz/geneweb`, entre autres). Elles ne sont pas couvertes par cette documentation officielle, embarquent des versions de GeneWeb potentiellement anciennes, et doivent être vérifiées avant un usage en production — mais elles donnent une bonne idée du fonctionnement général.

Exemple de lancement (dépôt communautaire [`docker-geneweb`](https://github.com/MauriceIsrael/docker-geneweb)) :

```bash
docker pull momoz/geneweb:latest

docker run -d \
    --name mon-geneweb \
    --restart unless-stopped \
    -p 2316:2316 -p 2317:2317 \
    -v ~/GenealogyData:/usr/local/var/geneweb \
    jeffernz/geneweb:latest
```

Variables d'environnement disponibles avec cette image (`--env VARIABLE=valeur` ajouté à la commande `docker run`) :

| Variable | Rôle |
|---|---|
| `HOST_IP` | À renseigner si `gwsetup` est accédé depuis une autre machine que celle qui exécute le conteneur |
| `LANGUAGE` | Langue de l'interface (ex. `en`, `fr`) |
| `TZ` | Fuseau horaire du conteneur (ex. `Europe/Paris`) |

<!-- TODO : vérifier si ces variables d'environnement sont spécifiques à cette image communautaire ou si elles ont un équivalent officiel dans le Dockerfile de geneweb/geneweb lui-même. -->

Une autre image communautaire (documentée sur la page [Docker](https://geneweb.tuxfamily.org/wiki/Docker) du wiki officiel, qui référence des images tierces plutôt qu'une image officielle du projet) illustre un mécanisme pratique pour la sécurisation : un fichier d'autorisation pour `gwd` (voir [Sécurité et contrôle d'accès](../divers/securite.md)) peut être fourni sous le nom `gwd_passwd` (une ligne `utilisateur:mot_de_passe` par personne autorisée) ; le script de lancement du conteneur détecte automatiquement sa présence et démarre `gwd` avec l'option `-auth` correspondante.
