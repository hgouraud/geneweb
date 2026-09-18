# Installation sous Unix / Linux

Cette page suppose que vous disposez déjà d'une distribution GeneWeb, précompilée ou compilée depuis les sources (voir [Binaires et sources](installation-binaires-sources.md)).

## Premier lancement

1. Ouvrez un terminal dans le dossier de la distribution.
2. Lancez le serveur :

```bash
./gwd.sh
```

3. Ouvrez votre navigateur à l'adresse [http://localhost:2317](http://localhost:2317).

## Lancer sur le port 80

Sous Unix, les ports inférieurs à 1024 nécessitent des privilèges élevés. Plutôt que d'exécuter GeneWeb en `root`, utilisez les capacités Linux (`capabilities`) pour autoriser uniquement la liaison au port :

```bash
sudo setcap 'cap_net_bind_service=+ep' gwd
./gwd -p 80
```

## Aller plus loin

<!--
TODO :
- Lancement en arrière-plan / service systemd (unit file d'exemple)
- Gestion des logs
- Dépendances système par distribution (Debian/Ubuntu, Fedora, etc.) si vous compilez depuis les sources
- Reverse proxy (nginx/apache) devant gwd pour exposition en production, HTTPS
-->
