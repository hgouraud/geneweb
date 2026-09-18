# Installation sous macOS

Cette page suppose que vous disposez déjà d'une distribution GeneWeb, précompilée ou compilée depuis les sources (voir [Binaires et sources](installation-binaires-sources.md)).

## Premier lancement

1. Extrayez l'archive téléchargée à l'emplacement de votre choix.
2. Double-cliquez sur `geneweb.command`.
3. Ouvrez votre navigateur à l'adresse [http://localhost:2317](http://localhost:2317).

## Autoriser GeneWeb (Gatekeeper)

macOS bloque par défaut les applications provenant de développeurs non identifiés. Il faut autoriser manuellement les deux exécutables principaux, **une seule fois** :

1. Dans le dossier `gw`, faites un clic droit (ou Ctrl+clic) sur `gwd`, puis choisissez **Ouvrir** dans le menu contextuel.
2. Cliquez sur **Ouvrir** dans la boîte de dialogue de sécurité qui apparaît.
3. Répétez les étapes 1 et 2 pour `gwsetup`.

Une fois cette autorisation accordée, `geneweb.command` fonctionne normalement pour tous les lancements suivants.

## Lancer sur le port 80

Comme sous Unix, les ports inférieurs à 1024 nécessitent des privilèges élevés. Sur macOS, la méthode recommandée est de rediriger le port 80 vers le port 2317 via `launchd`, plutôt que d'exécuter GeneWeb avec des droits élevés.

<!-- TODO : exemple de fichier plist launchd pour la redirection de port 80 -> 2317 -->

## Aller plus loin

<!-- TODO : particularités liées à l'architecture (Apple Silicon vs Intel) si les binaires diffèrent selon l'architecture -->
