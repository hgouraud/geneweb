# Mode CGI

Au-delà du mode serveur autonome (`gwd` lancé en démon, voir [Le serveur gwd](gwd.md)), GeneWeb peut fonctionner en **mode CGI**, derrière un serveur web existant (Apache...) plutôt qu'en écoutant lui-même sur un port — utile notamment en hébergement mutualisé, où l'on ne peut pas lancer son propre démon.

> Source : page [CGI/fr](https://geneweb.tuxfamily.org/wiki/CGI/fr) du wiki officiel.

## Exemple sous Linux/Unix

Un script shell type invoque `gwd` avec l'option `-cgi`, en plus des options habituelles :

```bash
OPTIONS="-blang -robot_xcl 40,70 -max_clients 15 -conn_tmout 120 -min_disp_req 30 -images_url http://monserveur.net/gw/images"
# -allowed_tags $GENEWEBDB/tags.txt
cd $GENEWEBSHARE
$DAEMON -hd $GENEWEBSHARE -dd $GENEWEBDOC -bd $GENEWEBDB -lang $LNG -log $LOGFILE -cgi $OPTIONS 2>/dev/null
```

Dans un environnement CGI hébergé, les feuilles de style (`.css`) et fichiers JavaScript sont envoyés par le serveur HTTP (Apache) lui-même plutôt que par `gwd` — il faut donc installer, à la racine web, un lien vers le dossier contenant ces fichiers de style (voir [Personnalisation](../divers/personnalisation.md)).

## Exemple sous Windows

Appeler un script CGI via un fichier batch/`cmd.exe` peut s'avérer délicat sous Windows. Une alternative consiste à appeler directement une copie de `gwd.exe`, avec son fichier d'arguments `gwd.arg` (voir [Le serveur gwd](gwd.md#fichier-doptions-gwdarg)), placée dans le dossier `/cgi-bin/` d'Apache — `gwd` fonctionne alors derrière Apache, accessible via `http://localhost/cgi-bin/gwd.exe`.

Exemple de `gwd.arg`, pour une installation dans `C:\Program Files (x86)\geneweb\` :
```
-hd
C:\Program Files (x86)\geneweb\gw
-bd
C:\Program Files (x86)\geneweb\bases
-log
C:\Program Files (x86)\geneweb\geneweb.log
-images_dir
C:\Program Files (x86)\geneweb\gw\images
-cgi
```

> **Piège classique** : dans ce fichier, chaque option et sa valeur occupent deux lignes séparées. Attention aussi au type de fin de ligne (CR, LF, ou CR/LF), qui diffère entre Linux, macOS et Windows — un fichier `gwd.arg` créé sur un système et copié tel quel sur un autre peut échouer à être lu correctement pour cette seule raison.

## Vérification

Pour valider que le mécanisme CGI fonctionne côté serveur avant de déboguer GeneWeb lui-même, on peut d'abord tester un script CGI minimal (`test-cgi.sh` ou équivalent) via le navigateur.
