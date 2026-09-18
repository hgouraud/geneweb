# Sécurité et contrôle d'accès

GeneWeb propose plusieurs mécanismes, combinables, pour contrôler qui peut consulter et modifier une base. **Ces mécanismes ne s'appliquent qu'en mode serveur autonome (démon `gwd`) — pas en mode CGI**, où c'est le contrôle d'accès de votre serveur HTTP (Apache, nginx...) qu'il faut utiliser à la place.

> Sources : pages [Access](https://geneweb.tuxfamily.org/wiki/Access), [Password](https://geneweb.tuxfamily.org/wiki/Password) et [gwd](https://geneweb.tuxfamily.org/wiki/gwd) du wiki officiel.

## Niveaux d'accès

| Niveau | Accès |
|---|---|
| **Visiteur** *(par défaut)* | Accès limité : les personnes plus âgées que `private_years` (150 ans par défaut, voir [Le fichier .gwf](../geneweb/base-gwf.md)) restent visibles, ainsi que toute personne explicitement marquée « publique » (voir [Signification des divers champs](../magicien/creation-champs.md#accès-confidentialité)) |
| **Ami (friend)** | Accès en lecture complet au contenu de la base |
| **Magicien (wizard)** | Accès complet en lecture **et en écriture** (création, édition, suppression, fusion) |

L'authentification se fait, selon le template utilisé, soit par une ligne de saisie directe `utilisateur:mot_de_passe`, soit par un bouton « ami » ou « magicien » qui ouvre une fenêtre de saisie (nom d'utilisateur + mot de passe, ou parfois un seul champ selon le contexte).

## Mots de passe

Les mots de passe magicien et ami peuvent être définis au lancement de `gwd` :

```bash
gwd -wizard <mot_de_passe>    # mot de passe magicien : accès complet, lecture et écriture
gwd -friend <mot_de_passe>    # mot de passe ami : accès en lecture complet
gwd -wjf                       # "wizard just friend" (permanent) : le magicien est traité comme un simple ami
```

Ces mots de passe peuvent aussi être définis dans le fichier `.gwf` de la base (`friend_passwd`, `wizard_passwd` — voir [Le fichier .gwf](../geneweb/base-gwf.md)) plutôt qu'en ligne de commande, ce qui est recommandé pour un usage en production (éviter d'exposer un mot de passe dans les logs de processus). Pour une gestion nominative plutôt qu'un mot de passe unique partagé, utiliser `friend_passwd_file`/`wizard_passwd_file` (fichier `utilisateur:mot_de_passe`, un par ligne) plutôt que `friend_passwd`/`wizard_passwd`.

> **Astuce** : si votre serveur HTTP en amont gère déjà une authentification de type `.htaccess`, il est recommandé de laisser `friend_passwd` vide pour éviter une double authentification aux visiteurs déjà identifiés — tout en gardant un mot de passe (ou fichier de mots de passe) pour les magiciens, qui devront s'authentifier une seconde fois auprès de GeneWeb lui-même.

## Restreindre l'accès à une base précise (`global-access.auth`)

Pour limiter l'accès à une seule base à un ensemble de personnes nommées :

1. Créer un fichier `global-access.auth` dans le dossier `bases/` (même structure que les fichiers de mots de passe : une ligne `utilisateur:mot_de_passe` par personne autorisée).
2. Référencer ce fichier via la variable `auth_file` du `.gwf` de cette base.

## Restreindre l'accès à toutes les bases (`-auth`)

Pour appliquer une restriction similaire à toutes les bases gérées par le serveur plutôt qu'à une seule :

```bash
gwd -auth <fichier.auth>
```

Même structure de fichier que ci-dessus (une ligne `utilisateur:mot_de_passe` par personne autorisée).

## Restriction par adresse IP

```bash
gwd -only <adresse_ip>
```

N'accepte les requêtes que depuis l'adresse IP indiquée. Utile notamment pour restreindre l'accès à l'interface d'administration `gwsetup` à une seule machine.

## Liste noire (`gwd.xlc`)

Le fichier `gwd.xlc`, présent dans le dossier `gw/` de la distribution (voir [Binaires et sources](../geneweb/installation-binaires-sources.md#contenu-détaillé-du-dossier-gw)), permet d'exclure l'accès à des domaines ou des utilisateurs spécifiques. <!-- TODO : syntaxe exacte du fichier, non détaillée dans les sources consultées jusqu'ici. -->

## Protection contre les robots d'indexation agressifs

```bash
gwd -robot_xcl <nombre>,<secondes>
```

Exclut une connexion si elle dépasse `<nombre>` requêtes en `<secondes>` secondes — protège notamment contre les outils d'aspiration de site comme HTTrack ou WebSite Extractor. Exemple rencontré en usage réel : `-robot_xcl 1000,1` (démarrage standard) ou `-robot_xcl 19,60` (déploiement CGI plus restrictif). Options associées : `-min_disp_req <nombre>` (nombre minimal de requêtes dans la trace robot, 6 par défaut), `-redirect <adresse>` (signale que le service a été redirigé ailleurs plutôt que de continuer à répondre).

## Autres options utiles

| Option | Effet |
|---|---|
| `-log <fichier>` | Redirige les traces de connexion vers ce fichier (`-` ou `<stdout>`/`<stderr>` pour rediriger vers la sortie standard/erreur) |
| `-log_level <n>` | Envoie au syslog les messages de sévérité ≤ `<n>` (défaut : 7) |
| `-trace_failed_passwd` | Journalise les tentatives de mot de passe échouées (sauf si `-digest` est actif) |
| `-login_tmout <secondes>` | Délai d'expiration de connexion pour la saisie de mots de passe en mode CGI (défaut 1800 s) |
| `-conn_tmout <secondes>` | Délai d'expiration d'une connexion, en mode démon (rencontré avec la valeur `120` dans un exemple de déploiement CGI réel — la distinction précise d'usage avec `-login_tmout` reste à clarifier) |
| `-cache-in-memory <base>` | Précharge cette base en mémoire |
| `-max_clients <nombre>` | Nombre maximal de clients traités simultanément (mode démon uniquement, pas en CGI ; pas de limite par défaut) |
| `-no_fork` | Empêche la création de processus enfants |
| `-no_host_address` | Force l'absence de résolution DNS inverse par adresse |
| `-version` | Affiche la version de GeneWeb, le dépôt source et le dernier commit |

## Exposition en production : recommandations

<!--
TODO :
- Recommandation de placer gwd derrière un reverse proxy (nginx/Apache) pour gérer HTTPS plutôt que d'exposer gwd directement
- Bonnes pratiques de séparation gwd (port utilisateur) / gwsetup (port admin, à ne jamais exposer publiquement)
-->

## HTTPS

GeneWeb ne gère pas nativement le TLS : deux approches, selon le mode de fonctionnement du serveur.

> Source : page [https/fr](https://geneweb.tuxfamily.org/wiki/https/fr) du wiki officiel. Procédure testée sur Ubuntu Server 16.04 en janvier 2017, valable pour Debian et dérivés ; **non utilisable sur macOS** à cette date. Des ajustements peuvent être nécessaires selon la plateforme et depuis cette date.

- **Mode CGI** (voir [Mode CGI](../geneweb/installation-cgi.md)) : configurer un `VirtualHost` Apache sur le port 443 avec le certificat TLS (par exemple Let's Encrypt), Apache se chargeant du HTTPS en amont de GeneWeb.
- **Mode service** (`gwd` sur le port 2317) : encapsuler `gwd` avec l'outil **stunnel4**, en configurant les certificats dans son fichier de configuration (un modèle est fourni dans `/usr/share/doc/stunnel4/examples/stunnel.conf-sample` sur les systèmes basés Debian).

Dans les deux cas, une redirection automatique du HTTP classique (port 80) vers HTTPS peut être ajoutée via un second `VirtualHost` Apache utilisant `RewriteEngine`/`RewriteRule`.

## Extensions (plugins) et sécurité

Le chargement de plugins non vérifiés est explicitement soumis à des options de sécurité dédiées — voir [Extensions (plugins)](plugins.md).
