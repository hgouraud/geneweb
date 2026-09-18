# FAQ / Dépannage

> Source : page [FAQ](https://geneweb.tuxfamily.org/wiki/FAQ) du wiki officiel.

## Pourquoi une personne apparaît-elle avec un prénom/nom « ? » ?

Lors d'un import GEDCOM, GeneWeb enregistre toujours les parents par couple. Quand le fichier GEDCOM ne mentionne que le père ou que la mère, GeneWeb ajoute automatiquement l'autre parent, avec un prénom et un nom composés d'un point d'interrogation. C'est normal : ça matérialise un parent manquant (mais qui a nécessairement existé) plutôt que d'inventer une identité.

## La liste des liens de parenté entre deux personnes me semble incomplète

C'est voulu : pour éviter un affichage trop long, GeneWeb n'affiche qu'une partie des chemins de parenté trouvés entre deux personnes. Les liens non affichés restent néanmoins réels et sont bien comptés dans le calcul de consanguinité — voir [Affichage](../utilisateur/affichage.md).

## Le statut décès/vivant d'une personne n'est pas celui attendu

GeneWeb distingue trois états : « vivant », « décédé », et « ne sait pas ». L'âge n'est affiché que pour les personnes considérées vivantes. S'il manque, c'est soit que la personne est marquée décédée (sans date connue), soit que son statut est resté à « ne sait pas » — un cas fréquent pour une base issue d'un import GEDCOM, ce format ne disposant pas d'une mention « personne actuellement vivante » explicite (voir [Convention date → MOD_IND](../magicien/navigation-maj-dates.md) pour le détail des trois états et leur détermination automatique).

## `gwd` ne démarre pas en mode CGI

Vérifiez d'abord le journal de votre serveur HTTP (typiquement Apache) — son emplacement varie selon le système. Un problème fréquent est une question de droits d'accès : le serveur web exécute souvent les CGI avec un utilisateur disposant de très peu de droits (typiquement `nobody` sous Unix), qui peut ne pas être autorisé à exécuter `gwd`. Vérifiez les droits d'accès sur `gwd` et l'ensemble des fichiers de GeneWeb.

## Ma connexion internet se coupe après un moment d'inactivité avec GeneWeb ouvert

<!-- TODO : le wiki mentionne un cas particulier lié aux connexions par modem à la demande et au mode hors-ligne de certains navigateurs — à préciser si ce cas est encore pertinent aujourd'hui (contenu ancien, daté). -->
