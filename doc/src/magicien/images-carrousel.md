# Portraits, blasons, images, carrousel

## Portrait individuel : envoyer / supprimer une image

Depuis le menu **Mise à jour** d'une fiche personnelle, les liens **Envoyer image** et **Supprimer image** apparaissent à deux conditions :
- le propriétaire de la base a autorisé cette fonctionnalité (paramètre de configuration — voir [Le fichier .gwf](../geneweb/base-gwf.md)) ;
- pour **Envoyer image**, le champ Image de la personne doit être vide. Si une image est déjà associée, il n'est pas proposé de la remplacer directement par ce lien : il faut d'abord passer par **Modifier** la personne et vider le champ Image, ou utiliser **Supprimer image** au préalable.

Le fichier envoyé doit être au format `.gif`, `.jpg` ou `.png`. Voir [Signification des divers champs](creation-champs.md#image) pour la convention de nommage par défaut des fichiers image.

> Source (section suivante) : page [image/fr](https://geneweb.tuxfamily.org/wiki/image/fr) du wiki officiel.

## Mécanisme d'affichage d'une image

Une image peut être invoquée directement (commande `m=IM`, ou `m=IMH` pour le même résultat encapsulé dans une page HTML autonome), ou par un lien textuel vers un fichier `.txt` décrivant l'appel à l'image (utile notamment pour une image avec zones cliquables — une « image map » — via la commande `m=SRC` avec le paramètre `v=` désignant ce fichier `.txt`). Ce mécanisme en deux étapes permet aussi le remplacement automatique de macros telles que `%s` (voir [Templates](../divers/templates.md#macros)) par l'adresse du serveur, pour que l'image reste accessible quel que soit le serveur sur lequel la base est déplacée.

La balise HTML `img` doit obligatoirement figurer dans la liste des balises autorisées par GeneWeb — par défaut `gw/tags.txt`, potentiellement surchargée par un fichier `tags.txt` propre à la base (voir [Localisation d'une base](../geneweb/base-localisation.md)), ou par l'option `gwd -allowed_tags` (voir [Le serveur gwd](../geneweb/gwd.md)) — sans quoi les images ne s'afficheraient pas dans les notes.

## Blasons

<!-- TODO : vérifier si les blasons familiaux utilisent le même mécanisme que le portrait individuel (champ image dédié ?) ou un système distinct — non confirmé dans les sources consultées jusqu'ici -->

## Galerie / carrousel

<!--
TODO :
- Mécanisme des notes de type "gallery"/"album" (carrousel d'images) : où et comment les créer
- Légendes et organisation des images au sein d'un carrousel
- Ce point recoupe potentiellement le travail déjà mené par ailleurs sur les modules d'image de GeneWeb (init_gallery.js, notes_upd_gallery.js) : à documenter une fois le comportement utilisateur final vérifié, plutôt que de décrire l'implémentation interne.
-->
