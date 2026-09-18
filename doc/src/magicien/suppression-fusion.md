# Suppression, fusion

## Suppression d'une personne

Le formulaire de suppression ne comporte qu'une case de confirmation — rien n'est fait sans validation explicite.

Point important : une personne supprimée n'est **pas réellement effacée** de la base. Ses informations personnelles (y compris prénom et patronyme) sont vidées, ainsi que son lien avec ses parents, mais une « entité personne » subsiste, toujours reliée à son éventuel conjoint et à ses éventuels enfants. En pratique, supprimer une personne isole surtout sa branche plutôt que de la faire disparaître complètement de la structure.

## Suppression d'une famille

De même, supprimer une famille ne supprime aucune des personnes qui la composaient : le père, la mère et les enfants sont simplement dissociés les uns des autres. Le père et la mère ne sont plus mariés entre eux (mais conservent leurs autres mariages éventuels), et les enfants se retrouvent sans parents renseignés. Il peut être nécessaire de supprimer ensuite, séparément, les personnes ainsi devenues isolées si elles n'ont plus lieu d'exister dans la base.

## Fusionner deux personnes (doublons)

Cas d'usage typique : la même personne a été saisie deux fois par erreur.

- **Si les deux fiches ont le même prénom et le même nom** : notez les numéros d'occurrence de chacune, choisissez **Fusionner** dans le menu, sélectionnez la fiche portant le bon numéro, puis laissez-vous guider.
- **Si les deux fiches ont des prénom/nom différents** : soit renommez l'une des deux pour qu'elle porte le même prénom/nom que l'autre (avec un numéro distinct) puis appliquez la méthode précédente ; soit lancez **Fusionner** depuis l'une des deux fiches et saisissez directement `Prénom.numéro Nom` de l'autre. Exemple : pour fusionner *Marie-Anne LE COMPTE* (occurrence 3) avec *Marianne LECONTE* (occurrence 2), on lance la fusion depuis la fiche de Marianne LECONTE et on saisit `Marie-Anne.3 LE COMPTE`.
- **Si les deux personnes ont chacune des parents** : le système propose de fusionner également les ascendants et les familles d'ascendants correspondantes. Il suffit de valider chaque étape proposée ; des formulaires intermédiaires (individus, familles) permettent de vérifier les informations avant chaque fusion.

**Astuce pratique** : ouvrez deux fenêtres de navigateur côte à côte. Dans la fenêtre de gauche, sélectionnez la première personne à fusionner et lancez **Fusionner** ; dans la fenêtre de droite qui s'ouvre, saisissez `Prénom.numéro Nom` de la seconde personne, puis validez par « Ok ». Vous pouvez ensuite fermer la fenêtre de droite.

> **Effet de bord à connaître** : fusionner un couple d'ascendants peut faire apparaître, après coup, chacun des deux membres du couple comme « marié deux fois avec la même personne » (avec potentiellement des enfants différents entre les deux mariages apparents). C'est normal : il suffit alors de fusionner ces deux familles apparentes entre elles (menu **Modifier**, puis la commande de fusion — voir [Fusionner deux familles](#fusionner-deux-familles) ci-dessous).

## Fusionner deux familles

1. Fusionnez d'abord les deux époux entre eux (s'ils sont des fiches distinctes), puis les deux épouses entre elles, en suivant la procédure de fusion de personnes ci-dessus.
2. Depuis la fiche de l'un des conjoints ainsi fusionnés, un menu **fusionner les doublons éventuels** propose alors de fusionner les deux familles elles-mêmes.

## Bonnes pratiques

- Une fusion n'est jamais totalement anodine : vérifiez systématiquement les formulaires intermédiaires plutôt que de valider par réflexe.
- En cas de doute sur une fusion en cours d'ascendants, il est toujours possible d'interrompre en abandonnant la navigation avant la validation finale d'une étape.
