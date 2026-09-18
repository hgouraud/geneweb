# Navigation et mise à jour

Depuis une fiche personnelle, le lien **Mise à jour** ouvre un menu dont le contenu s'adapte à la situation de la personne :

- **Modifier** et **Supprimer** sont toujours proposés (voir [Suppression, fusion](suppression-fusion.md) pour la suppression).
- **Envoyer image** (et, si une image existe déjà, **Supprimer image**) apparaît seulement si le propriétaire de la base a autorisé cette fonctionnalité — voir [Portraits, blasons, images, carrousel](images-carrousel.md).
- Pour chaque mariage de la personne : **Modifier/Famille** et **Supprimer/Famille**. S'il y a plusieurs mariages, un choix **Intervertir** permet d'échanger leur ordre d'affichage.
- **Ajouter/Famille** est toujours proposé en fin de liste : il ouvre le même formulaire que "Ajouter une famille" depuis l'accueil, avec la personne pré-inscrite comme parent. **Attention** : ce choix crée un nouveau mariage, ce n'est pas la manière d'ajouter un enfant à une union déjà existante (voir [Création d'individu / famille](creation.md#pièges-fréquents-à-éviter)).
- Si la personne n'a pas encore de parents connus, **Ajouter/Parents** propose de remonter d'une génération : ce choix crée également une nouvelle famille, avec la personne pré-inscrite comme enfant.
- **Changer les noms des enfants** permet de modifier en un seul formulaire le prénom, le patronyme et le numéro de tous les enfants d'une personne. Une facilité notable : si l'on efface le patronyme d'un enfant dans ce formulaire, il reprend automatiquement celui du parent — pratique pour renommer rapidement toute une branche en partant de l'ancêtre puis en répétant l'opération génération par génération.

## Principe général de validation

Dans tous les formulaires de mise à jour, rien n'est appliqué à la base avant d'avoir cliqué sur **Ok** en fin de formulaire. Certains boutons **Insérer/...** (Insérer/Enfant, Insérer/Titre, Insérer/Qualificatif, etc.) ne valident jamais directement : ils redemandent seulement un formulaire agrandi avec une zone supplémentaire disponible.

Après validation réussie, la page affichée indique le résultat (« Personne modifiée », « Famille ajoutée ou modifiée »...) et peut signaler des remarques (incohérences de dates ou d'ordre entre enfants) sans empêcher la modification. En cas d'erreur (personne déjà existante, personne inconnue), la page l'indique et rien n'est modifié — il suffit de revenir en arrière et de corriger.

Voir [Convention date → MOD_IND](navigation-maj-dates.md) pour les raccourcis de saisie disponibles dans les champs de date de ces formulaires.

## Raccourcis clavier

GeneWeb propose des raccourcis clavier pour accélérer certaines actions, disponibles selon les pages affichées. Ils reposent sur l'attribut HTML standard `accesskey`, dont le déclenchement (combinaison de touches à utiliser) varie selon le navigateur et le système d'exploitation — un peu d'expérimentation est nécessaire pour les découvrir sur votre configuration.

> Source : page [Shortcuts](https://geneweb.tuxfamily.org/wiki/shortcuts) du wiki officiel.

Exemple concret rencontré dans la documentation : pour sélectionner le troisième enfant du troisième conjoint d'une personne, on doit d'abord naviguer jusqu'au bon conjoint, puis taper une séquence de six touches suivie de `P` (ou son équivalent accentué) : `0 0 0 8 8 8 P` (ou `à à à ç ç ç p`). Le détail précis de cette numérotation par touches reste à documenter (elle semble reposer sur `accesskey`, un chiffre ou une lettre par conjoint/enfant selon leur position).

<!-- TODO : lister l'ensemble des raccourcis (touches et actions associées) une fois la table complète du wiki consultée en détail. -->
