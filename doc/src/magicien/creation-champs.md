# Signification des divers champs

Le formulaire **Modifier une personne** contient plusieurs zones dont le sens est précis et qu'il convient de bien distinguer, car chacune a un rôle différent dans la navigation et l'affichage.

## Identité de base

- **Prénom / Patronyme** : le prénom d'usage et le nom de famille. Avec le numéro d'occurrence, ils forment la clé unique de la personne (voir [Création d'individu / famille](creation.md)). Il est conseillé d'y mettre le prénom réellement utilisé, et de réserver les variantes (prénoms complets, surnoms familiers, anciennes orthographes) aux champs alias — cela évite de fausser les statistiques par prénom.

> **Particules** (« de », « von », « van »...) : GeneWeb a un traitement spécifique pour les particules des patronymes, notamment pour le tri et l'affichage. La liste des particules reconnues se configure dans le fichier de configuration (variable `particles`), sous forme d'une liste entre guillemets, **en majuscules et sans espaces** (l'affichage les remet en minuscules automatiquement) :
> ```
> particles="AF","AV","D","DAL","DE","DEN","DES","DI","DU","OF","UND","VAN","VON","ZU","ZUR"
> ```
> Pour ajouter une particule supplémentaire à celles déjà reconnues, il faut reprendre la liste existante en entier plutôt que se contenter d'ajouter la nouvelle entrée seule — sans quoi les particules par défaut seraient perdues. Une base dispose aussi d'un fichier `particles.txt` généré par GeneWeb (voir [Localisation d'une base](../geneweb/base-localisation.md)).

## Nom public et qualificatif

- **Nom public** : le nom par lequel la personne est le plus connue, par exemple *Louis XIV* (dont le prénom est *Louis* et le patronyme *de Bourbon*). Si ce champ est rempli, c'est lui qui s'affiche comme titre de la fiche, à la place de « prénom patronyme » — la personne devient également accessible par ce nom dans la recherche.
- **Qualificatif** : un sobriquet associé aux personnages célèbres, par exemple *le Gros* dans *Louis VI le Gros*. Une personne peut avoir plusieurs qualificatifs (ex. *Guillaume le Conquérant*, dit aussi *le Bâtard*) ; chacun devient un point d'accès à la fiche, sans pour autant être traité comme un nom de famille.

## Alias

- **Alias** : un nom sous lequel la personne est connue, mais qui ne fonctionne pas comme un prénom alternatif — la combinaison forme un tout indissociable. Exemple : *Aurore Dupin* dite *George Sand* (et non « George Dupin » ou « Aurore Sand »).
- **Prénom alias** : prénoms complets d'état civil, petits noms familiers (« Bobby » pour « Robert »), ou variantes orthographiques d'un prénom incertain. On peut en avoir plusieurs ; ils sont utilisables dans les recherches mais ne comptent pas dans les statistiques par prénom.
- **Nom alias** : une orthographe alternative du patronyme — usage peu fréquent.

## Image

Fichier `.gif`, `.jpg` ou `.png` associé à la personne, soit par une URL complète, soit par un simple nom de fichier recherché dans le dossier `images/<nom_de_base>/` au moment de l'affichage. Sans nom précisé, un nom par défaut est construit à partir du prénom, du numéro et du nom de famille (minuscules, sans accents, caractères spéciaux remplacés par `_`) — par exemple `anne_cecile.0.dupont_de_nemours` pour *Anne-Cécile Dupont de Nemours*.

## Occupation et titres

- **Occupation** : la profession. Pour un titre de noblesse, utilisez plutôt les champs **Titres**, pas ce champ.
- **Titres** : une personne peut avoir plusieurs titres, chacun décomposé en :
  - la **dénomination** (comte, duc, roi...)
  - le **lieu** (d'Anjou, de Bourgogne, de France...), affiché combiné à la dénomination et cliquable pour retrouver toutes les personnes portant ce même titre
  - un **nom** public associé spécifiquement à ce titre (ex. *Charles Quint* est *Charles Ier* comme roi d'Espagne)
  - un numéro d'ordre (« N-ième ») pour les titres numérotés, par exemple le « 3ème comte de Warwick »
  - un titre marqué **principal**, utilisé quand la personne est affichée de façon simplifiée ailleurs (fiche du conjoint, d'un enfant)
  - les dates de début et de fin de règne

## Accès (confidentialité)

Ce champ ne s'applique qu'aux personnes potentiellement vivantes (dates de moins de 100 ans) :
- Par défaut, une personne titrée (ou dont un parent est titré) est considérée « publique » et ses informations restent visibles.
- **public** force l'affichage systématique, titre de noblesse ou non.
- **privé** masque les informations personnelles même en présence d'un titre.

Un accès magicien ou ami (par mot de passe, voir [Sécurité et contrôle d'accès](../divers/securite.md)) lève toujours ces restrictions.

## Notes et sources

- **Notes** : texte libre sur la personne (voir [Notes diverses](creation-notes.md) pour les détails de rendu).
- **Sources** : une référence courte à l'origine de l'information (nom, sigle, titre d'ouvrage, code personnel) — pas destinée à un texte long ; pour une transcription complète d'un document, utilisez plutôt le champ Notes.

## Champs équivalents côté famille

Le formulaire famille reprend une logique similaire pour le mariage lui-même (date, lieu, notes et sources du mariage), en plus des zones prénom/patronyme/numéro pour chaque conjoint et chaque enfant. Pour les enfants, le patronyme est facultatif : s'il est omis, celui du père est repris automatiquement.
