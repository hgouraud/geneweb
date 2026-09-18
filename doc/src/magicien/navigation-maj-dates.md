# Convention date → MOD_IND

Dans les formulaires de mise à jour (accessibles via la commande d'URL `MOD_IND` pour un individu, ou l'équivalent pour une famille — voir [Commandes d'URL](../divers/templates.md#commandes-durl-m)), les champs de date acceptent une syntaxe abrégée pour exprimer la précision réelle de l'information disponible, sans avoir à utiliser un menu séparé.

## Raccourcis de précision de date

| Saisie | Signification |
|---|---|
| `?1912` | Peut-être en 1912 |
| `~1912` | Vers 1912 (environ) |
| `>1912` ou `1912/` | Après 1912 |
| `<1912` ou `/1912` | Avant 1912 |
| `/1912/` | Environ 1912 |

## Cas particulier du champ décès

Dans la zone décès, l'année peut aussi recevoir :
- `+` seul → décédé, mais on ne sait pas quand
- `-` seul → vivant

## Choix automatique du statut décès/vivant

Quand le premier menu de la zone « décès » est laissé sur `-` (« choix automatique ») et qu'aucune date ni lieu de décès n'est renseigné, GeneWeb déduit le statut à partir de l'âge calculé depuis la date de naissance :
- moins de 80 ans → considéré non décédé
- entre 81 et 120 ans → statut « ne sais pas »
- plus de 120 ans → considéré décédé

Une personne au statut « ne sais pas » n'aura pas son âge calculé sur sa fiche ni dans les listes d'anniversaires. Si une personne a entre 80 et 120 ans et est vivante, il faut donc sélectionner explicitement « non décédé » plutôt que de laisser faire le choix automatique.

## Nomenclature d'affichage des dates courtes

Sur les fiches individuelles et la plupart des pages, GeneWeb combine naissance et décès en une notation courte :

| Affichage | Signification |
|---|---|
| `1935-1950` | Né en 1935, mort en 1950 |
| `1935-` | Né en 1935, date de décès inconnue |
| `1935` | Né en 1935, toujours vivant |
| `†1950` | Date de naissance inconnue, décédé en 1950 |
| `†` | Dates de naissance et de décès inconnues |

> Source : page [date/fr](https://geneweb.tuxfamily.org/wiki/date/fr) du wiki officiel.

## Mois du calendrier républicain (raccourcis templm)

Pour saisir un mois du calendrier républicain (voir [Le format .gw](../divers/format-gw.md#format-des-dates) pour le suffixe `F` de calendrier) avec le moteur `templm`, des abréviations à deux lettres sont disponibles : `VD` (vendémiaire), `BR` (brumaire), `FM` (frimaire), `NI` (nivôse), `PL` (pluviôse), `VT` (ventôse), `GE` (germinal), `FL` (floréal), `PR` (prairial), `ME` (messidor), `TH` (thermidor), `FT` (fructidor), et `JC` pour le mois complémentaire. En cas d'hésitation, cliquer sur un champ mois vide déroule la liste complète ; taper une seule lettre ouvre un menu de suggestions (par exemple `V` propose `VD` et `VT`).

## Confort de saisie avec templm

Avec le moteur de template `templm`, la saisie d'une date est assistée : si jour et mois inférieurs à 10 sont tapés sur deux chiffres (`04` pour le 4), il n'est pas nécessaire de tabuler entre les champs — taper `04071776` remplit directement les trois champs jour/mois/année. Toute date incohérente (mois/jour invalide, 29 février hors année bissextile) fait s'effacer le champ concerné plutôt que d'accepter une date erronée. Le champ année accepte aussi des opérations d'addition/soustraction directement.

## Où ces raccourcis s'appliquent

Ces raccourcis sont surtout utiles dans les formulaires **famille** (ajout/modification), qui permettent de saisir directement une date de naissance ou de décès pour chaque personne créée, sans passer par un formulaire individuel séparé — ce qui accélère la saisie initiale d'une famille nombreuse.
