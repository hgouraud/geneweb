# Glossaire de traduction — Manuel GeneWeb (FR → EN/DE/ES/IT)

Ce glossaire fixe la terminologie à utiliser systématiquement lors de la
traduction du manuel, pour éviter que le même concept GeneWeb soit rendu par
des mots différents selon le fichier ou le traducteur.

**Règle d'or : quand un terme officiel existe déjà dans le logiciel lui-même
(options de `gwd`, noms de commandes de template, lexique de l'interface),
on l'utilise tel quel — même s'il ne s'agit pas de la traduction la plus
littérale.** L'objectif est la cohérence avec l'interface que le lecteur a
sous les yeux, pas l'élégance littéraire.

Statut de chaque terme :
- ✅ **Confirmé** : retrouvé directement dans le code/l'interface de GeneWeb dans cette langue (nom d'option `gwd`, nom de commande de template, entrée du lexique).
- 🟡 **Proposé** : traduction cohérente basée sur le vocabulaire généalogique/logiciel standard de la langue, **non vérifiée** dans l'interface réelle de GeneWeb.

> **Important — état actuel des colonnes DE / ES / IT**
> Contrairement à l'anglais (dont une bonne partie des termes est confirmée
> par les noms de code source eux-mêmes, en anglais par nature), l'allemand,
> l'espagnol et l'italien n'ont pas de source de vérité équivalente
> accessible depuis l'extérieur du dépôt. Le fichier `hd/lang/lexicon.txt`
> du dépôt GeneWeb contient bien les traductions officielles de l'interface
> dans une quinzaine de langues (format : blocs `[de: ... / en: ... / es:
> ... / fr: ... / it: ...]`), mais il est trop volumineux pour être consulté
> de façon fiable par recherche web. **Toutes les entrées DE/ES/IT ci-dessous
> sont donc actuellement marquées 🟡** (vocabulaire standard proposé, pas
> encore vérifié contre ce fichier).
>
> Pour les faire passer en ✅ : comme tu as déjà `geneweb/geneweb` cloné en
> local, le plus fiable est que tu lances par exemple
> `grep -n -B2 -A6 "fr: .*[Mm]agicien" hd/lang/lexicon.txt` (à adapter par
> terme cherché) sur quelques termes clés de ce glossaire, et que tu me
> transmettes les blocs trouvés — je corrigerai alors les entrées
> concernées avec les vraies traductions officielles.

## Rôles et niveaux d'accès

| Français | Anglais | Allemand | Espagnol | Italien | Statut | Note |
|---|---|---|---|---|---|---|
| Magicien | **Wizard** | Assistent 🟡 | Asistente 🟡 | Assistente 🟡 | EN ✅ / autres 🟡 | option `gwd -wizard` pour l'anglais ; PAS "Magician"/"Mago" |
| Ami | **Friend** | Freund 🟡 | Amigo 🟡 | Amico 🟡 | EN ✅ / autres 🟡 | option `gwd -friend` pour l'anglais |
| Visiteur | **Visitor** | Besucher 🟡 | Visitante 🟡 | Visitatore 🟡 | EN ✅ / autres 🟡 | |
| Accès (champ de confidentialité) | **Access** | Zugriff 🟡 | Acceso 🟡 | Accesso 🟡 | EN ✅ / autres 🟡 | commande de template `access` (anglais) |

## Entités et structure de la base

| Français | Anglais | Allemand | Espagnol | Italien | Statut | Note |
|---|---|---|---|---|---|---|
| Base (de données) | **base** | Datenbank 🟡 | Base de datos 🟡 | Database 🟡 | EN ✅ / autres 🟡 | garder « base » tel quel dans les noms de commande (`m=PPS`, etc.) |
| Fiche (personnelle) | **Personal page** | Personenseite 🟡 | Ficha personal 🟡 | Scheda personale 🟡 | EN ✅ / autres 🟡 | ES/IT ont un cognat direct de « fiche » (ficha/scheda) — bon signe, mais toujours à vérifier |
| Personne | **Individual** | Person 🟡 | Individuo 🟡 | Individuo 🟡 | EN ✅ / autres 🟡 | anglais : **individual** en contexte technique, **person** en texte courant |
| Famille | **Family** | Familie 🟡 | Familia 🟡 | Famiglia 🟡 | EN ✅ / autres 🟡 | `MOD_FAM`, commande de template `family` (anglais) |

## Champs d'un individu

| Français | Anglais | Allemand | Espagnol | Italien | Statut | Note |
|---|---|---|---|---|---|---|
| Prénom | **First name** | Vorname 🟡 | Nombre 🟡 | Nome 🟡 | EN ✅ / autres 🟡 | commande de template `first_name` (anglais) |
| Patronyme / Nom | **Surname** | Nachname 🟡 | Apellido 🟡 | Cognome 🟡 | EN ✅ / autres 🟡 | commande de template `surname` (anglais) — pas "last name" |
| Numéro (d'occurrence) | **Occurrence number** | Nummer 🟡 | Número de ocurrencia 🟡 | Numero di occorrenza 🟡 | 🟡 partout | pas de terme officiel identifié dans aucune langue |
| Nom public | **Public name** | Öffentlicher Name 🟡 | Nombre público 🟡 | Nome pubblico 🟡 | EN ✅ / autres 🟡 | commande de template `public_name` (anglais) |
| Qualificatif | **Qualifier** | Beiname 🟡 | Calificativo 🟡 | Qualificativo 🟡 | EN ✅ / autres 🟡 | commande de template `qualifier` (anglais) |
| Alias | **Alias** | Alias 🟡 | Alias 🟡 | Alias 🟡 | EN ✅ / autres 🟡 | mot identique dans les 4 langues, bonne probabilité que ce soit aussi le terme officiel |
| Sobriquet | **Nickname** | Spitzname 🟡 | Apodo 🟡 | Soprannome 🟡 | 🟡 partout | à clarifier : synonyme de « Qualificatif » ou champ distinct ? |
| Occupation | **Occupation** | Beruf 🟡 | Ocupación 🟡 | Professione 🟡 | EN ✅ / autres 🟡 | |
| Titre (de noblesse) | **Title** | Titel 🟡 | Título 🟡 | Titolo 🟡 | EN ✅ / autres 🟡 | commande de template `titles`, `nob_title` (anglais) |
| Image | **Image** | Bild 🟡 | Imagen 🟡 | Immagine 🟡 | EN ✅ / autres 🟡 | |
| Notes | **Notes** | Notizen 🟡 | Notas 🟡 | Note 🟡 | EN ✅ / autres 🟡 | |
| Sources | **Sources** | Quellen 🟡 | Fuentes 🟡 | Fonti 🟡 | EN ✅ / autres 🟡 | |

## Événements

| Français | Anglais | Allemand | Espagnol | Italien | Statut | Note |
|---|---|---|---|---|---|---|
| Naissance | **Birth** | Geburt 🟡 | Nacimiento 🟡 | Nascita 🟡 | EN ✅ / autres 🟡 | `birth`, `birth_date`, `birth_place`... (anglais) |
| Baptême | **Baptism** | Taufe 🟡 | Bautismo 🟡 | Battesimo 🟡 | EN ✅ / autres 🟡 | `baptism` (anglais) |
| Décès | **Death** | Tod 🟡 | Defunción 🟡 | Decesso 🟡 | EN ✅ / autres 🟡 | `death` (anglais) |
| Inhumation | **Burial** | Beerdigung 🟡 | Entierro 🟡 | Sepoltura 🟡 | EN ✅ / autres 🟡 | `burial` (anglais) |
| Crémation | **Cremation** | Einäscherung 🟡 | Cremación 🟡 | Cremazione 🟡 | EN ✅ / autres 🟡 | `cremated_date`, `cremation_place` (anglais) |
| Mariage | **Marriage** | Heirat 🟡 | Matrimonio 🟡 | Matrimonio 🟡 | EN ✅ / autres 🟡 | `marriage` (anglais) |
| Divorce | **Divorce** | Scheidung 🟡 | Divorcio 🟡 | Divorzio 🟡 | EN ✅ / autres 🟡 | `divorce` (anglais) |
| Témoin | **Witness** | Zeuge 🟡 | Testigo 🟡 | Testimone 🟡 | EN ✅ / autres 🟡 | `witness`, `fwitness` (anglais) |

## Relations familiales

| Français | Anglais | Allemand | Espagnol | Italien | Statut | Note |
|---|---|---|---|---|---|---|
| Conjoint | **Spouse** | Ehepartner 🟡 | Cónyuge 🟡 | Coniuge 🟡 | EN ✅ / autres 🟡 | `spouse` (anglais) |
| Enfant | **Child** | Kind 🟡 | Hijo/a 🟡 | Figlio/a 🟡 | EN ✅ / autres 🟡 | `child` (anglais) |
| Ascendant / Ancêtre | **Ancestor** | Vorfahre 🟡 | Ascendiente 🟡 | Antenato 🟡 | EN ✅ / autres 🟡 | `ancestor` (anglais) |
| Descendant | **Descendant** | Nachkomme 🟡 | Descendiente 🟡 | Discendente 🟡 | EN ✅ / autres 🟡 | `descendant` (anglais) |
| Cousin | **Cousin** | Cousin/e 🟡 | Primo/a 🟡 | Cugino/a 🟡 | EN ✅ / autres 🟡 | `cousin` (anglais) |
| Consanguinité | **Consanguinity** | Blutsverwandtschaft 🟡 | Consanguinidad 🟡 | Consanguineità 🟡 | EN ✅ / autres 🟡 | `consanguinity` (anglais) |

## Actions de mise à jour (formulaires)

| Français | Anglais | Allemand | Espagnol | Italien | Statut | Note |
|---|---|---|---|---|---|---|
| Ajouter | **Add** | Hinzufügen 🟡 | Añadir 🟡 | Aggiungere 🟡 | EN ✅ / autres 🟡 | `ADD_IND`, `ADD_FAM` (anglais) |
| Modifier | **Modify** | Ändern 🟡 | Modificar 🟡 | Modificare 🟡 | EN ✅ / autres 🟡 | `MOD_IND`, `MOD_FAM` (anglais) — pas "Edit" |
| Supprimer | **Delete** | Löschen 🟡 | Eliminar 🟡 | Eliminare 🟡 | EN ✅ / autres 🟡 | `DEL_IND`, `DEL_FAM` (anglais) |
| Fusionner | **Merge** | Zusammenführen 🟡 | Fusionar 🟡 | Unire 🟡 | EN ✅ / autres 🟡 | `MRG_IND`, `MRG_FAM` (anglais) |
| Relier (à une personne existante) | **Link** | Verknüpfen 🟡 | Vincular 🟡 | Collegare 🟡 | 🟡 partout | déduit du contexte, non confirmé même en anglais |
| Créer (une nouvelle personne) | **Create** | Erstellen 🟡 | Crear 🟡 | Creare 🟡 | EN ✅ / autres 🟡 | apparaît dans la liste des commandes de template (`create`, anglais) |

## Fichiers et formats

| Français | Anglais | Allemand | Espagnol | Italien | Statut | Note |
|---|---|---|---|---|---|---|
| Fichier .gw | **`.gw` file** | `.gw`-Datei 🟡 | archivo `.gw` 🟡 | file `.gw` 🟡 | EN ✅ / autres 🟡 | nom de format inchangé partout |
| Fichier .gwf | **`.gwf` file** | `.gwf`-Datei 🟡 | archivo `.gwf` 🟡 | file `.gwf` 🟡 | EN ✅ / autres 🟡 | nom de format inchangé partout |
| Base compilée (.gwb) | **`.gwb` database** | `.gwb`-Datenbank 🟡 | base de datos `.gwb` 🟡 | database `.gwb` 🟡 | EN ✅ / autres 🟡 | nom de format inchangé partout |
| Format wiki (notes) | **Wiki syntax** | Wiki-Syntax 🟡 | sintaxis wiki 🟡 | sintassi wiki 🟡 | 🟡 partout | terme d'usage courant, non vérifié comme "officiel" dans aucune langue |

## Divers

| Français | Anglais | Allemand | Espagnol | Italien | Statut | Note |
|---|---|---|---|---|---|---|
| Numérotation Sosa | **Sosa numbering** | Sosa-Nummerierung 🟡 | numeración Sosa 🟡 | numerazione Sosa 🟡 | EN ✅ / autres 🟡 | `sosa`, `sosa_ref` (anglais) — garder "Sosa" tel quel dans toutes les langues |
| Modules de template | **Templates** | Vorlagen 🟡 | plantillas 🟡 | template 🟡 | EN ✅ / autres 🟡 | l'italien emprunte souvent le mot anglais tel quel en informatique — à vérifier |
| Lexique (traductions UI) | **Lexicon** | Lexikon 🟡 | léxico 🟡 | lessico 🟡 | EN ✅ / autres 🟡 | terme employé dans le man `gwd` (« lexicon », anglais) |

## Comment faire évoluer ce glossaire

Ce fichier vit avec la documentation : chaque fois qu'un terme non couvert
ici apparaît dans une nouvelle page traduite, ajoutez-le avant de continuer
la traduction, avec son statut par langue (✅ confirmé ou 🟡 proposé). Un
terme marqué 🟡 doit être revu dès qu'un mainteneur, un contributeur de
longue date du projet, ou le fichier `hd/lang/lexicon.txt` lui-même peut le
confirmer ou le corriger.
