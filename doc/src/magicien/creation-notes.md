# Notes diverses

## Notes personnelles

Chaque personne dispose d'un champ **Notes**, destiné au texte libre (anecdotes, transcriptions de documents, informations qui ne correspondent à aucun champ structuré). Deux points à connaître :

- À l'affichage, le texte est rendu **en continu** : les sauts de ligne présents dans le formulaire ne produisent pas de passage à la ligne visuel. Pour forcer un retour à la ligne dans le rendu, il faut insérer une entité HTML de saut de ligne (`<br>`) à l'endroit voulu.
- Les notes acceptent la syntaxe wiki décrite dans [Le format wiki](../divers/format-wiki.md) — liens internes vers d'autres fiches, HTML, certaines variables interprétées à l'affichage.

## Distinction Notes / Sources

Le champ **Sources** (voir [Signification des divers champs](creation-champs.md)) est pensé pour une référence courte à l'origine d'une information (nom d'un document, sigle, code personnel), pas pour une transcription complète. Une copie longue d'un acte d'état civil, par exemple, relève du champ **Notes**, pas du champ **Sources**.

## Notes de mariage

Le formulaire famille propose des champs Notes et Sources équivalents, mais rattachés à l'union elle-même plutôt qu'à un individu (par exemple, une note sur les circonstances du mariage plutôt que sur l'un des deux conjoints).

## Notes générales de la base

<!-- TODO : où et comment éditer les notes générales de la base (distinctes des notes individuelles), accessibles depuis la page d'accueil -->

## Notes de magicien

Chaque magicien peut disposer d'une page personnelle décrivant son activité et son parcours (bloc `wizard-note` du [format .gw](../divers/format-gw.md#note-de-magicien-wizard-note)), qu'il peut éditer lui-même — au format HTML libre, enrichi de la syntaxe wiki disponible dans le contexte de GeneWeb.

Cette fonctionnalité n'est active que si le paramètre `authorized_wizards_notes` du [fichier .gwf](../geneweb/base-gwf.md) est à `yes`, et que la liste des magiciens est connue via le paramètre `wizard_passwd_file` (voir [Sécurité et contrôle d'accès](../divers/securite.md)). La page d'accueil propose alors, en pied de page, un lien vers la liste de ces notes.

Commandes d'URL associées :

| Commande | Effet |
|---|---|
| `m=WIZNOTES` | Liste des notes de magicien, triée par ordre alphabétique par défaut |
| `m=WIZNOTES;o=A` | Liste triée par ordre alphabétique (explicite) |
| `m=WIZNOTES;o=H` | Liste triée par ordre chronologique |
| `m=WIZNOTES;f=nom_magicien` | Affiche la note d'un magicien précis |
| `m=VIEW_WIZNOTES;v=0;f=nom_magicien` | Affiche le code source HTML de cette note (réservé aux magiciens) |

Les anciens magiciens dont la note est toujours présente dans la base apparaissent en fin de liste alphabétique. L'ordre de tri lui-même est gouverné par la syntaxe du fichier de mots de passe (`wizard_passwd_file`).

## Notes liées à un événement particulier

<!-- TODO : depuis GeneWeb 7, les événements personnels et familiaux disposent chacun de leur propre note/source (voir les champs e_note, e_src évoqués dans Templates) — à documenter une fois le formulaire de saisie d'événement examiné en détail -->
