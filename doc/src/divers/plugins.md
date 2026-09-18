# Extensions (plugins)

GeneWeb dispose d'un système de plugins permettant d'étendre ses fonctionnalités sans modifier le cœur du logiciel — par exemple pour ajouter des fonctionnalités propres à une base ou une communauté particulière.

> Source partielle : options `-plugin`/`-plugins` de `gwd`, trouvées dans la page [gwd](https://geneweb.tuxfamily.org/wiki/gwd) du wiki officiel. La page dédiée [plugins](https://geneweb.tuxfamily.org/wiki/plugins) n'a pas encore pu être consultée en détail.

## Charger un plugin

```bash
gwd -plugin <dossier>     # charge un plugin unique
gwd -plugins <dossier>    # charge tous les plugins présents dans ce dossier
```

Deux options associées, à combiner selon le besoin :

- **`-force`** : active le(s) plugin(s) pour toutes les bases du serveur (sans cette option, l'activation par défaut est plus restreinte — périmètre exact à confirmer).
- **`-unsafe`** : autorise le chargement de plugins non vérifiés.

Exemple rencontré dans la documentation du projet :
```bash
gwd -plugins <dossier> -unsafe -force
```

Une variable `plugins` existe aussi côté configuration (accessible en template via `bvar.plugins`, voir [Templates](templates.md#variables-de-configuration-bvar)) — confirmée dans la liste des variables de la page [Expert](https://geneweb.tuxfamily.org/wiki/expert) du wiki, sans détail sur son usage exact pour l'instant.

<!--
TODO — source à consulter : page [plugins](https://geneweb.tuxfamily.org/wiki/plugins) du wiki officiel.
- Structure attendue d'un plugin (fichiers, organisation du dossier)
- Ce que "sans -force" signifie exactement (activation par base plutôt que globale ?)
- Ce qui rend un plugin "vérifié" ou non (d'où l'option -unsafe)
- Plugins fournis en standard avec la distribution, le cas échéant (un dossier `plugins/` existe dans l'arborescence — voir Binaires et sources)
- Configuration associée dans le fichier .gwf (une variable bvar.plugins a été aperçue en passant dans une discussion GitHub — non confirmée ici)
-->
