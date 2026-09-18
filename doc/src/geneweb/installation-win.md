# Installation sous Windows

Cette page suppose que vous disposez déjà d'une distribution GeneWeb, précompilée ou compilée depuis les sources (voir [Binaires et sources](installation-binaires-sources.md)).

## Premier lancement

1. Extrayez l'archive `.zip` téléchargée à l'emplacement de votre choix.
2. Double-cliquez sur `START.htm`.
3. Ouvrez votre navigateur à l'adresse [http://localhost:2317](http://localhost:2317).

## Aller plus loin

<!--
TODO :
- Alternative de lancement via gwd.bat / gwsetup.bat (mentionnés dans la structure de distribution)
- Éventuelles alertes du pare-feu Windows au premier lancement, et comment les autoriser
- Compilation depuis les sources sous Windows : la chaîne de build historique passait par Cygwin/MinGW pour opam,
  ce qui peut être source de complications (voir issues GitHub liées à `make cold`). À vérifier si `configure.ml`
  simplifie ce point sur les versions récentes de master, avant de documenter une procédure de compilation native.
- Installateur .iss (Inno Setup) : le dépôt contient un fichier `geneweb.iss` — préciser s'il produit un installateur
  officiel distribué avec les releases
-->
