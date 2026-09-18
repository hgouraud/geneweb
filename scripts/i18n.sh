#!/usr/bin/env bash
# Aide-mémoire des commandes i18n pour le manuel GeneWeb.
# Prérequis (une fois) :
#   cargo install mdbook-i18n-helpers
#   # + les outils GNU gettext : msgmerge, msginit (souvent déjà présents
#   #   sous Linux/macOS ; sous macOS : brew install gettext && brew link gettext --force)
#
# Usage : ./scripts/i18n.sh <commande>
set -euo pipefail
cd "$(dirname "$0")/../doc"

case "${1:-}" in

  extract)
    # Régénère po/messages.pot à partir du contenu actuel de src/.
    # À lancer après CHAQUE modification du français.
    MDBOOK_OUTPUT='{"xgettext": {"pot-file": "messages.pot"}}' mdbook build -d po
    echo "-> doc/po/messages.pot régénéré."
    ;;

  update)
    # Fusionne le .pot à jour dans chaque fichier de traduction existant.
    # Les messages modifiés sont marqués "fuzzy" : ils gardent l'ancienne
    # traduction en attendant une relecture, mais ne seront PAS republiés
    # tels quels (voir "check" ci-dessous).
    "$0" extract
    for po in po/*.po; do
      [ -e "$po" ] || continue
      echo "-> mise à jour de $po"
      msgmerge --update --backup=off "$po" po/messages.pot
    done
    ;;

  new)
    # Démarre une nouvelle langue : ./scripts/i18n.sh new en
    lang="${2:?Usage: i18n.sh new <code_langue>}"
    "$0" extract
    msginit --no-translator -i po/messages.pot -l "$lang" -o "po/$lang.po"
    echo "-> po/$lang.po créé. Traduisez les entrées, puis lancez : ./scripts/i18n.sh build $lang"
    ;;

  check)
    # Liste, pour chaque langue, le nombre de messages non traduits ou
    # marqués "fuzzy" (donc pas encore prêts à publier).
    for po in po/*.po; do
      [ -e "$po" ] || continue
      lang=$(basename "$po" .po)
      untranslated=$(msgattrib --untranslated "$po" 2>/dev/null | grep -c '^msgid' || true)
      fuzzy=$(msgattrib --only-fuzzy "$po" 2>/dev/null | grep -c '^msgid' || true)
      echo "$lang : $untranslated non traduits, $fuzzy à relire (fuzzy)"
    done
    ;;

  build)
    # Construit la version traduite : ./scripts/i18n.sh build en
    lang="${2:?Usage: i18n.sh build <code_langue>}"
    MDBOOK_BOOK__LANGUAGE="$lang" mdbook build -d "book/$lang"
    echo "-> doc/book/$lang généré."
    ;;

  serve)
    # Prévisualise la version traduite en local avec rechargement automatique.
    lang="${2:?Usage: i18n.sh serve <code_langue>}"
    MDBOOK_BOOK__LANGUAGE="$lang" mdbook serve -d "book/$lang"
    ;;

  *)
    echo "Usage: $0 {extract|update|new <lang>|check|build <lang>|serve <lang>}"
    exit 1
    ;;
esac
