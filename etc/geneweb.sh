#!/bin/bash
# kill previous instances of gwsetup and gwd
# start gwsetup and gwd
# minimize window
# open start page

set -euo pipefail

# Determine language based on LANG environment variable
detect_language() {
    case "${LANG:-}" in
        de*) echo "de" ;;
        es*) echo "es" ;;
        fr*) echo "fr" ;;
        it*) echo "it" ;;
        lv*) echo "lv" ;;
        sv*) echo "sv" ;;
        *)   echo "en" ;;
    esac
}

# Localized messages
msg() {
    local key="$1"
    case "$key" in
        start_gwsetup)
            [[ "$LANG" = "fr" ]] && echo "Démarrage de gwsetup..." || echo "Starting gwsetup..."
            ;;
        start_gwd)
            [[ "$LANG" = "fr" ]] && echo "Démarrage de gwd..." || echo "Starting gwd..."
            ;;
        failed_gwsetup)
            [[ "$LANG" = "fr" ]] && echo "Échec gwsetup" || echo "Failed gwsetup"
            ;;
        failed_gwd)
            [[ "$LANG" = "fr" ]] && echo "Échec gwd" || echo "Failed gwd"
            ;;
        keep_open)
            if [[ "$LANG" = "fr" ]]; then
                echo "Gardez cette fenêtre ouverte tant que"
                echo "vous voulez utiliser GeneWeb dans votre navigateur"
            else
                echo "Keep this window open while you"
                echo "are using GeneWeb on your browser"
            fi
            ;;
    esac
}

# Kill process by name pattern
kill_process() {
    local pattern="$1"
    local pids
    pids=$(pgrep -f "$pattern" 2>/dev/null || true)
    if [[ -n "$pids" ]]; then
        echo "$pids" | xargs kill 2>/dev/null || true
        sleep 0.5
    fi
}

# Rotate log file
rotate_log() {
    local logfile="$1"
    if [[ -f "$logfile" ]]; then
        mv "$logfile" "${logfile}.old"
    fi
}

# Check if process started successfully
check_process() {
    local pattern="$1"
    local process_name="$2"
    sleep 1
    if ! pgrep -f "$pattern" >/dev/null 2>&1; then
        msg "❌ failed_$process_name"
        cat
        exit 1
    fi
}

# Main execution
main() {
    # Setup paths
    local SCRIPT_DIR
    SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
    local BASES_DIR="$SCRIPT_DIR/bases"
    export LANG=$(detect_language)
    # Stop any running instances
    kill_process '/gwd'
    kill_process '/gwsetup'

    # Test for gmplib
# Liste des bibliothèques requises
REQUIRED_LIBS=("libgmp" "libpcre2")

# Fonction pour tester la présence d'une bibliothèque
check_lib() {
    local lib_name="$1"
    local binaries=("./gwd" "./gwsetup")
    if command -v otool &> /dev/null; then
        # macOS : vérifier si gwd/gwsetup peuvent trouver la lib
        for bin in "${binaries[@]}"; do
            [ -f "$bin" ] && otool -L "$bin" 2>/dev/null | grep -q "$lib_name" && return 0
        done
    elif command -v ldd &> /dev/null; then
        # Linux : vérifier les dépendances
        for bin in "${binaries[@]}"; do
            [ -f "$bin" ] && ldd "$bin" 2>/dev/null | grep -q "$lib_name" && return 0
        done
    else
        # Pas d'outil de vérification disponible
        echo "⚠️  Impossible de vérifier les dépendances (otool/ldd manquant)"
        return 0
    fi
    return 1
}

# Fonction pour obtenir les instructions d'installation
get_install_instructions() {
    local lib_name="$1"
    case "$lib_name" in
        libgmp)
            if [[ "$OSTYPE" == "darwin"* ]]; then
                echo "  brew install gmp"
            else
                echo "  sudo apt-get install libgmp10  # Debian/Ubuntu"
                echo "  sudo yum install gmp            # RedHat/CentOS"
            fi
            ;;
        libpcre2)
            echo "Required if you intent to use ged2gwb"
            if [[ "$OSTYPE" == "darwin"* ]]; then
                echo "  brew install pcre2"
            else
                echo "  sudo apt-get install libpcre2-8-0  # Debian/Ubuntu"
                echo "  sudo yum install pcre2              # RedHat/CentOS"
            fi
            ;;
        *)
            echo "  Bibliothèque inconnue : $lib_name"
            ;;
    esac
}

# Vérifier toutes les bibliothèques requises
missing_libs=()
for lib in "${REQUIRED_LIBS[@]}"; do
    if ! check_lib "$lib"; then
        missing_libs+=("$lib")
    fi
done

# Afficher les erreurs si des bibliothèques manquent
if [ ${#missing_libs[@]} -gt 0 ]; then
    echo "❌ Erreur : bibliothèques manquantes"
    echo ""
    for lib in "${missing_libs[@]}"; do
        echo "📦 $lib"
        get_install_instructions "$lib"
        echo ""
    done
    exit 1
fi

echo "✅ Toutes les bibliothèques requises sont présentes"
    # Prepare directories and logs
    mkdir -p "$BASES_DIR"
    cd "$BASES_DIR"
    rotate_log "gwsetup.log"
    rotate_log "gwd.log"

    # Start gwsetup
    msg "start_gwsetup"
    "$SCRIPT_DIR/gw/gwsetup" -gd "$SCRIPT_DIR/gw" -lang "$LANG" > "$SCRIPT_DIR/gw/gwsetup.log" 2>&1 &
    check_process '/gwsetup' "gwsetup"

    # Start gwd
    msg "start_gwd"
    "$SCRIPT_DIR/gw/gwd" -bd "$BASES_DIR" -hd "$SCRIPT_DIR/gw" > "$SCRIPT_DIR/gw/gwd.log" 2>&1 &
    check_process '/gwd' "gwd"

    # Success message
    echo
    msg "keep_open"

    # Open browser and minimize terminal
    open "$SCRIPT_DIR/START.htm"
    osascript -e 'tell application "Terminal" to set miniaturized of first window whose name contains "GeneWeb" to true' 2>/dev/null || true

    # Keep script running
    cat
}

main "$@"