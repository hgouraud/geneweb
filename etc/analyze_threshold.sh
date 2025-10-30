#!/bin/bash
# analyze_threshold.sh - Analyse des seuils pour bigrammes/trigrammes

THRESHOLD=$1

if [ -z "$THRESHOLD" ]; then
    echo "Usage: $0 <threshold>"
    echo "Example: $0 1000"
    exit 1
fi

analyze_file() {
    local STATS_FILE=$1
    local LABEL=$2
    
    if [ ! -f "$STATS_FILE" ]; then
        echo "File not found: $STATS_FILE"
        return
    fi
    
    echo ""
    echo "═══════════════════════════════════════════════════════════"
    echo "  $LABEL"
    echo "═══════════════════════════════════════════════════════════"
    echo ""
    
    # Compter total
    TOTAL=$(wc -l < "$STATS_FILE")
    echo "Total ${LABEL,,}: $TOTAL"
    
    # Compter gardés
    KEPT=$(awk -v t="$THRESHOLD" '$2 >= t' "$STATS_FILE" | wc -l)
    echo "Kept (>= $THRESHOLD): $KEPT"
    
    # Réduction
    if [ "$TOTAL" -gt 0 ]; then
        REDUCTION=$(awk -v total="$TOTAL" -v kept="$KEPT" \
            'BEGIN { printf "%.2f", 100 * (total - kept) / total }')
        echo "Reduction: ${REDUCTION}%"
    fi
    
    # Estimation taille index
    SIZE_KB=$(awk -v kept="$KEPT" 'BEGIN { printf "%.1f", kept * 16 / 1024 }')
    echo "Estimated index size increase: ~${SIZE_KB} KB"
    
    echo ""
    echo "Top 20 most frequent:"
    echo "-----------------------------"
    sort -t$'\t' -k2 -rn "$STATS_FILE" | head -20 | \
        awk -F'\t' '{ printf "%3d. %-30s : %7d\n", NR, $1, $2 }'
    
    echo ""
    echo "Distribution by frequency range:"
    echo "--------------------------------"
    awk '{print $2}' "$STATS_FILE" | awk '
      $1 >= 10000 { range1++ }
      $1 >= 1000 && $1 < 10000 { range2++ }
      $1 >= 100 && $1 < 1000 { range3++ }
      $1 >= 10 && $1 < 100 { range4++ }
      $1 >= 5 && $1 < 10 { range5++ }
      $1 >= 2 && $1 < 5 { range6++ }
      $1 == 1 { range7++ }
      END {
        printf "  >= 10,000 : %6d entries\n", range1+0
        printf "  1,000-9,999 : %6d entries\n", range2+0
        printf "  100-999 : %6d entries\n", range3+0
        printf "  10-99 : %6d entries\n", range4+0
        printf "  5-9 : %6d entries\n", range5+0
        printf "  2-4 : %6d entries\n", range6+0
        printf "  1 : %6d entries\n", range7+0
      }
    '
}

echo "Analyzing threshold: $THRESHOLD"
echo "═══════════════════════════════════════════════════════════"

# Analyser les bigrammes
analyze_file "bigrams_stats.txt" "BIGRAMS"

# Analyser les trigrammes
analyze_file "trigrams_stats.txt" "TRIGRAMS"

echo ""
echo "═══════════════════════════════════════════════════════════"
echo "RECOMMENDATION:"
if [ -f "bigrams_stats.txt" ]; then
    KEPT=$(awk -v t="$THRESHOLD" '$2 >= t' "bigrams_stats.txt" | wc -l)
    if [ "$KEPT" -lt 100 ]; then
        echo "  Threshold too high - very few entries kept"
        echo "  Consider lowering to $(($THRESHOLD / 2)) or $(($THRESHOLD / 10))"
    elif [ "$KEPT" -gt 10000 ]; then
        echo "  Threshold too low - many entries kept"
        echo "  Consider raising to $(($THRESHOLD * 2)) or $(($THRESHOLD * 10))"
    else
        echo "  Threshold seems reasonable ($KEPT bigrams kept)"
        echo "  Index size impact: minimal"
    fi
fi
echo "═══════════════════════════════════════════════════════════"