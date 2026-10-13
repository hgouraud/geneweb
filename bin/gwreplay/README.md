# gwreplay

Replay onto a GeneWeb base the changes recorded by `history_diff` since a
given date.

Typical use: a base was damaged (full disk, corrupted patches…) and has to
be rebuilt from an older `.gw` dump. `gwreplay` reads what wizards changed
since that dump in the damaged base's `history_d` and `history` files, and
reapplies it to the rebuilt base.

`history_diff` must have been enabled (`history_diff=yes` in the `.gwf`)
during the period to recover.

## Bases involved

- **Source**: the base that received the changes (possibly damaged). It is
  only ever read.
- **Target** (`-target`): the base rebuilt from the dump. It is only written
  with `-write`. Always work on a copy.

## Stages

1. **Inventory** (always): scans `history_d` and the `history` log for the
   window and reports the persons and families changed, the references
   (witnesses, children, relation parents…) that cannot be resolved to a
   person key, duplicate witnesses and unreadable history files.
2. **Plan** (`-target BASE`, or `-target BASE -dry-run`): maps every change
   onto the target by person key and couple, and reports what would be
   modified, renamed or created, which families are blocked, and which
   references are missing in the target. Nothing is written.
3. **Write** (`-target BASE -write`): applies the plan and commits once.
4. **Verify** (`-target BASE -verify`): checks, field by field, that the
   last state recorded in the window is present in BASE. Read-only. BASE may
   be the source itself.

## Options

| Option | Meaning |
| :- | :- |
| `-bd DIR` | Bases folder (relative base names are looked up there) |
| `-since DATE` | Start of the window, `YYYY-MM-DD[ HH:MM:SS]` (required) |
| `-until DATE` | End of the window (default: no end) |
| `-resolve` | Also read the source base to resolve keys of persons never edited in the window and couples of families with one spouse in the history |
| `-target BASE` | Base to plan, write or verify against |
| `-dry-run` | With `-target`: show the plan only (default) |
| `-write` | With `-target`: apply the plan |
| `-yes` | With `-write`: do not ask before rewriting links to renamed persons |
| `-verify` | With `-target`: verify instead of planning |
| `-ok` | With `-verify`: leave out the records found OK (summary counts are kept) |
| `-history_d DIR` | `history_d` folder (default: `<bd>/BASE.gwb/history_d`) |
| `-history FILE` | History log (default: `<bd>/BASE.gwb/history`) |
| `-o DIR` | Report folder (default: `gwreplay_report`) |
| `-verbose` | Detailed listings on screen |

## Recovery procedure

```sh
# 0. Stop edits on the damaged base; copy it with history, history_d,
#    notes_d and wiznotes.

# 1. Rebuild the target from the last good dump
cd BASES && gwc -o roglo_restored roglo-2026-10-06.gw
update_nldb -bd BASES roglo_restored     # needed for link rewriting

# 2. Inventory and plan
gwreplay -bd BASES -since 2026-10-06 -resolve -verbose \
    -target roglo_restored roglo

# 3. Apply on a copy
cp -R BASES/roglo_restored.gwb BASES/roglo_try.gwb
gwreplay -bd BASES -since 2026-10-06 -resolve -target roglo_try -write roglo

# 4. Repair links and consanguinity
gwfixbase -bd BASES roglo_try
consang -bd BASES roglo_try

# 5. Check
gwreplay -bd BASES -since 2026-10-06 -resolve -target roglo_try -verify roglo
```

## What `-write` does

1. Creates the persons missing in the target (bare records first, so that
   every reference can be resolved).
2. Writes each changed person in its latest recorded state, with references
   translated to target persons. Unresolved references are dropped and
   logged; duplicate witnesses are removed.
3. Writes each changed family (modified when the couple already has a family
   in the target, created otherwise), with unions, children's parents and
   descendants updated. A child attached to another family is moved.
4. Re-indexes the notes links of everything written. For persons renamed by
   the replay, after confirmation (`[Y/n]`, or `-yes`), rewrites the links
   to their old key in person, family, base and wizard notes.
5. Commits once. The target is locked during the run.

## Reports

Written in the `-o` folder:

| File | Content |
| :- | :- |
| `summary.txt` | Inventory summary |
| `persons.tsv`, `families.tsv` | Changed records in the window |
| `log.tsv` | `history` log lines in the window |
| `unresolved.tsv` | References without a known key, with the records using them |
| `truncated.tsv` | History files that could not be read to the end |
| `base_lookup.tsv` | Lookups in the source base (`-resolve`); its last line names a record that crashed the process |
| `plan.tsv`, `plan_summary.txt` | The plan |
| `apply.log` | Every action and dropped reference of `-write` |
| `verify.tsv`, `verify_summary.txt` | Verification results, with `expected -> actual` for text fields |

## Limitations

- **Deletions and merges** (`dp`, `df`, `fp`, `ff` in the log) are not
  recorded in `history_d`. They are listed in the plan, to be handled by
  hand.
- **Notes** of the base (`notes`, `notes_d`, `wiznotes`) are not in
  `history_d`: copy the files changed in the window (`mn` lines in the log).
- **Changes GeneWeb makes without a history record** are not replayed and
  show up in `-verify`: a neuter spouse given a sex when a family is saved
  (`updateFamOk.ml`), links rewritten in other persons' notes after a rename
  (`notes.ml`).
- `related` links and consanguinity are not computed: run `gwfixbase` and
  `consang` afterwards.
- A history record that cannot be read (truncated, or written by another
  version of GeneWeb) stops the reading of that file; the records after it
  are lost and the file is listed in `truncated.tsv`.
