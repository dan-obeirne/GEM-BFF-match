# GEM ↔ BFF comparison

Compares GEM's GOGPT data with Beyond Fossil Fuels' gasDB **at whole-plant level**, flags only significant differences, and remembers review decisions so dismissed items don't keep coming back.

## Data
| Folder | Put here | Used |
|---|---|---|
| `data/gem/` | GOGPT-all download | newest file |
| `data/bff/` | gasDB units downloads (keep old ones) | newest; `bff-update.R` also uses the previous one |
| `data/bff-gem-source-exchange/` | `bff-to-gem-mapping-*.csv`, `bff-to-gem-notes-*.csv` | newest of each |

Files are ordered by the `YYYY-MM-DD` date in their name.

## Scripts
- **`full-compare.R`** — overall picture → `outputs/full-compare-<timestamp>.xlsx`
  - **Divergences**: matched plants whose key fields differ beyond the thresholds.
  - **BFF-only** / **GEM-only plants**: unmatched plants, each with a suggested match.
  - **ID issues**: cross-IDs to add or fix.
  - **Explained absences**: unmatched plants that BFF's mapping comments already explain.
  - **Reviewed**: items hidden by earlier decisions.
- **`bff-update.R`** — run after a new BFF release → `outputs/bff-update-<timestamp>.xlsx`
  - It looks at BFF units changed since the previous release (by `Last update`), plus new and removed units.
  - It keeps only changes that GEM's latest data doesn't already reflect.
  - Each row shows what BFF changed and BFF's sources.

Run them from this folder: `Rscript full-compare.R`, or `source()` them in RStudio.

## Settings
Everything you can tune is in `config.R`:
- capacity thresholds (`cap_pct`, `cap_abs_mw`, `cap_rule`)
- location tolerance (`max_dist_km`)
- year tolerance and one-sided year handling
- status, technology and country maps
- which BFF comments count as "explained"

## Reviewing and decisions
In any review tab, fill in:
- **decision**:
  - `agree_to_differ`: a genuine difference we're keeping.
  - `gem_updated`: GEM has been changed to align.
  - `bff_likely_wrong`: worth telling BFF.
  - `investigate`: still open, so it keeps showing.
- **scope**:
  - `until_changed` (default): stays hidden while both values stay within the thresholds of what you reviewed. A real change on either side brings it back as "values changed since review", with your note.
  - `permanent`: never flag this field for this plant again.
- **apply_to** (optional): limit the decision to some of the row's flags, e.g. `capacity, location`.
- **notes**: free text.

Save the workbook. The next run of either script imports decisions from every workbook in `outputs/` into `data/decisions.csv`. Keep that file in git, since it is the review history. To undo a decision, change it in the workbook, or enter a new one on the **Reviewed** tab.

## Code
Functions are in `R/`, written package-style so they can later move to an R package:
- `read.R`: read and harmonise the files
- `link.R`: ID links, plant clusters, match suggestions
- `summarise.R`, `compare.R`: plant summaries and comparison
- `decisions.R`: import and apply decisions
- `bff_changes.R`: release changes
- `output.R`: workbook writer
