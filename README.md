# GEM ↔ BFF comparison

Compares GEM's GOGPT data with Beyond Fossil Fuels' gasDB **at whole-plant level**, flags only significant differences, and remembers review decisions so dismissed items don't keep coming back.

For how the comparison works, see `project-guide.html` (open it in a browser).

## Quick start (first time)

You need R (4.3 or later) and git. You don't need RStudio: everything below runs in a terminal.

**1. Get the code**

```sh
git clone https://github.com/dan-obeirne/GEM-BFF-match.git
cd GEM-BFF-match
```

**2. Install the R packages** (once):

```sh
Rscript -e 'install.packages(c("tidyverse", "janitor", "openxlsx", "geosphere", "stringdist", "rmarkdown"))'
```

**3. Add the data.** The data files aren't in the repo, so make the folders and put the latest files in them:

```sh
mkdir -p data/gem data/bff data/bff-gem-source-exchange
```

| Folder | File | Where to get it |
|---|---|---|
| `data/gem/` | GOGPT all-units export, e.g. `GOGPT-all-2026-10-26T222507.csv` | GEM's GOGPT database: export all units as CSV |
| `data/bff/` | BFF gasDB units download, named `gasDB_units-YYYY-MM-DD.csv` | BFF's gasDB data browser. Keep the previous release here too, because `bff-update.R` compares the two newest. |
| `data/bff-gem-source-exchange/` | `bff-to-gem-mapping-YYYY-MM-DD.csv`, `bff-to-gem-notes-YYYY-MM-DD.csv` | Exchanged with BFF; export each tab as CSV |

Every file name must contain its date as `YYYY-MM-DD`, because the scripts pick the newest file in each
folder by that date. Rename downloads if needed. `data/decisions.csv` (the review history) is already in the
repo.

**4. Run** from the project folder:

```sh
Rscript full-compare.R        # full comparison           -> outputs/full-compare-<timestamp>.xlsx
Rscript bff-update.R          # after a new BFF release   -> outputs/bff-update-<timestamp>.xlsx
Rscript researcher-export.R   # for the researcher repo   -> outputs/bffcompare_<YYYYMMDD>.csv
open outputs                  # open the folder in Finder (macOS)
```

Each takes under a minute. `outputs/` isn't in git.

**5. Review and share decisions.** Fill in decisions in a workbook (see below) and run any script again.
The decisions are copied into `data/decisions.csv`. Commit and push that file so others get them:

```sh
git pull                      # get others' decisions first
git add data/decisions.csv
git commit -m "Review decisions"
git push
```

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

- **`researcher-export.R`**: findings for the GOGPT researcher repo → `outputs/bffcompare_<YYYYMMDD>.csv`
  - It writes one row per finding: a **disagreement** (a field where GEM and BFF differ) or a **lead** (a plant or unit BFF has and GEM seems not to).
  - It exports only what `full-compare.R` flags and no decision has dismissed.

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
