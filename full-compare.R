# BFF GEM comparison across key fields using the latest dataset of each.
#
# Produces outputs/full-compare-<timestamp>.xlsx. Fill in the `decision` (and
# optionally `scope`, `apply_to`, `notes`) columns in that workbook; decisions are
# imported automatically the next time either script runs.
#
# Settings (thresholds, distance tolerance, etc.) are in config.R.

suppressPackageStartupMessages(library(tidyverse))
source("config.R")
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

# ---- Read -------------------------------------------------------------------
gem_file  <- latest_file(cfg$gem_dir)
bff_file  <- latest_file(cfg$bff_dir)
map_file  <- latest_file(cfg$exchange_dir, "mapping.*\\.csv$")
note_file <- latest_file(cfg$exchange_dir, "notes.*\\.csv$")

bff_all <- read_bff(bff_file, cfg, drop_excluded = FALSE)
bff     <- filter(bff_all, !main_fuel %in% cfg$exclude_bff_fuels)
gem     <- read_gem(gem_file, cfg, countries = unique(bff_all$country))
mapping <- read_bff_mapping(map_file)
notes   <- read_bff_notes(note_file)

# ---- Compare ----------------------------------------------------------------
res <- run_comparison(gem, bff, mapping, cfg, unique(bff_all$bff_plant_id))
notes_by_cluster <- cluster_notes(notes, res$clusters)

# ---- Decisions ----------------------------------------------------------------
decisions <- import_decisions(cfg)

id_flags <- res$id_issues |>
  transmute(item_key, field = "id_issue", gem_value = gem_id, bff_value = bff_plant_id)
all_flags <- bind_rows(
  mutate(res$flags, sheet = "Divergences"),
  mutate(unmatched_flags(res$bff_only, "bff"), sheet = "BFF-only plants"),
  mutate(unmatched_flags(res$gem_only, "gem"), sheet = "GEM-only plants"),
  mutate(id_flags, sheet = "ID issues")
) |>
  apply_decisions(decisions, cfg)
shown <- filter(all_flags, !suppressed)

# ---- Output ----------------------------------------------------------------------
div_tbl <- divergence_table(filter(shown, sheet == "Divergences"), res$paired, notes_by_cluster)
bff_tbl <- unmatched_table(res$bff_only, "bff", filter(all_flags, sheet == "BFF-only plants"), notes_by_cluster)
gem_tbl <- unmatched_table(res$gem_only, "gem", filter(all_flags, sheet == "GEM-only plants"))
id_tbl  <- res$id_issues |>
  semi_join(filter(shown, sheet == "ID issues"), by = "item_key") |>
  mutate(decision = NA_character_, scope = NA_character_, notes = NA_character_) |>
  relocate(item_key, .after = last_col())
rev_tbl <- reviewed_table(all_flags)

sheet_counts <- tibble(
  tab = c("Divergences (plants)", "BFF-only plants", "GEM-only plants", "ID issues",
          "Explained absences", "Reviewed (hidden by earlier decisions)"),
  rows = c(NROW(div_tbl), NROW(bff_tbl), NROW(gem_tbl), NROW(id_tbl),
           sum(res$bff_only$explained) + sum(res$gem_only$explained), NROW(rev_tbl))
)
summary <- list(
  "Files used" = tibble(source = c("GEM", "BFF", "BFF mapping", "BFF notes"),
                        file = basename(c(gem_file, bff_file, map_file, note_file))),
  "Rows per tab" = sheet_counts,
  "Divergent fields by country (excluding reviewed)" =
    flag_counts(filter(shown, sheet == "Divergences"), res$gem_sum),
  "Link coverage" = res$coverage,
  "Settings (config.R)" = settings_table(cfg)
)

sheets <- list(
  "Divergences" = list(data = div_tbl, highlight = divergence_highlights,
    about = "Matched plants whose key data points diverge beyond the config.R thresholds. Highlighted cells = flagged fields. Fill in decision/scope/notes; apply_to (optional) limits a decision to some fields, e.g. 'capacity, location'."),
  "BFF-only plants" = list(data = bff_tbl,
    about = "BFF plants with no link to GEM. Check the suggested GEM match: if it's right, add the BFF ID to GEM; otherwise consider adding the plant."),
  "GEM-only plants" = list(data = gem_tbl,
    about = "GEM locations with no link to BFF. mapping_comment shows any BFF remark (e.g. 'We found different information')."),
  "ID issues" = list(data = id_tbl,
    about = "Cross-ID problems: BFF IDs GEM should add, links BFF lacks, stale IDs, and clusters that join several plants (labelled by shape; a cluster held together by a one-sided link is usually a wrong link)."),
  "Explained absences" = list(data = explained_table(res$bff_only, res$gem_only),
    about = "Unmatched plants already explained by BFF's mapping comments (config.R: explained_comment_regex). For information only."),
  "Reviewed" = list(data = rev_tbl,
    about = "Items hidden by earlier decisions. Enter a new decision here to change one (e.g. 'investigate' to bring it back).")
)

values <- bind_rows(
  select(all_flags, sheet, item_key, field, gem_value, bff_value),
  filter(all_flags, suppressed) |> transmute(sheet = "Reviewed", item_key, field, gem_value, bff_value)
)

out <- output_path(cfg, "full-compare")
write_review_workbook(out, summary, sheets, values)
message("Wrote ", out)
print(sheet_counts)
