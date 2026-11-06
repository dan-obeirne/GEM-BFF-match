# When a new BFF download is made available, run this script to find where the BFF
# update has created a divergence between the two datasets that GEM should check.
#
# 1. Save the new BFF download in data/bff/ (keep the previous one there too), and
#    the matching bff-to-gem mapping / notes files in data/bff-gem-source-exchange/.
# 2. Run this script. It compares the newest BFF file with the second newest:
#    - changed units: BFF "Last update" later than anything in the previous release
#    - new units: IDs not in the previous release
#    - removed units: IDs no longer in the release
# 3. Only changes that GEM's latest data does not already reflect are flagged.
#
# Produces outputs/bff-update-<timestamp>.xlsx. Decisions entered there are
# shared with full-compare.R. Settings are in config.R.

suppressPackageStartupMessages(library(tidyverse))
source("config.R")
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

# ---- Read -------------------------------------------------------------------
bff_files <- dated_files(cfg$bff_dir)
if (length(bff_files) < 2) stop("Need at least two BFF downloads in ", cfg$bff_dir, " (new + previous).")
gem_file  <- latest_file(cfg$gem_dir)
map_file  <- latest_file(cfg$exchange_dir, "mapping.*\\.csv$")
note_file <- latest_file(cfg$exchange_dir, "notes.*\\.csv$")

new_all  <- read_bff(bff_files[1], cfg, drop_excluded = FALSE)
prev_all <- read_bff(bff_files[2], cfg, drop_excluded = FALSE)
new_bff  <- filter(new_all, !main_fuel %in% cfg$exclude_bff_fuels)
prev_bff <- filter(prev_all, !main_fuel %in% cfg$exclude_bff_fuels)
gem      <- read_gem(gem_file, cfg, countries = unique(c(new_all$country, prev_all$country)))
mapping  <- read_bff_mapping(map_file)
notes    <- read_bff_notes(note_file)

# ---- What did BFF change? -------------------------------------------------------
chg <- bff_release_changes(new_all, prev_all)
chg$units <- filter(chg$units, !main_fuel %in% cfg$exclude_bff_fuels)
message("Previous release: ", basename(bff_files[2]), " (latest Last update ", chg$cutoff, ")")
message(sum(chg$units$change_type == "changed"), " changed, ", sum(chg$units$change_type == "new"),
        " new, ", nrow(chg$removed), " removed BFF units")

# ---- Compare GEM against the new and the previous release -------------------------
res  <- run_comparison(gem, new_bff, mapping, cfg, unique(new_all$bff_plant_id))
prev <- run_comparison(gem, prev_bff, mapping, cfg, unique(prev_all$bff_plant_id))

changes_by_cluster <- release_changes_by_cluster(chg$units, res$clusters)
relevant <- chg$units |>
  inner_join(filter(res$clusters, side == "bff"), by = c(bff_plant_id = "id")) |>
  mutate(fields = map(coalesce(fields_changed, ""), changed_to_compare_fields)) |>
  select(cluster_key, fields) |>
  unnest(fields) |>
  distinct(item_key = cluster_key, field = fields)

# A divergence is "from this update" when it touches a field BFF changed, and it
# either wasn't flagged against the previous release or BFF's value has moved.
update_flags <- res$flags |>
  semi_join(relevant, by = c("item_key", "field")) |>
  anti_join(prev$flags, by = c("item_key", "field", "bff_value"))

new_plants <- res$bff_only |>
  filter(!explained, cluster_key %in% changes_by_cluster$cluster_key)
removed_tbl <- removed_still_in_gem(chg$removed, new_all, gem, prev$clusters)

# ---- Decisions ----------------------------------------------------------------
decisions <- import_decisions(cfg)
removed_flags <- if (is.null(removed_tbl)) NULL else
  transmute(removed_tbl, item_key = paste("removed", bff_unit_id), field = "bff_removed",
            gem_value = gem_status, bff_value = paste(bff_status, format_mw(bff_capacity)))
all_flags <- bind_rows(
  mutate(update_flags, sheet = "Divergences"),
  mutate(unmatched_flags(new_plants, "bff"), sheet = "New BFF plants"),
  if (!is.null(removed_flags)) mutate(removed_flags, sheet = "Removed BFF units")
) |>
  apply_decisions(decisions, cfg)
shown <- filter(all_flags, !suppressed)

# ---- Output ----------------------------------------------------------------------
notes_since <- cluster_notes(notes, res$clusters, since = chg$cutoff, all = TRUE)
paired <- left_join(res$paired, changes_by_cluster, by = "cluster_key")
div_tbl <- divergence_table(filter(shown, sheet == "Divergences"), paired, notes_since)
new_tbl <- unmatched_table(new_plants |> left_join(changes_by_cluster, by = "cluster_key"), "bff",
                           filter(all_flags, sheet == "New BFF plants"), notes_since)
rem_tbl <- if (is.null(removed_tbl)) NULL else removed_tbl |>
  mutate(item_key = paste("removed", bff_unit_id), flags = "bff_removed") |>
  inner_join(filter(shown, sheet == "Removed BFF units") |> select(item_key, review_state, prev_decision, prev_note),
             by = "item_key") |>
  mutate(decision = NA_character_, scope = NA_character_, notes = NA_character_) |>
  select(country, flags, review_state, prev_decision, prev_note, bff_unit_id, bff_plant_id, bff_plant_name,
         bff_unit_name, bff_capacity, bff_status, bff_last_update, plant_removed,
         gem_location_id, gem_unit_ids, gem_plant_name, gem_capacity, gem_status,
         decision, scope, notes, item_key)

sheet_counts <- tibble(
  tab = c("Divergences (plants)", "New BFF plants", "Removed BFF units"),
  rows = c(NROW(div_tbl), NROW(new_tbl), NROW(rem_tbl))
)
summary <- list(
  "Files used" = tibble(source = c("GEM", "BFF (new)", "BFF (previous)", "BFF mapping", "BFF notes"),
                        file = basename(c(gem_file, bff_files[1], bff_files[2], map_file, note_file))),
  "BFF release changes" = tibble(
    measure = c("Changed units (Last update after previous release)", "New units", "Removed units",
                "Previous release latest Last update"),
    value = c(sum(chg$units$change_type == "changed"), sum(chg$units$change_type == "new"),
              nrow(chg$removed), as.character(chg$cutoff))),
  "Rows per tab (changes GEM does not reflect yet)" = sheet_counts,
  "Divergent fields by country (excluding reviewed)" =
    flag_counts(filter(shown, sheet == "Divergences"), res$gem_sum),
  "Settings (config.R)" = settings_table(cfg)
)
sheets <- list(
  "Divergences" = list(data = div_tbl, highlight = divergence_highlights,
    about = "Plants where this BFF release changed a key field and GEM's latest data still differs. bff_fields_changed / bff_note_* show what BFF changed and their sources."),
  "New BFF plants" = list(data = new_tbl,
    about = "Plants new in this BFF release with no GEM link. Check the suggested GEM match before adding."),
  "Removed BFF units" = list(data = rem_tbl,
    about = "Units BFF dropped in this release that GEM still lists as active (not retired/cancelled).")
)
values <- select(all_flags, sheet, item_key, field, gem_value, bff_value)

out <- output_path(cfg, "bff-update")
write_review_workbook(out, summary, sheets, values)
message("Wrote ", out)
print(sheet_counts)
