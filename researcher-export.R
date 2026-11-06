# Export GEM vs BFF findings for the GOGPT researcher repo.
#
# Writes outputs/<researcher_tool_name>_<YYYYMMDD>.csv (see config.R): one row per
# finding, either a "disagreement" (GEM and BFF differ on a field) or a "lead"
# (BFF has a plant or unit GEM seems not to). Only findings that full-compare.R
# flags and that no review decision has dismissed are exported, so record
# decisions first if you want something left out.
#
# Run from the project folder:  Rscript researcher-export.R

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
raw_csv <- function(path) read_csv(path, show_col_types = FALSE, col_types = cols(.default = col_character()))
gem_raw <- raw_csv(gem_file)
bff_raw <- raw_csv(bff_file)

# ---- Compare and apply decisions ----------------------------------------------
res <- run_comparison(gem, bff, mapping, cfg, unique(bff_all$bff_plant_id))
decisions <- import_decisions(cfg)
flags <- bind_rows(
  mutate(res$flags, sheet = "Divergences"),
  mutate(unmatched_flags(res$bff_only, "bff"), sheet = "BFF-only plants")
) |>
  apply_decisions(decisions, cfg)

# ---- Export -----------------------------------------------------------------
out <- researcher_export(res, flags, gem, bff, gem_raw, bff_raw, mapping, notes, cfg,
                         bff_date = file_date(bff_file))

path <- file.path(cfg$output_dir, sprintf("%s_%s.csv", cfg$researcher_tool_name, format(Sys.Date(), "%Y%m%d")))
dir.create(cfg$output_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(out, path, na = "")
message("Wrote ", path)
print(count(out, record_type, field))
