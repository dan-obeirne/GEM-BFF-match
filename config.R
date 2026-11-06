# Settings for the GEM <-> BFF comparison. Edit values here; both scripts read this file.

cfg <- list(

  # ---- File locations -------------------------------------------------------
  # The newest file in each folder is used (by the date in the file name).
  gem_dir        = "data/gem",
  bff_dir        = "data/bff",
  exchange_dir   = "data/bff-gem-source-exchange",
  decisions_file = "data/decisions.csv",
  output_dir     = "outputs",
  reviewer       = Sys.getenv("USER"),

  # ---- Capacity thresholds (compared at whole-plant level) -----------------
  # A capacity gap counts as significant according to cap_rule:
  #   "and" = gap > cap_abs_mw AND gap > cap_pct of the larger value
  #   "or"  = either condition
  #   "pct" = only the percentage condition
  #   "abs" = only the MW condition
  cap_pct    = 0.10,
  cap_abs_mw = 20,
  cap_rule   = "and",

  # ---- Location -----------------------------------------------------------
  # Plant centroids further apart than this (km) are flagged.
  max_dist_km = 2,

  # ---- Years ----------------------------------------------------------------
  # Start / retirement year ranges differing by more than this many years are flagged.
  year_tol = 1,
  # When only one side has a year:
  #   "gem_missing" = flag only when BFF has a year and GEM doesn't
  #   "both"        = flag whenever one side is missing
  #   "none"        = only compare when both sides have a year
  one_sided_years = "gem_missing",

  # ---- Scope ----------------------------------------------------------------
  exclude_bff_fuels = c("biofuel"),

  # ---- Match suggestions for unlinked plants --------------------------------
  suggest_link_km  = 5,     # look for candidates within this distance
  suggest_name_sim = 0.6,   # Jaro-Winkler similarity (0-1) for a "strong" suggestion

  # ---- BFF mapping-file comments -------------------------------------------
  # Unmatched plants whose BFF comment matches this regex are treated as already
  # explained and moved to the "Explained absences" tab.
  # Comments NOT matching (e.g. "We found different information", "GEM unit needs
  # correction") stay in the review list and are shown alongside the plant.
  explained_comment_regex = paste(
    "sub-threshold", "before 2023", "not fossil gas", "excluded: oil", "excluded: H2",
    "Below 15MWe", "excluded: retired", "industrial by product", "upgrade of current",
    "duplicated BFF",
    sep = "|"
  ),

  # ---- Vocabulary maps -------------------------------------------------------
  # GEM status -> common status
  gem_status_map = c(
    "cancelled - inferred 4 y" = "cancelled",
    "shelved - inferred 2 y"   = "shelved"
  ),
  # Common status -> status group used for the capacity-by-status comparison.
  # Statuses in the same group are treated as equivalent.
  status_groups = c(
    "operating"        = "operating",
    "construction"     = "construction",
    "pre-construction" = "planned",
    "announced"        = "planned",
    "shelved"          = "shelved",
    "mothballed"       = "mothballed",
    "retired"          = "retired",
    "cancelled"        = "cancelled"
  ),
  # GEM technology -> BFF technology code. Anything mapped to NA is treated as unknown.
  gem_tech_map = c(
    "combined cycle"      = "CCGT",
    "gas turbine"         = "OCGT",
    "steam turbine"       = "ST",
    "internal combustion" = "ICCC",
    "ICCC"                = "ICCC",
    "ISCC"                = "ISCC",
    "AFC"                 = "AFC",
    "unknown"             = NA
  ),
  bff_tech_unknown = c("not found"),
  # BFF country name -> GEM country name
  country_map = c(
    "Bosnia & Herzegovina" = "Bosnia and Herzegovina"
  )
)
