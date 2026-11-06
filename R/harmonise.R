#' BFF plant ID from a BFF unit ID ("AT-1-2" -> "AT-1")
bff_plant_id <- function(unit_id) {
  stringr::str_extract(unit_id, "^[A-Z]{2}-[0-9]+")
}

#' Map BFF country names onto GEM's names
harmonise_country <- function(x, cfg) {
  dplyr::coalesce(unname(cfg$country_map[x]), x)
}

#' Extract BFF plant IDs referenced in a GEM "Other IDs" string
#'
#' Works for both location strings ("Beyond Fossil Fuels: ES-28") and unit
#' strings ("Beyond Fossil Fuels: ES-28-1"); unit IDs are reduced to their plant.
#'
#' @return List of character vectors, one per input element.
extract_bff_plant_ids <- function(other_ids) {
  m <- stringr::str_match_all(other_ids, "Beyond Fossil Fuels:\\s*([A-Z]{2}-[0-9]+)")
  lapply(m, function(x) unique(stats::na.omit(x[, 2])))
}

#' Normalise plant names for fuzzy matching
normalise_name <- function(x) {
  x <- stringi::stri_trans_general(stringr::str_to_lower(x), "Latin-ASCII")
  x <- stringr::str_remove_all(
    x, "\\b(power station|power plant|kraftwerk|centrale|central|termocentrale|thermal|ccgt|chp|gas|plant|station|cogeneration|heizkraftwerk|hkw|gud)\\b"
  )
  stringr::str_squish(stringr::str_replace_all(x, "[^a-z0-9 ]", " "))
}

#' Collapse unique non-missing values into one display string
collapse_unique <- function(x, sep = " / ") {
  x <- unique(x[!is.na(x) & x != ""])
  if (length(x) == 0) NA_character_ else paste(x, collapse = sep)
}
