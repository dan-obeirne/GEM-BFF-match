#' List files in a folder, newest first
#'
#' Files are ordered by the first YYYY-MM-DD date in the file name, falling back
#' to modification time when the name has no date.
#'
#' @param dir Folder to search.
#' @param pattern Regex the file name must match.
#' @return Character vector of paths, newest first.
dated_files <- function(dir, pattern = "\\.csv$") {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  files <- files[!startsWith(basename(files), "~$")]
  if (length(files) == 0) stop("No files matching '", pattern, "' in ", dir)
  name_date <- as.Date(stringr::str_extract(basename(files), "\\d{4}-\\d{2}-\\d{2}"))
  file_date <- dplyr::coalesce(name_date, as.Date(file.mtime(files)))
  files[order(file_date, decreasing = TRUE)]
}

#' Newest file in a folder (see [dated_files()])
latest_file <- function(dir, pattern = "\\.csv$") dated_files(dir, pattern)[1]

#' Date of a data file, taken from its name (or modification time)
file_date <- function(path) {
  d <- as.Date(stringr::str_extract(basename(path), "\\d{4}-\\d{2}-\\d{2}"))
  if (is.na(d)) d <- as.Date(file.mtime(path))
  d
}

#' Read a BFF gasDB units download into the common unit schema
#'
#' Handles both the current header format ("Main fuel", "Country sub-region", no
#' GEM ID) and the older one ("fuel main", "gem unit id").
#'
#' @param path Path to the CSV.
#' @param cfg Settings list from config.R.
#' @param drop_excluded Drop units whose main fuel is in `cfg$exclude_bff_fuels`.
#' @return Tibble, one row per BFF unit.
read_bff <- function(path, cfg, drop_excluded = TRUE) {
  raw <- readr::read_csv(path, show_col_types = FALSE, guess_max = 1e5,
                         col_types = readr::cols(.default = readr::col_character())) |>
    janitor::clean_names()
  raw <- dplyr::rename(raw, dplyr::any_of(c(main_fuel = "fuel_main")))
  if (!"gem_unit_id" %in% names(raw)) raw$gem_unit_id <- NA_character_
  if (!"country_sub_region" %in% names(raw)) raw$country_sub_region <- NA_character_

  units <- raw |>
    dplyr::transmute(
      bff_unit_id,
      bff_plant_id    = bff_plant_id(bff_unit_id),
      country         = harmonise_country(country, cfg),
      plant_name, unit_name,
      capacity_mw     = readr::parse_number(capacity_mw),
      status          = stringr::str_to_lower(status),
      status_group    = unname(cfg$status_groups[status]),
      technology      = dplyr::if_else(technology %in% cfg$bff_tech_unknown, NA_character_, technology),
      main_fuel,
      start_year      = readr::parse_number(start_year),
      retire_year     = dplyr::coalesce(readr::parse_number(retirement_year),
                                        readr::parse_number(planned_retirement_year)),
      lat             = readr::parse_number(latitude),
      lon             = readr::parse_number(longitude),
      city,
      province        = country_sub_region,
      last_update     = as.Date(last_update),
      gem_unit_id_bff = gem_unit_id
    )
  if (drop_excluded) units <- dplyr::filter(units, !main_fuel %in% cfg$exclude_bff_fuels)
  units
}

#' Read a GEM GOGPT download into the common unit schema
#'
#' @param path Path to the CSV.
#' @param cfg Settings list from config.R.
#' @param countries Optional vector of countries to keep (e.g. those BFF covers).
#' @return Tibble, one row per GEM unit.
read_gem <- function(path, cfg, countries = NULL) {
  raw <- readr::read_csv(path, show_col_types = FALSE, guess_max = 1e5,
                         col_types = readr::cols(.default = readr::col_character())) |>
    janitor::clean_names()

  units <- raw |>
    dplyr::transmute(
      gem_unit_id, gem_location_id,
      country      = country_area,
      plant_name, unit_name,
      capacity_mw  = readr::parse_number(capacity_mw),
      status       = dplyr::coalesce(unname(cfg$gem_status_map[status]), status),
      status_group = unname(cfg$status_groups[status]),
      technology   = unname(cfg$gem_tech_map[turbine_engine_technology]),
      fuel,
      start_year   = readr::parse_number(start_year),
      retire_year  = dplyr::coalesce(readr::parse_number(retired_year),
                                     readr::parse_number(planned_retire)),
      lat          = readr::parse_number(latitude),
      lon          = readr::parse_number(longitude),
      city,
      province     = dplyr::coalesce(state_province, major_area_prefecture_district),
      last_update  = as.Date(last_updated),
      researcher, wiki_url,
      other_ids_location = other_i_ds_location,
      other_ids_unit     = other_i_ds_unit
    )
  if (!is.null(countries)) units <- dplyr::filter(units, country %in% countries)
  units
}

#' Read BFF's bff-to-gem mapping file
read_bff_mapping <- function(path) {
  readr::read_csv(path, show_col_types = FALSE,
                  col_types = readr::cols(.default = readr::col_character())) |>
    janitor::clean_names() |>
    dplyr::mutate(bff_last_update = as.Date(bff_last_update),
                  gem_last_update = as.Date(gem_last_update))
}

#' Read BFF's bff-to-gem notes (changelog with sources) file
read_bff_notes <- function(path) {
  readr::read_csv(path, show_col_types = FALSE, name_repair = "minimal",
                  col_types = readr::cols(.default = readr::col_character())) |>
    (\(x) x[, names(x) != ""])() |>
    janitor::clean_names() |>
    dplyr::select(quarter, date_of_adding_source, bff_unit_id, gem_unit_id,
                  changed_fields, sources, comments) |>
    dplyr::filter(!is.na(bff_unit_id)) |>
    dplyr::mutate(date = as.Date(date_of_adding_source),
                  bff_plant_id = bff_plant_id(bff_unit_id))
}
