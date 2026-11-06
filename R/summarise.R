#' Capacity by category as a named numeric vector (largest first)
cap_profile <- function(group, cap) {
  keep <- !is.na(group)
  if (!any(keep)) return(stats::setNames(numeric(), character()))
  p <- tapply(dplyr::coalesce(cap[keep], 0), group[keep], sum)
  p <- stats::setNames(as.numeric(p), names(p))
  p[order(-p, names(p))]
}

#' Format a capacity profile for display, e.g. "operating 400 | construction 200"
format_profile <- function(p) {
  if (length(p) == 0) return(NA_character_)
  paste(names(p), format_mw(p), collapse = " | ")
}

#' Parse a string made by [format_profile()] back into a named vector
parse_profile <- function(s) {
  if (is.na(s) || s == "") return(stats::setNames(numeric(), character()))
  parts <- strsplit(s, " \\| ")[[1]]
  vals <- as.numeric(sub(".* ", "", parts))
  stats::setNames(vals, sub(" [^ ]+$", "", parts))
}

format_mw <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(formatC(round(x, 1), format = "fg", digits = 10, drop0trailing = TRUE)))
}

#' Format a year range, e.g. "2001-2005" or "2001"
format_range <- function(lo, hi) {
  dplyr::case_when(is.na(lo) ~ NA_character_,
                   lo == hi ~ as.character(lo),
                   TRUE ~ paste0(lo, "-", hi))
}

#' Parse a range made by [format_range()] into c(lo, hi)
parse_range <- function(s) {
  if (is.na(s) || s == "") return(c(NA_real_, NA_real_))
  v <- as.numeric(strsplit(s, "-")[[1]])
  c(v[1], v[length(v)])
}

safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

#' Summarise one dataset's units to one row per plant cluster
#'
#' These are the key data points shown in the review outputs. Capacity,
#' technology and year ranges exclude cancelled units (start years also exclude
#' retired units, which BFF often drops); the status profile includes them.
#'
#' @param units Units from [read_gem()] or [read_bff()].
#' @param clusters Output of [build_clusters()].
#' @param side "gem" or "bff".
#' @return Tibble, one row per cluster_key, with list columns `status_vec` and
#'   `tech_vec` used by the comparison.
summarise_clusters <- function(units, clusters, side = c("gem", "bff")) {
  side <- match.arg(side)
  id_col <- if (side == "gem") "gem_location_id" else "bff_plant_id"
  map <- clusters[clusters$side == side, c("id", "cluster_key")]
  units$id <- units[[id_col]]
  units <- dplyr::left_join(units, map, by = "id")

  out <- units |>
    dplyr::group_by(cluster_key) |>
    dplyr::summarise(
      country     = collapse_unique(country),
      plant_name  = collapse_unique(plant_name),
      ids         = collapse_unique(id, ", "),
      n_units     = dplyr::n(),
      city        = collapse_unique(city),
      province    = collapse_unique(province),
      capacity    = sum(capacity_mw[status_group != "cancelled" | is.na(status_group)], na.rm = TRUE),
      status_vec  = list(cap_profile(status_group, capacity_mw)),
      tech_vec    = list(cap_profile(dplyr::if_else(status_group == "cancelled", NA_character_, technology), capacity_mw)),
      start_lo    = safe_min(start_year[!status_group %in% c("cancelled", "retired")]),
      start_hi    = safe_max(start_year[!status_group %in% c("cancelled", "retired")]),
      retire_lo   = safe_min(retire_year[status_group != "cancelled"]),
      retire_hi   = safe_max(retire_year[status_group != "cancelled"]),
      lat         = mean(unique(cbind(lat, lon))[, 1], na.rm = TRUE),
      lon         = mean(unique(cbind(lat, lon))[, 2], na.rm = TRUE),
      last_update = if (all(is.na(last_update))) as.Date(NA) else max(last_update, na.rm = TRUE),
      unit_ids    = collapse_unique(if (side == "gem") gem_unit_id else bff_unit_id, ", "),
      researcher  = if (side == "gem") collapse_unique(researcher, ", ") else NA_character_,
      wiki_url    = if (side == "gem") collapse_unique(wiki_url, " ") else NA_character_,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      status      = purrr::map_chr(status_vec, format_profile),
      technology  = purrr::map_chr(tech_vec, format_profile),
      start_year  = format_range(start_lo, start_hi),
      retire_year = format_range(retire_lo, retire_hi),
      lat = ifelse(is.nan(lat), NA_real_, lat),
      lon = ifelse(is.nan(lon), NA_real_, lon)
    )
  out
}

#' Latest BFF note (changelog entry) per cluster
#'
#' @param notes From [read_bff_notes()].
#' @param since Only use notes added after this date (NULL = all).
#' @param all If TRUE, concatenate every qualifying note rather than only the latest.
#' @return Tibble: cluster_key, bff_note_changes, bff_note_date, bff_note_sources.
cluster_notes <- function(notes, clusters, since = NULL, all = FALSE) {
  if (is.null(notes) || nrow(notes) == 0) {
    return(tibble::tibble(cluster_key = character(), bff_note_changes = character(),
                          bff_note_date = as.Date(character()), bff_note_sources = character()))
  }
  if (!is.null(since)) notes <- dplyr::filter(notes, date > since)
  map <- clusters[clusters$side == "bff", c("id", "cluster_key")]
  n <- notes |>
    dplyr::inner_join(map, by = c(bff_plant_id = "id")) |>
    dplyr::arrange(cluster_key, dplyr::desc(date))
  if (!all) n <- dplyr::slice_head(n, n = 1, by = cluster_key)
  n |>
    dplyr::group_by(cluster_key) |>
    dplyr::summarise(
      bff_note_changes = paste0(bff_unit_id, ": ", changed_fields,
                                ifelse(is.na(comments), "", paste0(" (", comments, ")")),
                                collapse = "\n"),
      bff_note_date    = max(date, na.rm = TRUE),
      bff_note_sources = collapse_unique(sources, "\n"),
      .groups = "drop"
    )
}
