#' What changed between two BFF releases?
#'
#' Changed units are those whose `last_update` is later than the newest
#' `last_update` in the previous release. New and removed units are found by ID.
#' For changed units, the key fields are also diffed against the previous
#' release so the output can say what changed.
#'
#' @param new_units,prev_units Units from [read_bff()] (unfiltered) for the new
#'   and previous release.
#' @return List: `cutoff` (date), `units` (changed + new units with
#'   change_type and fields_changed), `removed` (previous-release rows of
#'   removed units).
bff_release_changes <- function(new_units, prev_units) {
  cutoff <- max(prev_units$last_update, na.rm = TRUE)
  is_new <- !new_units$bff_unit_id %in% prev_units$bff_unit_id
  changed <- new_units[!is_new & !is.na(new_units$last_update) & new_units$last_update > cutoff, ]
  added <- new_units[is_new, ]

  fields <- c("plant_name", "capacity_mw", "status", "technology", "start_year", "retire_year", "lat", "lon")
  old <- prev_units[match(changed$bff_unit_id, prev_units$bff_unit_id), fields]
  diffs <- purrr::map_chr(seq_len(nrow(changed)), function(i) {
    parts <- purrr::map_chr(fields, function(f) {
      o <- old[[f]][i]; n <- changed[[f]][i]
      if (f %in% c("lat", "lon")) { if (!is.na(o) && !is.na(n) && abs(o - n) < 1e-4) return(NA_character_) }
      if (identical(o, n) || (is.na(o) && is.na(n))) return(NA_character_)
      paste0(f, ": ", dplyr::coalesce(as.character(o), "(blank)"), " -> ", dplyr::coalesce(as.character(n), "(blank)"))
    })
    collapse_unique(parts, "; ")
  })

  units <- dplyr::bind_rows(
    dplyr::mutate(changed, change_type = "changed", fields_changed = diffs),
    dplyr::mutate(added, change_type = "new", fields_changed = "new unit")
  )
  removed <- prev_units[!prev_units$bff_unit_id %in% new_units$bff_unit_id, ]
  list(cutoff = cutoff, units = units, removed = removed)
}

#' Per-cluster description of a BFF release's changes
release_changes_by_cluster <- function(change_units, clusters) {
  map <- clusters[clusters$side == "bff", c("id", "cluster_key")]
  change_units |>
    dplyr::inner_join(map, by = c(bff_plant_id = "id")) |>
    dplyr::group_by(cluster_key) |>
    dplyr::summarise(
      bff_change_type = paste(sort(unique(change_type)), collapse = ", "),
      bff_fields_changed = paste0(bff_unit_id, ": ", dplyr::coalesce(fields_changed, "last_update only"),
                                  collapse = "\n"),
      .groups = "drop"
    )
}

#' Which GEM field(s) a BFF field change can affect
changed_to_compare_fields <- function(fields_changed) {
  f <- character()
  if (grepl("new unit", fields_changed)) return(c("capacity", "status", "technology", "start_year", "retire_year", "location"))
  if (grepl("capacity_mw", fields_changed)) f <- c(f, "capacity", "status", "technology")
  if (grepl("status", fields_changed)) f <- c(f, "status", "capacity")
  if (grepl("technology", fields_changed)) f <- c(f, "technology")
  if (grepl("start_year", fields_changed)) f <- c(f, "start_year")
  if (grepl("retire_year", fields_changed)) f <- c(f, "retire_year")
  if (grepl("lat|lon", fields_changed)) f <- c(f, "location")
  unique(f)
}

#' BFF units removed in a release that GEM still lists as active
#'
#' A removed unit is flagged when a GEM unit points at it directly (unit-level
#' "Other IDs"), or when its whole BFF plant disappeared and the linked GEM
#' location still has units that are not retired or cancelled.
removed_still_in_gem <- function(removed, new_bff, gem, prev_clusters) {
  if (nrow(removed) == 0) return(NULL)
  active <- gem[!gem$status_group %in% c("retired", "cancelled"), ]
  unit_refs <- active |>
    dplyr::mutate(bu = stringr::str_match_all(other_ids_unit, "Beyond Fossil Fuels:\\s*([A-Z]{2}-[0-9]+-[^,;\\s]+)")) |>
    dplyr::mutate(bu = purrr::map(bu, ~ stats::na.omit(.x[, 2]))) |>
    dplyr::select(gem_unit_id, gem_location_id, bu) |>
    tidyr::unnest(bu)
  loc_map <- dplyr::distinct(dplyr::inner_join(
    dplyr::distinct(prev_clusters[prev_clusters$side == "bff", ], cluster_key, bff_plant_id = id),
    dplyr::distinct(prev_clusters[prev_clusters$side == "gem", ], cluster_key, gem_location_id = id),
    by = "cluster_key", relationship = "many-to-many"
  ), bff_plant_id, gem_location_id)

  removed |>
    dplyr::mutate(plant_removed = !bff_plant_id %in% new_bff$bff_plant_id) |>
    dplyr::left_join(loc_map, by = "bff_plant_id", relationship = "many-to-many") |>
    dplyr::rowwise() |>
    dplyr::mutate(
      gem_units_hit = list(unique(c(
        unit_refs$gem_unit_id[unit_refs$bu == bff_unit_id],
        if (plant_removed && !is.na(gem_location_id)) active$gem_unit_id[active$gem_location_id == gem_location_id]
      )))
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(lengths(gem_units_hit) > 0) |>
    dplyr::group_by(bff_unit_id) |>
    dplyr::summarise(
      country = dplyr::first(country), bff_plant_id = dplyr::first(bff_plant_id),
      bff_plant_name = dplyr::first(plant_name), bff_unit_name = dplyr::first(unit_name),
      bff_capacity = dplyr::first(capacity_mw), bff_status = dplyr::first(status),
      bff_last_update = dplyr::first(last_update), plant_removed = dplyr::first(plant_removed),
      gem_unit_ids = paste(unique(unlist(gem_units_hit)), collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      gem_hit = list(gem[gem$gem_unit_id %in% strsplit(gem_unit_ids, ", ")[[1]], ]),
      gem_plant_name = collapse_unique(gem_hit$plant_name),
      gem_location_id = collapse_unique(gem_hit$gem_location_id, ", "),
      gem_capacity = sum(gem_hit$capacity_mw, na.rm = TRUE),
      gem_status = format_profile(cap_profile(gem_hit$status_group, gem_hit$capacity_mw))
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-gem_hit)
}
