#' Export comparison findings for the GOGPT researcher repo
#'
#' One row per finding, in the column layout that repo expects:
#' * disagreement: a plant GEM has where BFF gives a different value. Written at
#'   unit level where units pair up (several BFF units against one GEM unit are
#'   added up, e.g. a combined-cycle block BFF splits into turbines), otherwise
#'   at plant level with a note.
#' * lead: a BFF plant with no GEM link, or a BFF unit with no GEM unit inside a
#'   plant whose capacity differs.
#'
#' Only findings this project already flags (config.R thresholds) and that no
#' decision has dismissed are exported.

researcher_columns <- c(
  "record_type", "country", "state", "gem_location_id", "gem_unit_id", "gem_plant_name", "gem_unit_name",
  "field", "gem_value", "other_value", "lead_name", "lead_unit_name", "lead_capacity_mw", "lead_fuel",
  "lead_status", "lead_start_year", "lead_owner", "lead_county", "lead_city", "lead_latitude",
  "lead_longitude", "other_source", "other_record_id", "other_record_url", "other_data_date",
  "match_method", "match_confidence", "note"
)

# BFF technology code -> GEM "Turbine/Engine Technology" wording
bff_tech_to_gem <- c(CCGT = "combined cycle", OCGT = "gas turbine", ST = "steam turbine",
                     ICCC = "internal combustion", ISCC = "ISCC", AFC = "AFC")

# BFF main fuel -> plain fuel name for leads
bff_fuel_words <- c(gas = "natural gas", oil = "fuel oil")

#' GEM unit <-> BFF unit pairs from both sides' cross-references
unit_pairs <- function(gem, bff, mapping) {
  from_bff <- if (is.null(mapping)) NULL else mapping |>
    dplyr::transmute(bff_unit_id,
                     gem_unit_id = paste(dplyr::coalesce(gem_unit_id, ""),
                                         dplyr::coalesce(gem_unit_id_in_bff_if_different, ""), sep = ",")) |>
    tidyr::separate_rows(gem_unit_id, sep = ",") |>
    dplyr::mutate(gem_unit_id = stringr::str_trim(gem_unit_id), src = "bff")
  from_gem <- gem |>
    dplyr::transmute(gem_unit_id, bff_unit_id = stringr::str_extract_all(other_ids_unit, "[A-Z]{2}-\\d+-\\d+")) |>
    tidyr::unnest(bff_unit_id) |>
    dplyr::mutate(src = "gem")
  dplyr::bind_rows(from_bff, from_gem) |>
    dplyr::filter(gem_unit_id %in% gem$gem_unit_id, bff_unit_id %in% bff$bff_unit_id) |>
    dplyr::distinct(gem_unit_id, bff_unit_id)
}

#' Match confidence per plant cluster, from how its links are supported
cluster_confidence <- function(clusters, edges, id_issues) {
  gem_side <- clusters[clusters$side == "gem", c("id", "cluster_key")]
  conf <- edges |>
    dplyr::inner_join(gem_side, by = c(gem_location_id = "id")) |>
    dplyr::group_by(cluster_key) |>
    dplyr::summarise(conf = if (all(link_source == "both")) "high" else "medium", .groups = "drop")
  weak_gem_ids <- unlist(strsplit(id_issues$gem_id[startsWith(id_issues$issue, "Plant cluster held together")], ", "))
  weak_keys <- clusters$cluster_key[clusters$side == "gem" & clusters$id %in% weak_gem_ids]
  dplyr::mutate(conf, conf = dplyr::if_else(cluster_key %in% weak_keys, "low", conf))
}

#' Short note from BFF's changelog for some units, or NA. Also reports whether
#' every cited source is non-independent (GEM itself, or banned).
changelog_note <- function(notes, bff_unit_ids, field_pattern, cfg) {
  n <- notes[notes$bff_unit_id %in% bff_unit_ids & !is.na(notes$sources), ]
  if (!is.null(field_pattern)) n <- n[stringr::str_detect(dplyr::coalesce(n$changed_fields, ""), field_pattern), ]
  if (nrow(n) == 0) return(list(note = NA_character_, dependent = FALSE))
  n <- n[order(n$date, decreasing = TRUE), ][1, ]
  dependent <- stringr::str_detect(n$sources, stringr::regex(cfg$researcher_dependent_sources, ignore_case = TRUE))
  src <- stringr::str_trunc(stringr::str_squish(n$sources), 150)
  list(note = sprintf("BFF's changelog (%s) cites %s; check that source.", n$date, src),
       dependent = dependent &&
         !stringr::str_detect(stringr::str_remove_all(n$sources, stringr::regex(cfg$researcher_dependent_sources, ignore_case = TRUE)),
                              "[a-z0-9-]+\\.[a-z]{2,}"))
}

#' Build the researcher export table
#'
#' @param res Output of [run_comparison()].
#' @param flags Flags after [apply_decisions()], with a `sheet` column.
#' @param gem_raw,bff_raw The GEM and BFF files read with all columns as text,
#'   used for values exactly as they appear in the export.
#' @param bff_date As-of date of the BFF release.
researcher_export <- function(res, flags, gem, bff, gem_raw, bff_raw, mapping, notes, cfg, bff_date) {
  shown <- flags[!flags$suppressed, ]
  clusters <- res$clusters
  gem_c <- dplyr::left_join(gem, clusters[clusters$side == "gem", c("id", "cluster_key")],
                            by = c(gem_location_id = "id"))
  bff_c <- dplyr::left_join(bff, clusters[clusters$side == "bff", c("id", "cluster_key")],
                            by = c(bff_plant_id = "id"))
  graw <- gem_raw |>
    dplyr::select(gem_unit_id = `GEM unit ID`, g_status = Status, g_tech = `Turbine/Engine Technology`,
                  g_cap = `Capacity (MW)`, g_start = `Start year`, g_retired = `Retired year`,
                  g_planned = `Planned retire`, g_lat = Latitude, g_lon = Longitude)
  gem_c <- dplyr::left_join(gem_c, graw, by = "gem_unit_id")
  braw <- bff_raw |>
    dplyr::select(bff_unit_id = `BFF unit ID`, b_owner = `Direct owner`, b_retired = `Retirement year`,
                  b_planned = `Planned retirement year`)
  bff_c <- dplyr::left_join(bff_c, braw, by = "bff_unit_id")

  conf <- cluster_confidence(clusters, res$links$edges, res$id_issues)
  base <- list(other_source = cfg$researcher_source_name,
               other_data_date = format(bff_date, "%Y-%m-%d"))

  # ---- Disagreements ----------------------------------------------------------
  div <- shown[shown$sheet == "Divergences", c("item_key", "field")]
  pairs <- unit_pairs(gem, bff, mapping)
  ug <- build_clusters(dplyr::transmute(pairs, gem_location_id = gem_unit_id, bff_plant_id = bff_unit_id),
                       pairs$gem_unit_id, pairs$bff_unit_id) |>
    dplyr::rename(unit_group = cluster_key)

  rows <- list()
  add <- function(...) rows[[length(rows) + 1]] <<- tibble::tibble(...)
  cap_differs_fine <- function(a, b) abs(a - b) > max(1, 0.01 * max(a, b))
  fmt_num <- function(x) ifelse(is.na(x), NA_character_, format(round(x, 1), trim = TRUE, drop0trailing = TRUE))

  for (k in unique(div$item_key)) {
    fields <- div$field[div$item_key == k]
    g <- gem_c[gem_c$cluster_key %in% k, ]
    b <- bff_c[bff_c$cluster_key %in% k, ]
    if (nrow(g) == 0 || nrow(b) == 0) next
    cf <- dplyr::coalesce(conf$conf[match(k, conf$cluster_key)], "high")
    plant <- list(country = g$country[1], gem_location_id = paste(unique(g$gem_location_id), collapse = ", "),
                  gem_plant_name = paste(unique(g$plant_name), collapse = "; "))
    groups <- ug[ug$id %in% c(g$gem_unit_id, b$bff_unit_id), ]
    emit <- function(field, gu, bu, gem_value, other_value, note = NA_character_) {
      unit_level <- nrow(gu) == 1
      if (!unit_level) note <- paste(stats::na.omit(c(
        sprintf(if (field == "Capacity (MW)") "Plant-level comparison: GEM's value is the total of units %s."
                else "Plant-level comparison covering GEM units %s.",
                paste0(gu$gem_unit_id, " (", gu$unit_name, ")", collapse = ", ")), note)), collapse = " ")
      if (nrow(bu) > 1 && field == "Capacity (MW)") note <- paste(stats::na.omit(c(
        sprintf("BFF lists this as %d units (%s); the BFF value is their total.", nrow(bu),
                paste(bu$unit_name, collapse = ", ")), note)), collapse = " ")
      add(record_type = "disagreement", country = plant$country,
          gem_location_id = paste(unique(gu$gem_location_id), collapse = ", "),
          gem_unit_id = if (unit_level) gu$gem_unit_id else NA_character_,
          gem_plant_name = paste(unique(gu$plant_name), collapse = "; "),
          gem_unit_name = if (unit_level) gu$unit_name else NA_character_,
          field = field, gem_value = gem_value, other_value = other_value,
          other_record_id = paste(bu$bff_unit_id, collapse = ", "),
          match_method = "shared_id", match_confidence = cf, note = note,
          .bff_units = list(bu$bff_unit_id), .cap_pct = NA_real_)
    }
    n_before <- length(rows)

    for (grp in unique(groups$unit_group)) {
      ids <- groups$id[groups$unit_group == grp]
      gu <- g[g$gem_unit_id %in% ids, ]
      bu <- b[b$bff_unit_id %in% ids, ]
      if (nrow(gu) == 0 || nrow(bu) == 0) next
      live_g <- gu[gu$status != "cancelled" | all(gu$status == "cancelled"), ]
      live_b <- bu[bu$status != "cancelled" | all(bu$status == "cancelled"), ]

      if ("capacity" %in% fields) {
        gc <- sum(live_g$capacity_mw, na.rm = TRUE); bc <- sum(live_b$capacity_mw, na.rm = TRUE)
        if (gc > 0 && bc > 0 && cap_differs_fine(gc, bc)) {
          emit("Capacity (MW)", gu, bu, if (nrow(gu) == 1) gu$g_cap else fmt_num(gc), fmt_num(bc))
          rows[[length(rows)]]$.cap_pct <- abs(gc - bc) / max(gc, bc)
        }
      }
      if ("status" %in% fields && length(unique(gu$status_group)) == 1) {
        bs <- unique(bu$status)
        main_bs <- bu$status[which.max(dplyr::coalesce(bu$capacity_mw, 0))]
        if (!identical(unname(cfg$status_groups[main_bs]), gu$status_group[1])) {
          note <- if (length(bs) > 1) sprintf("BFF gives its units different statuses: %s.",
                                              paste0(bu$unit_name, " ", bu$status, collapse = ", ")) else NA_character_
          emit("Status", gu, bu, paste(unique(gu$g_status), collapse = "; "), main_bs, note)
        }
      }
      if ("start_year" %in% fields) {
        gy <- suppressWarnings(min(live_g$start_year, na.rm = TRUE))
        by <- suppressWarnings(min(live_b$start_year, na.rm = TRUE))
        if (is.finite(by) && (!is.finite(gy) || abs(gy - by) > cfg$year_tol)) {
          note <- if (length(unique(stats::na.omit(live_b$start_year))) > 1)
            sprintf("BFF's units start between %d and %d; the earliest is given.",
                    min(live_b$start_year, na.rm = TRUE), max(live_b$start_year, na.rm = TRUE)) else NA_character_
          emit("Start year", gu, bu, if (is.finite(gy)) as.character(gy) else NA_character_, as.character(by), note)
        }
      }
      if ("retire_year" %in% fields) {
        gy <- suppressWarnings(min(gu$retire_year, na.rm = TRUE))
        by <- suppressWarnings(min(bu$retire_year, na.rm = TRUE))
        if (is.finite(by) && (!is.finite(gy) || abs(gy - by) > cfg$year_tol)) {
          retired <- any(c(gu$status, bu$status) == "retired")
          gv <- if (retired) gu$g_retired else gu$g_planned
          emit(if (retired) "Retired year" else "Planned retire", gu, bu,
               if (nrow(gu) == 1) gv else if (is.finite(gy)) as.character(gy) else NA_character_,
               as.character(by))
        }
      }
      if ("technology" %in% fields && length(unique(gu$technology)) == 1) {
        gt <- gu$technology[1]
        bt <- unique(stats::na.omit(bu$technology))
        bt <- if ("CCGT" %in% bt || all(c("OCGT", "ST") %in% bt)) "CCGT" else if (length(bt) == 1) bt else NA_character_
        if (!is.na(bt) && (is.na(gt) || bt != gt)) {
          note <- c(
            if (is.na(gt)) "GEM's technology is unknown.",
            if (length(unique(stats::na.omit(bu$technology))) > 1)
              sprintf("BFF lists the turbines separately (%s); together they are a combined-cycle block.",
                      paste0(bu$unit_name, " ", bu$technology, collapse = ", "))
          )
          emit("Turbine/Engine Technology", gu, bu, if (is.na(gt)) NA_character_ else gu$g_tech[1],
               unname(bff_tech_to_gem[bt]), if (length(note)) paste(note, collapse = " ") else NA_character_)
        }
      }
    }

    # Capacity gaps explained by BFF units GEM has no unit for -> unit leads
    if ("capacity" %in% fields) {
      unpaired <- b[!b$bff_unit_id %in% pairs$bff_unit_id & b$status != "cancelled", ]
      for (i in seq_len(nrow(unpaired))) {
        u <- unpaired[i, ]
        add(record_type = "lead", country = plant$country, gem_location_id = plant$gem_location_id,
            lead_name = u$plant_name, lead_unit_name = u$unit_name,
            lead_capacity_mw = fmt_num(dplyr::na_if(u$capacity_mw, 0)),
            lead_fuel = unname(dplyr::coalesce(bff_fuel_words[u$main_fuel], u$main_fuel)),
            lead_status = u$status, lead_start_year = as.character(u$start_year), lead_owner = u$b_owner,
            lead_county = u$province, lead_city = u$city,
            lead_latitude = as.character(u$lat), lead_longitude = as.character(u$lon),
            other_record_id = u$bff_unit_id, match_method = "shared_id", match_confidence = cf,
            note = sprintf("GEM has %s but no unit matching this BFF unit; it may be missing from GEM or recorded under another unit.",
                           plant$gem_plant_name),
            .bff_units = list(u$bff_unit_id), .cap_pct = NA_real_)
      }
      s <- res$paired[res$paired$cluster_key == k, ]
      if (length(rows) == n_before && dplyr::coalesce(s$gem_capacity, 0) > 0 && dplyr::coalesce(s$bff_capacity, 0) > 0) {
        emit("Capacity (MW)", g, b, fmt_num(s$gem_capacity), fmt_num(s$bff_capacity),
             "The units could not be paired one to one, so plant totals are compared.")
        rows[[length(rows)]]$.cap_pct <- abs(s$gem_capacity - s$bff_capacity) / max(s$gem_capacity, s$bff_capacity)
      }
    }

    if ("location" %in% fields) {
      s <- res$paired[res$paired$cluster_key == k, ]
      gl <- g[!is.na(g$g_lat), ]
      if (nrow(gl) > 0 && !is.na(s$bff_lat)) {
        gl <- gl[1, ]
        note <- sprintf("BFF's coordinates are %.1f km from GEM's.", s$distance_km)
        for (f in c("Latitude", "Longitude")) {
          add(record_type = "disagreement", country = plant$country, gem_location_id = plant$gem_location_id,
              gem_plant_name = plant$gem_plant_name, field = f,
              gem_value = if (f == "Latitude") gl$g_lat else gl$g_lon,
              other_value = sprintf("%.5f", if (f == "Latitude") s$bff_lat else s$bff_lon),
              other_record_id = paste(unique(b$bff_plant_id), collapse = ", "),
              match_method = "shared_id", match_confidence = cf, note = note,
              .bff_units = list(b$bff_unit_id), .cap_pct = NA_real_)
        }
      }
    }
  }

  # ---- Plant leads (BFF plants with no GEM link) -----------------------------
  bo <- res$bff_only[!res$bff_only$explained, ]
  keep <- shown$item_key[shown$sheet == "BFF-only plants"]
  bo <- bo[bo$cluster_key %in% keep, ]
  # BFF says GEM excluded the plant on purpose (e.g. bioenergy, heat only): not a lead
  bo <- bo[!stringr::str_detect(dplyr::coalesce(bo$mapping_comment, ""), "GEM excluded"), ]
  for (i in seq_len(nrow(bo))) {
    p <- bo[i, ]
    u <- bff_c[bff_c$cluster_key == p$cluster_key, ]
    live <- u[u$status != "cancelled" | all(u$status == "cancelled"), ]
    cap <- sum(live$capacity_mw, na.rm = TRUE)
    threshold <- if (p$country %in% cfg$researcher_eu_uk) cfg$researcher_lead_mw_eu else cfg$researcher_lead_mw
    if (cap > 0 && cap < threshold) next
    statuses <- unique(live$status)
    main_status <- live$status[which.max(dplyr::coalesce(live$capacity_mw, 0))]
    sug <- !is.na(p$suggestion)
    method <- if (!sug) "none" else switch(p$suggestion,
      "strong (near + similar name)" = "name_and_distance",
      "nearby, different name" = "distance_only",
      "name_and_distance")
    mconf <- if (!sug) "medium" else if (method == "name_and_distance" && p$suggestion != "similar name, far or no coordinates") "high" else "low"
    note <- c(
      if (sug) sprintf("GEM may already have this as %s (%s, %s km); if so, only the BFF ID needs adding to GEM.",
                       p$suggested_name, sub("^[^|]*\\| ?", "", p$suggested_key),
                       dplyr::coalesce(as.character(p$suggested_dist_km), "distance unknown")),
      if (length(statuses) > 1) sprintf("BFF's units have different statuses (%s); the status of the largest is given.",
                                        paste(statuses, collapse = ", ")),
      if (!is.na(p$mapping_comment)) sprintf("BFF's mapping comment: \"%s\".", p$mapping_comment)
    )
    add(record_type = "lead", country = p$country,
        gem_location_id = if (sug && method != "distance_only") sub("^[^|]*\\| ?", "", p$suggested_key) else NA_character_,
        lead_name = u$plant_name[1],
        lead_capacity_mw = if (cap > 0) fmt_num(cap) else NA_character_,
        lead_fuel = paste(unique(dplyr::coalesce(unname(bff_fuel_words[u$main_fuel]), u$main_fuel)), collapse = ", "),
        lead_status = main_status,
        lead_start_year = { y <- suppressWarnings(min(live$start_year, na.rm = TRUE)); if (is.finite(y)) as.character(y) else NA_character_ },
        lead_owner = collapse_unique(u$b_owner, "; "), lead_county = collapse_unique(u$province, "; "),
        lead_city = collapse_unique(u$city, "; "),
        lead_latitude = if (is.na(p$lat)) NA_character_ else sprintf("%.5f", p$lat),
        lead_longitude = if (is.na(p$lon)) NA_character_ else sprintf("%.5f", p$lon),
        other_record_id = paste(unique(u$bff_plant_id), collapse = ", "),
        match_method = method, match_confidence = mconf,
        note = if (length(note)) paste(note, collapse = " ") else NA_character_,
        .bff_units = list(u$bff_unit_id), .cap_pct = NA_real_)
  }

  if (length(rows) == 0) return(tibble::as_tibble(stats::setNames(
    replicate(length(researcher_columns), character(), simplify = FALSE), researcher_columns)))
  out <- dplyr::bind_rows(rows)

  # ---- Changelog sources, independence, low-confidence filter ----------------
  field_patterns <- c("Capacity (MW)" = "capacity", "Status" = "status", "Start year" = "start",
                      "Retired year" = "retire", "Planned retire" = "retire",
                      "Turbine/Engine Technology" = "techn", "Latitude" = "lat|lon|location",
                      "Longitude" = "lat|lon|location")
  cl <- purrr::map2(out$.bff_units, out$field, function(ids, f) {
    changelog_note(notes, ids, if (is.na(f)) NULL else field_patterns[[f]], cfg)
  })
  out$note <- purrr::map2_chr(out$note, cl, function(n, c)
    if (is.na(c$note)) n else if (is.na(n)) c$note else paste(n, c$note))
  out <- out[!purrr::map_lgl(cl, "dependent"), ]
  big <- out$record_type == "lead" | out$field %in% "Status" | dplyr::coalesce(out$.cap_pct, 0) > 0.2
  out <- out[out$match_confidence != "low" | big, ]

  out$state <- NA_character_
  out$other_source <- base$other_source
  out$other_record_url <- NA_character_
  out$other_data_date <- base$other_data_date
  for (col in setdiff(researcher_columns, names(out))) out[[col]] <- NA_character_
  out |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ if (is.list(.x)) .x else as.character(.x))) |>
    dplyr::select(dplyr::all_of(researcher_columns)) |>
    dplyr::arrange(country, gem_location_id, record_type, gem_unit_id, field)
}
