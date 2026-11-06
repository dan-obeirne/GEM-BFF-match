#' Build GEM location <-> BFF plant link edges from both sides' cross-references
#'
#' GEM side: BFF IDs in GEM's "Other IDs (location)" and "Other IDs (unit)".
#' BFF side: BFF's mapping file (GEM location / unit IDs, plus "GEM unit ID in
#' BFF if different") and, for old-format downloads, BFF's own GEM unit ID column.
#'
#' @param gem GEM units from [read_gem()].
#' @param bff BFF units from [read_bff()] (after fuel exclusion).
#' @param mapping BFF mapping from [read_bff_mapping()], or NULL.
#' @param bff_all_plant_ids All BFF plant IDs before fuel exclusion, used to
#'   explain stale references.
#' @return List with `edges` (valid links, tagged by `link_source`) and
#'   `stale` (references whose target is not in the other dataset).
build_links <- function(gem, bff, mapping = NULL, bff_all_plant_ids = unique(bff$bff_plant_id)) {
  unit_to_loc <- dplyr::distinct(gem, gem_unit_id, gem_location_id)

  # GEM side
  gem_edges <- gem |>
    dplyr::mutate(bp = purrr::map2(extract_bff_plant_ids(other_ids_location),
                                   extract_bff_plant_ids(other_ids_unit), union)) |>
    dplyr::select(gem_location_id, gem_unit_id, bp) |>
    tidyr::unnest(bp) |>
    dplyr::transmute(gem_location_id, bff_plant_id = bp, ref = gem_unit_id, src = "gem")

  # BFF side: mapping file
  bff_edges <- tibble::tibble(gem_location_id = character(), bff_plant_id = character(),
                              ref = character(), src = character())
  if (!is.null(mapping)) {
    map_plant <- dplyr::if_else(
      mapping$bff_plant_id %in% bff$bff_plant_id | is.na(mapping$bff_unit_id),
      mapping$bff_plant_id, bff_plant_id(mapping$bff_unit_id)
    )
    m <- mapping |> dplyr::mutate(bff_plant_id = map_plant) |> dplyr::filter(!is.na(bff_plant_id))
    gem_unit_refs <- m |>
      dplyr::transmute(bff_plant_id, bff_unit_id,
                       gem_unit_id = paste(dplyr::coalesce(gem_unit_id, ""),
                                           dplyr::coalesce(gem_unit_id_in_bff_if_different, ""), sep = ",")) |>
      tidyr::separate_rows(gem_unit_id, sep = ",") |>
      dplyr::mutate(gem_unit_id = stringr::str_trim(gem_unit_id)) |>
      dplyr::filter(gem_unit_id != "") |>
      dplyr::left_join(unit_to_loc, by = "gem_unit_id")
    bff_edges <- dplyr::bind_rows(
      m |> dplyr::filter(!is.na(gem_location_id)) |>
        dplyr::transmute(gem_location_id, bff_plant_id, ref = bff_unit_id, src = "bff"),
      gem_unit_refs |>
        dplyr::transmute(gem_location_id = dplyr::coalesce(gem_location_id, paste0("unit:", gem_unit_id)),
                         bff_plant_id, ref = bff_unit_id, src = "bff")
    )
  }
  # BFF side: GEM unit IDs inside the BFF download itself (old format only)
  if (any(!is.na(bff$gem_unit_id_bff))) {
    own <- bff |>
      dplyr::filter(!is.na(gem_unit_id_bff)) |>
      dplyr::select(bff_plant_id, bff_unit_id, gem_unit_id = gem_unit_id_bff) |>
      tidyr::separate_rows(gem_unit_id, sep = ",") |>
      dplyr::mutate(gem_unit_id = stringr::str_trim(gem_unit_id)) |>
      dplyr::left_join(unit_to_loc, by = "gem_unit_id") |>
      dplyr::transmute(gem_location_id = dplyr::coalesce(gem_location_id, paste0("unit:", gem_unit_id)),
                       bff_plant_id, ref = bff_unit_id, src = "bff")
    bff_edges <- dplyr::bind_rows(bff_edges, own)
  }

  all_edges <- dplyr::bind_rows(gem_edges, bff_edges) |>
    dplyr::mutate(ok_gem = gem_location_id %in% gem$gem_location_id,
                  ok_bff = bff_plant_id %in% bff$bff_plant_id)

  stale <- dplyr::filter(all_edges, !(ok_gem & ok_bff)) |>
    dplyr::mutate(issue = dplyr::case_when(
      src == "gem" & bff_plant_id %in% bff_all_plant_ids ~ "GEM links a BFF plant that is outside scope (excluded fuel)",
      src == "gem" ~ "GEM links a BFF plant ID that is not in the BFF file",
      src == "bff" & !ok_gem ~ "BFF links a GEM ID that is not in the GEM file",
      TRUE ~ "BFF links a BFF plant that is outside scope (excluded fuel)"
    )) |>
    dplyr::mutate(gem_id = sub("^unit:", "", gem_location_id)) |>
    dplyr::group_by(issue, gem_id, bff_plant_id) |>
    dplyr::summarise(refs = collapse_unique(ref, ", "), .groups = "drop")

  edges <- dplyr::filter(all_edges, ok_gem & ok_bff) |>
    dplyr::group_by(gem_location_id, bff_plant_id) |>
    dplyr::summarise(
      link_source = dplyr::case_when(
        all(c("gem", "bff") %in% src) ~ "both",
        "gem" %in% src ~ "gem_only",
        TRUE ~ "bff_only"
      ),
      .groups = "drop"
    )
  list(edges = edges, stale = stale)
}

#' Group linked GEM locations and BFF plants into plant clusters
#'
#' Connected components over the link edges (simple label propagation, no
#' igraph needed). Every GEM location and BFF plant gets a cluster, including
#' unlinked ones (single-sided clusters).
#'
#' @return Tibble with `side` ("gem"/"bff"), `id` (GEM location ID or BFF plant
#'   ID), `cluster_key` (stable across runs: sorted BFF IDs | sorted GEM IDs).
build_clusters <- function(edges, gem_location_ids, bff_plant_ids) {
  nodes <- c(paste0("G:", unique(gem_location_ids)), paste0("B:", unique(bff_plant_ids)))
  comp <- stats::setNames(seq_along(nodes), nodes)
  e1 <- paste0("G:", edges$gem_location_id)
  e2 <- paste0("B:", edges$bff_plant_id)
  repeat {
    m <- pmin(comp[e1], comp[e2])
    best <- tapply(c(m, m), c(e1, e2), min)
    new <- comp
    new[names(best)] <- pmin(comp[names(best)], best)
    if (identical(new, comp)) break
    comp <- new
  }
  tibble::tibble(node = names(comp), comp = unname(comp)) |>
    dplyr::mutate(side = ifelse(startsWith(node, "G:"), "gem", "bff"),
                  id = substring(node, 3)) |>
    dplyr::group_by(comp) |>
    dplyr::mutate(cluster_key = paste(
      paste(sort(id[side == "bff"]), collapse = ","),
      paste(sort(id[side == "gem"]), collapse = ","),
      sep = " | "
    )) |>
    dplyr::ungroup() |>
    dplyr::select(side, id, cluster_key)
}

#' Suggest likely matches for plants that have no link to the other dataset
#'
#' Candidates are plant clusters on the other side in the same country. The best
#' candidate is the most similar name within `cfg$suggest_link_km`; if nothing is
#' that close, a very similar name (>= 0.9) anywhere in the country is offered.
#'
#' @param from Cluster summaries for the unlinked plants (one side).
#' @param to Cluster summaries for the other side.
#' @return `from`'s cluster_key plus suggestion columns.
suggest_links <- function(from, to, cfg) {
  empty <- tibble::tibble(cluster_key = from$cluster_key, suggested_key = NA_character_,
                          suggested_name = NA_character_, suggested_dist_km = NA_real_,
                          suggested_name_sim = NA_real_, suggestion = NA_character_)
  if (nrow(from) == 0 || nrow(to) == 0) return(empty)
  out <- purrr::map_dfr(seq_len(nrow(from)), function(i) {
    f <- from[i, ]
    cand <- to[to$country == f$country, ]
    if (nrow(cand) == 0) return(empty[i, ])
    d <- if (is.na(f$lat) || is.na(f$lon)) rep(NA_real_, nrow(cand)) else
      geosphere::distHaversine(c(f$lon, f$lat), cbind(cand$lon, cand$lat)) / 1000
    s <- stringdist::stringsim(normalise_name(f$plant_name), normalise_name(cand$plant_name), method = "jw")
    near <- which(!is.na(d) & d <= cfg$suggest_link_km)
    pick <- if (length(near)) near[order(-s[near], d[near])][1] else {
      named <- which(s >= 0.9)
      if (length(named)) named[order(-s[named])][1] else NA_integer_
    }
    if (is.na(pick)) return(empty[i, ])
    tibble::tibble(
      cluster_key = f$cluster_key,
      suggested_key = cand$cluster_key[pick],
      suggested_name = cand$plant_name[pick],
      suggested_dist_km = round(d[pick], 2),
      suggested_name_sim = round(s[pick], 2),
      suggestion = dplyr::case_when(
        !is.na(d[pick]) & d[pick] <= cfg$suggest_link_km & s[pick] >= cfg$suggest_name_sim ~ "strong (near + similar name)",
        !is.na(d[pick]) & d[pick] <= cfg$suggest_link_km ~ "nearby, different name",
        TRUE ~ "similar name, far or no coordinates"
      )
    )
  })
  out
}
