#' Run the full GEM vs BFF plant-level comparison
#'
#' @param gem GEM units ([read_gem()]).
#' @param bff BFF units ([read_bff()], fuel-filtered).
#' @param mapping BFF mapping ([read_bff_mapping()]) or NULL.
#' @param bff_all_plant_ids BFF plant IDs before fuel exclusion.
#' @return List: links, clusters, gem_sum, bff_sum, paired, flags (long),
#'   bff_only, gem_only (with mapping comments and suggestions), id_issues, coverage.
run_comparison <- function(gem, bff, mapping, cfg, bff_all_plant_ids = unique(bff$bff_plant_id)) {
  links <- build_links(gem, bff, mapping, bff_all_plant_ids)
  clusters <- build_clusters(links$edges, gem$gem_location_id, bff$bff_plant_id)

  gem_sum <- summarise_clusters(gem, clusters, "gem")
  bff_sum <- summarise_clusters(bff, clusters, "bff")
  paired  <- pair_clusters(gem_sum, bff_sum)
  flags   <- compare_clusters(paired, cfg)

  # ---- Unmatched plants -----------------------------------------------------
  comments <- mapping_comments(mapping, gem, clusters)
  bff_only <- bff_sum |>
    dplyr::filter(!cluster_key %in% paired$cluster_key) |>
    dplyr::left_join(comments, by = "cluster_key")
  gem_only <- gem_sum |>
    dplyr::filter(!cluster_key %in% paired$cluster_key) |>
    dplyr::left_join(comments, by = "cluster_key")
  explain <- function(x) !is.na(x$mapping_comment) &
    stringr::str_detect(x$mapping_comment, stringr::regex(cfg$explained_comment_regex, ignore_case = TRUE))
  bff_only$explained <- explain(bff_only)
  gem_only$explained <- explain(gem_only)

  bff_only <- dplyr::left_join(bff_only, suggest_links(bff_only[!bff_only$explained, ], gem_sum, cfg),
                               by = "cluster_key")
  gem_only <- dplyr::left_join(gem_only, suggest_links(gem_only[!gem_only$explained, ], bff_sum, cfg),
                               by = "cluster_key")

  # ---- ID issues --------------------------------------------------------------
  gem_names <- dplyr::distinct(gem, gem_location_id, gem_plant_name = plant_name, country)
  bff_names <- dplyr::distinct(bff, bff_plant_id, bff_plant_name = plant_name)
  one_sided <- links$edges |>
    dplyr::filter(link_source != "both") |>
    dplyr::transmute(
      issue = dplyr::if_else(link_source == "bff_only",
                             "GEM is missing this BFF ID (link only in BFF mapping)",
                             "BFF mapping is missing this link (link only in GEM)"),
      gem_id = gem_location_id, bff_plant_id, detail = NA_character_)
  stale <- links$stale |> dplyr::transmute(issue, gem_id, bff_plant_id, detail = paste("refs:", refs))
  multi <- multi_cluster_issues(clusters, links$edges, gem_names, bff_names)
  id_issues <- dplyr::bind_rows(one_sided, stale) |>
    dplyr::left_join(dplyr::distinct(gem_names, gem_location_id, .keep_all = TRUE), by = c(gem_id = "gem_location_id")) |>
    dplyr::left_join(bff_names |> dplyr::distinct(bff_plant_id, .keep_all = TRUE), by = "bff_plant_id") |>
    dplyr::bind_rows(multi) |>
    dplyr::mutate(item_key = paste(issue, gem_id, bff_plant_id, sep = " | ")) |>
    dplyr::select(issue, country, gem_id, gem_plant_name, bff_plant_id, bff_plant_name, detail, item_key) |>
    dplyr::arrange(issue, country)

  coverage <- tibble::tibble(
    measure = c("GEM units (in BFF countries)", "GEM locations", "GEM locations linked to BFF",
                "BFF units (in scope)", "BFF plants", "BFF plants linked to GEM",
                "Matched plant clusters", "Links found by both sides", "Links only in GEM", "Links only in BFF mapping",
                "Stale ID references"),
    value = c(nrow(gem), dplyr::n_distinct(gem$gem_location_id), dplyr::n_distinct(links$edges$gem_location_id),
              nrow(bff), dplyr::n_distinct(bff$bff_plant_id), dplyr::n_distinct(links$edges$bff_plant_id),
              nrow(paired), sum(links$edges$link_source == "both"), sum(links$edges$link_source == "gem_only"),
              sum(links$edges$link_source == "bff_only"), nrow(links$stale))
  )

  list(links = links, clusters = clusters, gem_sum = gem_sum, bff_sum = bff_sum, paired = paired,
       flags = flags, bff_only = bff_only, gem_only = gem_only, id_issues = id_issues, coverage = coverage)
}

#' Classify plant clusters that join more than one GEM location or BFF plant
#'
#' Each such cluster becomes one ID issue whose wording depends on its shape:
#' * held together by a one-sided link: the cluster would split apart without
#'   links that only GEM or only BFF has, so a wrong link is the likely cause;
#' * otherwise, several BFF plants : 1 GEM location (BFF splits the site, e.g.
#'   by phase or owner), 1 BFF plant : several GEM locations (GEM splits it), or
#'   several : several.
#' The first three are usually intentional; mark them agree_to_differ /
#' permanent once checked. The item key includes every ID, so the issue comes
#' back if the cluster's membership changes.
#'
#' @param edges Valid link edges from [build_links()].
#' @param gem_names,bff_names Lookup tables of plant names (and GEM country).
#' @return Tibble with the ID-issue columns (issue, country, gem_id, gem_plant_name,
#'   bff_plant_id, bff_plant_name, detail).
multi_cluster_issues <- function(clusters, edges, gem_names, bff_names) {
  multi <- clusters |>
    dplyr::group_by(cluster_key) |>
    dplyr::filter(sum(side == "gem") > 1 | sum(side == "bff") > 1) |>
    dplyr::ungroup()
  if (nrow(multi) == 0)
    return(tibble::tibble(issue = character(), country = character(), gem_id = character(),
                          gem_plant_name = character(), bff_plant_id = character(),
                          bff_plant_name = character(), detail = character()))

  # Would the cluster fall apart using only links both sides agree on?
  both <- edges[edges$link_source == "both", ]
  core <- build_clusters(both, multi$id[multi$side == "gem"], multi$id[multi$side == "bff"]) |>
    dplyr::select(side, id, core_key = cluster_key)
  multi <- dplyr::left_join(multi, core, by = c("side", "id"))

  key_edges <- dplyr::inner_join(edges, multi[multi$side == "gem", c("id", "cluster_key")],
                                 by = c(gem_location_id = "id"))
  one_sided <- key_edges |>
    dplyr::filter(link_source != "both") |>
    dplyr::group_by(cluster_key) |>
    dplyr::summarise(one_sided = paste0(gem_location_id, " - ", bff_plant_id, " (only in ",
                                        dplyr::if_else(link_source == "gem_only", "GEM", "BFF"), ")",
                                        collapse = "; "),
                     .groups = "drop")

  gn <- dplyr::distinct(gem_names, gem_location_id, .keep_all = TRUE)
  bn <- dplyr::distinct(bff_names, bff_plant_id, .keep_all = TRUE)
  multi |>
    dplyr::left_join(gn, by = c(id = "gem_location_id")) |>
    dplyr::left_join(bn, by = c(id = "bff_plant_id")) |>
    dplyr::group_by(cluster_key) |>
    dplyr::summarise(
      n_gem = sum(side == "gem"), n_bff = sum(side == "bff"),
      n_core = dplyr::n_distinct(core_key),
      country = collapse_unique(country[side == "gem"], ", "),
      gem_id = paste(id[side == "gem"], collapse = ", "),
      gem_plant_name = paste(gem_plant_name[side == "gem"], collapse = "; "),
      bff_plant_id = paste(id[side == "bff"], collapse = ", "),
      bff_plant_name = paste(bff_plant_name[side == "bff"], collapse = "; "),
      .groups = "drop"
    ) |>
    dplyr::left_join(one_sided, by = "cluster_key") |>
    dplyr::mutate(
      issue = dplyr::case_when(
        n_core > 1 ~ "Plant cluster held together by a link only one side has (check the link)",
        n_gem == 1 ~ "Several BFF plants share one GEM location (check BFF split is intentional)",
        n_bff == 1 ~ "One BFF plant covers several GEM locations (check GEM split is intentional)",
        TRUE ~ "Several BFF plants and GEM locations in one cluster (check the links)"
      ),
      detail = paste0(n_bff, " BFF : ", n_gem, " GEM",
                      dplyr::if_else(is.na(one_sided), "", paste0("; one-sided links: ", one_sided)))
    ) |>
    dplyr::select(issue, country, gem_id, gem_plant_name, bff_plant_id, bff_plant_name, detail)
}

#' BFF mapping-file comments per cluster
mapping_comments <- function(mapping, gem, clusters) {
  if (is.null(mapping)) return(tibble::tibble(cluster_key = character(), mapping_comment = character()))
  unit_to_loc <- dplyr::distinct(gem, gem_unit_id, gem_location_id)
  m <- mapping |>
    dplyr::filter(!is.na(comments)) |>
    dplyr::left_join(unit_to_loc, by = "gem_unit_id", suffix = c("", "_u")) |>
    dplyr::mutate(gem_location_id = dplyr::coalesce(gem_location_id, gem_location_id_u))
  dplyr::bind_rows(
    m |> dplyr::inner_join(clusters[clusters$side == "bff", ], by = c(bff_plant_id = "id")),
    m |> dplyr::inner_join(clusters[clusters$side == "gem", ], by = c(gem_location_id = "id"))
  ) |>
    dplyr::group_by(cluster_key) |>
    dplyr::summarise(mapping_comment = collapse_unique(comments, "; "), .groups = "drop")
}

#' Build the wide, one-row-per-plant Divergences table from flags + summaries
#'
#' @param flags Flags after [apply_decisions()] with suppressed rows removed.
#' @param notes_by_cluster Output of [cluster_notes()].
divergence_table <- function(flags, paired, notes_by_cluster) {
  if (nrow(flags) == 0) return(NULL)
  per_row <- flags |>
    dplyr::group_by(item_key) |>
    dplyr::summarise(
      flags = paste(field, collapse = ", "),
      review_state = paste(sort(unique(review_state)), collapse = ", "),
      prev_decision = collapse_unique(prev_decision, ", "),
      prev_note = collapse_unique(prev_note, "; "),
      .groups = "drop"
    )
  paired |>
    dplyr::inner_join(per_row, by = c(cluster_key = "item_key")) |>
    dplyr::left_join(notes_by_cluster, by = "cluster_key") |>
    dplyr::mutate(country = gem_country, item_key = cluster_key,
                  decision = NA_character_, scope = NA_character_, apply_to = NA_character_, notes = NA_character_,
                  .sort = pmax(gem_capacity, bff_capacity, na.rm = TRUE)) |>
    dplyr::arrange(country, dplyr::desc(.sort)) |>
    dplyr::select(country, flags, review_state, prev_decision, prev_note,
                  gem_plant_name, bff_plant_name, gem_ids, bff_ids,
                  gem_city, bff_city, gem_province, bff_province, distance_km,
                  gem_capacity, bff_capacity, gem_status, bff_status,
                  gem_technology, bff_technology, gem_start_year, bff_start_year,
                  gem_retire_year, bff_retire_year, gem_last_update, bff_last_update,
                  gem_researcher, gem_wiki_url,
                  dplyr::any_of(c("bff_change_type", "bff_fields_changed")),
                  bff_note_date, bff_note_changes, bff_note_sources,
                  decision, scope, apply_to, notes, item_key)
}

divergence_highlights <- list(
  capacity    = c("gem_capacity", "bff_capacity"),
  status      = c("gem_status", "bff_status"),
  technology  = c("gem_technology", "bff_technology"),
  start_year  = c("gem_start_year", "bff_start_year"),
  retire_year = c("gem_retire_year", "bff_retire_year"),
  location    = c("distance_km", "gem_city", "bff_city")
)

#' Flags for unmatched (one-sided) plants, ready for [apply_decisions()]
unmatched_flags <- function(only, side) {
  only <- only[!only$explained, ]
  tibble::tibble(item_key = only$cluster_key, field = paste0(side, "_only"),
                 gem_value = if (side == "gem") only$status else NA_character_,
                 bff_value = if (side == "bff") only$status else NA_character_)
}

#' One-row-per-plant table for unmatched plants (review tab)
unmatched_table <- function(only, side, flags, notes_by_cluster = NULL) {
  only <- only[!only$explained, ]
  f <- flags[!flags$suppressed, c("item_key", "review_state", "prev_decision", "prev_note")]
  only <- dplyr::inner_join(only, f, by = c(cluster_key = "item_key"))
  if (nrow(only) == 0) return(NULL)
  if (!is.null(notes_by_cluster)) only <- dplyr::left_join(only, notes_by_cluster, by = "cluster_key")
  out <- only |>
    dplyr::mutate(flags = paste0(side, "_only"), item_key = cluster_key,
                  decision = NA_character_, scope = NA_character_, notes = NA_character_,
                  .active = !is.na(status) & stringr::str_detect(status, "operating|construction|planned")) |>
    dplyr::arrange(country, dplyr::desc(.active), dplyr::desc(capacity)) |>
    dplyr::select(country, flags, review_state, prev_decision, prev_note,
                  plant_name, ids, unit_ids, city, province, capacity, status, technology,
                  start_year, retire_year, lat, lon, last_update,
                  dplyr::any_of(c("researcher", "wiki_url", "bff_change_type")),
                  mapping_comment, suggestion, suggested_name, suggested_key,
                  suggested_dist_km, suggested_name_sim,
                  dplyr::any_of(c("bff_note_date", "bff_note_changes", "bff_note_sources")),
                  decision, scope, notes, item_key)
  names(out)[names(out) %in% c("plant_name", "ids", "unit_ids", "city", "province", "capacity", "status",
                                "technology", "start_year", "retire_year", "lat", "lon", "last_update",
                                "researcher", "wiki_url")] <-
    paste0(side, "_", names(out)[names(out) %in% c("plant_name", "ids", "unit_ids", "city", "province", "capacity",
                                                   "status", "technology", "start_year", "retire_year", "lat", "lon",
                                                   "last_update", "researcher", "wiki_url")])
  out
}

#' Explained absences (informational tab)
explained_table <- function(bff_only, gem_only) {
  dplyr::bind_rows(
    bff_only[bff_only$explained, ] |> dplyr::mutate(only_in = "BFF"),
    gem_only[gem_only$explained, ] |> dplyr::mutate(only_in = "GEM")
  ) |>
    dplyr::select(only_in, country, plant_name, ids, capacity, status, mapping_comment) |>
    dplyr::arrange(only_in, country)
}

#' Rows suppressed by earlier decisions (informational, but decisions can be changed here)
reviewed_table <- function(flags_all) {
  r <- flags_all[flags_all$suppressed, ]
  if (nrow(r) == 0) return(NULL)
  r |>
    dplyr::group_by(item_key) |>
    dplyr::summarise(flags = paste(field, collapse = ", "),
                     prev_decision = collapse_unique(prev_decision, ", "),
                     scope = collapse_unique(scope, ", "),
                     prev_note = collapse_unique(prev_note, "; "),
                     prev_decided_on = max(prev_decided_on, na.rm = TRUE),
                     gem_values = paste(field, gem_value, sep = ": ", collapse = "\n"),
                     bff_values = paste(field, bff_value, sep = ": ", collapse = "\n"),
                     .groups = "drop") |>
    dplyr::mutate(decision = NA_character_, notes = NA_character_) |>
    dplyr::relocate(item_key, .after = dplyr::last_col()) |>
    dplyr::rename(prev_scope = scope) |>
    dplyr::mutate(scope = NA_character_, .before = notes)
}
