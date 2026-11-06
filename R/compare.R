#' Is a capacity gap significant under the configured thresholds?
#'
#' Vectorised. Missing values are treated as 0 MW.
cap_differs <- function(a, b, cfg) {
  a <- dplyr::coalesce(a, 0)
  b <- dplyr::coalesce(b, 0)
  d <- abs(a - b)
  p <- ifelse(pmax(a, b) > 0, d / pmax(a, b), 0)
  switch(cfg$cap_rule,
         and = d > cfg$cap_abs_mw & p > cfg$cap_pct,
         or  = d > cfg$cap_abs_mw | p > cfg$cap_pct,
         pct = p > cfg$cap_pct,
         abs = d > cfg$cap_abs_mw,
         stop("cap_rule must be one of 'and', 'or', 'pct', 'abs'"))
}

#' Do two capacity profiles (e.g. capacity by status) differ significantly in any category?
profile_differs <- function(p1, p2, cfg) {
  cats <- union(names(p1), names(p2))
  if (length(cats) == 0) return(FALSE)
  any(cap_differs(p1[cats], p2[cats], cfg))
}

#' Do two capacity profiles differ in *mix* (shares), ignoring total size?
#'
#' The second profile is rescaled to the first's total before comparing, so a
#' plant that is simply bigger on one side (already caught by the capacity check)
#' is not also flagged for status/technology. A category present on only one side,
#' or a real shift in shares, is still flagged.
mix_differs <- function(p1, p2, cfg) {
  t1 <- sum(p1); t2 <- sum(p2)
  if (t1 > 0 && t2 > 0) p2 <- p2 * t1 / t2
  profile_differs(p1, p2, cfg)
}

#' Do two year ranges differ, given tolerance and one-sided rules?
years_differ <- function(g_lo, g_hi, b_lo, b_hi, cfg) {
  g_na <- is.na(g_lo); b_na <- is.na(b_lo)
  dplyr::case_when(
    g_na & b_na ~ FALSE,
    g_na ~ cfg$one_sided_years %in% c("gem_missing", "both"),
    b_na ~ cfg$one_sided_years == "both",
    TRUE ~ abs(g_lo - b_lo) > cfg$year_tol | abs(g_hi - b_hi) > cfg$year_tol
  )
}

dist_km <- function(lat1, lon1, lat2, lon2) {
  ok <- !(is.na(lat1) | is.na(lon1) | is.na(lat2) | is.na(lon2))
  out <- rep(NA_real_, length(lat1))
  if (any(ok)) out[ok] <- geosphere::distHaversine(cbind(lon1[ok], lat1[ok]), cbind(lon2[ok], lat2[ok])) / 1000
  out
}

format_coords <- function(lat, lon) ifelse(is.na(lat), NA_character_, sprintf("%.4f, %.4f", lat, lon))

#' Join GEM and BFF cluster summaries side by side (clusters present on both sides)
pair_clusters <- function(gem_sum, bff_sum) {
  g <- dplyr::rename_with(gem_sum, ~ paste0("gem_", .x), -cluster_key)
  b <- dplyr::rename_with(bff_sum, ~ paste0("bff_", .x), -cluster_key)
  dplyr::inner_join(g, b, by = "cluster_key") |>
    dplyr::mutate(distance_km = round(dist_km(gem_lat, gem_lon, bff_lat, bff_lon), 2))
}

#' Compare matched plant clusters field by field
#'
#' @param paired Output of [pair_clusters()].
#' @return Long tibble: one row per cluster x diverging field, with the GEM and
#'   BFF values as display strings (also used as the decision fingerprint).
compare_clusters <- function(paired, cfg) {
  p <- paired
  checks <- list(
    capacity = list(
      flag = cap_differs(p$gem_capacity, p$bff_capacity, cfg),
      g = format_mw(p$gem_capacity), b = format_mw(p$bff_capacity)),
    status = list(
      flag = purrr::map2_lgl(p$gem_status_vec, p$bff_status_vec, mix_differs, cfg = cfg),
      g = p$gem_status, b = p$bff_status),
    technology = list(
      flag = purrr::map2_lgl(p$gem_tech_vec, p$bff_tech_vec, mix_differs, cfg = cfg),
      g = p$gem_technology, b = p$bff_technology),
    start_year = list(
      flag = years_differ(p$gem_start_lo, p$gem_start_hi, p$bff_start_lo, p$bff_start_hi, cfg),
      g = p$gem_start_year, b = p$bff_start_year),
    retire_year = list(
      flag = years_differ(p$gem_retire_lo, p$gem_retire_hi, p$bff_retire_lo, p$bff_retire_hi, cfg),
      g = p$gem_retire_year, b = p$bff_retire_year),
    location = list(
      flag = !is.na(p$distance_km) & p$distance_km > cfg$max_dist_km,
      g = format_coords(p$gem_lat, p$gem_lon), b = format_coords(p$bff_lat, p$bff_lon))
  )
  purrr::imap_dfr(checks, function(ch, field) {
    tibble::tibble(item_key = p$cluster_key, field = field,
                   gem_value = ch$g, bff_value = ch$b)[ch$flag, ]
  })
}

#' Have a field's values stayed "the same" since a decision was recorded?
#'
#' Uses the same thresholds as the comparison, so small drift (a few MW, a few
#' hundred metres) does not re-open a reviewed item.
values_unchanged <- function(field, old, new, cfg) {
  if (is.na(old) && is.na(new)) return(TRUE)
  if (is.na(old) || is.na(new)) return(FALSE)
  if (field == "capacity") return(!cap_differs(as.numeric(old), as.numeric(new), cfg))
  if (field %in% c("status", "technology"))
    return(!mix_differs(parse_profile(old), parse_profile(new), cfg))
  if (field %in% c("bff_only", "gem_only", "bff_removed"))
    return(!profile_differs(parse_profile(old), parse_profile(new), cfg))
  if (field %in% c("start_year", "retire_year")) {
    o <- parse_range(old); n <- parse_range(new)
    return(all(abs(o - n) <= cfg$year_tol))
  }
  if (field == "location") {
    o <- as.numeric(strsplit(old, ", ")[[1]]); n <- as.numeric(strsplit(new, ", ")[[1]])
    return(dist_km(o[1], o[2], n[1], n[2]) <= cfg$max_dist_km)
  }
  identical(old, new)
}
