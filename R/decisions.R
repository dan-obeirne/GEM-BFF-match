decision_values   <- c("agree_to_differ", "gem_updated", "bff_likely_wrong", "investigate")
suppressing_values <- c("agree_to_differ", "gem_updated", "bff_likely_wrong")
scope_values      <- c("until_changed", "permanent")

empty_decisions <- function() {
  tibble::tibble(source_workbook = character(), item_key = character(), field = character(),
                 gem_value = character(), bff_value = character(), decision = character(),
                 scope = character(), note = character(), reviewer = character(),
                 decided_on = as.Date(character()))
}

#' Read the persistent decisions log
read_decisions <- function(cfg) {
  if (!file.exists(cfg$decisions_file)) return(empty_decisions())
  readr::read_csv(cfg$decisions_file, show_col_types = FALSE,
                  col_types = readr::cols(.default = readr::col_character(),
                                          decided_on = readr::col_date()))
}

#' Import decisions filled in on review workbooks into the decisions log
#'
#' Scans every workbook in `cfg$output_dir`. Each workbook's decisions replace
#' whatever was previously imported from that same workbook, so editing or
#' clearing a decision in a workbook is picked up on the next run.
#'
#' A row's decision applies to every field in its `flags` column, or only to the
#' fields listed in `apply_to` (comma-separated) when that is filled in.
#'
#' @return The updated decisions log (also written to `cfg$decisions_file`).
import_decisions <- function(cfg) {
  log <- read_decisions(cfg)
  books <- list.files(cfg$output_dir, pattern = "\\.xlsx$", full.names = TRUE)
  books <- books[!startsWith(basename(books), "~$")]
  if (length(books) == 0) return(log)

  imported <- purrr::map_dfr(books, function(book) {
    sheets <- openxlsx::getSheetNames(book)
    if (!"_values" %in% sheets) return(NULL)
    values <- openxlsx::read.xlsx(book, sheet = "_values")
    review_sheets <- unique(values$sheet)
    purrr::map_dfr(intersect(review_sheets, sheets), function(sh) {
      rows <- openxlsx::read.xlsx(book, sheet = sh, skipEmptyRows = FALSE)
      # Review tabs may have a one-line description above the header row
      if (!"item_key" %in% names(rows)) rows <- openxlsx::read.xlsx(book, sheet = sh, startRow = 2)
      if (!all(c("item_key", "decision") %in% names(rows))) return(NULL)
      rows <- rows[!is.na(rows$decision) & rows$decision != "", , drop = FALSE]
      if (nrow(rows) == 0) return(NULL)
      for (col in c("scope", "notes", "apply_to")) if (!col %in% names(rows)) rows[[col]] <- NA_character_
      rows |>
        dplyr::transmute(item_key, decision = stringr::str_trim(decision),
                         scope = dplyr::coalesce(dplyr::na_if(scope, ""), "until_changed"),
                         note = as.character(notes), apply_to = as.character(apply_to)) |>
        dplyr::inner_join(values[values$sheet == sh, c("item_key", "field", "gem_value", "bff_value")],
                          by = "item_key", relationship = "many-to-many") |>
        dplyr::filter(is.na(apply_to) | apply_to == "" |
                        purrr::map2_lgl(field, apply_to, ~ .x %in% stringr::str_trim(strsplit(.y, ",")[[1]]))) |>
        dplyr::transmute(source_workbook = basename(book), item_key, field,
                         gem_value = as.character(gem_value), bff_value = as.character(bff_value),
                         decision, scope, note, reviewer = cfg$reviewer,
                         decided_on = as.Date(file.mtime(book), tz = Sys.timezone()))
    })
  })

  if (nrow(imported) == 0) imported <- empty_decisions()
  bad <- imported |> dplyr::filter(!decision %in% decision_values | !scope %in% scope_values)
  if (nrow(bad) > 0) {
    warning(nrow(bad), " decision row(s) have an unrecognised decision/scope and were skipped: ",
            paste(unique(paste(bad$decision, bad$scope)), collapse = "; "), call. = FALSE)
    imported <- dplyr::anti_join(imported, bad, by = names(imported))
  }

  # Keep reviewer/date from earlier imports when nothing about the decision changed
  if (nrow(imported) > 0 && nrow(log) > 0) {
    prior <- dplyr::select(log, source_workbook, item_key, field, decision, scope, note,
                           old_reviewer = reviewer, old_date = decided_on)
    imported <- imported |>
      dplyr::left_join(prior, by = c("source_workbook", "item_key", "field", "decision", "scope", "note")) |>
      dplyr::mutate(reviewer = dplyr::coalesce(old_reviewer, reviewer),
                    decided_on = dplyr::coalesce(old_date, decided_on)) |>
      dplyr::select(-old_reviewer, -old_date)
  }

  log <- dplyr::bind_rows(
    dplyr::filter(log, !source_workbook %in% basename(books)),
    imported
  ) |> dplyr::distinct()
  dir.create(dirname(cfg$decisions_file), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(log, cfg$decisions_file, na = "")
  log
}

#' Apply the decisions log to the current set of flags
#'
#' For each flagged (item, field), the most recent decision wins:
#' * no decision                      -> shown, review_state "new"
#' * investigate                      -> shown, "investigating"
#' * suppressing + permanent          -> hidden
#' * suppressing + until_changed      -> hidden while both values are within the
#'                                       comparison thresholds of the recorded
#'                                       values; otherwise shown as
#'                                       "values changed since review"
#'
#' @param flags Long table with item_key, field, gem_value, bff_value.
#' @return `flags` with review_state, prev_decision, prev_note, prev_decided_on, suppressed.
apply_decisions <- function(flags, decisions, cfg) {
  latest <- decisions |>
    dplyr::arrange(item_key, field, dplyr::desc(decided_on), dplyr::desc(source_workbook)) |>
    dplyr::distinct(item_key, field, .keep_all = TRUE) |>
    dplyr::select(item_key, field, d_gem = gem_value, d_bff = bff_value,
                  prev_decision = decision, scope, prev_note = note, prev_decided_on = decided_on)
  out <- dplyr::left_join(flags, latest, by = c("item_key", "field"))
  same <- purrr::pmap_lgl(
    list(out$field, out$d_gem, out$gem_value, out$d_bff, out$bff_value, out$prev_decision),
    function(f, og, ng, ob, nb, d) {
      if (is.na(d)) return(NA)
      values_unchanged(f, og, ng, cfg) && values_unchanged(f, ob, nb, cfg)
    }
  )
  out |>
    dplyr::mutate(
      review_state = dplyr::case_when(
        is.na(prev_decision) ~ "new",
        prev_decision == "investigate" ~ "investigating",
        scope == "permanent" ~ "suppressed",
        same ~ "suppressed",
        TRUE ~ "values changed since review"
      ),
      suppressed = review_state == "suppressed"
    ) |>
    dplyr::select(-d_gem, -d_bff)
}
