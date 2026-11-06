#' Write a review workbook
#'
#' @param path Output .xlsx path.
#' @param summary Named list of data frames shown stacked on the Summary tab.
#' @param sheets Named list; each element is a list with `data` (data frame or
#'   NULL), optional `highlight` (named list: field -> columns to highlight when
#'   that field is in the row's `flags`), and optional `about` (one-line description).
#' @param values Long table (sheet, item_key, field, gem_value, bff_value) saved in a
#'   hidden `_values` tab so decisions can be read back by [import_decisions()].
write_review_workbook <- function(path, summary, sheets, values) {
  wb <- openxlsx::createWorkbook()
  hdr <- function(fill) openxlsx::createStyle(textDecoration = "bold", fgFill = fill, border = "bottom",
                                              wrapText = TRUE, valign = "top")
  st <- list(other = hdr("#E7E6E6"), gem = hdr("#DDEBF7"), bff = hdr("#FCE4D6"), dec = hdr("#E2EFDA"))
  flag_style <- openxlsx::createStyle(fgFill = "#FFEB9C")
  wrap_style <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
  title_style <- openxlsx::createStyle(textDecoration = "bold", fontSize = 13)

  # Summary
  openxlsx::addWorksheet(wb, "Summary")
  r <- 1
  for (nm in names(summary)) {
    openxlsx::writeData(wb, "Summary", nm, startRow = r)
    openxlsx::addStyle(wb, "Summary", title_style, rows = r, cols = 1)
    openxlsx::writeData(wb, "Summary", summary[[nm]], startRow = r + 1, headerStyle = st$other)
    r <- r + nrow(summary[[nm]]) + 3
  }
  openxlsx::setColWidths(wb, "Summary", cols = 1:8, widths = c(55, rep(16, 7)))

  # Dropdown lists
  openxlsx::addWorksheet(wb, "_lists")
  openxlsx::writeData(wb, "_lists", data.frame(decision = decision_values))
  openxlsx::writeData(wb, "_lists", data.frame(scope = scope_values), startCol = 2)

  for (nm in names(sheets)) {
    s <- sheets[[nm]]
    d <- s$data
    openxlsx::addWorksheet(wb, nm)
    start <- 1
    if (!is.null(s$about)) {
      openxlsx::writeData(wb, nm, s$about, startRow = 1)
      openxlsx::addStyle(wb, nm, openxlsx::createStyle(textDecoration = "italic"), rows = 1, cols = 1)
      start <- 2
    }
    if (is.null(d) || nrow(d) == 0) {
      openxlsx::writeData(wb, nm, "Nothing to review.", startRow = start)
      next
    }
    d <- as.data.frame(d)
    openxlsx::writeData(wb, nm, d, startRow = start, headerStyle = st$other, keepNA = FALSE)
    cols <- names(d)
    hrow <- start
    rows <- (start + 1):(start + nrow(d))
    for (i in seq_along(cols)) {
      style <- if (startsWith(cols[i], "gem_")) st$gem else if (startsWith(cols[i], "bff_")) st$bff else
        if (cols[i] %in% c("decision", "scope", "apply_to", "notes")) st$dec else NULL
      if (!is.null(style)) openxlsx::addStyle(wb, nm, style, rows = hrow, cols = i)
    }
    widths <- vapply(cols, function(cn) {
      if (cn %in% c("item_key", "prev_note", "notes", "mapping_comment", "detail")) 40 else
        if (grepl("note_changes|note_sources|values|wiki", cn)) 45 else
          if (grepl("plant_name|status|technology|issue|suggest", cn)) 28 else
            if (grepl("ids|city|province|flags|review_state", cn)) 20 else 12
    }, numeric(1))
    openxlsx::setColWidths(wb, nm, cols = seq_along(cols), widths = widths)
    openxlsx::addStyle(wb, nm, wrap_style, rows = rows, cols = seq_along(cols), gridExpand = TRUE)
    openxlsx::freezePane(wb, nm, firstActiveRow = hrow + 1, firstActiveCol = 3)
    openxlsx::addFilter(wb, nm, rows = hrow, cols = seq_along(cols))

    if (!is.null(s$highlight) && "flags" %in% cols) {
      fl <- strsplit(d$flags, ", ")
      for (field in names(s$highlight)) {
        hit <- which(vapply(fl, function(x) field %in% x, logical(1)))
        hc <- match(s$highlight[[field]], cols)
        hc <- hc[!is.na(hc)]
        if (length(hit) && length(hc))
          openxlsx::addStyle(wb, nm, flag_style, rows = start + hit, cols = hc,
                             gridExpand = TRUE, stack = TRUE)
      }
    }
    if ("decision" %in% cols)
      openxlsx::dataValidation(wb, nm, cols = match("decision", cols), rows = rows, type = "list",
                               value = sprintf("'_lists'!$A$2:$A$%d", length(decision_values) + 1))
    if ("scope" %in% cols)
      openxlsx::dataValidation(wb, nm, cols = match("scope", cols), rows = rows, type = "list",
                               value = sprintf("'_lists'!$B$2:$B$%d", length(scope_values) + 1))
  }

  openxlsx::addWorksheet(wb, "_values")
  openxlsx::writeData(wb, "_values", as.data.frame(values), keepNA = FALSE)
  openxlsx::sheetVisibility(wb)[match(c("_lists", "_values"), names(wb))] <- "hidden"

  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

#' Thresholds table for the Summary tab
settings_table <- function(cfg) {
  tibble::tibble(
    setting = c("cap_rule", "cap_pct", "cap_abs_mw", "max_dist_km", "year_tol", "one_sided_years",
                "exclude_bff_fuels", "suggest_link_km", "suggest_name_sim"),
    value = c(cfg$cap_rule, cfg$cap_pct, cfg$cap_abs_mw, cfg$max_dist_km, cfg$year_tol, cfg$one_sided_years,
              paste(cfg$exclude_bff_fuels, collapse = ", "), cfg$suggest_link_km, cfg$suggest_name_sim)
  )
}

#' Flag counts by country and field for the Summary tab
flag_counts <- function(flags_shown, sum_tbl) {
  if (nrow(flags_shown) == 0) return(tibble::tibble(note = "No divergences"))
  flags_shown |>
    dplyr::left_join(dplyr::select(sum_tbl, cluster_key, country), by = c(item_key = "cluster_key")) |>
    dplyr::count(country, field) |>
    tidyr::pivot_wider(names_from = field, values_from = n, values_fill = 0) |>
    dplyr::arrange(country)
}

output_path <- function(cfg, prefix) {
  file.path(cfg$output_dir, paste0(prefix, "-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".xlsx"))
}
