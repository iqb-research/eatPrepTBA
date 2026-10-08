# Collapse coding rows without constructing a tibble for every variable. Most
# variables occur only once; only repeated keys need conflict resolution.
design_order_metadata_merge <- function(raw, progress = FALSE) {
  grouped <- dplyr::group_by(raw[c("unit_key", "variable_id")],
                            .data$unit_key, .data$variable_id)
  groups <- dplyr::group_rows(grouped)
  keys <- dplyr::group_keys(grouped)
  count <- length(groups)
  first <- vapply(groups, `[[`, integer(1), 1L)
  scalar_columns <- c("variable_ref", "variable_source_type", "variable_level",
                      "variable_page", "variable_page_always_visible")
  values <- lapply(raw[scalar_columns], function(column) column[first])
  section <- as.list(raw$variable_section[first])
  element <- as.list(raw$variable_element[first])
  source_refs <- vector("list", count)
  has_derive <- "derive_sources" %in% names(raw)
  has_sources <- "variable_sources" %in% names(raw)
  metadata_progress <- missing_progress_start("Variable metadata", count, progress)
  on.exit(missing_progress_done(metadata_progress), add = TRUE)

  for (i in seq_len(count)) {
    rows <- groups[[i]]
    if (length(rows) > 1L) {
      for (column in scalar_columns) {
        distinct <- unique(raw[[column]][rows][!is.na(raw[[column]][rows])])
        if (column %in% c("variable_ref", "variable_source_type", "variable_level") &&
            length(distinct) > 1L) {
          cli::cli_abort("Conflicting {.field {column}} for unit/variable {keys$unit_key[[i]]} / {keys$variable_id[[i]]}.")
        }
        values[[column]][i] <- if (length(distinct) == 1L) distinct[[1L]] else NA
      }
      path <- function(column) {
        distinct <- unique(as.list(raw[[column]][rows]))
        distinct <- Filter(function(value) length(value) && !all(is.na(value)), distinct)
        if (length(distinct) == 1L) distinct[[1L]] else NA_integer_
      }
      section[i] <- list(path("variable_section"))
      element[i] <- list(path("variable_element"))
    } else {
      if (!length(section[[i]]) || all(is.na(section[[i]]))) section[i] <- list(NA_integer_)
      if (!length(element[[i]]) || all(is.na(element[[i]]))) element[i] <- list(NA_integer_)
    }
    refs <- if (has_derive) as.character(unlist(raw$derive_sources[rows], use.names = FALSE)) else character()
    if (!length(refs) && has_sources) {
      sources <- raw$variable_sources[rows]
      # A single source table is already rectangular. Empty tables contribute
      # no rows, but their columns must remain available in mixed legacy data.
      sources <- if (length(sources) == 1L &&
                       (is.null(sources[[1L]]) || is.data.frame(sources[[1L]])))
        sources[[1L]] else dplyr::bind_rows(sources)
      if (!is.null(sources) && nrow(sources)) {
        if ("variable_source_direct" %in% names(sources) && any(sources$variable_source_direct %in% TRUE)) {
          sources <- sources[sources$variable_source_direct %in% TRUE, , drop = FALSE]
        }
        if ("variable_source_ref" %in% names(sources)) {
          refs <- as.character(sources$variable_source_ref)
        } else if ("variable_source_id" %in% names(sources)) {
          refs <- paste0(".alias:", as.character(sources$variable_source_id))
        }
      }
    }
    source_refs[[i]] <- unique(refs[!is.na(refs) & nzchar(refs) & refs != ".alias:NA"])
    missing_progress_update(metadata_progress)
  }
  tibble::tibble(
    unit_key = keys$unit_key, variable_id = keys$variable_id,
    variable_ref = as.character(values$variable_ref),
    variable_source_type = as.character(values$variable_source_type),
    variable_level = as.integer(values$variable_level),
    variable_page = suppressWarnings(as.numeric(values$variable_page)),
    variable_section = section, variable_element = element,
    variable_page_always_visible = as.logical(values$variable_page_always_visible),
    .source_refs = source_refs
  )
}
