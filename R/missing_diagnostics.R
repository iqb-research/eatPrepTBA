# Summarize net changes in row-aligned coding tables. `added` describes this
# invocation, not the persistent response_present origin marker.
missing_change_report <- function(before, after,
                                  added = rep(FALSE, nrow(after)),
                                  reasons = rep(NA_character_, nrow(after)),
                                  classified = TRUE, basis = NULL, detail = TRUE) {
  checkmate::assert_data_frame(before)
  checkmate::assert_data_frame(after, nrows = nrow(before))
  fields <- c("code_type", "code_id", "code_score", "code_status")
  assert_cols(before, fields, "before")
  assert_cols(after, fields, "after")
  checkmate::assert_logical(added, len = nrow(after), any.missing = FALSE)
  checkmate::assert_character(reasons, len = nrow(after))
  checkmate::assert_subset(reasons[!is.na(reasons)], c("order", "sources"))
  checkmate::assert_flag(classified)
  checkmate::assert_flag(detail)
  checkmate::assert_logical(basis, len = nrow(after), any.missing = FALSE,
                            null.ok = TRUE)

  changed <- function(old, new) {
    missing_old <- is.na(old)
    missing_new <- is.na(new)
    xor(missing_old, missing_new) |
      (!missing_old & !missing_new & old != new)
  }
  from <- as.character(before$code_type)
  to <- as.character(after$code_type)
  type_changed <- changed(from, to)
  id_changed <- changed(before$code_id, after$code_id)
  score_changed <- changed(before$code_score, after$code_score)
  status_changed <- changed(as.character(before$code_status),
                             as.character(after$code_status))
  analytical_changed <- type_changed | id_changed | score_changed
  existing <- !added
  first_classified <- existing & is.na(from) & !is.na(to)
  derived_invalid <- rep(FALSE, nrow(after))
  if (!is.null(basis)) {
    derived_invalid <- existing & !basis &
      from %in% "MISSING_INVALID_RESPONSE" & to %in% "MISSING_NOT_REACHED"
  }

  flags <- tibble::tibble(
    n_rows = rep(TRUE, nrow(after)),
    n_added = added,
    n_added_classified = added & !is.na(to),
    n_added_unresolved = added & is.na(to),
    n_existing = existing,
    n_changed = existing & analytical_changed,
    n_unchanged = existing & !analytical_changed,
    n_first_classified = first_classified,
    n_reclassified = existing & type_changed & !first_classified,
    n_derived_invalid_to_not_reached = derived_invalid,
    n_only_id_or_score = existing & !type_changed & (id_changed | score_changed),
    n_type_changed = existing & type_changed,
    n_id_changed = existing & id_changed,
    n_score_changed = existing & score_changed,
    n_status_changed = status_changed,
    n_unchanged_order = existing & !analytical_changed & reasons %in% "order",
    n_unchanged_sources = existing & !analytical_changed & reasons %in% "sources"
  )
  totals <- lapply(flags, sum)
  transition_rows <- existing & type_changed
  transitions <- tibble::tibble(
    from_code_type = from[transition_rows],
    to_code_type = to[transition_rows],
    change = ifelse(first_classified[transition_rows], "first_classification",
                     ifelse(derived_invalid[transition_rows],
                            "derived_invalid_to_not_reached", "reclassification"))
  ) %>%
    dplyr::count(.data$from_code_type, .data$to_code_type, .data$change, name = "n")

  # Never carry person identifiers or response values into printed diagnostics.
  unit_keys <- intersect(c("booklet_id", "testlet_no", "unit_booklet_no",
                            "unit_key", "unit_alias"), names(after))
  if (!detail) {
    by_unit <- tibble::tibble()
  } else if (length(unit_keys)) {
    by_unit <- dplyr::bind_cols(tibble::as_tibble(after[unit_keys]), flags) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(unit_keys))) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(names(flags)), sum),
                       .groups = "drop")
  } else {
    by_unit <- tibble::as_tibble(totals)
    if (!nrow(after)) by_unit <- by_unit[0, ]
  }
  list(classified = classified, totals = totals,
       transitions = transitions, by_unit = by_unit)
}

# Emit one message condition, so callers composing completion and recoding can
# print a single combined report without changing the returned response table.
emit_missing_report <- function(report, diagnostics = c("compact", "full", "none"),
                                source = "recode_missings") {
  diagnostics <- match.arg(diagnostics)
  if (diagnostics == "none") return(invisible(report))
  checkmate::assert_string(source)
  n <- report$totals
  count <- format_response_count
  lines <- c("i" = paste0(source, "(): ", count(n$n_rows), " output rows."))

  if (!report$classified) {
    lines <- c(lines, "*" = paste0("Added: ", count(n$n_added), " rows."),
               "*" = "Completion only: missing classification and position assignment were not run.")
    coding_unchanged <- n$n_changed == 0L && n$n_status_changed == 0L
    lines <- c(lines, "*" = if (coding_unchanged) {
      "Existing coding fields were preserved; newly added coding fields remain missing."
    } else {
      paste0("Existing rows with analytical changes: ", count(n$n_changed),
             "; technical status changes: ", count(n$n_status_changed), ".")
    })
  } else {
    lines <- c(lines,
      "*" = paste0("Added: ", count(n$n_added), " rows (",
                     count(n$n_added_classified), " classified; ",
                     count(n$n_added_unresolved), " still unclassified)."),
      "*" = paste0("Existing rows: ", count(n$n_changed),
                     " analytically changed; ", count(n$n_unchanged), " unchanged.")
    )
    if (n$n_first_classified > 0L) {
      lines <- c(lines, "*" = paste0("First classifications: ", count(n$n_first_classified), "."))
    }
    transitions <- report$transitions
    for (i in which(transitions$change != "first_classification")) {
      to <- if (is.na(transitions$to_code_type[[i]])) "unclassified" else transitions$to_code_type[[i]]
      detail <- if (transitions$change[[i]] == "derived_invalid_to_not_reached") " (derived)" else ""
      lines <- c(lines, "*" = paste0(transitions$from_code_type[[i]], " -> ", to,
                                      detail, ": ", count(transitions$n[[i]]), "."))
    }
    if (n$n_only_id_or_score > 0L) {
      lines <- c(lines, "*" = paste0("Only code ID or score changed: ",
                                      count(n$n_only_id_or_score), " further existing rows."))
    }
    if (n$n_unchanged_order + n$n_unchanged_sources > 0L) {
      lines <- c(lines, "*" = paste0("Among unchanged existing rows: ",
        count(n$n_unchanged_order), " lack reliable order; ",
        count(n$n_unchanged_sources), " lack complete source information."))
    }
    lines <- c(lines, "*" = paste0("Technical status changes: ", count(n$n_status_changed), "."))
  }

  if (diagnostics == "full") {
    lines <- c(lines, "*" = paste0("Changed fields in existing rows (counts overlap): type ",
      count(n$n_type_changed), "; ID ", count(n$n_id_changed),
      "; score ", count(n$n_score_changed), "."))
    initial <- report$transitions[report$transitions$change == "first_classification", ]
    for (i in seq_len(nrow(initial))) {
      lines <- c(lines, "*" = paste0("Previously unclassified -> ", initial$to_code_type[[i]],
                                      ": ", count(initial$n[[i]]), "."))
    }
    groups <- report$by_unit
    unit_keys <- setdiff(names(groups), names(n))
    for (i in seq_len(nrow(groups))) {
      values <- vapply(unit_keys, function(key) {
        value <- groups[[key]][[i]]
        if (is.na(value)) "NA" else as.character(value)
      }, character(1))
      label <- if (length(unit_keys)) paste(paste0(unit_keys, "=", values), collapse = ", ") else "All rows"
      detail <- paste0(label, ": ", count(groups$n_added[[i]]),
        " added; ", count(groups$n_changed[[i]]), " existing changed; ",
        count(groups$n_unchanged[[i]]), " existing unchanged.")
      if (groups$n_unchanged_order[[i]] + groups$n_unchanged_sources[[i]] > 0L) {
        detail <- paste0(detail, " Among unchanged: ", count(groups$n_unchanged_order[[i]]),
          " lack reliable order; ", count(groups$n_unchanged_sources[[i]]),
          " lack complete source information.")
      }
      lines <- c(lines, "*" = detail)
    }
  }
  # Interpolate each assembled line as a value, so braces in unit keys or
  # aliases remain literal text rather than becoming cli template expressions.
  values <- as.list(unname(lines))
  names(values) <- paste0("report_line_", seq_along(lines))
  templates <- paste0("{", names(values), "}")
  names(templates) <- names(lines)
  cli::cli_inform(templates, .envir = list2env(values, parent = environment()))
  invisible(report)
}
