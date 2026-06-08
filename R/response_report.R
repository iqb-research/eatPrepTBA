response_report_to_tibble <- function(resp,
                                      progress = "Preparing response report") {
  entries <-
    resp %>%
    purrr::compact() %>%
    purrr::list_flatten()

  if (length(entries) == 0) {
    return(tibble::tibble())
  }

  entries %>%
    tibble::enframe(name = NULL) %>%
    dplyr::mutate(
      value = purrr::map(value, response_report_entry_to_tibble,
                         .progress = progress)
    ) %>%
    tidyr::unnest(value)
}

response_report_entry_to_tibble <- function(x) {
  x <- purrr::discard(x, is.null)

  responses <- x$responses
  if (is.null(responses) || length(responses) == 0) {
    responses <- list()
  }
  x$responses <- NULL

  laststate <- x$laststate
  if (is.null(laststate) || length(laststate) == 0) {
    laststate <- NA_character_
  }
  x$laststate <- NULL

  x <-
    purrr::map(x, function(value) {
      if (is.list(value) || length(value) != 1) {
        list(value)
      } else {
        value
      }
    })

  x$responses <- list(responses)
  x$laststate <- if (is.list(laststate)) list(laststate) else laststate

  tibble::as_tibble_row(x)
}

filter_response_units <- function(responses_raw, units_filter_off = NULL) {
  if (is.null(units_filter_off) || nrow(responses_raw) == 0) {
    return(responses_raw)
  }

  unit_filter_cols <- intersect(c("unitname", "originalUnitId"), names(responses_raw))

  if (length(unit_filter_cols) == 0) {
    return(responses_raw)
  }

  responses_raw %>%
    dplyr::filter(
      !dplyr::if_any(
        dplyr::all_of(unit_filter_cols),
        function(x) x %in% units_filter_off
      )
    )
}

preserve_empty_response_payloads <- function(responses) {
  if (!"responses" %in% names(responses)) {
    responses$responses <- NA_character_
  }

  if ("coded" %in% names(responses)) {
    empty_coded_payload <- is.na(responses$responses) &
      !is.na(responses$coded) &
      responses$coded == "[]"
    responses$responses[empty_coded_payload] <- "[]"

    if ("coded_ts" %in% names(responses)) {
      if (!"responses_ts" %in% names(responses)) {
        responses$responses_ts <- NA_character_
      }
      responses$responses_ts[empty_coded_payload] <- responses$coded_ts[empty_coded_payload]
    }
  }

  responses
}

expected_response_slot_ids <- function() {
  list(
    required = c("elementCodes", "stateVariableCodes"),
    optional = c("geometryVariableCodes")
  )
}

response_payload_slot_ids <- function(payload, is_parsed = TRUE) {
  if (is.null(payload) || length(payload) == 0) {
    return(character())
  }

  if (!is_parsed) {
    payload <- payload[!is.na(payload) & payload != ""]

    if (length(payload) == 0) {
      return(character())
    }

    payload <- purrr::map(
      payload,
      function(x) {
        tryCatch(
          jsonlite::parse_json(x),
          error = function(cnd) list()
        )
      }
    ) %>%
      purrr::flatten()
  }

  ids <- purrr::map_chr(
    payload,
    function(x) {
      id <- tryCatch(x$id, error = function(cnd) NULL)

      if (is.null(id) || length(id) == 0 || is.na(id[[1]])) {
        NA_character_
      } else {
        as.character(id[[1]])
      }
    }
  )

  ids[!is.na(ids) & ids != ""]
}

response_slot_diagnostics <- function(responses,
                                      is_parsed = TRUE,
                                      response_col = "responses") {
  expected <- expected_response_slot_ids()

  empty_diagnostics <- list(
    n_payloads = 0L,
    observed = character(),
    missing_required = character(),
    missing_optional = character(),
    unknown = character(),
    missing_required_counts = integer(),
    missing_optional_counts = integer()
  )

  if (!response_col %in% names(responses) || nrow(responses) == 0) {
    return(empty_diagnostics)
  }

  payload_ids <- purrr::map(
    responses[[response_col]],
    response_payload_slot_ids,
    is_parsed = is_parsed
  )

  non_empty_payload_ids <- payload_ids[lengths(payload_ids) > 0]

  if (length(non_empty_payload_ids) == 0) {
    return(empty_diagnostics)
  }

  observed <- unique(unlist(non_empty_payload_ids, use.names = FALSE))
  known <- c(expected$required, expected$optional)

  missing_required_counts <- purrr::map_int(
    stats::setNames(expected$required, expected$required),
    function(id) sum(!purrr::map_lgl(non_empty_payload_ids, function(x) id %in% x))
  )

  missing_optional_counts <- purrr::map_int(
    stats::setNames(expected$optional, expected$optional),
    function(id) sum(!purrr::map_lgl(non_empty_payload_ids, function(x) id %in% x))
  )

  list(
    n_payloads = length(non_empty_payload_ids),
    observed = observed,
    missing_required = names(missing_required_counts)[missing_required_counts > 0],
    missing_optional = names(missing_optional_counts)[missing_optional_counts > 0],
    unknown = setdiff(observed, known),
    missing_required_counts = missing_required_counts,
    missing_optional_counts = missing_optional_counts
  )
}

announce_response_slot_diagnostics <- function(responses,
                                               source = "Response data",
                                               is_parsed = TRUE,
                                               response_col = "responses") {
  diagnostics <- response_slot_diagnostics(
    responses,
    is_parsed = is_parsed,
    response_col = response_col
  )

  if (diagnostics$n_payloads == 0) {
    return(responses)
  }

  if (length(diagnostics$missing_required) > 0) {
    missing_required <- format_response_slot_counts(
      diagnostics$missing_required_counts[diagnostics$missing_required],
      diagnostics$n_payloads
    )

    cli::cli_alert_warning(
      "{source}: missing required response slot ids: {missing_required}. Output behavior is unchanged.",
      wrap = TRUE
    )
  }

  if (length(diagnostics$missing_optional) > 0) {
    missing_optional <- format_response_slot_counts(
      diagnostics$missing_optional_counts[diagnostics$missing_optional],
      diagnostics$n_payloads
    )

    cli::cli_alert_info(
      "{source}: optional response slot ids absent: {missing_optional}. Output behavior is unchanged.",
      wrap = TRUE
    )
  }

  if (length(diagnostics$unknown) > 0) {
    unknown <- format_response_slot_examples(diagnostics$unknown)

    if ("responses" %in% diagnostics$unknown) {
      cli::cli_alert_warning(
        "{source}: found unexpected inner response slot ids: {unknown}. The inner slot id {.field responses} is not part of the expected current Testcenter response-slot structure. Output behavior is unchanged.",
        wrap = TRUE
      )
    } else {
      cli::cli_alert_warning(
        "{source}: found unexpected inner response slot ids: {unknown}. Output behavior is unchanged.",
        wrap = TRUE
      )
    }
  }

  responses
}

format_response_slot_counts <- function(counts, total) {
  paste0(names(counts), " (", as.integer(counts), "/", total, " payloads)", collapse = ", ")
}

format_response_slot_examples <- function(ids, n = 5) {
  ids <- unique(as.character(ids))
  shown <- head(ids, n)
  label <- paste(shown, collapse = ", ")

  if (length(ids) > n) {
    label <- paste0(label, ", and ", length(ids) - n, " more")
  }

  label
}

announce_empty_nested_response_payloads <- function(responses_raw,
                                                    source = "Response report") {
  if (!"responses" %in% names(responses_raw) || nrow(responses_raw) == 0) {
    return(responses_raw)
  }

  empty_payload <- purrr::map_lgl(
    responses_raw$responses,
    function(x) is.null(x) || length(x) == 0
  )
  n_empty <- sum(empty_payload)

  if (n_empty == 0) {
    return(responses_raw)
  }

  if (n_empty == nrow(responses_raw)) {
    cli::cli_alert_warning(
      "{source}: every response row ({n_empty}) has an empty nested payload; rows are kept and will become {.code responses = NA} after preparation."
    )
  } else {
    cli::cli_alert_info(
      "{source}: kept {n_empty} response row{?s} with empty nested payloads; these rows become {.code responses = NA} after preparation."
    )
  }

  responses_raw
}

announce_missing_response_payloads <- function(responses,
                                               source = "Response data") {
  if (!"responses" %in% names(responses) || nrow(responses) == 0) {
    return(responses)
  }

  n_missing <- sum(is.na(responses$responses))

  if (n_missing == 0) {
    return(responses)
  }

  if (n_missing == nrow(responses)) {
    cli::cli_alert_warning(
      "{source}: every response row ({n_missing}) has a missing payload; rows are kept with {.code responses = NA} and should be completed with {.fn complete_design}."
    )
  } else {
    cli::cli_alert_info(
      "{source}: kept {n_missing} response row{?s} with missing payloads as {.code responses = NA}; complete them later with {.fn complete_design}."
    )
  }

  responses
}

announce_response_unit_filter <- function(n_before,
                                          n_after,
                                          units_filter_off = NULL) {
  if (is.null(units_filter_off) || n_before == n_after) {
    return(invisible(NULL))
  }

  n_removed <- n_before - n_after

  if (n_after == 0) {
    cli::cli_alert_warning(
      "{.arg units_filter_off} removed all {n_before} response row{?s}; returning an empty tibble."
    )
  } else {
    cli::cli_alert_info(
      "{.arg units_filter_off} removed {n_removed} response row{?s}."
    )
  }

  invisible(NULL)
}

announce_failed_response_groups <- function(failed_groups) {
  failed_groups <- failed_groups[!is.na(failed_groups) & failed_groups != ""]

  if (length(failed_groups) == 0) {
    return(invisible(NULL))
  }

  cli::cli_alert_warning(
    "Response reports could not be returned or parsed for {length(failed_groups)} group{?s}: {format_response_group_examples(failed_groups)}."
  )

  invisible(NULL)
}

format_response_group_examples <- function(groups, n = 5) {
  groups <- unique(as.character(groups))
  shown <- head(groups, n)
  label <- paste(shown, collapse = ", ")

  if (length(groups) > n) {
    label <- paste0(label, ", and ", length(groups) - n, " more")
  }

  label
}
