response_report_to_tibble <- function(resp) {
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
      value = purrr::map(value, response_report_entry_to_tibble)
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
