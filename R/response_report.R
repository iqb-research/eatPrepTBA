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
    empty_coded_payload <- is.na(responses$responses) & responses$coded == "[]"
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
