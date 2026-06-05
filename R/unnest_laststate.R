unnest_laststate <- function(json) {
  is_missing_laststate <- function(x) {
    is.null(x) || (length(x) == 1 && is.atomic(x) && is.na(x))
  }

  parse_laststate <- function(x) {
    if (is_missing_laststate(x)) {
      return(NULL)
    }

    if (is.character(x) && length(x) == 1) {
      jsonlite::parse_json(x, simplifyVector = TRUE)
    } else {
      x
    }
  }

  if (length(json) == 0 || all(purrr::map_lgl(json, is_missing_laststate))) {
    return(tibble::tibble(PLAYER = NA_character_))
  }

  json_parsed <-
    json %>%
    purrr::map(parse_laststate) %>%
    purrr::compact()

  if (length(json_parsed) == 0) {
    return(tibble::tibble(PLAYER = NA_character_))
  }

  laststate_tbl <-
    json_parsed %>%
    purrr::map(tibble::as_tibble) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::distinct()

  # TODO: This is only necessary due to bugs in older TC versions (2024)
  if (nrow(laststate_tbl) == 1) {
    return(laststate_tbl)
  } else {
    # This (hopefully) addresses the problem that there are multiple laststates around
    laststate_tbl <-
      laststate_tbl %>%
      tidyr::fill(dplyr::everything(), .direction = "downup") %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(c("PLAYER", "RESPONSE_PROGRESS", "PRESENTATION_PROGRESS")),
          function(x) {
            update_laststate(information = dplyr::cur_column(), current_states = x)
          }),
      ) %>%
      dplyr::distinct()

    if (nrow(laststate_tbl) == 1) {
      return(laststate_tbl)
    } else {
      # If it cannot be harmonized, this error will be thrown
      tibble::tibble(laststate_error = TRUE)
    }
  }
}

# TODO: This is only necessary due to bugs in older TC versions (2024)

# The most preferred state comes first
laststates_lookup <-
  list(
    RESPONSE_PROGRESS = c(
      "complete",
      "some",
      "none"
    ),
    PRESENTATION_PROGRESS = c(
      "complete",
      "some",
      "none"
    ),
    PLAYER = c(
      "RUNNING",
      "LOADING"
    )
  )


update_laststate <- function(information, current_states) {
  # Always return the highest available state
  intersect(laststates_lookup[[information]], current_states)[1]
}
