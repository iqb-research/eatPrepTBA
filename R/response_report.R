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
