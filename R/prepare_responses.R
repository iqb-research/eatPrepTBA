#' Prepares responses
#'
#' @param responses Tibble. Responses retrieved from the IQB Testcenter via [get_responses()] or from an extracted csv and read via [read_responses()].
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function returns the responses in a format where the values are still list columns that need to be unpacked.
#'
#' @return A tibble.
#'
#' @export
prepare_responses <- function(responses) {
  if (all(purrr::map_lgl(responses$responses, is_parsed_response_list))) {
    return(prepare_parsed_responses(responses))
  }

  responses %>%
    dplyr::mutate(
      responses = purrr::map(responses, prepare_response_values, .progress = TRUE)
    ) %>%
    tidyr::unnest(
      c(responses)
    ) %>%
    dplyr::rename(dplyr::any_of(c(
      "variable_id" = "id",
      "variable_status" = "status"
    )))
}

prepare_parsed_responses <- function(responses) {
  responses %>%
    dplyr::mutate(
      responses = purrr::map(responses, normalize_parsed_response_list)
    ) %>%
    tidyr::unnest_longer(responses, keep_empty = TRUE) %>%
    tidyr::unnest_wider(responses) %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of("value"), function(x) purrr::map(x, as.list))
    ) %>%
    dplyr::rename(dplyr::any_of(c(
      "variable_id" = "id",
      "variable_status" = "status"
    )))
}

is_parsed_response_list <- function(x) {
  is.list(x) || is_missing_response(x)
}

normalize_parsed_response_list <- function(x) {
  if (is_missing_response(x)) {
    list(list(id = NA_character_))
  } else {
    x
  }
}

is_missing_response <- function(x) {
  is.null(x) || length(x) == 0 || (is.atomic(x) && length(x) == 1 && is.na(x))
}

prepare_response_values <- function(x) {
  if (is_missing_response(x)) {
    return(tibble::tibble(id = NA))
  }

  if (is.character(x)) {
    resp <-
      x %>%
      jsonlite::parse_json(simplifyVector = TRUE) %>%
      tibble::as_tibble()
  } else {
    resp <- response_values_to_tibble(x)
  }

  if (tibble::has_name(resp, "value")) {
    resp %>%
      dplyr::mutate(
        value = purrr::map(value, as.list)
      )
  } else {
    resp
  }
}

response_values_to_tibble <- function(x) {
  tibble::tibble(
    id = vapply(x, function(item) {
      value <- item[["id"]]

      if (is.null(value) || length(value) == 0) {
        NA_character_
      } else {
        as.character(value[[1]])
      }
    }, character(1), USE.NAMES = FALSE),
    status = vapply(x, function(item) {
      value <- item[["status"]]

      if (is.null(value) || length(value) == 0) {
        NA_character_
      } else {
        as.character(value[[1]])
      }
    }, character(1), USE.NAMES = FALSE),
    value = lapply(x, function(item) item[["value"]])
  )
}
