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
  checkmate::assert_tibble(responses)
  assert_cols(responses, "responses", "responses")

  responses %>%
    dplyr::mutate(
      responses = purrr::map(responses, function(x) {
        if (!is.null(x) & !is.na(x)) {
          resp <-
            x %>%
            jsonlite::parse_json(simplifyVector = TRUE) %>%
            tibble::as_tibble()
        } else {
          resp <- tibble::tibble(id = NA)
        }

        if (tibble::has_name(resp, "value")) {
          resp %>%
            dplyr::mutate(
              value = purrr::map(value, as.list)
            )
        } else {
          resp
        }
      }, .progress = TRUE)
    ) %>%
    tidyr::unnest(
      c(responses)
    ) %>%
    dplyr::rename(dplyr::any_of(c(
      "variable_id" = "id",
      "variable_status" = "status"
    )))
}
