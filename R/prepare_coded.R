#' Prepares codes
#'
#' @param responses Tibble. Responses retrieved from the IQB Testcenter via [get_responses()] or from an extracted csv and read via [read_responses()]. It can be used for data gathered by the StarS player.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function returns the responses in a format where the values are still list columns that need to be unpacked.
#'
#' @return A tibble.
#'
#' @export
prepare_coded <- function(responses) {
  responses %>%
    dplyr::mutate(
      coded = purrr::map(coded, function(x) {
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
      c(coded)
    ) %>%
    tidyr::unnest(value) %>% dplyr::mutate(
      value = purrr::map_chr(value, as.character)
    ) %>%
    dplyr::rename(dplyr::any_of(c(
      "variable_id" = "id",
      "code_id" = "code",
      "code_score" = "score",
      "code_status" = "status"
    )))
}
