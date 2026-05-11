#' Reads responses files
#'
#' @param files Character. Vector of paths to the csv files from the IQB Testcenter to be read.
#'
#' @description
#' This function returns responses for downloaded responses files.
#'
#' @return A tibble.
#'
#' @export
read_responses <- function(files) {
  if (length(files) == 1) {
    responses_raw <-
      readr::read_delim(files, delim = ";",
                        col_types = readr::cols(.default = readr::col_character()))
  } else {
    responses_raw <-
      tibble::tibble(
        file = files
      ) %>%
      dplyr::mutate(
        data = purrr::map(file, function(file) {
          readr::read_delim(file, delim = ";",
                            col_types = readr::cols(.default = readr::col_character()))
        })
      ) %>%
      tidyr::unnest(
        data
      )
  }

  prepare_response_table(responses_raw, responses_are_parsed = FALSE)
}
