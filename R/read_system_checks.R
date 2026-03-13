#' Reads and prepares system check file
#'
#' @param file Character. Path to the csv file from the IQB Testcenter to be read.
#'
#' @description
#' This function only returns the system check data for a downloaded system check file.
#'
#' @return A tibble.
#'
#' @export
read_system_checks <- function(file) {
  system_checks_raw <- readr::read_delim(file, delim = ";")

  system_checks_raw %>%
    dplyr::rename(responses = Responses) %>%
    dplyr::mutate(
      responses = purrr::map(responses, function(x) {
        content <-
          x %>%
          stringr::str_replace_all("`", "\"") %>%
          jsonlite::parse_json() %>%
          purrr::map(purrr::pluck, "content")

        contents <-
          content %>%
          purrr::map(function(cont) {
            if (!is.null(cont) && cont != "[]") {
              cont %>%
                jsonlite::parse_json(simplifyVector = TRUE) %>%
                tibble::as_tibble()
            } else {
              NULL
            }
          }) %>%
          purrr::reduce(dplyr::bind_rows)
      })
    ) %>%
    tidyr::unnest(
      c(responses)
    ) %>%
    dplyr::rename(any_of(c(
      variable_id = "id"
      # group_id = "groupname",
      # login_name = "loginname",
      # code = "code",
      # booklet_id = "bookletname",
      # unit_key = "unitname",
      # player = "PLAYER",
      # presentation_progress = "PRESENTATION_PROGRESS",
      # response_progress = "RESPONSE_PROGRESS",
      # page_no = "CURRENT_PAGE_NR",
      # page_id = "CURRENT_PAGE_ID",
      # page_count = "PAGE_COUNT",
      # variable_id = "id",
      # value = "value",
      # status = "status"
    )))
}
