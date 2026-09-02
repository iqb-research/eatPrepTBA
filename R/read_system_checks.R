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
  assert_existing_files(file, "file")

  system_checks_raw <- readr::read_delim(file, delim = ";")

  system_checks_raw %>%
    dplyr::rename(responses = Responses) %>%
    dplyr::mutate(
      responses = purrr::map2(responses, seq_along(responses), function(x, row_idx) {
        # Keep source rows with missing response payloads available after unnesting.
        if (is.null(x) || length(x) == 0 || is.na(x) || x == "") {
          return(tibble::tibble())
        }

        # replace backticks that appear in the export
        s_fixed <- stringr::str_replace_all(x, c("`" = "\""))

        # quick validate before parsing
        if (!jsonlite::validate(s_fixed)) {
          warning("Responses JSON invalid or not parsable at row ", row_idx)
          return(tibble::tibble())
        }

        parsed <- tryCatch(
          jsonlite::parse_json(s_fixed),
          error = function(e) {
            warning("Failed to parse Responses JSON at row ", row_idx, ": ", e$message)
            return(NULL)
          }
        )

        if (is.null(parsed)) {
          return(tibble::tibble())
        }
        content <- parsed %>% purrr::map(purrr::pluck, "content")

        contents <-
          content %>%
          purrr::map(function(cont) {
            if (!is.null(cont) &&
                length(cont) > 0 &&
                !is.na(cont) &&
                cont != "" &&
                cont != "[]") {
              # cont itself can be JSON; parse it into a tibble
              tryCatch(
                jsonlite::parse_json(cont, simplifyVector = TRUE) %>%
                  tibble::as_tibble(),
                error = function(e) {
                  warning("Inner content JSON parse failed at row ", row_idx, ": ", e$message)
                  tibble::tibble()
                }
              )
            } else {
              tibble::tibble()
            }
          }) %>%
          purrr::reduce(dplyr::bind_rows, .init = tibble::tibble())

        contents
      })
    ) %>%
    tidyr::unnest(c(responses), keep_empty = TRUE) %>%
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
