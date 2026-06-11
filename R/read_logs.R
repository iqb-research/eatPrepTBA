#' Reads log files
#'
#' @param files Character. Vector of paths to the csv files from the IQB Testcenter to be read.
#'
#' @description
#' This function only returns the logs for a downloaded testtakers file.
#'
#' @return A tibble.
#'
#' @export
read_logs <- function(files) {
  assert_existing_files(files)

  if (length(files) == 1) {
    logs_raw <-
      readr::read_delim(files, delim = ";",
                        col_types = readr::cols(.default = readr::col_character()))
  } else {
    logs_raw <-
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

  # For legacy reasons, this has to be added
  # TODO: Can this be removed at a later point in time?
  if (tibble::has_name(logs_raw, "originalUnitId")) {
    unit_cols <- c(
      unit_key = "originalUnitId",
      unit_alias = "unitname"
    )

    logs_raw <-
      logs_raw %>%
      dplyr::mutate(
        originalUnitId = ifelse(is.na(originalUnitId) | originalUnitId == "", unitname, originalUnitId)
      )
  } else {
    unit_cols <- c(
      unit_key = "unitname"
    )
  }

  logs_raw %>%
    dplyr::select(
      dplyr::any_of(c(
        file = "file",
        group_id = "groupname",
        login_name = "loginname",
        login_code = "code",
        booklet_id = "bookletname",
        unit_cols,
        ts = "timestamp",
        log_entry = "logentry"
      ))
    )
}
