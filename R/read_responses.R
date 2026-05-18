#' Reads responses files
#'
#' @param files Character. Vector of paths to the csv files from the IQB Testcenter to be read.
#'
#' @description
#' This function reads response files downloaded from the IQB Testcenter and
#' prepares them for further processing with [code_responses()]. Rows with empty
#' response payloads are kept as rows with `responses = NA` so that the observed
#' unit structure remains available; final missing codes are assigned later by
#' [complete_design()] when the coded data are checked against the full test
#' design.
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

  # For legacy reasons, this has to be added
  # TODO: Can this be removed at a later point in time?
  if (tibble::has_name(responses_raw, "originalUnitId")) {
    unit_cols <- c(
      unit_key = "originalUnitId",
      unit_alias = "unitname"
    )

    responses_raw <-
      responses_raw %>%
      dplyr::mutate(
        originalUnitId = ifelse(is.na(originalUnitId), unitname, originalUnitId)
      )
  } else {
    unit_cols <- c(
      unit_key = "unitname"
    )
  }

  responses_raw %>%
    dplyr::select(
      dplyr::any_of(c(
        file = "file",
        group_id = "groupname",
        login_name = "loginname",
        login_code = "code",
        booklet_id = "bookletname",
        unit_cols,
        responses_nest = "responses",
        laststate_nest = "laststate"
      ))
    ) %>%
    dplyr::group_by(
      dplyr::across(dplyr::any_of(c("file",
                                    "group_id", "login_name",
                                    "login_code", "booklet_id",
                                    "unit_key", "unit_alias")))
    ) %>%
    dplyr::summarise(
      responses_nest = list(responses_nest),
      laststate_nest = list(laststate_nest)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      responses_nest = purrr::map(responses_nest,
                                  function(x) unnest_responses(x, is_parsed = FALSE),
                                  .progress = "Preparing responses"),
      laststate_nest = purrr::map(laststate_nest,
                                  function(x) unnest_laststate(x),
                                  .progress = "Preparing last state"),
    ) %>%
    tidyr::unnest(c("responses_nest", "laststate_nest"), keep_empty = TRUE) %>%
    dplyr::group_by(
      dplyr::across(dplyr::any_of(c("file", "group_id", "login_name",
                                    "login_code", "booklet_id", "unit_key")))
    ) %>%
    tidyr::pivot_wider(
      names_from = c("id"),
      values_from = dplyr::any_of(c("content", "ts")),
      names_glue = "{id}_{.value}"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(
      dplyr::any_of(c(
        coded = "responses_content",
        responses = "elementCodes_content",
        geometry_variables = "geometryVariableCodes_content",
        state_variables = "stateVariableCodes_content",
        coded_ts = "responses_ts",
        responses_ts = "elementCodes_ts",
        geometry_variables_ts = "geometryVariableCodes_ts",
        state_variables_ts = "stateVariableCodes_ts",
        player = "PLAYER",
        presentation_progress = "PRESENTATION_PROGRESS",
        response_progress = "RESPONSE_PROGRESS",
        page_no = "CURRENT_PAGE_NR",
        page_id = "CURRENT_PAGE_ID",
        page_count = "PAGE_COUNT"
      ))
    )
}
