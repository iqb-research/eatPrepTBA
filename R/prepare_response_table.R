prepare_response_table <- function(responses_raw, responses_are_parsed) {
  if (nrow(responses_raw) == 0) {
    return(tibble::tibble())
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
        originalUnitId = ifelse(is.na(originalUnitId) | originalUnitId == "", unitname, originalUnitId)
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
      dplyr::across(dplyr::any_of(c("file", "group_id", "login_name",
                                    "login_code", "booklet_id",
                                    "unit_key", "unit_alias")))
    ) %>%
    dplyr::summarise(
      responses_nest = list(responses_nest),
      laststate_nest = list(laststate_nest),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      responses_nest = purrr::map(responses_nest,
                                  function(x) unnest_responses(x, is_parsed = responses_are_parsed),
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
        state_variables = "stateVariableCodes_content",
        coded_ts = "responses_ts",
        responses_ts = "elementCodes_ts",
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
