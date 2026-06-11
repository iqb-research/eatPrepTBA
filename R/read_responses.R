#' Reads responses files
#'
#' @param files Character. Vector of paths to the csv files from the IQB Testcenter to be read.
#' @param diagnostics Character. Controls response slot diagnostics. Use `"compact"`
#'   for concise feedback, `"full"` for all details, or `"none"` to
#'   suppress these diagnostics.
#'
#' @description
#' This function reads response files downloaded from the IQB Testcenter and
#' prepares them for further processing with [code_responses()]. Rows with empty
#' response payloads are kept as rows with `responses = NA` so that the observed
#' unit structure remains available; final missing codes are assigned later by
#' [complete_design()] when the coded data are checked against the full test
#' design. The raw nested response slot ids are checked for missing required
#' slots and unexpected new slots, but the returned data are not changed by these
#' diagnostics.
#'
#' @return A tibble.
#'
#' @export
read_responses <- function(files,
                           diagnostics = c("compact", "full", "none")) {
  cli_setting()

  diagnostics <- match.arg(diagnostics)

  responses_raw <- read_response_files(files)

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

  responses_raw <- announce_response_slot_diagnostics(
    responses_raw,
    "Read responses",
    is_parsed = FALSE,
    diagnostics = diagnostics
  )

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
      responses_nest = map_response_preparation(
        responses_nest,
        function(x) unnest_responses(x, is_parsed = FALSE),
        "Preparing responses",
        "Prepared responses",
        diagnostics = diagnostics
      ),
      laststate_nest = map_response_preparation(
        laststate_nest,
        function(x) unnest_laststate(x),
        "Preparing last state",
        "Prepared last state",
        diagnostics = diagnostics
      ),
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
    ) %>%
    preserve_empty_response_payloads() %>%
    announce_missing_response_payloads("Read responses", diagnostics = diagnostics)
}

read_response_files <- function(files) {
  if (length(files) == 1) {
    return(read_response_file(files))
  }

  start <- Sys.time()
  cli::cli_alert_info("Reading {format_response_count(length(files))} response files.")

  responses_raw <-
    tibble::tibble(
      file = files
    ) %>%
    dplyr::mutate(
      data = purrr::map(file, read_response_file)
    )

  cli::cli_text(
    "Read {format_response_count(length(files))} response files in {format_response_elapsed(Sys.time() - start)}."
  )

  start <- Sys.time()
  cli::cli_alert_info("Combining response files.")

  responses_raw <- tidyr::unnest(responses_raw, data)

  cli::cli_text(
    "Combined response files in {format_response_elapsed(Sys.time() - start)}."
  )

  responses_raw
}

read_response_file <- function(file) {
  readr::read_delim(
    file,
    delim = ";",
    col_types = readr::cols(.default = readr::col_character())
  )
}
