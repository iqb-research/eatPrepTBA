#' Get responses directly from Testcenter
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param groups Character. Name of the groups to be retrieved or all groups if not specified.
#' @param units_filter_off Character. Names of the units to be removed from the dataset.
#' @param diagnostics Character. Controls response slot diagnostics. Use `"compact"`
#'   for concise feedback, `"full"` for all details, or `"none"` to
#'   suppress these diagnostics.
#'
#' @description
#' This function returns responses for the selected groups and prepares the
#' nested response report from the Testcenter for further processing with
#' [code_responses()]. Units with empty response payloads are kept as rows with
#' `responses = NA` so that the observed unit structure remains available; final
#' missing codes are assigned later by [complete_design()] when the coded data
#' are checked against the full test design. The raw nested response slot ids are
#' checked for missing required slots and unexpected new slots, but the returned
#' data are not changed by these diagnostics.
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' get_responses,WorkspaceTestcenter-method
setGeneric("get_responses", function(workspace,
                                     groups = NULL,
                                     units_filter_off = NULL,
                                     diagnostics = c("compact", "full", "none")) {
  cli_setting()

  standardGeneric("get_responses")
})

#' @describeIn get_responses Get responses of a given Testcenter workspace
setMethod("get_responses",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace,
                   groups = NULL,
                   units_filter_off = NULL,
                   diagnostics = c("compact", "full", "none")) {
            diagnostics <- match.arg(diagnostics)

            if (is.null(groups)) {
              groups <- get_results(workspace)$groupName
            }

            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            # TODO: Loop, but no safe-run by now
            run_req <- function(group) {
              resp <- base_req(
                method = "GET",
                endpoint = c("workspace", ws_id, "report", "response"),
                query = list(dataIds = group)
              ) %>%
                httr2::req_perform()

              # Hotfix of `Can't retrieve empty body.` To be tested.
              tryCatch(
                httr2::resp_body_json(resp),
                error = function(e) {
                  NULL
                }
              )
            }

            # resp <-
            #   run_safe(run_req,
            #            error_message = "Responses could not be retrieved.")

            resp <-
              groups %>%
              stats::setNames(groups) %>%
              purrr::map(run_req, .progress = "Downloading responses")

            failed_groups <- names(resp)[purrr::map_lgl(resp, is.null)]
            announce_failed_response_groups(failed_groups)

            resp <- purrr::compact(resp)

            if (length(resp) > 0) {
              responses_raw <- response_report_to_tibble(resp)

              if (nrow(responses_raw) == 0) {
                cli::cli_alert_warning("Response reports contained no rows; returning an empty tibble.")
                return(tibble::tibble())
              }

              n_before_filter <- nrow(responses_raw)
              responses_raw <- filter_response_units(responses_raw, units_filter_off)
              announce_response_unit_filter(
                n_before_filter,
                nrow(responses_raw),
                units_filter_off
              )

              if (nrow(responses_raw) == 0) {
                return(tibble::tibble())
              }

              responses_raw <- announce_response_slot_diagnostics(
                responses_raw,
                "Downloaded responses",
                is_parsed = TRUE,
                diagnostics = diagnostics
              )

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
                    group_id = "groupname",
                    login_name = "loginname",
                    login_code = "code",
                    booklet_id = "bookletname",
                    unit_cols,
                    responses_nest = "responses",
                    laststate_nest = "laststate"
                  ))
                ) %>%
                # Hotfix to remove empty group data (better do that earlier?)
                dplyr::filter(
                  !is.na(group_id)
                ) %>%
                dplyr::group_by(
                  dplyr::across(dplyr::any_of(c("group_id", "login_name",
                                                "login_code", "booklet_id",
                                                "unit_key", "unit_alias")))
                ) %>%
                dplyr::summarise(
                  responses_nest = list(purrr::list_flatten(responses_nest)),
                  laststate_nest = list(laststate_nest),
                  .groups = "drop"
                ) %>%
                dplyr::mutate(
                  responses_nest = purrr::map(responses_nest,
                                              function(x) unnest_responses(x, is_parsed = TRUE),
                                              .progress = response_preparation_progress(
                                                "Preparing responses",
                                                "Prepared responses"
                                              )),
                  laststate_nest = purrr::map(laststate_nest,
                                              function(x) unnest_laststate(x),
                                              .progress = response_preparation_progress(
                                                "Preparing last state",
                                                "Prepared last state"
                                              )),
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
                announce_missing_response_payloads("Downloaded responses")
            } else {
              cli::cli_alert_warning("No response reports were returned for the selected groups; returning an empty tibble.")
              tibble::tibble()
            }
          })
