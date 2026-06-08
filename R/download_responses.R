#' Downloads responses directly from Testcenter
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param groups Character. Name of the groups to be retrieved or all groups if not specified.
#' @param units_filter_off Character. Names of the units to be removed from the dataset.
#'
#' @description
#' This function downloads the raw response report for the selected groups. It
#' keeps one row per reported unit, including units whose nested response
#' payload is empty. These empty payloads are represented as empty list entries
#' in the raw output; after [get_responses()] prepares the data, they appear as
#' `responses = NA` and are left to [complete_design()] for design-based missing
#' completion. The raw nested response slot ids are checked for missing required
#' slots and unexpected new slots, but the returned data are not changed by these
#' diagnostics.
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' download_responses,WorkspaceTestcenter-method
setGeneric("download_responses", function(workspace,
                                          groups = NULL,
                                          units_filter_off = NULL) {
  cli_setting()

  standardGeneric("download_responses")
})

#' @describeIn download_responses Get responses of a given Testcenter workspace
setMethod("download_responses",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace,
                   groups = NULL,
                   units_filter_off = NULL) {
            if (is.null(groups)) {
              groups <- get_results(workspace)$groupName
            }

            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            # TODO: Loop, but no safe-run by now
            run_req <- function(group) {
              body <- base_req(
                method = "GET",
                endpoint = c("workspace", ws_id, "report", "response"),
                query = list(dataIds = group)
              ) %>%
                httr2::req_perform()

              # Makes it a bit safer in case of an empty body.
              tryCatch(
                body %>% httr2::resp_body_json(),
                error = function(cnd) {
                  return(NULL)
                }
              )
            }

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

              responses_raw <- announce_empty_nested_response_payloads(
                responses_raw,
                "Downloaded response report"
              )

              n_before_filter <- nrow(responses_raw)
              responses_raw <- filter_response_units(responses_raw, units_filter_off)
              announce_response_unit_filter(
                n_before_filter,
                nrow(responses_raw),
                units_filter_off
              )

              responses_raw <- announce_response_slot_diagnostics(
                responses_raw,
                "Downloaded response report",
                is_parsed = TRUE
              )

              responses_raw
            } else {
              cli::cli_alert_warning("No response reports were returned for the selected groups; returning an empty tibble.")
              tibble::tibble()
            }
          })
