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
#' completion.
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
                error = function(cnd) {
                  cli::cli_alert_warning("Group {group} is empty")
                  return(NULL)
                },
                body %>% httr2::resp_body_json()
              )
            }

            n_groups <- length(groups)

            resp <-
              groups %>%
              purrr::map(run_req, .progress = "Downloading responses")

            if (!is.null(resp)) {
              responses_raw <- response_report_to_tibble(resp)

              responses_raw
            } else {
              tibble::tibble()
            }
          })
