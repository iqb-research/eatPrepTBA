#' Get coding report
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to retrieve the coding report.
#'
#' @description
#' This function returns the coding report of the given IQB Studio workspace.
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' get_coding_report,WorkspaceStudio-method
setGeneric("get_coding_report", function(workspace) {
  cli_setting()

  standardGeneric("get_coding_report")
})

#' @describeIn get_coding_report Upload a file in a defined workspace
setMethod("get_coding_report",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace) {
            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            # units <-
            #   list_units(workspace) %>%
            #   purrr::map(function(ws) {
            #     ws$units %>%
            #       purrr::list_transpose() %>%
            #       tibble::as_tibble() %>%
            #       dplyr::mutate(ws_id = ws$ws_id, ws_label = ws$ws_label)
            #   }) %>%
            #   dplyr::bind_rows()

            run_req <- function(ws_id) {
              req <- function() {
                base_req(method = "GET",
                         endpoint = c(
                           "workspaces",
                           ws_id,
                           "units",
                           "scheme"
                         )) %>%
                  httr2::req_perform() %>%
                  httr2::resp_body_json() %>%
                  purrr::list_transpose() %>%
                  tibble::as_tibble()
              }

              return(req)
            }


            coding_reports <-
              purrr::map(ws_id,
                       function(ws_id) {
                         run_safe(run_req(ws_id),
                                  error_message = "Coding report could not be generated..",
                                  default = tibble::tibble())
                       }) %>%
              dplyr::bind_rows()

            coding_reports %>%
              tidyr::extract(
                unit, into = c("ws_id", "unit_id", "unit_key", "unit_label"),
                regex = "<a href=#/a/(\\d+)/(\\d+)>([^:]+): (.+)</a>"
              ) %>%
              dplyr::mutate(
                unit_id = as.integer(unit_id)
              ) %>%
              dplyr::select(
                ws_id,
                unit_id,
                unit_key,
                unit_label,
                variable_id = variable,
                item,
                validation,
                coding_type = codingType
              )
          })
