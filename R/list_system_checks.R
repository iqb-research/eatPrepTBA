#' List system check files
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve system check file list from the API.
#'
#' @description
#' This function returns a list of system checks that were completed in a given workspace.
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' list_system_checks,WorkspaceTestcenter-method
setGeneric("list_system_checks", function(workspace) {
  standardGeneric("list_system_checks")
})

#' @describeIn list_system_checks List all system checks in a given Testcenter workspace.
setMethod("list_system_checks",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace) {
            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            run_req <- function() {
              base_req(method = "GET",
                       endpoint = c("workspace", ws_id,
                                    "sys-check", "reports", "overview")) %>%
                httr2::req_perform() %>%
                httr2::resp_body_json()
            }

            resp <-
              run_safe(run_req,
                       error_message = "System checks could not be listed.",
                       default = tibble::tibble())


            if (length(resp) > 0) {
              resp %>%
                purrr::list_transpose() %>%
                tibble::enframe() %>%
                tidyr::pivot_wider() %>%
                dplyr::select(
                  id, count, label
                ) %>%
                tidyr::unnest(c(id, count, label))
            } else {
              cli::cli_alert_info("No system checks were found.")

              return(tibble::tibble())
            }
          })
