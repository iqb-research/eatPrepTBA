#' List unit files
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve unit list from the API.
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' list_units,WorkspaceStudio-method,WorkspaceTestcenter-method
setGeneric("list_units", function(workspace) {
  standardGeneric("list_units")
})

#' @describeIn list_units List all units in a given IQB Studio workspace
#' @details
#' This function returns a list of all units in a given workspace.
setMethod("list_units",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace) {
            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id
            ws_label <- workspace@ws_label

            run_req <- function(ws_id) {
              req <- function() {
                base_req(method = "GET",
                         endpoint = c("workspaces", ws_id, "units")) %>%
                  httr2::req_perform() %>%
                  httr2::resp_body_json()
              }

              return(req)
            }

            list(ws_id = ws_id, ws_label = ws_label) %>%
              purrr::list_transpose() %>%
              purrr::map(function(ws) {
                ws_units <-
                  run_safe(run_req(ws$ws_id),
                           error_message = "Unit listing was not successful.",
                           default = list()) %>%
                  purrr::map(function(unit) {
                    list(
                      unit_id = unit[["id"]],
                      unit_key = unit[["key"]],
                      unit_label = unit[["name"]]
                    )
                  })

                c(
                  ws,
                  list(units = ws_units)
                )
              })
          })

#' @describeIn list_units List all units in a given IQB Testcenter workspace
#' @details
#' This function serves as a wrapper for [list_files()].
setMethod("list_units",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace) {
            ws_units <- list_files(workspace, type = "Unit")

            ws_units %>%
              dplyr::filter(stringr::str_detect(name, "\\.xml")) %>%
              dplyr::pull("name") %>%
              stringr::str_remove("\\.xml")
          })
