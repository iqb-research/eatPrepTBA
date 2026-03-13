#' List groups
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to retrieve group list from the API.
#'
#' @return A character vector.
#' @export
#'
#' @aliases
#' list_groups,WorkspaceStudio-method
setGeneric("list_groups", function(workspace) {
  cli_setting()

  standardGeneric("list_groups")
})

#' @describeIn list_groups List all groups in a defined workspace
setMethod("list_groups",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace) {
            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            run_req <- function() {
              base_req(method = "GET",
                       endpoint = c("workspaces", ws_id, "groups")) %>%
                httr2::req_perform() %>%
                httr2::resp_body_json()
            }

            resp <-
              run_safe(run_req,
                       error_message = "Group listing was not successful.",
                       default = tibble::tibble())

            resp %>%
              purrr::list_simplify()
          })
