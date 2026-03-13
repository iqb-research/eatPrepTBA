#' Get states
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to retrieve state list from the API.
#'
#' @details
#' Returns a list of states of a workspace group in the IQB Studio, i.e., their id, their label, and their associated colors.
#'
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' get_states,WorkspaceStudio-method
setGeneric("get_states", function(workspace) {
  standardGeneric("get_states")
})

#' @describeIn get_states List all units in a defined workspace
setMethod("get_states",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace) {
            base_req <- workspace@login@base_req
            wsg_id <- workspace@wsg_id

            run_req <- function() {
              base_req(method = "GET",
                       endpoint = c("workspace-groups", wsg_id)) %>%
                httr2::req_perform() %>%
                httr2::resp_body_json()
            }

            resp <-
              run_safe(run_req,
                       error_message = "States could not be loaded.")

            if (!is.null(resp$settings$states)) {
              states <-
                resp$settings$states %>%
                purrr::list_transpose() %>%
                tibble::as_tibble() %>%
                tibble::add_case(
                  id = 0,
                  label = "",
                  .before = 1
                )

              return(states)
            }
          })
