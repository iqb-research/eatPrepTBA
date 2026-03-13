#' Add workspace groups for a Studio workspace
#'
#' @description
#' This function adds groups to a Studio workspace.
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to add groups
#' @param group_names Character. Names of the groups to be added (already available groups will not result in duplicate groups)
#'
#' @return NULL
#'
#' @aliases
#' add_groups,WorkspaceStudio-method
#'
#' @keywords internal
setGeneric("add_groups", function(workspace,
                                  group_names) {
  cli_setting()

  standardGeneric("add_groups")
})


#' @describeIn add_groups Add groups in a defined workspace
setMethod("add_groups",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace,
                   group_names) {
            cli_setting()

            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            run_req <- function(ws_id, group_name) {
              req <- function(group_name) {
                base_req(method = "PATCH",
                         endpoint = c("workspaces", ws_id, "group-name")) %>%
                  httr2::req_body_json(data = list(groupName = group_name), auto_unbox = TRUE) %>%
                  httr2::req_perform()
              }

              resp <-
                purrr::map(group_names, req) %>%
                purrr::map_int(httr2::resp_status) %>%
                purrr::set_names(group_names)

              if (any(resp != 200)) {
                n_fail <- sum(resp != 200)
                cli::cli_alert_warning("Please note that {n_fail} group{?s} could not be prepared for {.ws-label workspace} {.ws-id {ws_id}}")
              }

              return(resp)
            }

            resp_groups <-
              purrr::map(
                ws_id,
                run_req
              ) %>%
              purrr::set_names(ws_id)
            # n_success <- sum(resp_groups == 200)
            # if (n_success > 0) {
            #   cli::cli_alert_success("Successfully added {n_success} group{?s}
            #                          to the provided {.ws-label workspaces}", wrap = TRUE)
            # }
          })
