#' Get workspace settings
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param metadata Logical. Should the unit and item metadata profiles be retrieved. Defaults to `TRUE`.
#'
#' @description
#' This function returns the workspace settings for a single workspace.
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' get_settings,WorkspaceStudio-method
setGeneric("get_settings", function(workspace, metadata = TRUE) {
  standardGeneric("get_settings")
})

#' @describeIn get_settings Get unit information and coding scheme in a defined workspace
setMethod("get_settings",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace, metadata = TRUE) {
            # TODO: This function should only run once (for access_workspace or for access_workspace_group)
            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id
            wsg_id <- workspace@wsg_id

            # General metadata
            ws_general <-
              tibble::tibble(
                ws_id = as.integer(ws_id),
                ws_label = workspace@ws_label,
                wsg_id = as.integer(wsg_id),
                wsg_label = workspace@wsg_label,
              )

            # Workspace unit groups and default settings
            run_req_ws <- function(ws_id) {
              req <- function() {
                base_req(method = "GET",
                         endpoint = c("workspaces", ws_id)) %>%
                  httr2::req_perform() %>%
                  httr2::resp_body_json()
              }

              return(req)
            }

            resp_ws <-
              purrr::map(
                ws_id,
                function(ws_id) {
                  run_safe(run_req_ws(ws_id),
                           error_message = "Workspace settings could not be retrieved.",
                           default = list())
                }
              )

            # Default settings
            ws_prep <-
              resp_ws %>%
              purrr::map(function(ws) prepare_ws_settings(ws = ws, metadata = metadata)) %>%
              dplyr::bind_rows()

            # Workspace group settings for states
            run_req_wsg <- function(wsg_id) {
              req <- function() {
                base_req(method = "GET",
                         endpoint = c("workspace-groups", wsg_id)) %>%
                  httr2::req_perform() %>%
                  httr2::resp_body_json()
              }

              return(req)
            }

            resp_wsg <-
              purrr::map(
                unique(wsg_id),
                function(wsg_id) {
                  run_safe(run_req_wsg(wsg_id),
                           error_message = "Workspace group settings could not be retrieved.",
                           default = list())
                }
              )

            wsg_prep <-
              resp_wsg %>%
              purrr::map(
                function(wsg) prepare_wsg_settings(wsg)
              ) %>%
              dplyr::bind_rows()

            ws_general %>%
              dplyr::left_join(ws_prep, by = dplyr::join_by("ws_id")) %>%
              dplyr::left_join(wsg_prep, by = dplyr::join_by("wsg_id"))
          })

prepare_ws_settings <- function(ws, metadata) {
  if (!is.null(ws$settings)) {
    ws_defaults <-
      ws$settings %>%
      purrr::discard(names(.) %in% c("unitGroups", "states")) %>%
      purrr::compact() %>%
      tibble::as_tibble() %>%
      dplyr::rename(
        # TODO: Add other entries to this list
        dplyr::any_of(c(
          default_editor = "defaultEditor",
          default_player = "defaultPlayer",
          default_schemer = "defaultSchemer",
          item_md_profile = "itemMDProfile",
          unit_md_profile = "unitMDProfile",
          stable_modules_only = "stableModulesOnly"
        ))
      ) %>%
      dplyr::mutate(
        ws_id = ws$id
      )

    # Unit groups
    if (!is.null(ws$settings$unitGroups)) {
      ws_groups <-
        ws$settings$unitGroups %>%
        unlist() %>%
        sort()

      ws_defaults <-
        ws_defaults %>%
        dplyr::mutate(groups = list(ws_groups))
    }

    ws_defaults
  } else {
    tibble::tibble(ws_id = ws$id)
  }
}

prepare_wsg_settings <- function(wsg) {
  if (!is.null(wsg$settings$states) & length(wsg$settings$states) != 0) {
    wsg$settings$states %>%
      purrr::map(function(x) unlist(x) %>% tibble::enframe() %>% tidyr::pivot_wider()) %>%
      purrr::reduce(dplyr::bind_rows) %>%
      dplyr::rename(
        state_id = id,
        state_label = label,
        state_color = color
      ) %>%
      tidyr::nest(states = dplyr::any_of(c("state_id", "state_color", "state_label"))) %>%
      dplyr::mutate(
        wsg_id = wsg$id
      )
  } else {
    tibble::tibble(
      wsg_id = wsg$id,
      states = tibble::tibble(state_id = NA)
    )
  }
}
