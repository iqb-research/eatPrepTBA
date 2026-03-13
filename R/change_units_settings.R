#' Change settings of multiple units
#'
#' @description
#' This function updates the Studio metadata, i.e., versions for player, editor, or schemer, and the groups and the states of multiple units. To change a single unit, use [change_unit_settings()].
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param unit_ids IDs of the units to be changed.
#' @param player New player version.
#' @param editor New editor version.
#' @param schemer New schemer version.
#' @param group_name New group name.
#' @param state New state.
#'
#' @return NULL
#' @aliases
#' change_units_settings,WorkspaceStudio-method
#' @export
#'
#' @keywords internal
setGeneric("change_units_settings", function(workspace,
                                             unit_ids,
                                             player = NULL,
                                             editor = NULL,
                                             schemer = NULL,
                                             group_name = NULL,
                                             state = NULL) {
  cli_setting()

  standardGeneric("change_units_settings")
})


#' @describeIn change_units_settings Get unit information and coding scheme in a defined workspace
setMethod("change_units_settings",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace,
                   unit_ids,
                   player = NULL,
                   editor = NULL,
                   schemer = NULL,
                   group_name = NULL,
                   state = NULL) {
            unit_ids %>%
              purrr::walk(
                function(id) {
                  change_unit_settings(
                    workspace = workspace,
                    unit_id = id,
                    player = player,
                    editor = editor,
                    schemer = schemer,
                    group_name = group_name,
                    state = state
                  )

                }, .progress = TRUE
              )
          })
