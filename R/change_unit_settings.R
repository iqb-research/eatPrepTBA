#' Change unit settings
#'
#' @description
#' This function updates the Studio metadata, i.e., versions for player, editor, or schemer, and the groups and the states of a single unit. To change multiple units, use [change_units_settings()]. Please note that invalid inputs will result in a default version (e.g., the newest one in case of `editor`, `player`, or `schemer`) or be ignored (in case of group_name or `state`).
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param unit_id Integer. Unit ID to be changed.
#' @param unit_key Character. New unit key. Please note that this has to be between 3 and 20 characters, whereas only capital and small letters of a-z, digits of 0-9, and underscores _ are allowed.
#' @param unit_name Character. New unit name.
#' @param description Character. New unit description / notes.
#' @param player Character. New minor player version, format `"A.B"` (A = major version, B = minor version). E.g., 2.10 (major release 2, minor version 10). Patch version will be omitted (`"A.B.C"` becomes `"A.B"`, e.g., `"2.10.4"` becomes `"2.10"`).
#' @param editor Character. New minor editor version. Patch version will be omitted.
#' @param schemer Character. New minor schemer version. Patch version will be omitted.
#' @param group_name Character. New group name.
#' @param state Numeric or character. New state id (when numeric) or label (when character).
#'
#' @return NULL
#'
#' @aliases
#' change_unit_settings,WorkspaceStudio-method
#'
#' @keywords internal
setGeneric("change_unit_settings", function(workspace,
                                            unit_id,
                                            unit_key = NULL,
                                            unit_name = NULL,
                                            description = NULL,
                                            player = NULL,
                                            editor = NULL,
                                            schemer = NULL,
                                            group_name = NULL,
                                            state = NULL) {
  cli_setting()

  standardGeneric("change_unit_settings")
})


#' @describeIn change_unit_settings Get unit information and coding scheme in a defined workspace
setMethod("change_unit_settings",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace,
                   unit_id,
                   unit_key = NULL,
                   unit_name = NULL,
                   description = NULL,
                   player = NULL,
                   editor = NULL,
                   schemer = NULL,
                   group_name = NULL,
                   state = NULL) {
            cli_setting()

            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            body <-
              settings(
                workspace = workspace,
                unit_id = unit_id,
                unit_key = unit_key,
                unit_name = unit_name,
                description = description,
                player = player,
                editor = editor,
                schemer = schemer,
                group_name = group_name,
                state = state
              )

            run_req <- function() {
              base_req(method = "PATCH",
                       endpoint = c("workspaces", ws_id, "units", unit_id, "properties")) %>%
                httr2::req_body_json(data = body, auto_unbox = TRUE) %>%
                httr2::req_perform()
            }

            # For the error message
            message <- glue::glue("Settings could not be changed for unit with
                                  id {{.unit-id {unit_id}}} {{.url https://www.iqb-studio.de/#/a/{ws_id}/{unit_id}/properties}}.")

            run_safe(run_req,
                     error_message = message)

          })

settings <- function(workspace,
                     unit_id,
                     unit_key,
                     unit_name,
                     description,
                     player,
                     editor,
                     schemer,
                     group_name,
                     state) {
  set <-
    list(
      unit_key = unit_key,
      unit_name = unit_name,
      description = description,
      player = player,
      editor = editor,
      schemer = schemer,
      group_name = group_name,
      state = state
    )

  # This approach ensures that no other arguments become valid
  set_protect <-
    list(
      id = unit_id
    )

  # This pattern ensures that a major.minor or major.minor.patch version is given
  if (!is.null(set$unit_key)) {
    n_letters <- stringr::str_count(set$unit_key)
    if (n_letters >= 3 & n_letters <= 20 & set$unit_key %>% stringr::str_detect("^[A-Za-z0-9_]+$")) {
      set_protect$key <- set$unit_key
    } else {
      cli::cli_alert_warning("{.unit-key unit_key} must be between 3 and 20 characters and only allows for the capital or small letters a-z, digits 0-9, as well as the underscore _", wrap = TRUE)
    }
  }

  if (!is.null(set$unit_name)) {
    set_protect$name <- set$unit_name
  }

  if (!is.null(set$description)) {
    set_protect$description <- set$description
  }

  # This pattern ensures that a major.minor or major.minor.patch version is given
  version_pattern <- "^[0-9]+\\.[0-9]+(\\.[0-9]+)?$"
  minor_pattern <- "^[0-9]+\\.[0-9]+\\.[0-9]+$"
  minor_pattern_exclusive <- "^([0-9]+\\.[0-9]+)\\.[0-9]+$"

  # TODO: Could be refactored as the pattern is always the same (only difference
  # is list location and final string)
  if (! is.null(set$player) && is.character(set$player) && set$player %>% stringr::str_detect(version_pattern)) {
    if (set$player %>% stringr::str_detect(minor_pattern)) {
      set$player <- stringr::str_extract(set$player, minor_pattern_exclusive, group = TRUE)
    }

    set_protect$player <- glue::glue("iqb-player-aspect@{set$player}")
  }

  if (! is.null(set$editor) && is.character(set$editor) && set$editor %>% stringr::str_detect(version_pattern)) {
    if (set$editor %>% stringr::str_detect(minor_pattern)) {
      set$editor <- stringr::str_extract(set$editor, minor_pattern_exclusive, group = TRUE)
    }

    set_protect$editor <- glue::glue("iqb-editor-aspect@{set$editor}")
  }

  if (! is.null(set$schemer) && is.character(set$schemer) && set$schemer %>% stringr::str_detect(version_pattern)) {
    if (set$schemer %>% stringr::str_detect(minor_pattern)) {
      set$schemer <- stringr::str_extract(set$schemer, minor_pattern_exclusive, group = TRUE)
    }

    set_protect$schemer <- glue::glue("iqb-schemer@{set$schemer}")
  }

  if (!is.null(set$group_name)) {
    group_names <- c(list_groups(workspace), "")

    if (!set$group_name %in% group_names) {
      set_protect$groupName <- NULL
      cli::cli_alert_danger("group_name = {set$group_name} is not a valid group name for this workspace")
    } else {
      set_protect$groupName <- set$group_name
    }
  }

  if (!is.null(set$state)) {
    states <- get_states(workspace)

    if (is.null(states)) {
      cli::cli_alert_danger("There are no states defined for this for this workspace")
    } else {
      if (is.numeric(set$state)) {
        if (!set$state %in% states$id) {
          set_protect$state <- NULL
          cli::cli_alert_danger("state = {set$state} is not a valid state for this workspace")
        } else {
          set_protect$state <- set$state
        }
      } else {
        if (!set$state %in% states$label) {
          set_protect$state <- NULL
          cli::cli_alert_danger("state = {set$state} is not a valid state for this workspace")
        } else {
          set_protect$state <- states %>% dplyr::filter(label == set$state) %>% dplyr::pull(id)
        }
      }
    }
  }

  return(set_protect)
}

# settings(workspace, id = 1, state = 2)
