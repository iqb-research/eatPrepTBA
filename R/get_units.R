#' Get multiple units with resources
#'
#' @param workspace [Workspace-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param units Tibble. Recently queried units of this workspace that should be updated. Please note that units that are no longer in the workspace will automatically be deleted by the procedure.
#' @param metadata Logical. Should the metadata be added? Defaults to `TRUE`.
#' @param unit_definition Logical. Should the unit definition be added? Defaults to `FALSE`. This function also infers the pages of the variables from the unit definition.
#'
#' @description
#' This function returns the unit information for multiple units.
#'git
#' @return A tibble.
#' @export
#'
#' @aliases
#' get_units,WorkspaceTestcenter-method,WorkspaceStudio-method
setGeneric("get_units", function(workspace,
                                 units = NULL,
                                 metadata = TRUE,
                                 unit_definition = FALSE) {
  standardGeneric("get_units")
})

#' @describeIn get_units Get multiple unit information and coding schemes in a given Studio workspace
setMethod("get_units",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace,
                   units = NULL,
                   metadata = TRUE,
                   unit_definition = FALSE) {
            cli_setting()

            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id
            ws_label <- workspace@ws_label

            cli::cli_h2("Preparing {.unit-label unit} database")

            cli::cli_h3("Retrieving {.ws-label workspace} settings")

            ws_settings <- get_settings(workspace)

            ws_states <-
              ws_settings %>%
              dplyr::select(ws_id, states) %>%
              tidyr::unnest(states)

            ws_info <-
              ws_settings %>%
              dplyr::select(
                ws_id, ws_label, wsg_id, wsg_label
              )

            cli::cli_text("Prepared settings for {length(ws_id)} {.ws-label workspace{?s}}")

            units_old <- units

            run_req <- function(ws_id) {
              req <- function() {
                base_req(method = "GET",
                         endpoint = c("workspaces", ws_id, "units", "properties")) %>%
                  httr2::req_perform() %>%
                  httr2::resp_body_json(check_type = TRUE)
              }

              return(req)
            }

            cli::cli_h3("Retrieving {.unit-label units}")

            resp_metadata <-
              purrr::map(ws_id,
                         function(ws_id) {
                           run_safe(run_req(ws_id),
                                    error_message = "Units could not be retrieved.",
                                    default = list()) %>%
                             read_units(ws_id = ws_id)
                         }) %>%
              dplyr::bind_rows()

            units_new <-
              resp_metadata %>%
              dplyr::left_join(ws_info, by = c("ws_id")) %>%
              dplyr::relocate(
                names(ws_info),
                .before = "unit_id"
              ) %>%
              # TODO: Does this slow down get_units()? (necessary for state instantiation)
              dplyr::rowwise() %>%
              dplyr::mutate(
              #   # Fresh units might not have a state
                state_id = ifelse("state_id" %in% names(.), state_id, NA_character_)
              ) %>%
              dplyr::ungroup() %>%
              dplyr::left_join(ws_states, by = dplyr::join_by("ws_id", "state_id")) %>%
              dplyr::relocate(
                names(ws_states),
                .after = "group_name"
              ) %>%
              # Filters off empty workspaces
              dplyr::filter(!is.na(unit_id))

            if (!is.null(units_old)) {
              # Filter for units that are available (automatic update)
              units_old <-
                units_old %>%
                dplyr::semi_join(units_new %>% dplyr::select(ws_id, unit_id),
                                 by = dplyr::join_by("ws_id", "unit_id"))

              units_change_old <-
                units_old %>%
                get_last_change() %>%
                dplyr::rename(last_change_old = last_change)

              units_change_new <-
                units_new %>%
                get_last_change() %>%
                dplyr::rename(last_change_new = last_change)

              update_units <-
                units_change_new %>%
                dplyr::left_join(units_change_old, by = dplyr::join_by("unit_id")) %>%
                dplyr::filter(
                  # Units that were not in the workspace or units that have newer schemes
                  is.na(last_change_old) | (!is.na(last_change_old) & (last_change_new > last_change_old))
                ) %>%
                dplyr::select(unit_id)

              n_updates <- nrow(update_units)

              cli::cli_alert_info("Found {cli::no(n_updates)} update{?s} in units.")
            } else {
              update_units <-
                units_new %>%
                dplyr::distinct(unit_id)
            }

            units_to_update <-
              units_new %>%
              dplyr::semi_join(update_units, by = dplyr::join_by("unit_id"))

            if (!is.null(units_old)) {
              units_no_update <-
                units_old %>%
                dplyr::anti_join(update_units, by = dplyr::join_by("unit_id"))
            }

            if (metadata | (!is.null(units_old) && tibble::has_name(units_no_update, "items_list"))) {
              units_to_update <- read_metadata(units = units_to_update)

              if (!is.null(units_old) && tibble::has_name(units_no_update, "unit_metadata")) {
                cli::cli_alert_info("Reading metadata of supplied units.")

                units_no_update <-
                  read_metadata(units = units_no_update) %>%
                  dplyr::select(-dplyr::any_of(c("unit_metadata")))
              }
            }

            if (unit_definition | (!is.null(units_old) && tibble::has_name(units_no_update, "unit_definition"))) {
              units_to_update <- read_definition(units = units_to_update, base_req = base_req)

              if (!is.null(units_old) && !tibble::has_name(units_no_update, "unit_definition")) {
                cli::cli_alert_info("Adding unit definitions of supplied units.")

                units_no_update <- read_definition(units = units_no_update, base_req = base_req)
              }
            }

            units_final <-
              units_to_update %>%
              dplyr::arrange(unit_id)

            if (!is.null(units_old)) {
              units_final <-
                units_no_update %>%
                dplyr::bind_rows(units_to_update) %>%
                dplyr::arrange(unit_id)
            }

            attr(units_final, "ws_settings") <- ws_settings
            class(units_final) <- c("tba_units", class(units_final))

            return(units_final)
          })

read_units <- function(ws, ws_id) {
  if (length(ws) == 0) {
    return(tibble::tibble(ws_id = ws_id))

    cli::cli_alert_info("No units to be retrieved from {.ws-id {ws_id}}")
  }

  ws %>%
    purrr::map(read_unit,
               .progress = list(
                 type ="custom",
                 extra = list(
                   ws_id = ws_id
                 ),
                 format = "Reading {.unit-label units} for {.ws-label workspace} {.ws-id {cli::pb_extra$ws_id}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
                 format_done = "Read {cli::pb_total} {.unit-label unit{?s}} of {.ws-label workspace} {.ws-id {cli::pb_extra$ws_id}} in {cli::pb_elapsed}.",
                 format_failed = "Failed to read {.ws-label workspace} {.ws-id {cli::pb_extra$ws_id}}",
                 show_after = 0,
                 clear = FALSE
               )) %>%
    dplyr::bind_rows() %>%
    dplyr::select(any_of(c(
      unit_id = "id",
      unit_key = "key",
      unit_label = "name",
      group_name = "groupName",
      state_id = "state",
      description = "description",
      unit_variables = "variables",
      unit_metadata = "metadata",
      coding_scheme = "scheme",
      player = "player",
      editor = "editor",
      schemer = "schemer",
      scheme_type = "schemeType",
      last_change_definition = "lastChangedDefinition",
      last_change_definition_user = "lastChangedDefinitionUser",
      last_change_scheme = "lastChangedScheme",
      last_change_scheme_user = "lastChangedSchemeUser",
      last_change_metadata = "lastChangedMetadata",
      last_change_metadata_user = "lastChangedMetadataUser"
    ))) %>%
    dplyr::mutate(ws_id)
}

read_unit <- function(unit) {
  unit_prepared <-
    unit %>%
    purrr::discard(names(.) %in% c("metadata", "variables")) %>%
    purrr::compact() %>%
    tibble::as_tibble()

  metadata <-
    unit %>%
    purrr::pluck("metadata")

  variables <-
    unit %>%
    purrr::pluck("variables")

  if (length(variables) > 0) {
    variables_prep <-
      variables %>%
      purrr::list_transpose() %>%
      tibble::as_tibble()

    # For legacy reasons
    if (!tibble::has_name(variables_prep, "alias")) {
      variables_prep <-
        variables_prep %>%
        dplyr::mutate(
          alias = id
        )
    }

    variables <-
      variables_prep %>%
      dplyr::mutate(
        alias = ifelse(is.na(alias), id, alias)
      ) %>%
      dplyr::select(
        dplyr::any_of(c(
          "variable_id" = "alias",
          "variable_ref" = "id",
          "variable_page" = "page",
          "variable_type" = "type",
          "variable_format" = "format",
          "variable_values" = "values",
          "variable_multiple" = "multiple",
          "variable_nullable" = "nullable",
          "variable_values_complete" = "valuesComplete",
          "variable_value_position_labels" = "valuePositionLabels"
        ))
      ) %>%
      dplyr::mutate(
        # This could be a bug when calling units
        dplyr::across(dplyr::any_of(c("variable_multiple", "variable_nullable", "variable_values_complete")),
                      list_to_logical),
        variable_values = purrr::map(variable_values, function(x) {
          if (length(x) == 0) {
            out <- tibble::tibble(value = NA, value_label = NA)
          } else {
            out <- x %>%
              purrr::list_transpose() %>%
              tibble::as_tibble() %>%
              dplyr::rename(dplyr::any_of(c(
                "value_label" = "label"
              )))
          }
        })
      )
  }

  unit_prepared %>%
    dplyr::mutate(
      metadata = list(metadata),
      variables = list(variables)
    )
}

list_to_logical <- function(x) {
  purrr::map_lgl(x, function(x) {
    if (is.null(x)) {
      return(NA)
    }

    x %>%
      stringr::str_to_upper() %>%
      as.logical()
  })
}

# unit_key <- units_to_update$unit_key[[15]]
# ws_id <- units_to_update$ws_id[[15]]
# unit_id <- units_to_update$unit_id[[15]]

get_definition <- function(unit_key, ws_id, unit_id, base_req) {
  run_req_definition <- function() {
    base_req(method = "GET",
             endpoint = c("workspaces", ws_id, "units", unit_id, "definition")) %>%
      httr2::req_perform() %>%
      httr2::resp_body_json() %>%
      purrr::pluck("definition")
  }

  message <- glue::glue("Unit definition for {{.unit-key {unit_key}}} could not be retrieved.
               Please check: {{.href https://www.iqb-studio.de/#/a/{unit_id}/{unit_id}}}.")

  # Todo: Check if this could also be in a loop somehow?
  # resp_definition <-
  run_safe(run_req_definition,
           error_message = message)

  # if (!is.null(resp_definition)) {
  #   prepare_definition(resp_definition)
  # }
}

read_definition <- function(units, base_req) {
  unit_keys <- units$unit_key
  ws_ids <- units$ws_id
  n_ws_ids <- length(unique(ws_ids))

  if (length(unit_keys) > 0) {
    units_def <-
      units %>%
      dplyr::mutate(
        definition = purrr::pmap_chr(
          list(unit_key, ws_id, unit_id),
          function(unit_key, ws_id, unit_id) get_definition(unit_key, ws_id, unit_id, base_req),
          .progress = list(
            type ="custom",
            extra = list(
              unit_keys = pad_ids(unit_keys),
              ws_ids = ws_ids,
              n_ws_ids = n_ws_ids
            ),
            format = "Retrieving unit definitions for {.ws-label workspace} {.ws-id {cli::pb_extra$ws_ids[cli::pb_current+1]}}, {.unit-label unit} {.unit-key {cli::pb_extra$unit_keys[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
            format_done = "Retrieved unit definitions of {cli::pb_total} {.unit-label unit{?s}} of {cli::pb_extra$n_ws_ids} {.ws-label workspace{?s}} metadata in {cli::pb_elapsed}.",
            format_failed = "Failed at retrieving unit definition for {.ws-label workspace} {.ws-id {cli::pb_extra$ws_ids[cli::pb_current+1]}}, {.unit-label unit} {.unit-key {cli::pb_extra$unit_keys[cli::pb_current+1]}}",
            clear = FALSE
          ))
      )

    # readr::write_rds(units_def, "D:/data/units_def.RData")
    # units_def <- readr::read_rds("D:/data/units_def.RData")

    # units_def %>% dplyr::slice(12)

    unit_pages_prep <-
      units_def %>%
      dplyr::select(ws_id, unit_id, unit_key, definition) %>%
      dplyr::group_by(ws_id) %>%
      dplyr::mutate(
        # The process is pretty memory-demanding; therefore batching is applied
        batch = ((seq_along(ws_id) - 1) %/% 10) + 1
      ) %>%
      dplyr::ungroup()

    unit_pages_nest <-
      unit_pages_prep %>%
      tidyr::nest(
        variable_pages = c(unit_id, unit_key, definition)
      )

    ws_ids <- unit_pages_nest$ws_id
    batches <- unit_pages_nest$batch
    n_ws_batches <- unit_pages_nest %>%
      dplyr::group_by(ws_id) %>%
      dplyr::mutate(max_batch = max(batch)) %>%
      dplyr::pull(max_batch)
    n_ws_ids <- length(unique(ws_ids))

    cli::cli_h3("Extracting page information from unit definitions")

    missing_string <- "___MISSING___"

    unit_pages <-
      unit_pages_nest %>%
      dplyr::mutate(
        variable_pages = purrr::map(variable_pages,
                                    eatAutoCode::extract_variable_location,
                                    missing = missing_string,
                                    .progress = list(
                                      type ="custom",
                                      extra = list(
                                        ws_ids = ws_ids,
                                        batches = batches,
                                        n_ws_ids = n_ws_ids,
                                        n_ws_batches = n_ws_batches
                                      ),
                                      show_after = 0,
                                      format = "Extracting pages from unit definitions for {.ws-label workspace} {.ws-id {cli::pb_extra$ws_ids[cli::pb_current+1]}}, batch {cli::pb_extra$batches[cli::pb_current+1]} of {cli::pb_extra$n_ws_batches[cli::pb_current+1]} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
                                      format_done = "Extracted pages from unit definitions from {cli::pb_extra$n_ws_ids} {.ws-label workspace{?s}} metadata in {cli::pb_elapsed}.",
                                      format_failed = "Failed at extracting information from unit definition for {.ws-label workspace} {.ws-id {cli::pb_extra$ws_ids[cli::pb_current+1]}}",
                                      clear = FALSE
                                    ))
      )
    # unit_pages %>% .$variable_pages %>% .[[1]] %>% .$variable_pages
    # variable_pages %>% dplyr::count(value_default) %>% View
    # variable_pages %>% View

    variable_pages <-
      unit_pages %>%
      tidyr::unnest(variable_pages) %>%
      dplyr::mutate(
        variable_pages = purrr::map(variable_pages, function(variable_pages) {
          variable_pages %>%
            tibble::as_tibble() %>%
            dplyr::mutate(dplyr::across(dplyr::any_of(c("value_default")),
                                        function(x) {
                                          # To conform to coding routine
                                          if (typeof(x) == "logical") {
                                            stringr::str_to_lower(x)
                                          } else {
                                            ifelse(x == missing_string, NA_character_, as.character(x))
                                          }
                                        }))
        })
      ) %>%
      tidyr::unnest(variable_pages, keep_empty = TRUE) %>%
      tidyr::unnest(variable_path, keep_empty = TRUE) %>%
      dplyr::select(
        dplyr::all_of(c(
          "ws_id",
          "unit_id",
          "unit_key"
        )),
        dplyr::any_of(c(
          "variable_ref" = "variable_ref",
          "variable_page_always_visible" = "variable_page_always_visible",
          "variable_dependencies" = "variable_dependencies",
          "value_default" = "value_default",

          "variable_page" = "pages",
          "variable_section" = "sections",
          "variable_element" = "elements",
          "variable_content" = "content"
        ))
      ) %>%
      dplyr::mutate(
        variable_dependencies = purrr::map2(variable_page_always_visible,
                                            variable_dependencies,
                                            function(page_always_visible, dependencies) {
                                              if (page_always_visible & length(dependencies) != 0) {
                                                dependencies
                                              } else {
                                                tibble::tibble(
                                                  variable_dependency_ref = NA_character_,
                                                  variable_dependency_path = tibble::tibble(),
                                                  variable_dependency_page_always_visible = NA)
                                              }
                                            })
      ) %>%
      tidyr::unnest(variable_dependencies, keep_empty = TRUE) %>%
      tidyr::unnest(dplyr::any_of("variable_dependency_path"), keep_empty = TRUE) %>%
      dplyr::filter(is.na(variable_dependency_page_always_visible) |
                      !variable_dependency_page_always_visible) %>%
      dplyr::rename(
        dplyr::any_of(c(
          "variable_ref" = "variable_ref",
          "variable_page_always_visible" = "variable_page_always_visible",
          "variable_dependencies" = "variable_dependencies",
          "value_default" = "value_default",

          "variable_dependency_page" = "pages",
          "variable_dependency_section" = "sections",
          "variable_dependency_content" = "content",
          "variable_dependency_element" = "element"
        ))
      ) %>%
      dplyr::distinct() %>%

      dplyr::group_by(dplyr::across(
        c(
          dplyr::all_of(c("ws_id", "unit_id", "unit_key")),
          dplyr::any_of(c("variable_ref",
                          "value_default",
                          "variable_page",
                          "variable_section",
                          "variable_content",
                          "variable_element",
                          "variable_page_always_visible")))
      )) %>%
      dplyr::summarise(
        dplyr::across(dplyr::any_of(c("variable_dependency_page",
                                      "variable_dependency_section",
                                      "variable_dependency_content",
                                      "variable_dependency_element")),
                      function(x) safe_min(x)),
        .groups = "drop"
      ) %>%
      dplyr::group_by(dplyr::across(
        c(
          dplyr::all_of(c("ws_id", "unit_id", "unit_key")),
          dplyr::any_of(c("variable_page_always_visible"))
        )
      )) %>%
      dplyr::mutate(
        # The minimum page number that is not always visible
        variable_page_min = ifelse(!variable_page_always_visible, safe_min(variable_page), NA)
      ) %>%
      dplyr::group_by(dplyr::across(
        c(
          dplyr::all_of(c("ws_id", "unit_id", "unit_key"))
        )
      )) %>%
      tidyr::fill(variable_page_min, .direction = "updown") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        dplyr::across(dplyr::any_of(c(
          "variable_page",
          "variable_dependency_page")),
          function(x) x - .data[["variable_page_min"]])
      ) %>%
      dplyr::select(-dplyr::any_of("variable_page_min"))

    if (tibble::has_name(variable_pages, "variable_dependency_page")) {
      variable_pages <-
        variable_pages %>%
        dplyr::mutate(
          variable_page = dplyr::case_when(
            variable_page_always_visible & !is.na(variable_dependency_page) ~ variable_dependency_page,
            variable_page_always_visible ~ 0,
            .default = variable_page),
          variable_section = ifelse(variable_page_always_visible & !is.na(variable_dependency_section),
                                    variable_dependency_section,
                                    variable_section)
        ) %>%
        dplyr::select(-dplyr::matches("^variable_dependency"))
    }

    units %>%
      dplyr::left_join(
        variable_pages %>% tidyr::nest(variable_pages = -dplyr::any_of(c("ws_id", "unit_id", "unit_key"))),
        by = dplyr::join_by("ws_id", "unit_id", "unit_key")
      ) %>%
      dplyr::left_join(
        unit_pages_prep %>% dplyr::rename(unit_definition = definition),
        by = dplyr::join_by("ws_id", "unit_id", "unit_key")
      )
  } else {
    units
  }
}

get_last_change <- function(units) {
  units %>%
    dplyr::select(unit_id,
                  last_change_definition,
                  last_change_metadata,
                  last_change_scheme) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("last_change"), lubridate::as_datetime),
      last_change = max(dplyr::c_across(dplyr::starts_with("last_change")))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(unit_id, last_change)
}

# setMethod("get_units",
#           signature = signature(workspace = "WorkspaceTestcenter"),
#           function(workspace,
#                    unit_ids = NULL,
#                    unit_keys = NULL,
#                    metadata = TRUE,
#                    unit_definition = FALSE,
#                    coding_scheme = FALSE,
#                    verbose = FALSE) {
#             get_files(
#               workspace = workspace,
#               id = id,
#               type = "unit",
#               list_fun = listUnits,
#               get_fun = function(workspace, id) get_unit(workspace = workspace,
#                                                        id = id,
#                                                        prepare = prepare)
#             ) %>%
#               dplyr::rename(unitname = id)
#           })

safe_min <- function(x, na.rm = FALSE) {
  result <- min(x, na.rm = na.rm)
  if (na.rm && is.infinite(result)) NA else result
}

safe_max <- function(x, na.rm = FALSE) {
  result <- max(x, na.rm = na.rm)
  if (na.rm && is.infinite(result)) NA else result
}
