#' Get responses directly from Testcenter
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param groups Character. Name of the groups to be retrieved or all groups if not specified.
#'
#' @description
#' This function returns responses for the selected groups.
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' get_responses,WorkspaceTestcenter-method
setGeneric("get_responses", function(workspace,
                                     groups = NULL) {
  cli_setting()

  standardGeneric("get_responses")
})

#' @describeIn get_responses Get responses of a given Testcenter workspace
setMethod("get_responses",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace,
                   groups = NULL) {
            if (is.null(groups)) {
              groups <- get_results(workspace)$groupName
            }

            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            # TODO: Loop, but no safe-run by now
            run_req <- function(group) {
              resp <- base_req(
                method = "GET",
                endpoint = c("workspace", ws_id, "report", "response"),
                query = list(dataIds = group)
              ) %>%
                httr2::req_perform()

              # Hotfix of `Can't retrieve empty body.` To be tested.
              tryCatch(
                httr2::resp_body_json(resp),
                error = function(e) {
                  warning("Failed to parse response for group: ", paste(group, collapse = ", "))
                  NULL
                }
              )
            }

            # resp <-
            #   run_safe(run_req,
            #            error_message = "Responses could not be retrieved.")

            n_groups <- length(groups)

            resp <-
              groups %>%
              purrr::map(run_req, .progress = "Downloading responses") %>%
              purrr::compact()

            if (!is.null(resp)) {
              responses_raw <-
                resp %>%
                purrr::flatten() %>%
                # Rectangularize (zu tibble)
                tibble::enframe(name = NULL) %>%
                # Schleife zum Spreaden der Einträge (Auslesen in tibble)
                dplyr::mutate(
                  # Ladebalken?
                  # TODO: Was genau fliegt hier raus?
                  value = purrr::map(value, function(x) {
                    x %>%
                      purrr::discard(is.null) %>%
                      tibble::as_tibble()
                  })
                ) %>%
                # Entpacken
                tidyr::unnest(value)

              # For legacy reasons, this has to be added
              # TODO: Can this be removed at a later point in time?
              if (tibble::has_name(responses_raw, "originalUnitId")) {
                unit_cols <- c(
                  unit_key = "originalUnitId",
                  unit_alias = "unitname"
                )

                responses_raw <-
                  responses_raw %>%
                  dplyr::mutate(
                    originalUnitId = ifelse(is.na(originalUnitId) | originalUnitId == "", unitname, originalUnitId)
                  )
              } else {
                unit_cols <- c(
                  unit_key = "unitname"
                )
              }

              responses_raw %>%
                dplyr::select(
                  dplyr::any_of(c(
                    group_id = "groupname",
                    login_name = "loginname",
                    login_code = "code",
                    booklet_id = "bookletname",
                    unit_cols,
                    responses_nest = "responses",
                    laststate_nest = "laststate"
                  ))
                ) %>%
                dplyr::group_by(
                  dplyr::across(dplyr::any_of(c("group_id", "login_name",
                                                "login_code", "booklet_id",
                                                "unit_key", "unit_alias")))
                ) %>%
                dplyr::summarise(
                  responses_nest = list(responses_nest),
                  laststate_nest = list(laststate_nest)
                ) %>%
                dplyr::ungroup() %>%
                dplyr::mutate(
                  responses_nest = purrr::map(responses_nest,
                                              function(x) unnest_responses(x, is_parsed = TRUE),
                                              .progress = "Preparing responses"),
                  laststate_nest = purrr::map(laststate_nest,
                                              function(x) unnest_laststate(x),
                                              .progress = "Preparing last state"),
                ) %>%
                tidyr::unnest(c("responses_nest", "laststate_nest"), keep_empty = TRUE) %>%
                dplyr::group_by(
                  dplyr::across(dplyr::any_of(c("file", "group_id", "login_name",
                                                "login_code", "booklet_id", "unit_key")))
                ) %>%
                tidyr::pivot_wider(
                  names_from = c("id"),
                  values_from = dplyr::any_of(c("content", "ts")),
                  names_glue = "{id}_{.value}"
                ) %>%
                dplyr::ungroup() %>%
                dplyr::rename(
                  dplyr::any_of(c(
                    coded = "responses_content",
                    responses = "elementCodes_content",
                    state_variables = "stateVariableCodes_content",
                    coded_ts = "responses_ts",
                    responses_ts = "elementCodes_ts",
                    state_variables_ts = "stateVariableCodes_ts",
                    player = "PLAYER",
                    presentation_progress = "PRESENTATION_PROGRESS",
                    response_progress = "RESPONSE_PROGRESS",
                    page_no = "CURRENT_PAGE_NR",
                    page_id = "CURRENT_PAGE_ID",
                    page_count = "PAGE_COUNT"
                  ))
                )
            } else {
              tibble::tibble()
            }
          })
