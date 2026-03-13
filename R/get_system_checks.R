#' Get system check reports
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param groups Character. Name of the groups to be retrieved or all groups if not specified.
#' @param prepare Logical. Should the unit responses be prepared, i.e., should the JSON objects in the responses be unpacked? Defaults to `TRUE`.
#'
#' @description
#' This function returns system check reports for the selected  (or all) groups.
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' get_system_checks,WorkspaceTestcenter-method
setGeneric("get_system_checks", function(workspace,
                                         groups = NULL,
                                         prepare = TRUE) {
  cli_setting()

  standardGeneric("get_system_checks")
})

#' @describeIn get_system_checks Get responses of a given Testcenter workspace
setMethod("get_system_checks",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace,
                   groups = NULL,
                   prepare = TRUE) {
            if (is.null(groups)) {
              groups <- list_system_checks(workspace)$id

              if (is.null(groups)) {
                cli::cli_abort("There seems to be no system check data that could be retrieved from this workspace. Please check.")
              }
            }

            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            # TODO: Loop, but no safe-run by now
            run_req <- function(group) {
              base_req(method = "GET",
                       endpoint = c("workspace", ws_id, "report", "sys-check"),
                       query = list(dataIds = group)) %>%
                httr2::req_perform() %>%
                httr2::resp_body_json()
            }

            # resp <-
            #   run_safe(run_req,
            #            error_message = "Responses could not be retrieved.")

            n_groups <- length(groups)

            resp <-
              groups %>%
              purrr::map(run_req, .progress = "Downloading responses")

            if (!is.null(resp)) {
              system_checks <-
                resp %>%
                purrr::map(
                  function(sc) {
                    sc %>%
                      purrr::list_transpose() %>%
                      tibble::as_tibble()
                  }
                ) %>%
                purrr::reduce(dplyr::bind_rows) %>%
                dplyr::mutate(
                  dplyr::across(c(environment, network, fileData),
                                function(x) {
                                  purrr::map(x, function(y) {
                                    if (length(y) > 0) {
                                      y %>%
                                        purrr::list_transpose() %>%
                                        tibble::as_tibble() %>%
                                        dplyr::select(name = label, value) %>%
                                        tidyr::pivot_wider() %>%
                                        tidyr::unnest(cols = dplyr::everything())
                                    } else {
                                      tibble::tibble()
                                    }
                                  })
                                }),
                  dplyr::across(c(questionnaire),
                                function(x) {
                                  purrr::map(x, function(y) {
                                    y %>%
                                      purrr::list_transpose() %>%
                                      tibble::as_tibble() %>%
                                      dplyr::select(name = id, value) %>%
                                      tidyr::pivot_wider() %>%
                                      tidyr::unnest(cols = dplyr::everything())
                                  })
                                })
                ) %>%
                tidyr::unnest(
                  c(environment, network, unit, fileData, questionnaire),
                  keep_empty = TRUE
                )

              if (prepare) {
                system_checks <-
                  system_checks %>%
                  dplyr::mutate(
                    responses = purrr::map(responses, function(x) {
                      content <-
                        x %>%
                        purrr::map(purrr::pluck, "content")

                      contents <-
                        content %>%
                        purrr::map(function(cont) {
                          if (!is.null(cont) && cont != "[]") {
                            cont %>%
                              jsonlite::parse_json(simplifyVector = TRUE) %>%
                              tibble::as_tibble() %>%
                              dplyr::mutate(value = as.character(value))
                          } else {
                            NULL
                          }
                        }) %>%
                        purrr::reduce(dplyr::bind_rows)
                    })
                  ) %>%
                  tidyr::unnest(responses, keep_empty = TRUE)
              }

              system_checks
            } else {
              tibble::tibble()
            }
          })
