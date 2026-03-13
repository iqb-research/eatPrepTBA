#' Get logs
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param groups Character. Name of the groups to be retrieved  or all groups if not specified. Please note, that this has to be specified in all-capital letters.
#'
#' @description
#' This function returns logs from the IQB Testcenter
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' get_logs,WorkspaceTestcenter-method
setGeneric("get_logs", function(workspace, groups = NULL) {
  standardGeneric("get_logs")
})

#' @describeIn get_logs Get responses of a given Testcenter workspace
setMethod("get_logs",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace, groups = NULL) {
            if (is.null(groups)) {
              groups <- get_results(workspace)$groupName
            }

            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            # TODO: Loop, but no safe-run by now
            run_req <- function(group) {
              resp <- base_req(method = "GET",
                               endpoint = c("workspace", ws_id, "report", "log"),
                               query = list(dataIds = group, useNewVersion = FALSE)) %>%
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

            n_groups <- length(groups)

            resp <-
              groups %>%
              purrr::map(run_req, .progress = "Downloading logs") %>%
              purrr::compact()

            if (!is.null(resp)) {
              logs_raw <-
                resp %>%
                tibble::enframe(name = NULL) %>%
                dplyr::mutate(
                  value = purrr::map(value, function(x) x %>% purrr::list_transpose() %>% tibble::as_tibble(),
                                     .progress = "Preparing logs")
                ) %>%
                tidyr::unnest(value)

              if (tibble::has_name(logs_raw, "originalUnitId")) {
                unit_cols <- c(
                  unit_key = "originalUnitId",
                  unit_alias = "unitname"
                )

                logs_raw <-
                  logs_raw %>%
                  dplyr::mutate(
                    originalUnitId = ifelse(is.na(originalUnitId) | originalUnitId == "", unitname, originalUnitId)
                  )
              } else {
                unit_cols <- c(
                  unit_key = "unitname"
                )
              }

              logs_raw %>%
                dplyr::select(
                  dplyr::any_of(c(
                    group_id = "groupname",
                    login_name = "loginname",
                    login_code = "code",
                    booklet_id = "bookletname",
                    unit_cols,
                    ts = "timestamp",
                    log_entry = "logentry"
                  ))
                )
            } else {
              tibble::tibble()
            }
          })
