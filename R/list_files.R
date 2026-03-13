#' List files
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve file list from the API.
#' @param type Character (optional). Type of the files to retrieve from the API. If no type is specified, all files are listed.
#' @param dependencies Logical. Should the dependencies be listed along with the resources? Defaults to `FALSE`.
#'
#' @description
#' Please note the wrapper functions [list_units()], [list_booklets()], and [list_testtakers()].
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' list_files,WorkspaceTestcenter-method
setGeneric("list_files", function(workspace, type = NULL, dependencies = FALSE) {
  cli_setting()

  standardGeneric("list_files")
})

#' @describeIn list_files List all files in a given IQB Testcenter workspace
setMethod("list_files",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace, type = NULL, dependencies = FALSE) {
            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            run_req <- function() {
              base_req(method = "GET",
                       endpoint = c("workspace", ws_id, "files")) %>%
                httr2::req_perform() %>%
                httr2::resp_body_json()
            }

            resp <-
              tryCatch(
                error = function(cnd) {
                  cli::cli_alert_danger("File listing was not successful.",
                                        wrap = TRUE)

                  cli::cli_text("{.strong Status:}  {cnd$status} | {cnd$message}")

                  # Default return
                  return(tibble::tibble())
                },
                run_req()
              )

            all_types <-
              resp %>%
              tibble::enframe(name = "type_filter")

            if (!is.null(type)) {
              all_types <-
                all_types %>%
                dplyr::filter(type_filter %in% type)
            }

            all_files <-
              all_types %>%
              dplyr::select(-type_filter) %>%
              dplyr::mutate(
                value = purrr::map(value,
                                   function(x) {
                                     purrr::list_transpose(x) %>%
                                       tibble::as_tibble()
                                   })
              ) %>%
              tidyr::unnest(value)

            if (dependencies) {
              run_req_dependencies <- function() {
                base_req(method = "POST",
                         endpoint = c("workspace", ws_id, "files-dependencies")) %>%
                  httr2::req_body_json(data = list(body = as.list(all_files$name)), auto_unbox = TRUE) %>%
                  httr2::req_perform() %>%
                  httr2::resp_body_json()
              }

              resp_dependencies <-
                tryCatch(
                  error = function(cnd) {
                    cli::cli_alert_danger("Retrieving file dependencies was not successful. Returning data without dependencies",
                                          wrap = TRUE)

                    cli::cli_text("{.strong Status:}  {cnd$status} | {cnd$message}")

                    # Default return
                    return(resp)
                  },
                  run_req_dependencies()
                )

              all_types_dependencies <-
                resp_dependencies %>%
                tibble::enframe(name = "type_filter")

              if (!is.null(type)) {
                all_types_dependencies <-
                  all_types_dependencies %>%
                  dplyr::filter(type_filter %in% type)
              }

              all_types_dependencies %>%
                dplyr::select(-type_filter) %>%
                dplyr::mutate(
                  value = purrr::map(value,
                                     function(x) {
                                       purrr::list_transpose(x) %>%
                                         tibble::as_tibble()
                                     })
                ) %>%
                tidyr::unnest(value)
            } else {
              all_files
            }

            # TODO: prepare columns `report` and `info`
          })
