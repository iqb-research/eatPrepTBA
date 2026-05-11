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

            if (length(resp) > 0) {
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

              prepare_response_table(responses_raw, responses_are_parsed = TRUE)
            } else {
              tibble::tibble()
            }
          })
