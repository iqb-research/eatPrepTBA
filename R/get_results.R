#' Get results
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve results information.
#'
#' @description
#' This function returns responses for the selected groups.
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' get_results,WorkspaceTestcenter-method
setGeneric("get_results", function(workspace) {
  standardGeneric("get_results")
})

#' @describeIn get_results Get results of a given Testcenter workspace
setMethod("get_results",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace) {
            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            run_req <- function() {
              base_req(method = "GET",
                       endpoint = c("workspace", ws_id, "results")) %>%
                httr2::req_perform() %>%
                httr2::resp_body_json()
            }

            resp <-
              run_safe(run_req,
                       error_message = "Results list could not be retrieved.")

            if (!is.null(resp)) {
              resp %>%
                # Rectangularize (zu tibble)
                tibble::enframe(name = NULL) %>%
                # Schleife zum Spreaden der Einträge (Auslesen in tibble)
                dplyr::mutate(
                  # Ladebalken?
                  value = purrr::map(value, function(x) tibble::as_tibble(x))
                ) %>%
                # Entpacken
                tidyr::unnest(value)
            } else {
              tibble::tibble()
            }
          })
