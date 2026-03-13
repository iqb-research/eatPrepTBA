#' List testtakers files
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve testtakers list from the API.
#'
#' @description
#' This function serves as a wrapper for [list_files()].
#'
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' list_testtakers,WorkspaceTestcenter-method
setGeneric("list_testtakers", function(workspace) {
  standardGeneric("list_testtakers")
})

#' @describeIn list_testtakers List all testtakers in a given IQB Testcenter workspace
setMethod("list_testtakers",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace) {
            ws_testtakers <- list_files(workspace, type = "Testtakers")

            ws_testtakers %>%
              dplyr::filter(stringr::str_detect(name, "\\.xml")) %>%
              dplyr::pull("name") %>%
              stringr::str_remove("\\.xml")
          })
