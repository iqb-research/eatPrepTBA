#' List booklet files
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve booklet list from the API.
#'
#' @description
#' This function serves as a wrapper for [list_files()].
#'
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' list_booklets,WorkspaceTestcenter-method
setGeneric("list_booklets", function(workspace) {
  standardGeneric("list_booklets")
})

#' @describeIn list_booklets List all booklets in a given IQB Testcenter workspace
setMethod("list_booklets",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace) {
            ws_booklets <- list_files(workspace, type = "Booklet")

            ws_booklets %>%
              dplyr::filter(stringr::str_detect(name, "\\.xml")) %>%
              dplyr::pull("name") %>%
              stringr::str_remove("\\.xml")
          })
