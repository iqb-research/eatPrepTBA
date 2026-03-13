#' List unit resource files
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve unit resource list (coding scheme and aspect data) from the API.
#'
#' @description
#' This function serves as a wrapper for [list_files()].
#'
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' list_resources,WorkspaceTestcenter-method
setGeneric("list_resources", function(workspace) {
  standardGeneric("list_resources")
})

#' @describeIn list_resources List all resources in a given IQB Testcenter workspace
setMethod("list_resources",
          signature = signature(workspace = "Workspace"),
          function(workspace) {
            ws_resources <- list_files(workspace, type = "Resource")

            ws_resources %>%
              dplyr::filter(stringr::str_detect(name,
                                                "\\.(voud|vocs|vomd)")) %>%
              dplyr::pull("name")
          })


