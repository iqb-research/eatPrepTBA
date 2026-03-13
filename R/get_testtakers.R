#' Get a testtakers from IQB Testcenter
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param files Character (optional). Names of the testtakers files to be retrieved. Defaults to `NULL` which returns all testtakers defined on the workspace.
#'
#' @description
#' This function returns all testtakers with specified files.
#'
#' @return A tibble.
#'
#' @aliases
#' get_testtakers,WorkspaceTestcenter-method
#'
#' @keywords internal
setGeneric("get_testtakers", function(workspace, files = NULL) {
  cli_setting()

  standardGeneric("get_testtakers")
})

#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve unit information and resources from the API.
#'
#' @describeIn get_testtakers Get testtakers in a Testcenter workspace
setMethod("get_testtakers",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace,
                   files = NULL) {
            cli_setting()

            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            testtakers_available <- list_files(workspace, type = "Testtakers")

            if (is.null(files)) {
              files <- testtakers_available$name
            }

            if (!any(files %in% testtakers_available$name)) {
              not_found <- setdiff(file, testtakers_available$name)
              cli::cli_abort("Did not find queried {.testtaker-label testtaker} files: {.testtaker-id {not_found}}")
            }

            # TODO: Loop, but no safe-run by now
            run_req <- function(file) {
              base_req(method = "GET",
                       endpoint = c("workspace", ws_id, "file", "Testtakers", file)) %>%
                httr2::req_perform() %>%
                httr2::resp_body_string() %>%
                xml2::read_xml()
            }

            testtakers <-
              files %>%
              purrr::map(run_req,
                         .progress = list(
                           type ="custom",
                           extra = list(
                             files = pad_ids(files)
                           ),
                           format = "Downloading testtakers {.testtaker-id {cli::pb_extra$files[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
                           format_done = "Downloaded {cli::pb_total} testtaker file{?s} in {cli::pb_elapsed}.",
                           clear = FALSE)
              ) %>%
              purrr::map(read_testtakers,
                         .progress = list(
                           type ="custom",
                           extra = list(
                             files = pad_ids(files)
                           ),
                           format = "Preparing testtakers {.testtaker-id {cli::pb_extra$files[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
                           format_done = "Prepared {cli::pb_total} testtaker file{?s} in {cli::pb_elapsed}.",
                           clear = FALSE)) %>%
              dplyr::bind_rows()

            return(testtakers)
          })
