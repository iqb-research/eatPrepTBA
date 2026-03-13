#' Get a booklets from IQB Testcenter
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve unit information and resources from the API.
#' @param files Character (optional). Names of the booklet files to be retrieved. Defaults to `NULL` which returns all booklets defined on the workspace.
#'
#' @description
#' This function returns all booklets with specified files.
#'
#' @return A tibble.
#'
#' @aliases
#' get_booklets,WorkspaceTestcenter-method
#'
#' @keywords internal
setGeneric("get_booklets", function(workspace, files = NULL) {
  cli_setting()

  standardGeneric("get_booklets")
})

#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve unit information and resources from the API.
#'
#' @describeIn get_booklets Get booklets in a Testcenter workspace
setMethod("get_booklets",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace,
                   files = NULL) {
            cli_setting()

            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            booklets_available <- list_files(workspace, type = "Booklet")

            if (is.null(files)) {
              files <- booklets_available$name
            }

            if (!any(files %in% booklets_available$name)) {
              not_found <- setdiff(file, booklets_available$name)
              cli::cli_abort("Did not find queried {.booklet-label booklet} files: {.booklet-id {not_found}}")
            }

            # TODO: Loop, but no safe-run by now
            run_req <- function(file) {
              base_req(method = "GET",
                       endpoint = c("workspace", ws_id, "file", "Booklet", file)) %>%
                httr2::req_perform() %>%
                httr2::resp_body_string() %>%
                xml2::read_xml()
            }

            booklets <-
              files %>%
              purrr::map(run_req,
                         .progress = list(
                           type ="custom",
                           extra = list(
                             files = pad_ids(files)
                           ),
                           format = "Downloading booklet {.booklet-id {cli::pb_extra$files[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
                           format_done = "Downloaded {cli::pb_total} {.booklet-label booklet} file{?s} in {cli::pb_elapsed}.",
                           clear = FALSE)
              ) %>%
              purrr::map(read_booklet,
                         .progress = list(
                           type ="custom",
                           extra = list(
                             files = pad_ids(files)
                           ),
                           format = "Preparing booklet {.booklet-id {cli::pb_extra$files[cli::pb_current+1]}} ({cli::pb_current}/{cli::pb_total}): {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
                           format_done = "Prepared {cli::pb_total} {.booklet-label booklet} file{?s} in {cli::pb_elapsed}.",
                           clear = FALSE)) %>%
              dplyr::bind_rows()

            # Return
            if (tibble::has_name(booklets, "unit_alias")) {
              booklets %>%
                dplyr::mutate(
                  unit_alias = dplyr::coalesce(unit_alias, unit_key)
                )
            } else {
              booklets %>%
                dplyr::mutate(
                  unit_alias = unit_key
                )
            }
          })
