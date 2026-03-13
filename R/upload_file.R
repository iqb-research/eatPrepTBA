#' Upload file
#'
#' @param workspace [Workspace-class]. Workspace information necessary to upload file via the API.
#' @param path Character. Path to the file to be uploaded.
#' @param status Character. Filters for status messages (`"success"`, `"info"`, `"error"`, or `"warning"`)
#' @param verbose Logical. Should status messages be reported? Defaults to `FALSE`.
#'
#' @description
#' This function uploads a file to the IQB Testcenter.
#'
#' @return NULL
#' @export
#'
#' @aliases
#' upload_file,Workspace-method
setGeneric("upload_file", function(workspace,
                                   path,
                                   status = c("success", "info", "error", "warning"),
                                   verbose = FALSE) {
  cli_setting()

  standardGeneric("upload_file")
})

#' @describeIn upload_file Upload a file into a given workspace
setMethod("upload_file",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace,
                   path,
                   status = c("success", "info", "error", "warning"),
                   verbose = FALSE) {
            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id

            run_req <- function() {
              base_req(
                method = "POST",
                endpoint = c(
                  "workspace",
                  ws_id,
                  "file"
                )
              ) %>%
                httr2::req_body_multipart(fileforvo = curl::form_file(path)) %>%
                httr2::req_perform() %>%
                httr2::resp_body_json()
            }

            resp <-
              run_safe(run_req,
                       error_message = "File {.path {path}} could not be uploaded.")

            # TODO: Must be redone if reporting structure changes
            if (verbose) {
              full_messages <-
                resp %>%
                tibble::enframe(name = "path_extract",
                                value = "message") %>%
                dplyr::mutate(
                  message = purrr::map(message, function(message) {
                    message %>%
                      tibble::enframe(name = "status",
                                      value = "message") %>%
                      tidyr::unnest(message) %>%
                      tidyr::unnest(message)
                  })
                ) %>%
                tidyr::unnest(message)

              agg_messages <-
                full_messages %>%
                dplyr::mutate(
                  file_ext = stringr::str_remove(path_extract, ".+_Extract/"),
                  file = stringr::str_remove(file_ext, "\\..+$"),
                  ext = stringr::str_extract(file_ext, "\\..+$") %>% stringr::str_remove("\\."),
                  message = stringr::str_remove(message,
                                                stringr::str_glue("of name `{file_ext}` ")
                  )) %>%
                dplyr::filter(status %in% status) %>%
                dplyr::group_by(status, message, file) %>%
                dplyr::summarise(
                  ext = list(ext)
                ) %>%
                dplyr::mutate(
                  ext = purrr::map_chr(ext, function(ext) {
                    if (length(ext) == 1) {
                      out <- stringr::str_glue(".{ext}")
                    } else {
                      concat <- stringr::str_c(ext, collapse = "|")
                      out <- stringr::str_glue(".({concat})")
                    }
                    as.character(out)
                  }),
                  file_ext = stringr::str_c(file, ext)
                ) %>%
                dplyr::summarise(
                  file_ext = list(file_ext)
                )

              agg_messages %>%
                as.list() %>%
                purrr::list_transpose() %>%
                purrr::walk(function(x) {
                  switch(x[["status"]],
                         "success" = cli::cli_alert_success(x[["message"]]),
                         "info" = cli::cli_alert_info(x[["message"]]),
                         "warning" = cli::cli_alert_warning(x[["message"]]),
                         "error" = cli::cli_alert_danger(x[["message"]])
                  )

                  cli::cli_li(x[["file_ext"]])

                  cli::cli_par()
                })
            }
          })
