setGeneric("get_files", function(workspace,
                                 id = NULL,
                                 type,
                                 list_fun,
                                 get_fun) {
  cli_setting()

  standardGeneric("get_files")
})

setMethod("get_files",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace,
                   id = NULL,
                   type,
                   list_fun,
                   get_fun) {
            ws_file_ids_all <- list_fun(workspace)
            if (is.null(id)) {
              ws_file_ids <- ws_file_ids_all
            } else {
              ws_file_ids <- intersect(id, ws_file_ids_all)
              n_ws_file_ids <- length(ws_file_ids)

              if (n_ws_file_ids == 0) {
                msg <- glue::glue(
                  "No {{.{type} {{type}}}} information could be retrieved for: {{.{type} {{id}}}}"
                )
                cli::cli_abort(msg)
              }

              ws_file_not <- setdiff(id, ws_file_ids_all)
              n_ws_file_not <- length(ws_file_not)
              if (length(ws_file_not) >= 1) {
                msg <- glue::glue(
                  "No {{.{type} {{type}}}} information could be retrieved for {{n_ws_file_not}} {{type}}{{?s}}: {{.{type} {{ws_file_not}}}}"
                )
                cli::cli_alert_danger(msg)
              }
            }

            ws_files_tbl <-
              tibble::enframe(ws_file_ids, name = NULL, value = "id") %>%
              dplyr::mutate(
                resource = purrr::map(id,
                                      .f = function(x) get_fun(workspace = workspace, id = x),
                                      .progress = list(name = glue::glue(
                                        "Downloading {type} information"
                                      )))
              ) %>%
              tidyr::unnest(resource)

            return(ws_files_tbl)
          })

setMethod("get_files",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace,
                   id = NULL,
                   type,
                   list_fun,
                   get_fun) {
            ws_file_ids_all <-
              list_fun(workspace) %>%
              purrr::map_int("unit_id")

            if (is.null(id)) {
              ws_file_ids <- ws_file_ids_all
            } else {
              ws_file_ids <- intersect(id, ws_file_ids_all)
              n_ws_file_ids <- length(ws_file_ids)

              if (n_ws_file_ids == 0) {
                msg <- glue::glue(
                  "No {{.{type} {{type}}}} information could be retrieved for: {{.{type} {{id}}}}"
                )
                cli::cli_abort(msg)
              }

              ws_file_not <- setdiff(id, ws_file_ids_all)
              n_ws_file_not <- length(ws_file_not)
              if (length(ws_file_not) >= 1) {
                msg <- glue::glue(
                  "No {{.{type} {{type}}}} information could be retrieved for {{n_ws_file_not}} {{type}}{{?s}}: {{.{type} {{ws_file_not}}}}"
                )
                cli::cli_alert_danger(msg)
              }
            }

            ws_files_tbl <-
              ws_file_ids %>%
              tibble::enframe(name = NULL, value = "id") %>%
              dplyr::mutate(
                resource = purrr::map(id,
                                      .f = function(x) get_fun(workspace = workspace, id = x),
                                      .progress = list(name = glue::glue(
                                        "Downloading {type} information"
                                      )))
              ) %>%
              tidyr::unnest(resource)

            return(ws_files_tbl)
          })
