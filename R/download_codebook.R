#' Download codebook
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to download codebook via the API.
#' @param path Character. Path of the directory for the codebook files to be downloaded to. The default filename is determined by the id and the label of the IQB Studio workspace.
#' @param file_prefix Character (optional). Path prefix, e.g., a date to keep track of different versions of the codebook.
#' @param unit_keys Character. Keys (short names) of the units in the workspace the codebook should be retrieved from. If set to `NULL` (default), the codebook will be generated for the all units.
#' @param format Character. Either `"docx"` (default) or `"json"`.
#' @param missings_profile Missings profile. (Currently without effect.)
#' @param only_coded Logical. Should only variables with codes be shown?
#' @param general_instructions Logical. Should the general coding instructions be printed? Defaults to `TRUE`.
#' @param hide_item_var_relation Logocal. Should item-variable relations be printed? Defaults to `FALSE`.
#' @param derived Logical. Should the derived variables be printed? Defaults to `TRUE`.
#' @param manual Logical. Should only items with manual coding be printed? Defaults to `TRUE`.
#' @param closed Logical. Should items that could be automatically coded be printed? Defaults to `TRUE`.
#' @param show_score Logical. Should the score be printed? Defaults to `FALSE`.
#' @param code_label_to_upper Logical. Should the code labels be printed in capital letters? Defaults to `TRUE`.
#'
#' @description
#' This function downloads codebooks from the IQB Studio.
#'
#' @return NULL
#' @export
#'
#' @aliases
#' download_codebook,WorkspaceStudio-method
setGeneric("download_codebook", function(workspace,
                                         path,
                                         file_prefix = "",
                                         unit_keys = NULL,
                                         # Exportformat
                                         format = c("docx", "json"),
                                         # Missings-Profil
                                         missings_profile = NULL,
                                         # Nur Variablen mit Codes
                                         only_coded = FALSE,
                                         # Allgemeine Hinweise für jede Variable
                                         general_instructions = TRUE,
                                         # Item-Variable-Relation für jede Variable
                                         hide_item_var_relation = TRUE,
                                         # Abgeleitete Variablen
                                         derived = TRUE,
                                         # Manuell kodierte Variablen
                                         manual = TRUE,
                                         # Geschlossen kodierte Variablen
                                         closed = TRUE,
                                         # Bewertung anzeigen
                                         show_score = FALSE,
                                         # Code-Label in Großbuchstaben
                                         code_label_to_upper = TRUE) {
  cli_setting()

  standardGeneric("download_codebook")
})

#' @describeIn download_codebook Upload a file in a defined workspace
setMethod("download_codebook",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace,
                   path,
                   file_prefix = "",
                   unit_keys = NULL,
                   format = c("docx", "json"),
                   missings_profile = NULL,
                   only_coded = FALSE,
                   general_instructions = TRUE,
                   hide_item_var_relation = TRUE,
                   derived = TRUE,
                   manual = TRUE,
                   closed = TRUE,
                   show_score = FALSE,
                   code_label_to_upper = TRUE) {
            format <- match.arg(format)

            base_req <- workspace@login@base_req
            ws_id <- workspace@ws_id
            ws_label <- workspace@ws_label

            # Prepare units (with or without filter)
            units <- list_units(workspace)

            # TODO: Add mechanism to detect units that are not in the workspace
            if (!is.null(unit_keys)) {
              units <-
                units %>%
                purrr::map(
                  function(ws) {
                    ws$units <-
                      ws$units %>%
                      purrr::keep(function(x) x$unit_key %in% unit_keys)

                    return(ws)
                  }
                )
            }

            unit_ids <-
              units %>%
              purrr::discard(function(ws) length(ws$units) == 0) %>%
              purrr::map(
                function(ws) {
                  ws$units <-
                    ws$units %>%
                    purrr::map_int("unit_id")

                  return(ws)
                }
              )

            # Normalize query params
            run_req <- function(ws) {
              query_params <-
                list(
                  format = format,
                  hasOnlyVarsWithCodes = only_coded,
                  generalInstructions = general_instructions,
                  hideItemVarRelation = hide_item_var_relation,
                  derived = derived,
                  onlyManual = manual,
                  closed = closed,
                  showScore = show_score,
                  codeLabelToUpper = code_label_to_upper,
                  id = ws$units
                ) %>%
                purrr::map(stringr::str_to_lower)

              final_path <- stringr::str_glue("{path}/{file_prefix}{ws$ws_label}.{format}")

              req <- function() {
                base_req(method = "GET",
                         endpoint = c(
                           "workspaces",
                           ws$ws_id,
                           "units",
                           "coding-book"
                         ),
                         query = query_params) %>%
                  httr2::req_perform(path = final_path)

                cli::cli_alert_success("Codebook of workspace {.ws-id {ws$ws_id}}: {.ws-label {ws$ws_label}} was successfully downloaded to {.file {final_path}}", wrap = TRUE)
              }

              return(req)
            }

            unit_ids %>%
              purrr::map(
                function(ws) {
                  final_path <- stringr::str_glue("{path}/{file_prefix}{ws$ws_label}.{format}")
                  run_safe(run_req(ws),
                           error_message = "Codebook could not be generated. Please check if you have already,
                     opened {.file {final_path}} (that migh cause the error).")

                })
          })
