#' Prepares a rectangular codebook
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to download codebook via the API.
#' @param unit_keys Character. Keys (short names) of the units in the workspace the codebook should be retrieved from. If set to `NULL` (default), the codebook will be generated for the all units.
#' @param missings Tibble (optional). Missing table to be added to each variable.
#' @param missings_profile Missings profile. (Currently without effect.)
#' @param only_coded Logical. Should only variables with codes be shown? Defaults to `TRUE`.
#' @param general_instructions Logical. Should the general coding instructions be printed? Defaults to `FALSE`. (Currently not displayed.)
#' @param hide_item_var_relation Logical. Should item-variable relations be printed? Defaults to `TRUE`.
#' @param derived Logical. Should the derived variables be printed? Defaults to `TRUE`.
#' @param manual Logical. Should only items with manual coding be printed? Defaults to `TRUE`.
#' @param closed Logical. Should items that could be automatically coded be printed? Defaults to `TRUE`.
#' @param show_score Logical. Should the score be printed? Defaults to `FALSE`.
#' @param code_label_to_upper Logical. Should the code labels be printed in capital letters? Defaults to `TRUE`.
#'
#' @description
#' This function is a wrapper around [download_units()] that provides a rectangular codebook.
#'
#' @return A tibble.
#' @export
#'
#' @aliases
#' prepare_codebook,WorkspaceStudio-method
setGeneric("prepare_codebook", function(workspace,
                                        unit_keys = NULL,
                                        missings = NULL,
                                        # Missings-Profil
                                        missings_profile = NULL,
                                        # Nur Variablen mit Codes
                                        only_coded = FALSE,
                                        # Allgemeine Hinweise für jede Variable
                                        general_instructions = FALSE,
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

  standardGeneric("prepare_codebook")
})

#' @describeIn prepare_codebook Download a file of a defined workspace
setMethod("prepare_codebook",
          signature = signature(workspace = "WorkspaceStudio"),
          function(workspace,
                   unit_keys = NULL,
                   missings = NULL,
                   missings_profile = NULL,
                   only_coded = FALSE,
                   general_instructions = FALSE,
                   hide_item_var_relation = TRUE,
                   derived = TRUE,
                   manual = TRUE,
                   closed = TRUE,
                   show_score = FALSE,
                   code_label_to_upper = TRUE) {
            tmp <- tempdir()
            tmp_codebooks <- stringr::str_glue("{tmp}/codebooks")
            dir.create(tmp_codebooks, showWarnings = FALSE)

            # missings <-
            #   tibble::tibble(
            #     id = c("-97", "-98"),
            #     label = c("MISSING - CODING IMPOSSIBLE", "MISSING - INVALID RESPONSE"),
            #     description = c("Kodierung nicht möglich", "Ungültige Antwort")
            #   )

            download_codebook(workspace,
                              path = tmp_codebooks,
                              format = "json",

                              unit_keys = unit_keys,
                              missings_profile = missings_profile,
                              only_coded = only_coded,
                              general_instructions = general_instructions,
                              hide_item_var_relation = hide_item_var_relation,
                              derived = derived,
                              manual = manual,
                              closed = closed,
                              show_score = show_score,
                              code_label_to_upper = code_label_to_upper)

            missing_codes <- missings
            if (!is.null(missing_codes)) {
              missing_codes <-
                missings %>%
                as.list() %>%
                purrr::list_transpose(simplify = FALSE)
            }

            codebooks <-
              list.files(tmp_codebooks, "\\.json$", full.names = TRUE) %>%
              tibble::enframe(name = NULL, value = "path") %>%
              dplyr::mutate(
                codebook = purrr::map(path, jsonlite::read_json)
              ) %>%
              dplyr::select(-path) %>%
              dplyr::mutate(
                codebook = purrr::map(codebook, prepare_codebook_units)
              ) %>%
              tidyr::unnest(codebook) %>%
              dplyr::mutate(
                variables = purrr::map(variables, prepare_codebook_variables)
              ) %>%
              tidyr::unnest(variables) %>%
              dplyr::mutate(
                codes = purrr::map(codes, function(codes) prepare_codebook_codes(codes, missing_codes = missing_codes))
              ) %>%
              tidyr::unnest(codes)

            # Clean-up
            list.files(tmp_codebooks, full.names = TRUE) %>%
              purrr::map(function(x) file.remove(x)) %>%
              invisible()

            return(codebooks)
          })

#' @keywords internal
prepare_codebook_units <- function(units,
                                   unit_entries = c("key", "name", "variables")) {
  units %>%
    purrr::map(function(unit) {
      unit[unit_entries]
    }) %>%
    purrr::list_transpose() %>%
    tibble::as_tibble() %>%
    dplyr::rename(any_of(c(
      "unit_key" = "key",
      "unit_label" = "name"
    )))
}

#' @keywords internal
prepare_codebook_variables <- function(variables,
                                       variable_entries = c("id", "label", "codes")) {
  variables %>%
    purrr::map(function(variable) {
      variable[variable_entries]
    }) %>%
    purrr::list_transpose() %>%
    tibble::as_tibble() %>%
    dplyr::rename(any_of(c(
      "variable_id" = "id",
      "variable_label" = "label"
    )))
}

#' @keywords internal
prepare_codebook_codes <- function(codes,
                                   code_entries = c("id", "label", "description"),
                                   missing_codes) {

  codes %>%
    purrr::map(function(code) {
      code[code_entries]
    })

  c(
    codes,
    missing_codes %>% purrr::map(function(x) x[code_entries])
  ) %>%
    purrr::list_transpose() %>%
    tibble::as_tibble() %>%
    dplyr::rename(any_of(c(
      "code_id" = "id",
      "code_label" = "label",
      "code_description" = "description"
    )))
}

