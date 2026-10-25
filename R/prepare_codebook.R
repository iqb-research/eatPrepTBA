#' Prepares a rectangular codebook
#'
#' @param workspace [WorkspaceStudio-class]. Workspace information necessary to download codebook via the API.
#' @param unit_keys Character. Exact, case-sensitive unit keys (short names)
#'   returned by [list_units()], not numeric IDs or labels. Use `NULL` (default)
#'   for all units. An empty vector or unknown keys raise an error before export.
#' @param missings Tibble (optional). Missing-value codes with columns `id`,
#'   `label`, and `description`, added to each variable. When a Studio profile
#'   is also selected, these codes replace profile codes with the same `id`;
#'   all other profile codes are retained. Studio itself is not modified.
#' @param missings_profile Character (optional). Exact, case-sensitive label of
#'   a missing-value profile configured in Studio. With `NULL` (default), no
#'   profile is selected. Profile codes are added to each variable; the Studio
#'   field `code` becomes `code_id` in the returned table. An unknown label
#'   raises an error before downloading.
#' @param only_coded Logical. Should only variables with codes be shown? Defaults to `FALSE`.
#' @param general_instructions Logical. Should the general coding instructions be printed? Defaults to `FALSE`. (Currently not displayed.)
#' @param hide_item_var_relation Logical. Should item-variable relations be printed? Defaults to `TRUE`.
#' @param derived Logical. Should the derived variables be printed? Defaults to `TRUE`.
#' @param manual Logical. Should only items with manual coding be printed? Defaults to `TRUE`.
#' @param closed Logical. Should items that could be automatically coded be printed? Defaults to `TRUE`.
#' @param show_score Logical. Should the score be printed? Defaults to `FALSE`.
#'   When present in the export, `score` is returned as a character column;
#'   missing scores are `NA`, so the table can be written directly to Excel.
#' @param code_label_to_upper Logical. Should the code labels be printed in capital letters? Defaults to `TRUE`.
#'
#' @description
#' This function is a wrapper around [download_codebook()] that provides a rectangular codebook.
#'
#' @details
#' Missing downloads and exports containing no units, variables, or codes raise
#' an explicit error. Check the selected units and export filters in that case.
#' Download errors retain the original API error; preparation stops if any
#' requested workspace export fails.
#'
#' To include missing-value codes maintained in Studio, set `missings_profile`
#' to the exact profile label shown in Studio's codebook export dialog.
#' These codes are returned separately for each unit by Studio and added to
#' each variable in the prepared table. The numeric Studio `code` is used as
#' `code_id`; the missing category's identifier (for example, `mci`) is not used as a code ID.
#'
#' To supply your own missing-value codes, pass a tibble through `missings`.
#' If both arguments are supplied, your entries replace profile entries with
#' matching code IDs, including their labels and descriptions. Other profile
#' entries are retained, and additional user entries are appended. These
#' changes affect only the returned table; they do not modify Studio.
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
            tmp_codebooks <- tempfile("eatPrepTBA-codebooks-")
            dir.create(tmp_codebooks)
            on.exit(unlink(tmp_codebooks, recursive = TRUE), add = TRUE)

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

            codebook_paths <- list.files(tmp_codebooks, "\\.json$", full.names = TRUE)
            if (!length(codebook_paths)) {
              cli::cli_abort("No JSON codebook was downloaded. Check {.arg unit_keys}, {.fn list_units}, and any preceding download error messages.")
            }

            missing_codes <- missings
            if (!is.null(missing_codes)) {
              missing_codes <-
                missings %>%
                as.list() %>%
                purrr::list_transpose(simplify = FALSE)
            }

            codebooks <-
              codebook_paths %>%
              tibble::enframe(name = NULL, value = "path") %>%
              dplyr::mutate(
                codebook = purrr::map(path, jsonlite::read_json)
              ) %>%
              dplyr::select(-path) %>%
              dplyr::mutate(
                codebook = purrr::map(codebook, prepare_codebook_units)
              ) %>%
              tidyr::unnest(codebook)

            if (!nrow(codebooks)) {
              cli::cli_abort("The Studio JSON codebook contains no units. Check the selected units and export filters.")
            }

            codebooks <- codebooks %>%
              dplyr::mutate(
                variables = purrr::map(variables, prepare_codebook_variables)
              ) %>%
              tidyr::unnest(variables)

            if (!nrow(codebooks)) {
              cli::cli_abort("The Studio JSON codebook contains no variables. Check {.arg only_coded}, {.arg derived}, {.arg manual}, and {.arg closed} and the coding schemes of the selected units.")
            }

            codebooks <- codebooks %>%
              dplyr::mutate(
                codes = purrr::map2(codes, missings, function(codes, profile_codes) {
                  combined_missing_codes <- combine_codebook_missing_codes(
                    profile_codes, missing_codes
                  )
                  prepare_codebook_codes(codes, missing_codes = combined_missing_codes)
                })
              ) %>%
              dplyr::select(-missings) %>%
              tidyr::unnest(codes)

            if (!nrow(codebooks)) {
              cli::cli_abort("The Studio JSON codebook contains no codes for the selected variables, and no missing-value codes were added. Check the coding schemes and export filters.")
            }

            return(codebooks)
          })

#' @keywords internal
prepare_codebook_units <- function(units,
                                   unit_entries = c("key", "name", "variables", "missings")) {
  columns <- units %>%
    purrr::map(function(unit) {
      if (is.null(unit$missings)) unit$missings <- list()
      unit[unit_entries]
    }) %>%
    purrr::list_transpose()
  if ("missings" %in% unit_entries) {
    # Keep even a single profile entry nested within its unit.
    columns$missings <- purrr::map(units, function(unit) {
      if (is.null(unit$missings)) list() else unit$missings
    })
  }
  columns %>%
    tibble::as_tibble() %>%
    dplyr::rename(any_of(c(
      "unit_key" = "key",
      "unit_label" = "name"
    )))
}

# Studio stores missing codes separately from each variable's regular codes.
combine_codebook_missing_codes <- function(profile_codes, missing_codes) {
  profile_codes <- purrr::map(profile_codes, function(missing) {
    if (is.null(missing$code) || length(missing$code) != 1L ||
        !is.atomic(missing$code) || is.na(missing$code) ||
        !nzchar(as.character(missing$code))) {
      cli::cli_abort("A Studio profile missing-value entry has no valid {.field code}.")
    }
    list(id = as.character(missing$code), label = missing$label,
         description = missing$description)
  })
  own_ids <- vapply(missing_codes, function(missing) as.character(missing$id), character(1))
  profile_codes <- purrr::discard(profile_codes, function(missing) missing$id %in% own_ids)
  c(profile_codes, missing_codes)
}

#' @keywords internal
prepare_codebook_variables <- function(variables,
                                       variable_entries = c("id", "label", "codes")) {
  if (!is.null(names(variables)) && any(variable_entries %in% names(variables))) {
    variables <- list(variables)
  }

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

  if (!is.null(names(codes)) && any(code_entries %in% names(codes))) {
    codes <- list(codes)
  }

  columns <- c(
    codes,
    missing_codes %>% purrr::map(function(x) x[code_entries])
  ) %>%
    purrr::list_transpose()

  if ("score" %in% names(columns)) {
    # Missing codes have no score. Keep one scalar per row, including NA,
    # and preserve Studio's score text instead of retaining a list-column.
    columns$score <- purrr::map_chr(columns$score, function(score) {
      if (is.null(score)) NA_character_ else as.character(score)
    })
  }

  columns %>%
    tibble::as_tibble() %>%
    dplyr::rename(any_of(c(
      "code_id" = "id",
      "code_label" = "label",
      "code_description" = "description"
    )))
}

