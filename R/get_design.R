#' Recovers a design from IQB Testcenter
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve ressources of testtakers and booklets.
#' @param units Tibble (optional). Unit data retrieved from [get_units()]. Coding
#' schemes supply the variables for each unit. Units without a coding scheme
#' use `unit_variables$variable_id` instead, for example StarS units. May also
#' contain `unit_codes` prepared by [add_coding_scheme()] to save time.
#' @param overwrite Logical. Should existing `unit_codes` be rebuilt? Defaults
#' to `FALSE`, which reuses codes prepared by [add_coding_scheme()].
#' @param mode Character. Only testtakers with the specified modes will be filtered. Defaults to `"run-hot-return"`.
#'
#' @description
#' Returns the assigned booklets and units for testtakers in a Testcenter
#' workspace. Supplying `units` expands the design to variables and restricts
#' it to units with available variables. Supply all units used in the test.
#' With `units = NULL`, the result remains at unit level, without `variable_id`.
#'
#' @details
#' Variable selection is performed separately for each unit, supporting mixed
#' tests with and without coding schemes. Existing schemes supply their base
#' and derived variables, excluding deactivated (`BASE_NO_VALUE`) variables.
#' Only a missing or empty scheme (without `variableCodings`) allows a fallback
#' to `unit_variables`; a scheme containing only deactivated variables does not.
#' Cached `unit_codes` are reused unless `overwrite = TRUE`. Empty cached
#' results allow the fallback only if the underlying scheme is missing or empty;
#' otherwise they remain empty until rebuilt with `overwrite = TRUE`.
#' The fallback uses the normalized IDs from [get_units()], including its alias
#' handling. Duplicate IDs are collapsed and missing or blank IDs are ignored.
#' Units without any available variable IDs contribute no variable-level rows.
#'
#' The variable list comes from unit metadata, not observed responses, so it can
#' include variables that nobody answered. For StarS data, check that the IDs in
#' the unit metadata match those returned by [prepare_coded()]. A variable-level
#' design alone does not supply the coding metadata required by [complete_design()].
#'
#' @return A tibble.
#'
#' @aliases
#' get_design,WorkspaceTestcenter-method
#' @export
setGeneric("get_design", function(workspace, units = NULL, overwrite = FALSE, mode = "run-hot-return") {
  cli_setting()

  standardGeneric("get_design")
})

#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve design information.
#'
#' @describeIn get_design Get design in a Testcenter workspace
setMethod("get_design",
          signature = signature(workspace = "WorkspaceTestcenter"),
          function(workspace,
                   units = NULL,
                   overwrite = FALSE,
                   mode = "run-hot-return") {
            cli_setting()

            cli::cli_h2("Recovering design")

            cli::cli_h3("Retrieving {.testtaker-label testtakers}")
            testtakers <- get_testtakers(workspace)

            cli::cli_h3("Retrieving {.booklet-label booklets}")
            booklets <- get_booklets(workspace)

            cli::cli_h3("Merging {.testtaker-label testtakers} and {.booklet-label booklets}")
            testtakers_booklets <-
              testtakers %>%
              dplyr::filter(login_mode == mode) %>%
              dplyr::left_join(booklets, by = dplyr::join_by("booklet_id"), relationship = "many-to-many") %>%
              dplyr::select(
                dplyr::any_of(c(
                  "group_id",
                  "group_label",
                  "login_name",
                  "login_mode",
                  "login_code",
                  "booklet_id",
                  "booklet_label",
                  "booklet_no",
                  "testlet_id",
                  "testlet_label",
                  "testlet_no",
                  "unit_key",
                  "unit_label",
                  "unit_alias",
                  "unit_testlet_no",
                  "unit_booklet_no"
                ))
              )

            if (is.null(units)) {
              return(testtakers_booklets)
            }

            cli::cli_h3("Preparing {.unit-label units}")
            units_cs_merge <- prepare_design_variables(units, overwrite = overwrite)

            cli::cli_h3("Merging set of {.testtaker-label testtakers} and {.booklet-label booklets} with {.unit-label units}")
            testtakers_booklets %>%
              dplyr::semi_join(units_cs_merge, by = "unit_key") %>%
              dplyr::left_join(units_cs_merge, by = dplyr::join_by("unit_key"), relationship = "many-to-many") %>%
              dplyr::filter(
                login_mode == mode
              )
          })

prepare_design_variables <- function(units, overwrite = FALSE) {
  checkmate::assert_tibble(units)
  checkmate::assert_flag(overwrite)
  assert_cols(units, "unit_key", "units")

  empty <- tibble::tibble(unit_key = units$unit_key[0], variable_id = character())
  if (nrow(units) == 0L) return(empty)

  use_cached <- "unit_codes" %in% names(units) && !overwrite
  variable_source <- vapply(seq_len(nrow(units)), function(i) {
    if (use_cached && NROW(units$unit_codes[[i]]) > 0L) return("codes")
    if (!"coding_scheme" %in% names(units)) return("variables")

    scheme <- units$coding_scheme[[i]]
    if (is.null(scheme) || length(scheme) != 1L || is.na(scheme) ||
        scheme %in% c("", "NA", "null")) return("variables")

    # Inspect the unfiltered scheme: an empty prepared table may instead mean
    # that every variable was deliberately deactivated.
    codings <- purrr::pluck(jsonlite::parse_json(scheme), "variableCodings")
    if (length(codings) == 0L) return("variables")
    if (all(vapply(codings, function(variable) {
      identical(variable$sourceType, "BASE_NO_VALUE")
    }, logical(1)))) return("empty")
    "codes"
  }, character(1))

  variable_tables <- vector("list", nrow(units))
  use_variables <- variable_source == "variables"
  if (any(use_variables)) {
    assert_cols(units, "unit_variables", "units")
    variable_tables[use_variables] <- units$unit_variables[use_variables]
  }
  use_codes <- variable_source == "codes"
  if (any(use_codes)) {
    units_cs <- add_coding_scheme(
      units[use_codes, , drop = FALSE],
      overwrite = overwrite,
      filter_has_codes = TRUE
    )
    variable_tables[use_codes] <- units_cs$unit_codes
  }

  variables <- lapply(seq_along(variable_tables), function(i) {
    table <- variable_tables[[i]]
    if (NROW(table) == 0L) return(empty)

    assert_cols(table, "variable_id", paste0("variables for unit ", units$unit_key[[i]]))
    ids <- as.character(table$variable_id)
    ids <- ids[!is.na(ids) & nzchar(trimws(ids))]
    tibble::tibble(unit_key = rep(units$unit_key[i], length(ids)), variable_id = ids)
  })
  dplyr::distinct(dplyr::bind_rows(variables), unit_key, variable_id)
}
