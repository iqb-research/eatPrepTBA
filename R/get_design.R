#' Recovers a design from IQB Testcenter
#'
#' @param workspace [WorkspaceTestcenter-class]. Workspace information necessary to retrieve ressources of testtakers and booklets.
#' @param units Tibble (optional). Unit data retrieved from the IQB Studio after setting the argument `metadata = TRUE` for [get_units()] -- otherwise the item values could only be inferred from the variable source tree, i.e., item scores are taken from variable scores that are no source variables for other derived variables. Could optionally also contain `unit_codes` prepared by `add_coding_scheme()` (saves some time).
#' @param overwrite Logical. Should column `unit_codes` be overwritten if they exist on `units`. Defaults to `FALSE`, i.e., `unit_codes` will be used if they were added to `units` beforehand by applying `add_coding_schemes()`.
#' @param mode Character. Only testtakers with the specified modes will be filtered. Defaults to `"run-hot-return"`.
#'
#' @description
#' This function returns a frame of all booklets and testtakers in a given Testcenter instance. Optionally, coding schemes can be used to add variables that provides for a dataset to be merged to coded responses. Please note that adding a the units object will remove the other test parts from the design that are not reflected by the units. A remedy would be to retrieve all units that are used in the test.
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
            units_cs <-
              add_coding_scheme(
                units = units,
                overwrite = overwrite,
                filter_has_codes = TRUE
              )

            units_cs_merge <-
              units_cs %>%
              dplyr::select(
                unit_key, unit_codes
              ) %>%
              tidyr::unnest(unit_codes) %>%
              dplyr::select(unit_key, variable_id)

            cli::cli_h3("Merging set of {.testtaker-label testtakers} and {.booklet-label booklets} with {.unit-label units}")
            testtakers_booklets %>%
              dplyr::semi_join(units_cs_merge %>% dplyr::select(unit_key)) %>%
              dplyr::left_join(units_cs_merge, by = dplyr::join_by("unit_key"), relationship = "many-to-many") %>%
              dplyr::filter(
                login_mode == mode
              )
          })
