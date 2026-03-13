#' Complete design with coded responses
#'
#' @param coded Tibble. Response data coded with [code_responses()]. The argument `prepare` must be `TRUE`.
#' @param units Tibble. Unit data retrieved from the IQB Studio after setting the argument `unit_definition = TRUE` for [get_units()] -- otherwise the page order of variables could not be correctly inferred from the variable source tree. Could already be treated by [`add_coding_scheme()`] to save some time.
#' @param design Tibble. Design retrieved from Testcenter via [get_design()] or an object formatted in the same way.
#' @param identifiers Character. Contains person identifiers of the dataset `coded`. Defaults to `c("group_id", "login_name", "login_code")` which corresponds to the identifiers of the IQB Testcenter.
#' @param overwrite Logical. Should column `unit_codes` be overwritten if they exist on `units`. Defaults to `FALSE`, i.e., `unit_codes` will be used if they were added to `units` beforehand by applying `add_coding_schemes()`.
#' @param missings Tibble (optional). Provide missing meta data with `code_id`, `status`, `score`, and `code_type`. Defaults to `NULL` and uses default scheme. (Currently, only one missing scheme is supported.)
#'
#' This function automatically completes missings for coded responses.
#'
#' @return A tibble.
#' @export
complete_design <- function(coded,
                            units,
                            design,
                            identifiers = c("group_id", "login_name", "login_code"),
                            overwrite = FALSE,
                            missings = NULL
) {
  # if (is.null(missings)) {
  #   missings <-
  #     tibble::tribble(
  #       ~code_id, ~code_status, ~code_score, ~code_type,
  #       -96, "NOT_REACHED", 0, "MISSING_NOT_REACHED",
  #       -97, "CODING_ERROR", 0, "MISSING_CODING_IMPOSSIBLE",
  #       -98, "INVALID", 0, "MISSING_INVALID_RESPONSE",
  #       -99, "DISPLAYED", 0, "MISSING_BY_OMISSION"
  #     )
  # }

  cli_setting()

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
    dplyr::select(dplyr::any_of(c(
      "unit_key", "variable_id", "variable_source_type",
      "variable_level", "variable_page", "variable_section", "variable_page_always_visible"
    )))

  identifiers <- intersect(names(design), identifiers)

  # Merge codes and design
  design_coded <-
    design %>%
    dplyr::mutate(
      booklet_merge = stringr::str_to_upper(booklet_id)
    ) %>%
    dplyr::left_join(
      coded %>% dplyr::mutate(booklet_merge = stringr::str_to_upper(booklet_id)) %>% dplyr::select(-booklet_id),
      by = dplyr::join_by(!!! c(identifiers, "booklet_merge", "unit_key", "unit_alias", "variable_id"))
    ) %>%
    dplyr::select(-dplyr::any_of(c("variable_source_type"))) %>%
    dplyr::left_join(units_cs_merge,
                     by = dplyr::join_by("unit_key", "variable_id")) %>%
    dplyr::group_by(
      dplyr::across(dplyr::any_of(identifiers))
    ) %>%
    dplyr::mutate(
      id_used = !all(is.na(code_status))
    ) %>%
    dplyr::ungroup()

  design_missings <-
    design_coded %>%
    dplyr::mutate(
      # TODO: Diese Korrektur nach Auto-Coder-Fix entfernen!
      code_type = dplyr::case_when(
        # -99 mbo: Omissions (needs to be recoded with -99)
        # Real omissions, flagged by manual coding
        (is.na(value) | code_id == -99) & code_status == "DISPLAYED" ~ "MISSING_BY_OMISSION",
        # Omissions found by the Studio
        is.na(code_type) & code_status %in% c("DISPLAYED", "PARTLY_DISPLAYED") ~ "MISSING_BY_OMISSION",
        # -98 mir: Invalid responses due to derivation (e.g., numbers in solver) or manual coding
        is.na(code_type) & code_status %in% c("INVALID", "DERIVE_ERROR") ~ "MISSING_INVALID_RESPONSE",
        # -97 mci: Coding errors und Coding impossible
        is.na(code_type) & code_status == "CODING_ERROR" ~ "MISSING_CODING_IMPOSSIBLE",
        # -96 mnr: Not reached (needs to be recoded with -99)
        is.na(code_type) & (code_status == "NOT_REACHED" | is.na(code_status)) ~ "MISSING_NOT_REACHED",
        # -93 mnc: No codes available -- reserviere -94 für Missing By Design
        is.na(code_type) & code_status == "NO_CODING" ~ "NO_CODING",
        # -90: Coding incomplete
        code_status %in% c("CODING_INCOMPLETE", "DERIVE_PENDING", "INTENDED_INCOMPLETE") ~ code_status,
        .default = code_type
      ),
      code_id = dplyr::case_match(
        code_type,
        "MISSING_BY_OMISSION" ~ -99,
        "MISSING_INVALID_RESPONSE" ~ -98,
        "MISSING_CODING_IMPOSSIBLE" ~ -97,
        "MISSING_NOT_REACHED" ~ -96,
        "INTENDED_INCOMPLETE" ~ -95,
        "NO_CODING" ~ -93,
        # Should not be in the final dataset after manual coding!
        c("CODING_INCOMPLETE", "DERIVE_PENDING") ~ -90,

        .default = code_id
      )
    )

  not_reached_classification <-
    design_missings %>%
    # Units müssen absteigend hier geordnet werden, damit cumany() funktioniert
    # arrange(booklet_no, testlet_no, block_no, desc(unit_no)) %>%
    dplyr::group_by(dplyr::across(
      dplyr::any_of(c(
        identifiers, "booklet_no", "testlet_no", "unit_booklet_no"
      ))
    )) %>%
    dplyr::summarise(
      not_reach = all(code_type == "MISSING_NOT_REACHED" | is.na(code_type)),
      .groups = "drop"
    )

  not_reached_cases <-
    not_reached_classification %>%
    dplyr::group_by(dplyr::across(
      dplyr::any_of(c(
        identifiers, "booklet_no", "testlet_no"
      ))
    )) %>%
    dplyr::arrange(dplyr::across(dplyr::any_of(c(identifiers, "booklet_no", "testlet_no"))), dplyr::desc(unit_booklet_no)) %>%
    dplyr::mutate(
      nr = dplyr::cumall(not_reach),
      # Da es keine Unit danach mehr geben kann, wird sie diese hypothetische Unit als NOT_REACHED behandelt
      lag_nr = dplyr::lag(nr, default = TRUE),
      check_nr = nr | lag_nr
    ) %>%
    dplyr::ungroup()

  design_missings %>%
    dplyr::left_join(
      not_reached_cases,
      by = dplyr::join_by(!!! identifiers, "booklet_no", "testlet_no", "unit_booklet_no")
    ) %>%
    dplyr::mutate(
      code_type = dplyr::case_when(
        # Setzt -96, wenn Unit leer oder teilweise befüllt, aber letzte Unit vor Ende oder leeren Units
        (code_type == "MISSING_NOT_REACHED" | is.na(code_type)) &
          check_nr ~  "MISSING_NOT_REACHED",
        # Kodiert andernfalls auf -99
        (code_type == "MISSING_NOT_REACHED" | is.na(code_type))
        ~  "MISSING_BY_OMISSION",
        .default = code_type),
      code_id = dplyr::case_when(
        code_type == "MISSING_NOT_REACHED" & (is.na(code_id) | code_id != -96) ~ -96,
        code_type == "MISSING_BY_OMISSION" & (is.na(code_id) | code_id != -99) ~ -99,
        .default = code_id
      ),
      code_score = dplyr::case_when(
        code_id %in% c(-99, -98) ~ 0,
        code_id %in% c(-97, -96, -95, -93, -90) ~ NA,
        .default = code_score
      )
    ) %>%
    dplyr::arrange(dplyr::across(
      dplyr::any_of(c(
        identifiers, "booklet_no", "testlet_no", "unit_booklet_no", "variable_page", "variable_section", "variable_level"
      ))
    )) %>%
    dplyr::select(
      -dplyr::any_of(c(
        "not_reach", "nr", "lag_nr", "check_nr"
      ))
    )
}
