#' Complete the expected response design
#'
#' @param coded Tibble of coded responses, as returned by [eatPrepTBA::code_responses()]
#'   with `prepare = TRUE`.
#' @param units Tibble of Studio units. Prepared `unit_codes` can be reused.
#'   Retrieve unit definitions to use page and element locations for ordering,
#'   and metadata to obtain the Studio item mapping.
#' @param design Tibble returned by [get_design()], or an equivalent design.
#'   Both unit-level and variable-level designs are supported. All active coding
#'   variables of every supplied unit occurrence are completed, even when the
#'   supplied design lists only a subset of variables.
#' @param identifiers Character vector of person identifiers. Defaults to the
#'   Testcenter identifiers `group_id`, `login_name`, and `login_code`.
#' @param overwrite Logical. Rebuild existing `unit_codes`? Defaults to `FALSE`.
#' @param missings Optional missing-value scheme with `code_id`, `code_status`,
#'   `code_score`, and `code_type`. Used only when missing classification is
#'   enabled. The scheme never fills or changes the technical `code_status`.
#' @param recode_omissions_to_not_reached `FALSE` (default), `TRUE`, or `NULL`.
#'   `FALSE` adds expected positions and classifies missings while preserving
#'   omissions; existing not-reached values before later basis-variable work
#'   become omissions. `TRUE` additionally recodes trailing omissions to not
#'   reached. `NULL` only completes the design: supplied coding fields remain
#'   unchanged and newly inserted coding fields remain missing. No positions
#'   are computed in that mode.
#' @param order_overrides Optional local basis-variable ordering, passed to
#'   [get_design_order()].
#' @param item_selection Optional item-variable selection, passed to
#'   [get_design_order()]. It affects item positions, not the response row set
#'   or variable positions.
#' @param use_variable_names_for_recoding Logical. Explicitly confirm natural
#'   variable-name order for missing classification. Defaults to `FALSE`.
#'   Known physical positions and manual overrides remain usable in either mode.
#' @param diagnostics Character. `"compact"` prints a summary of actual changes
#'   in this call; `"full"` adds counts by booklet/testlet/unit occurrence;
#'   `"none"` suppresses the summary. Added rows and changes to existing rows
#'   are counted separately, regardless of earlier `response_present` values.
#'
#' @description
#' Completes coded responses with the expected variables of each booklet.
#' Expected positions are shared by everyone assigned the same booklet.
#' Missing classification then runs separately per person and testlet.
#'
#' @details
#' The technical `code_status` is always preserved, including `NA`. The logical
#' `response_present` records whether a row originated in the response data,
#' rather than inferring its origin from possibly missing coding fields. An
#' existing presence marker is retained when completing again. `id_used` retains
#' its historical meaning: at least one supplied technical status is nonmissing
#' for that person.
#'
#' Only basis variables determine the not-reached boundary. Derived variables
#' cannot act as evidence of later work. Valid and invalid derived results
#' become not reached when all their transitive basis sources are known and
#' demonstrably not reached. A mixture of omissions and not-reached sources is
#' insufficient; otherwise these derived results are preserved. This rule applies
#' whenever classification is enabled, regardless of the previous score.
#' The missing scheme updates `code_type`, `code_id`, and `code_score`
#' together; `code_status` remains intact. Use the original coded input when
#' comparing policies, since replaced analytical fields cannot be reconstructed.
#' Uncertain ordering preserves existing categories and leaves unclassified
#' rows unresolved. Names may resolve uncertainty only with explicit confirmation;
#' conflicts with known physical order require an `order_overrides` entry.
#'
#' The same operations can be called separately: complete with `NULL`, obtain
#' a static table using [get_design_order()], and pass that table to
#' [recode_missings()]. Classify before filtering to selected items, so later
#' basis-variable work remains available as evidence.
#'
#' @return A tibble with completed response rows and `response_present`.
#'   With `FALSE` or `TRUE`, also includes `variable_order`, `item_order`,
#'   `order_group`, and `order_source` (and the Studio `item_id` when available).
#' @export
complete_design <- function(coded,
                            units,
                            design,
                            identifiers = c("group_id", "login_name", "login_code"),
                            overwrite = FALSE,
                            missings = NULL,
                            recode_omissions_to_not_reached = FALSE,
                            order_overrides = NULL,
                            item_selection = NULL,
                            use_variable_names_for_recoding = FALSE,
                            diagnostics = c("compact", "full", "none")) {
  diagnostics <- match.arg(diagnostics)
  checkmate::assert_character(identifiers, min.len = 1L, any.missing = FALSE)
  checkmate::assert_flag(overwrite)
  checkmate::assert_flag(use_variable_names_for_recoding)
  if (!is.null(recode_omissions_to_not_reached)) {
    checkmate::assert_flag(recode_omissions_to_not_reached)
  }
  checkmate::assert_tibble(design)
  checkmate::assert_tibble(coded)
  checkmate::assert_tibble(units)
  identifiers <- intersect(identifiers, names(design))
  if (!length(identifiers)) {
    cli::cli_abort("{.arg design} must contain at least one of {.arg identifiers}.")
  }
  occurrence_keys <- c(identifiers, "booklet_id", "booklet_no", "testlet_no",
                       "unit_booklet_no", "unit_key", "unit_alias")
  assert_cols(design, occurrence_keys, "design")
  coded_keys <- c(identifiers, "booklet_id", "unit_key", "unit_alias", "variable_id")
  code_fields <- c("code_status", "value", "code_id", "code_type", "code_score")
  assert_cols(coded, c(coded_keys, code_fields), "coded")
  if ("response_present" %in% names(coded)) {
    checkmate::assert_logical(coded$response_present, len = nrow(coded),
                             any.missing = FALSE)
  } else {
    coded$response_present <- rep(TRUE, nrow(coded))
  }

  cli_setting()
  prepared_units <- suppressMessages(add_coding_scheme(
    units, overwrite = overwrite, filter_has_codes = TRUE))
  metadata <- design_order_metadata(prepared_units)
  # The dependency graph stays on the unit table, rather than being copied to
  # every person's response rows.
  row_metadata <- metadata %>%
    dplyr::select(-dplyr::any_of(c("source_ids", "basis_sources", "sources_known")))
  completed <- complete_design_rows(design, row_metadata, occurrence_keys,
                                    code_fields)

  # Unit aliases normally disambiguate repeated units. If they do not, require
  # the occurrence columns in coded instead of attaching one response twice.
  join_keys <- c(identifiers, ".booklet_merge", "unit_key", "unit_alias",
                 "variable_id",
                 intersect(c("booklet_no", "testlet_no", "unit_booklet_no"),
                           names(coded)))
  completed$.booklet_merge <- stringr::str_to_upper(completed$booklet_id)
  coded$.booklet_merge <- stringr::str_to_upper(coded$booklet_id)
  if (anyDuplicated(coded[join_keys])) {
    cli::cli_abort("{.arg coded} contains duplicate response keys. Supply one row per variable and unit occurrence.")
  }
  ambiguous <- completed %>%
    dplyr::select(dplyr::all_of(join_keys)) %>%
    dplyr::filter(duplicated(.) | duplicated(., fromLast = TRUE)) %>%
    dplyr::semi_join(coded, by = join_keys)
  if (nrow(ambiguous)) {
    cli::cli_abort("Repeated unit occurrences cannot be distinguished in {.arg coded}. Include {.field testlet_no} and {.field unit_booklet_no} (and {.field booklet_no} for repeated booklet assignments).")
  }
  # Expected-design metadata takes precedence over copied response metadata;
  # the actual coding fields and presence marker always come from coded.
  coded_payload <- coded %>%
    dplyr::select(-dplyr::any_of(c(
      "booklet_id", setdiff(intersect(names(coded), names(completed)), join_keys)
    )))
  marker <- utils::tail(make.unique(c(names(completed), names(coded_payload),
                               ".completion_supplied")), 1L)
  coded_payload[[marker]] <- rep(TRUE, nrow(coded_payload))
  completed <- completed %>%
    dplyr::left_join(coded_payload, by = join_keys, relationship = "one-to-one") %>%
    dplyr::mutate(response_present = dplyr::coalesce(.data$response_present, FALSE)) %>%
    dplyr::select(-dplyr::any_of(".booklet_merge")) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(identifiers))) %>%
    dplyr::mutate(id_used = any(!is.na(code_status))) %>%
    dplyr::ungroup()

  added <- is.na(completed[[marker]])
  completed[[marker]] <- NULL
  before <- completed

  if (is.null(recode_omissions_to_not_reached)) {
    if (diagnostics != "none") {
      report <- missing_change_report(before, completed, added = added, classified = FALSE)
      emit_missing_report(report, diagnostics, source = "complete_design")
    }
    return(completed)
  }

  completed <- completed %>%
    dplyr::select(-dplyr::any_of(c(
      "variable_order", "item_order", "order_group", "order_source", "item_order_source", "item_id"
    )))
  positions <- get_design_order(completed, prepared_units,
                                order_overrides = order_overrides,
                                item_selection = item_selection)
  out <- recode_missings_impl(
    completed, prepared_units, positions = positions,
    identifiers = identifiers, missings = missings,
    recode_omissions_to_not_reached = recode_omissions_to_not_reached,
    use_variable_names_for_recoding = use_variable_names_for_recoding
  )
  if (diagnostics != "none") {
    report <- missing_change_report(before, out$data, added = added,
                                    reasons = out$reasons, basis = out$basis)
    emit_missing_report(report, diagnostics, source = "complete_design")
  }
  out$data
}

# Complete all active variables, propagating unit-level design columns while
# retaining variable-specific design columns only on their original variables.
complete_design_rows <- function(design, metadata, occurrence_keys, code_fields) {
  metadata_fields <- setdiff(names(metadata), c("unit_key", "variable_id"))
  design <- design %>%
    dplyr::select(-dplyr::any_of(c(
      metadata_fields, code_fields, "response_present", "id_used"
    )))
  unknown_units <- setdiff(unique(design$unit_key), unique(metadata$unit_key))
  if (length(unknown_units)) {
    cli::cli_abort("No active variable metadata for design units: {unknown_units}.")
  }
  variable_keys <- c(occurrence_keys, "variable_id")
  if ("variable_id" %in% names(design)) {
    if (anyDuplicated(design[variable_keys])) {
      cli::cli_abort("{.arg design} contains duplicate variable/occurrence keys.")
    }
    unknown <- design %>%
      dplyr::anti_join(metadata, by = c("unit_key", "variable_id"))
    if (nrow(unknown)) {
      cli::cli_abort("Design variables absent from active unit metadata: {unique(paste(unknown$unit_key, unknown$variable_id, sep = '/'))}.")
    }
  }
  extra_columns <- setdiff(names(design), variable_keys)
  variable_only <- grepl("^(variable_|item_|order_)", extra_columns)
  unit_columns <- extra_columns[!variable_only & vapply(extra_columns, function(column) {
    counts <- design %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(occurrence_keys))) %>%
      dplyr::summarise(.count = dplyr::n_distinct(.data[[column]]),
                       .groups = "drop")
    all(counts$.count <= 1L)
  }, logical(1))]
  occurrences <- design %>%
    dplyr::select(dplyr::all_of(c(occurrence_keys, unit_columns))) %>%
    dplyr::distinct()
  expanded <- occurrences %>%
    dplyr::left_join(metadata, by = "unit_key", relationship = "many-to-many")
  variable_columns <- setdiff(extra_columns, unit_columns)
  if (length(variable_columns)) {
    expanded <- expanded %>%
      dplyr::left_join(
        design %>% dplyr::select(dplyr::all_of(c(variable_keys, variable_columns))),
        by = variable_keys, relationship = "one-to-one"
      )
  }
  expanded
}
