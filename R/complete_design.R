#' Complete the expected response design
#'
#' @param coded Tibble of coded responses, as returned by [eatPrepTBA::code_responses()]
#'   with `prepare = TRUE`.
#' @param units Tibble of Studio units. Prepared `unit_codes` can be reused.
#'   Retrieve unit definitions to use page and element locations for ordering,
#'   and metadata to obtain the Studio item mapping. Only units referenced by
#'   `design` are prepared and checked; unused units are ignored.
#' @param design Tibble returned by [get_design()], or an equivalent design.
#'   Both unit-level and variable-level designs are supported. All active coding
#'   variables of every supplied unit occurrence are completed, even when the
#'   supplied design lists only a subset of variables. Confirmed deactivated
#'   variables (`BASE_NO_VALUE`) listed in a variable-level design are excluded
#'   with a warning; the unit occurrences and their active variables are retained.
#' @param identifiers Character vector of person identifiers. Defaults to the
#'   Testcenter identifiers `group_id`, `login_name`, and `login_code`.
#' @param overwrite Logical. Rebuild existing `unit_codes`? Defaults to `FALSE`.
#' @param missings Optional missing-value scheme with `code_id`, `code_status`,
#'   `code_score`, and `code_type`. Used only when missing classification is
#'   enabled. The scheme never fills or changes the technical `code_status`.
#' @param recode_omissions_to_not_reached `FALSE` (default), `TRUE`, or `NULL`.
#'   `FALSE` adds expected positions and classifies missings while preserving
#'   omissions and existing numerical not-reached codes. `TRUE` additionally recodes trailing omissions to not
#'   reached. `NULL` only completes the design: supplied coding fields remain
#'   unchanged and newly inserted coding fields remain missing. No positions
#'   are computed in that mode.
#' @param order_overrides Optional local basis-variable ordering table with
#'   `unit_key`, `variable_id`, and `local_order`, optionally restricted by booklet
#'   and unit occurrence. Available in the eatPrepTBA rule set.
#' @param item_selection Optional Studio item-variable selection. It masks
#'   item positions without renumbering them, and does not filter response rows
#'   or the universe used for classification.
#' @param use_variable_names_for_recoding Logical. Explicitly trust natural
#'   variable-name order for missing classification. Defaults to `FALSE`.
#'   Known physical positions and manual overrides remain usable in either mode.
#' @param diagnostics Character. `"compact"` prints a summary of actual changes
#'   in this call; `"full"` adds counts by booklet/testlet/unit occurrence;
#'   `"none"` suppresses the summary. Added rows and changes to existing rows
#'   are counted separately, regardless of earlier `response_present` values.
#' @param missing_policy Rule set: `"eatPrepTBA"` (default) or `"coding_box"`.
#'   The latter reproduces the Coding Box item resolver at commit
#'   `39468e5a28a24dc9ec860aee46daf8d4ed5c8682` on the expected long-format rows.
#' @param order_method `NULL` selects `"hybrid"` for eatPrepTBA and `"vomd"`
#'   for Coding Box. Explicit alternatives are `"vomd"`, `"structure"` (unit
#'   pages and elements), or `"hybrid"` (consistent VOMD and structure constraints).
#'   Always-visible pages do not establish physical before/after relations.
#'   Coding Box requires VOMD and does not permit overrides or trusted names.
#' @param not_reached_scope `NULL` selects `"testlet"` for eatPrepTBA and
#'   `"unit"` for Coding Box. Explicit alternatives are `"unit"`, `"testlet"`,
#'   and `"booklet"`. Coding Box forbids trailing-omission recoding in Unit scope.
#' @param recode_existing_not_reached Logical, default `FALSE`. In eatPrepTBA,
#'   optionally correct existing numerical not-reached codes before proven later
#'   work to omission. Uncoded technical NOT_REACHED is classified in either mode.
#' @param derived_not_reached `NULL` selects `"recode"` for eatPrepTBA and
#'   `"preserve"` for Coding Box. `"recode"` replaces valid or invalid derived
#'   results when all known basis sources are demonstrably not reached in the
#'   trailing region. `"preserve"` retains existing numerical derived results.
#' @param input_missings Optional input schema with `code_type` and `code_id`,
#'   for eatPrepTBA. Output remapping through `missings` retains recognition of
#'   nonconflicting standard incoming IDs. Ambiguous input IDs require an
#'   explicit schema. Coding Box recognizes its selected output profile exactly.
#'
#' @description
#' Completes coded responses with the expected variables of each booklet.
#' Expected positions are shared by everyone assigned the same booklet.
#' Missing classification runs separately per person within the selected scope.
#'
#' @details
#' The technical `code_status` is always preserved, including `NA`. The logical
#' `response_present` records whether a row originated in the response data,
#' rather than inferring its origin from possibly missing coding fields. An
#' existing presence marker is retained when completing again. `id_used` retains
#' its historical meaning: at least one supplied technical status is nonmissing
#' for that person.
#'
#' VOMD items and their required source closure provide activity evidence;
#' eatPrepTBA additionally includes every active basis variable. Unlinked basis
#' variables receive a display index but establish only Unit-level comparisons
#' when no within-Unit location is known. A dense index does not resolve unknown
#' presentation relationships. Coding Box leaves variables outside its item and
#' uniquely anchored source universe unchanged.
#'
#' When missing classification is enabled, Coding Box requires every VOMD item
#' mapping to resolve to an active variable. A mapping to a deactivated variable
#' still causes an error after that variable is excluded from the design.
#' Correct the Studio item mapping and reload the metadata. eatPrepTBA continues
#' and records unresolved mappings in the `vomd_unresolved` attribute.
#'
#' eatPrepTBA gives source evidence priority when considering a derived result
#' for not-reached replacement, preventing that result from anchoring itself.
#' Otherwise existing derived results also count as activity. Coding-error and
#' no-coding results count as activity in eatPrepTBA only with a stored value;
#' Coding Box follows its own technical-status and numerical-result rules.
#' Uncertain ordering and incomplete sources retain existing analytical results
#' and expose unresolved diagnostics rather than inventing a score.
#'
#' The original analytical fields are retained as `code_id_input`,
#' `code_score_input`, and `code_type_input`. Subsequent classification starts
#' from these fields, so changing rule sets or profiles restores original input.
#' Deliberate edits to the analytical input must also update the corresponding
#' input fields or remove all three input fields before reclassification.
#' Classification precedes item selection and never widens the response table
#' or creates missing-by-design rows outside the supplied design.
#'
#' @return A tibble with completed response rows and `response_present`.
#'   With `FALSE` or `TRUE`, also includes `variable_order`, `item_order`,
#'   `order_group`, `order_source`, `position_group`, `position_source`, and
#'   the original analytical input fields. Attributes `missing_policy` and
#'   `missing_diagnostics` record effective options and row-aligned diagnostics.
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
                            diagnostics = c("compact", "full", "none"),
                            missing_policy = c("eatPrepTBA", "coding_box"),
                            order_method = NULL,
                            not_reached_scope = NULL,
                            recode_existing_not_reached = FALSE,
                            derived_not_reached = NULL,
                            input_missings = NULL) {
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
  design_order_assert_keys(design, "unit_key", "design")
  coded_keys <- c(identifiers, "booklet_id", "unit_key", "unit_alias", "variable_id")
  code_fields <- c("code_status", "value", "code_id", "code_type", "code_score")
  assert_cols(coded, c(coded_keys, code_fields), "coded")
  code_fields <- c(code_fields, "code_id_input", "code_score_input", "code_type_input")
  if ("response_present" %in% names(coded)) {
    checkmate::assert_logical(coded$response_present, len = nrow(coded),
                             any.missing = FALSE)
  } else {
    coded$response_present <- rep(TRUE, nrow(coded))
  }

  cli_setting()
  units <- design_order_units_for_keys(units, design$unit_key)
  prepared_units <- if (nrow(units)) {
    suppressMessages(add_coding_scheme(
      units, overwrite = overwrite, filter_has_codes = TRUE))
  } else units
  metadata <- design_order_metadata(prepared_units)
  # The dependency graph stays on the unit table, rather than being copied to
  # every person's response rows.
  row_metadata <- metadata %>%
    dplyr::select(-dplyr::any_of(c("source_ids", "basis_sources", "sources_known")))
  completed <- complete_design_rows(design, row_metadata, occurrence_keys,
                                    code_fields, prepared_units)

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

  missing_policy <- match.arg(missing_policy)
  settings <- missing_policy_settings(missing_policy, order_method, not_reached_scope,
    recode_existing_not_reached, derived_not_reached,
    use_variable_names_for_recoding, order_overrides, input_missings)
  completed <- completed %>%
    dplyr::select(-dplyr::any_of(c(
      "variable_order", "item_order", "order_group", "order_source", "item_order_source", "item_id",
      "position_group", "position_source", "item_position", "analysis_included",
      "box_position", "box_included", "box_is_item"
    )))
  positions <- get_design_order(completed, prepared_units,
                                order_overrides = order_overrides,
                                item_selection = item_selection,
                                order_method = settings$order_method,
                                use_variable_names_for_recoding = use_variable_names_for_recoding)
  out <- recode_missings_impl(
    completed, prepared_units, positions = positions,
    identifiers = identifiers, missings = missings,
    recode_omissions_to_not_reached = recode_omissions_to_not_reached,
    use_variable_names_for_recoding = use_variable_names_for_recoding,
    missing_policy = settings$missing_policy, order_method = settings$order_method,
    not_reached_scope = settings$not_reached_scope,
    recode_existing_not_reached = recode_existing_not_reached,
    derived_not_reached = settings$derived_not_reached, input_missings = input_missings
  )
  if (diagnostics != "none") {
    report <- missing_change_report(before, out$data, added = added,
                                    reasons = out$reasons, basis = out$basis)
    emit_missing_report(report, diagnostics, source = "complete_design")
    emit_missing_policy_report(out)
  }
  out$data
}

# Complete all active variables, propagating unit-level design columns while
# retaining variable-specific design columns only on their original variables.
complete_design_rows <- function(design, metadata, occurrence_keys, code_fields,
                                 units = NULL) {
  metadata_fields <- setdiff(names(metadata), c("unit_key", "variable_id"))
  design <- design %>%
    dplyr::select(-dplyr::any_of(c(
      metadata_fields, code_fields, "response_present", "id_used"
    )))
  unknown_units <- setdiff(unique(design$unit_key), unique(metadata$unit_key))
  if (length(unknown_units)) {
    cli::cli_abort(c(
      "No active variable metadata for design units: {unknown_units}.",
      "i" = "Check that these units are present in {.arg units} and contain active coding variables.",
      "i" = "If existing {.field unit_codes} are outdated, rebuild them with {.code add_coding_scheme(units, overwrite = TRUE)} using the matching Studio coding schemes."
    ))
  }
  variable_keys <- c(occurrence_keys, "variable_id")
  if ("variable_id" %in% names(design)) {
    if (anyDuplicated(design[variable_keys])) {
      cli::cli_abort("{.arg design} contains duplicate variable/occurrence keys.")
    }
    unknown <- design %>%
      dplyr::distinct(.data$unit_key, .data$variable_id) %>%
      dplyr::anti_join(metadata, by = c("unit_key", "variable_id"))
    if (nrow(unknown)) {
      inactive <- complete_design_inactive_variables(units, unknown)
      unknown <- dplyr::anti_join(unknown, inactive,
                                  by = c("unit_key", "variable_id"))
      if (nrow(unknown)) {
        cli::cli_abort(c(
          "Design variables absent from active unit metadata: {paste(unknown$unit_key, unknown$variable_id, sep = '/')}.",
          "i" = "These variables are not confirmed as deactivated ({.val BASE_NO_VALUE}).",
          "i" = "Check that {.arg design} and {.arg units} use the same Studio unit versions and variable aliases.",
          "i" = "If {.field unit_codes} are outdated, rebuild them with {.code add_coding_scheme(units, overwrite = TRUE)}, then recreate the design using those units.",
          "i" = "Inspect the affected keys with {.code rlang::last_error()$variables} and the available active keys with {.code rlang::last_error()$available_variables}."
        ), class = "eatPrepTBA_design_metadata_error", variables = unknown,
        available_variables = dplyr::distinct(metadata, .data$unit_key,
                                               .data$variable_id))
      }
      if (nrow(inactive)) {
        cli::cli_warn(c(
          "Excluded deactivated design variables ({.val BASE_NO_VALUE}): {paste(inactive$unit_key, inactive$variable_id, sep = '/')}.",
          "i" = "These variables are omitted from the completed data and missing classification. Unit occurrences and all active variables are retained."
        ), class = "eatPrepTBA_inactive_design_variables", variables = inactive)
      }
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

# Confirm exclusions against the chosen cache, consulting the original scheme
# only for aliases that are absent from that cache. Missing codes alone do not
# establish deactivation, and a bad raw scheme must not invalidate a usable cache.
complete_design_inactive_variables <- function(units, requested) {
  empty <- requested[0, c("unit_key", "variable_id"), drop = FALSE]
  if (is.null(units) || !nrow(requested)) return(empty)
  candidates <- vector("list", nrow(units))
  for (i in seq_len(nrow(units))) {
    key <- as.character(units$unit_key[[i]])
    ids <- requested$variable_id[requested$unit_key == key]
    if (!length(ids)) next
    cached <- if ("unit_codes" %in% names(units)) units$unit_codes[[i]] else NULL
    if (is.null(cached)) cached <- tibble::tibble()
    definition_columns <- c("variable_id", "variable_source_type")
    definitions <- dplyr::select(cached, dplyr::any_of(definition_columns))
    cached_ids <- if ("variable_id" %in% names(cached)) as.character(cached$variable_id) else character()
    if (any(!ids %in% cached_ids) && "coding_scheme" %in% names(units)) {
      raw <- tryCatch(
        suppressMessages(prepare_coding_scheme(units$coding_scheme[[i]],
                                               filter_has_codes = FALSE)),
        error = function(error) NULL
      )
      if (!is.null(raw) && "variable_id" %in% names(raw)) {
        definitions <- dplyr::bind_rows(definitions,
          dplyr::select(raw[!raw$variable_id %in% cached_ids, , drop = FALSE],
                        dplyr::any_of(definition_columns)))
      }
    }
    if (!all(c("variable_id", "variable_source_type") %in% names(definitions))) next
    candidates[[i]] <- tibble::tibble(
      unit_key = key, variable_id = as.character(definitions$variable_id),
      variable_source_type = as.character(definitions$variable_source_type)
    ) %>% dplyr::filter(.data$variable_id %in% ids)
  }
  definitions <- dplyr::bind_rows(candidates)
  if (!nrow(definitions)) return(empty)
  definitions %>%
    dplyr::group_by(.data$unit_key, .data$variable_id) %>%
    dplyr::filter(all(.data$variable_source_type %in% "BASE_NO_VALUE")) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(.data$unit_key, .data$variable_id)
}
