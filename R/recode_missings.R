# Internal orchestration: complete_design() is the public entry point.
#' @keywords internal
#' @noRd
recode_missings <- function(data, units, positions = NULL,
                            identifiers = c("group_id", "login_name", "login_code"),
                            missings = NULL, recode_omissions_to_not_reached = FALSE,
                            use_variable_names_for_recoding = FALSE,
                            diagnostics = c("compact", "full", "none"),
                            missing_policy = "eatPrepTBA", order_method = NULL,
                            not_reached_scope = NULL, recode_existing_not_reached = FALSE,
                            derived_not_reached = NULL, input_missings = NULL) {
  diagnostics <- match.arg(diagnostics)
  out <- recode_missings_impl(data, units, positions, identifiers, missings,
    recode_omissions_to_not_reached, use_variable_names_for_recoding,
    missing_policy, order_method, not_reached_scope,
    recode_existing_not_reached, derived_not_reached, input_missings)
  if (diagnostics != "none") {
    report <- missing_change_report(data, out$data, reasons = out$reasons, basis = out$basis)
    emit_missing_report(report, diagnostics, source = "recode_missings")
    emit_missing_policy_report(out)
  }
  out$data
}

missing_policy_settings <- function(missing_policy = "eatPrepTBA", order_method = NULL,
                                     not_reached_scope = NULL,
                                     recode_existing_not_reached = FALSE,
                                     derived_not_reached = NULL,
                                     use_variable_names_for_recoding = FALSE,
                                     order_overrides = NULL, input_missings = NULL) {
  missing_policy <- match.arg(missing_policy, c("eatPrepTBA", "coding_box"))
  order_method <- if (is.null(order_method)) "vomd" else
    match.arg(order_method, c("vomd", "structure", "hybrid"))
  not_reached_scope <- if (is.null(not_reached_scope)) {
    if (missing_policy == "coding_box") "unit" else "testlet"
  } else match.arg(not_reached_scope, c("unit", "testlet", "booklet"))
  derived_not_reached <- if (is.null(derived_not_reached)) {
    if (missing_policy == "coding_box") "preserve" else "recode"
  } else match.arg(derived_not_reached, c("recode", "preserve"))
  checkmate::assert_flag(recode_existing_not_reached)
  checkmate::assert_flag(use_variable_names_for_recoding)
  if (missing_policy == "coding_box" &&
      (order_method != "vomd" || recode_existing_not_reached ||
       derived_not_reached != "preserve" || use_variable_names_for_recoding ||
       !is.null(order_overrides) || !is.null(input_missings))) {
    cli::cli_abort("The coding_box policy requires VOMD order and preserved numerical not-reached and derived results. Use eatPrepTBA for overrides, name ordering, input_missings or corrective options.")
  }
  list(missing_policy = missing_policy, order_method = order_method,
       not_reached_scope = not_reached_scope,
       recode_existing_not_reached = recode_existing_not_reached,
       derived_not_reached = derived_not_reached,
       use_variable_names_for_recoding = use_variable_names_for_recoding)
}

missing_default_profile <- function() {
  tibble::tribble(
    ~code_type, ~code_id, ~code_score,
    "MISSING_NOT_REACHED", -96, NA_real_,
    "MISSING_CODING_IMPOSSIBLE", -97, NA_real_,
    "MISSING_INVALID_RESPONSE", -98, 0,
    "MISSING_BY_OMISSION", -99, 0,
    "MISSING_BY_DESIGN", -94, NA_real_,
    "INTENDED_INCOMPLETE", -95, NA_real_,
    "NO_CODING", -93, NA_real_,
    "CODING_INCOMPLETE", -90, NA_real_,
    "DERIVE_PENDING", -90, NA_real_
  )
}

missing_output_profile <- function(missings = NULL) {
  profile <- missing_default_profile()
  if (is.null(missings)) return(profile)
  checkmate::assert_data_frame(missings)
  assert_cols(missings, c("code_type", "code_id", "code_status", "code_score"), "missings")
  checkmate::assert_character(missings$code_type, any.missing = FALSE, unique = TRUE)
  checkmate::assert_numeric(missings$code_id, finite = TRUE)
  checkmate::assert_numeric(missings$code_score, finite = TRUE)
  replacement <- tibble::as_tibble(missings[c("code_type", "code_id", "code_score")])
  dplyr::bind_rows(profile[!profile$code_type %in% replacement$code_type, ], replacement)
}

missing_input_profile <- function(input_missings, output_profile) {
  if (!is.null(input_missings)) {
    checkmate::assert_data_frame(input_missings)
    assert_cols(input_missings, c("code_type", "code_id"), "input_missings")
    checkmate::assert_character(input_missings$code_type, any.missing = FALSE)
    checkmate::assert_numeric(input_missings$code_id, any.missing = FALSE, finite = TRUE)
    profile <- dplyr::distinct(tibble::as_tibble(input_missings[c("code_type", "code_id")]))
    if (anyDuplicated(profile$code_id)) cli::cli_abort("Each input_missings code ID must identify exactly one missing type.")
    return(profile)
  }
  # Output remapping must not erase standard incoming IDs. Conflicts require
  # an explicit input schema rather than an arbitrary interpretation.
  profile <- dplyr::distinct(dplyr::bind_rows(
    missing_default_profile()[c("code_type", "code_id")],
    output_profile[c("code_type", "code_id")]))
  # Retain conflicting declarations for the classifier's ambiguity diagnostic;
  # its decoder deliberately excludes IDs that identify multiple categories.
  profile
}

missing_restore_input <- function(data) {
  snapshots <- c("code_id_input", "code_score_input", "code_type_input")
  if (any(snapshots %in% names(data)) && !all(snapshots %in% names(data))) {
    cli::cli_abort("Supply all three analytical input fields, or remove all three to establish a new classification baseline.")
  }
  for (field in c("code_id", "code_score", "code_type")) {
    snapshot <- paste0(field, "_input")
    if (!snapshot %in% names(data)) data[[snapshot]] <- data[[field]]
    data[[field]] <- data[[snapshot]]
  }
  data
}

recode_missings_impl <- function(data, units, positions = NULL,
                                 identifiers = c("group_id", "login_name", "login_code"),
                                 missings = NULL, recode_omissions_to_not_reached = FALSE,
                                 use_variable_names_for_recoding = FALSE,
                                 missing_policy = "eatPrepTBA", order_method = NULL,
                                 not_reached_scope = NULL,
                                 recode_existing_not_reached = FALSE,
                                 derived_not_reached = NULL, input_missings = NULL) {
  checkmate::assert_tibble(data)
  checkmate::assert_tibble(units)
  checkmate::assert_tibble(positions, null.ok = TRUE)
  checkmate::assert_character(identifiers, any.missing = FALSE, min.len = 1L)
  checkmate::assert_flag(recode_omissions_to_not_reached)
  settings <- missing_policy_settings(missing_policy, order_method, not_reached_scope,
    recode_existing_not_reached, derived_not_reached, use_variable_names_for_recoding,
    input_missings = input_missings)
  design_keys <- c("booklet_id", "testlet_no", "unit_booklet_no", "unit_key", "unit_alias", "variable_id")
  assert_cols(data, c(design_keys, "value", "code_status", "code_type", "code_id", "code_score"), "data")
  identifiers <- intersect(identifiers, names(data))
  if (!length(identifiers)) cli::cli_abort("No person identifier from identifiers is present in data.")
  checkmate::assert_numeric(data$code_id)
  checkmate::assert_numeric(data$code_score)
  checkmate::assert_numeric(data$unit_booklet_no, any.missing = FALSE, finite = TRUE, lower = 0)
  keys <- tibble::as_tibble(data[c(identifiers, design_keys, intersect("booklet_no", names(data)))])
  keys$booklet_id <- stringr::str_to_upper(keys$booklet_id)
  if (anyDuplicated(keys)) cli::cli_abort("data contains duplicate person/variable occurrences.")
  required_keys <- c("booklet_id", "unit_booklet_no", "unit_key", "variable_id")
  if (any(vapply(keys[required_keys], anyNA, logical(1)))) cli::cli_abort("Booklet, unit position, unit, and variable keys in data must not be missing.")
  if (is.null(positions)) {
    positions <- get_design_order(data, units, order_method = settings$order_method,
      use_variable_names_for_recoding = use_variable_names_for_recoding)
  }
  assert_cols(positions, c(design_keys, "variable_order"), "positions")
  precedence <- attr(positions, "design_precedence", exact = TRUE)
  unresolved <- attr(positions, "vomd_unresolved", exact = TRUE)
  if (settings$missing_policy == "coding_box" && !is.null(unresolved) && nrow(unresolved)) {
    cli::cli_abort("The coding_box policy cannot resolve {nrow(unresolved)} VOMD item mappings to active coding variables. Correct the metadata first.")
  }
  position_keys <- positions[design_keys]
  position_keys$booklet_id <- stringr::str_to_upper(position_keys$booklet_id)
  if (anyDuplicated(position_keys)) cli::cli_abort("positions contains duplicate variable occurrences.")
  checkmate::assert_integerish(positions$variable_order, any.missing = FALSE, lower = 1)
  raw <- c(design_keys, identifiers, "booklet_no", "value", "response_present",
    "code_status", "code_id", "code_score", "code_type", "code_id_input", "code_score_input", "code_type_input")
  columns <- setdiff(names(positions)[!vapply(positions, is.list, logical(1))], raw)
  position_keys[columns] <- positions[columns]
  matched <- dplyr::left_join(keys[design_keys], position_keys, by = design_keys, relationship = "many-to-one")
  if (anyNA(matched$variable_order)) cli::cli_abort("positions does not provide an order for every row of data.")
  if ("variable_order" %in% names(data) &&
      !identical(as.numeric(data$variable_order), as.numeric(matched$variable_order))) cli::cli_abort("The order in positions conflicts with variable_order in data.")
  result <- missing_restore_input(data)
  result$code_type <- as.character(result$code_type)
  for (column in columns) result[[column]] <- matched[[column]]
  order_keys <- keys[c(identifiers, "booklet_id", intersect("booklet_no", names(keys)))]
  order_keys$variable_order <- result$variable_order
  if (anyDuplicated(order_keys)) cli::cli_abort("variable_order must uniquely identify positions within each person's booklet.")
  static <- dplyr::distinct(dplyr::bind_cols(keys[design_keys], variable_order = result$variable_order))
  if (anyDuplicated(static[design_keys]) || anyDuplicated(static[c("booklet_id", "variable_order")])) {
    cli::cli_abort("A variable occurrence must have the same static variable_order for all people assigned to its booklet.")
  }
  occurrence_fields <- setdiff(design_keys, "variable_id")
  expected_occurrences <- dplyr::distinct(position_keys[occurrence_fields])
  assignments <- dplyr::distinct(keys[c(identifiers, "booklet_id", intersect("booklet_no", names(keys)))])
  expected <- dplyr::left_join(assignments, expected_occurrences, by = "booklet_id", relationship = "many-to-many")
  if (nrow(dplyr::anti_join(expected, dplyr::distinct(keys[setdiff(names(keys), "variable_id")]), by = names(expected)))) {
    cli::cli_abort("The complete data table is missing unit occurrences from positions; classify before filtering to items.")
  }
  metadata <- suppressMessages(design_order_metadata(units))
  assert_cols(metadata, c("unit_key", "variable_id", "variable_source_type", "variable_level", "basis_sources", "sources_known"), "units")
  if (anyDuplicated(metadata[c("unit_key", "variable_id")])) cli::cli_abort("units contains duplicate variable metadata.")
  if (nrow(dplyr::anti_join(result[c("unit_key", "variable_id")], metadata, by = c("unit_key", "variable_id")))) cli::cli_abort("units does not contain metadata for every variable in data.")
  bases <- metadata[grepl("^(BASE|BASIS)", metadata$variable_source_type) |
    (is.na(metadata$variable_source_type) & metadata$variable_level %in% 0), c("unit_key", "variable_id")]
  occurrence_rows <- dplyr::group_rows(dplyr::group_by(keys,
    dplyr::across(dplyr::all_of(setdiff(names(keys), "variable_id")))))
  for (rows in occurrence_rows) {
    missing_bases <- setdiff(bases$variable_id[bases$unit_key == result$unit_key[rows[1]]], result$variable_id[rows])
    if (length(missing_bases)) cli::cli_abort("The complete data table is missing expected basis variables {missing_bases}; classify before filtering to items.")
  }
  profile <- missing_output_profile(missings)
  if (settings$missing_policy == "coding_box") {
    out <- classify_coding_box(result, metadata, profile,
      recode_omissions_to_not_reached, settings$not_reached_scope, identifiers)
  } else {
    out <- classify_eatpreptba(result, metadata, profile,
      input_profile = missing_input_profile(input_missings, profile), precedence = precedence,
      recode_omissions_to_not_reached = recode_omissions_to_not_reached,
      not_reached_scope = settings$not_reached_scope,
      recode_existing_not_reached = recode_existing_not_reached,
      derived_not_reached = settings$derived_not_reached, identifiers = identifiers)
  }
  settings$recode_omissions_to_not_reached <- recode_omissions_to_not_reached
  settings$coding_box_reference <- if (settings$missing_policy == "coding_box") "39468e5a28a24dc9ec860aee46daf8d4ed5c8682" else NULL
  attr(out$data, "missing_policy") <- settings
  attr(out$data, "missing_diagnostics") <- tibble::tibble(row = seq_len(nrow(out$data)), reason = out$diagnostics)
  attr(out$data, "vomd_unresolved") <- unresolved
  out$settings <- settings
  out
}

emit_missing_policy_report <- function(out) {
  config <- out$settings
  text <- paste0("Rule set: ", config$missing_policy, "; order: ", config$order_method,
    "; scope: ", config$not_reached_scope, "; derived results: ", config$derived_not_reached, ".")
  cli::cli_inform(c("i" = "{text}"))
  failures <- out$diagnostics[!is.na(out$diagnostics)]
  if (length(failures)) {
    counts <- table(failures)
    detail <- paste0(names(counts), "=", as.integer(counts), collapse = "; ")
    cli::cli_inform(c("i" = "Classification diagnostics: {detail}."))
  }
  unresolved <- attr(out$data, "vomd_unresolved", exact = TRUE)
  if (!is.null(unresolved) && nrow(unresolved)) {
    cli::cli_inform(c("i" = "Unresolved static VOMD item mappings: {nrow(unresolved)}; inspect the vomd_unresolved attribute."))
  }
}
