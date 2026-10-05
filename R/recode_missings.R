#' Classify omissions and not-reached responses using established positions
#'
#' @param data Tibble containing the complete variable-level response table,
#'   including the design keys, `value`, and the four `code_*` fields.
#' @param units Tibble of units, optionally prepared with [add_coding_scheme()].
#' @param positions Optional position table returned by [eatPrepTBA::get_design_order()].
#'   If omitted, `data` must already contain `variable_order`. The order is never
#'   inferred again by this function.
#' @param identifiers Character vector of person identifiers.
#' @param missings Optional missing-value schema with `code_type`, `code_id`,
#'   `code_status`, and `code_score`. Entries override the default schema.
#' @param recode_omissions_to_not_reached Logical. With `FALSE`, existing omissions
#'   are retained. With `TRUE`, omissions after the last worked-on basis variable
#'   are additionally classified as not reached.
#'
#' @description
#' Classifies each person, booklet, and testlet separately. Only basis variables
#' determine the last reached position. Derived variables use the position of
#' their last transitive basis source, not their artificial position in the
#' complete ordering. Technical `code_status` values, including `NA`, and stored
#' response values are never changed.
#' Valid and invalid basis results count as reached. Coding-error, no-coding,
#' and pending results count as work only when a nonmissing response value is
#' stored. With `FALSE`, omissions also count as reached; with `TRUE`, they can
#' belong to the trailing region. Recognized negative missing IDs can identify
#' basis missings when their analytical type and technical status are absent.
#' If `response_present` is supplied, `FALSE` identifies rows added from the
#' design. Such derived rows receive a missing classification only when their
#' complete basis-source tree supports it. Without this column, supplied rows
#' are treated as observed input rows.
#' Classify before filtering the table to selected items: every active basis
#' variable of each supplied unit occurrence must be present. A supplied position
#' table also permits validation of entirely absent unit occurrences. Without
#' that table, an entirely removed occurrence cannot be detected.
#'
#' An existing invalid derived result can become not reached only with `TRUE`,
#' when all known basis sources are omissions or not reached and its last source
#' lies beyond the reached boundary. In that exception only `code_type` and
#' `code_score` change: the original `code_id` and `code_status` are retained.
#' Valid derived results and existing coding-error/no-coding results are protected.
#'
#' @return The input table with updated analytical missing types, IDs, and scores,
#'   preserving its row order and row count.
#' @export
recode_missings <- function(data, units, positions = NULL,
                            identifiers = c("group_id", "login_name", "login_code"),
                            missings = NULL,
                            recode_omissions_to_not_reached = FALSE) {
  checkmate::assert_tibble(data)
  checkmate::assert_tibble(units)
  checkmate::assert_tibble(positions, null.ok = TRUE)
  checkmate::assert_character(identifiers, any.missing = FALSE, min.len = 1L)
  checkmate::assert_flag(recode_omissions_to_not_reached)
  checkmate::assert_tibble(missings, null.ok = TRUE)

  design_keys <- c("booklet_id", "testlet_no", "unit_booklet_no", "unit_key",
                   "unit_alias", "variable_id")
  assert_cols(data, c(design_keys, "value", "code_status", "code_type",
                      "code_id", "code_score"), "data")
  identifiers <- intersect(identifiers, names(data))
  if (!length(identifiers)) {
    cli::cli_abort("No person identifier from {.arg identifiers} is present in {.arg data}.")
  }
  checkmate::assert_numeric(data$code_id)
  checkmate::assert_numeric(data$code_score)

  # Case-insensitive booklet matching agrees with complete_design(). This is an
  # internal key only; the original booklet spelling is preserved in the output.
  keys <- tibble::as_tibble(data[c(identifiers, design_keys)])
  keys$booklet_id <- stringr::str_to_upper(keys$booklet_id)
  duplicate_keys <- keys
  if ("booklet_no" %in% names(data)) duplicate_keys$booklet_no <- data$booklet_no
  if (anyDuplicated(duplicate_keys)) {
    cli::cli_abort("{.arg data} contains duplicate person/variable occurrences.")
  }
  required_keys <- c("booklet_id", "unit_booklet_no", "unit_key", "variable_id")
  if (any(vapply(keys[required_keys], anyNA, logical(1)))) {
    cli::cli_abort("Booklet, unit position, unit, and variable keys in {.arg data} must not be missing.")
  }

  result <- data
  if (!is.null(positions)) {
    assert_cols(positions, c(design_keys, "variable_order"), "positions")
    checkmate::assert_integerish(positions$variable_order, any.missing = FALSE, lower = 1)
    position_keys <- positions[design_keys]
    position_keys$booklet_id <- stringr::str_to_upper(position_keys$booklet_id)
    if (anyDuplicated(position_keys)) {
      cli::cli_abort("{.arg positions} contains duplicate variable occurrences.")
    }
    raw_columns <- c(identifiers, "booklet_no", "value", "response_present",
                     "code_id", "code_status", "code_type", "code_score")
    scalar_columns <- names(positions)[vapply(positions, function(column) !is.list(column), logical(1))]
    position_columns <- setdiff(scalar_columns, c(design_keys, raw_columns))
    position_keys[position_columns] <- positions[position_columns]
    matched <- dplyr::left_join(keys, position_keys, by = design_keys,
                                relationship = "many-to-one")
    if (anyNA(matched$variable_order)) {
      cli::cli_abort("{.arg positions} does not provide an order for every row of {.arg data}.")
    }
    if ("variable_order" %in% names(data) &&
        !identical(as.numeric(data$variable_order), as.numeric(matched$variable_order))) {
      cli::cli_abort("The order in {.arg positions} conflicts with {.field variable_order} in {.arg data}.")
    }
    authoritative <- c("variable_order", "item_order", "order_group", "order_source",
                        "item_id", "item_order_source")
    for (column in position_columns) {
      if (column %in% authoritative || !column %in% names(result)) {
        result[[column]] <- matched[[column]]
      }
    }
    occurrence_fields <- setdiff(design_keys, "variable_id")
    expected_occurrences <- dplyr::distinct(position_keys[occurrence_fields])
    assignments <- keys[c(identifiers, "booklet_id", "testlet_no")]
    if ("booklet_no" %in% names(data)) assignments$booklet_no <- data$booklet_no
    assignments <- dplyr::distinct(assignments)
    expected <- dplyr::left_join(assignments, expected_occurrences,
                                 by = c("booklet_id", "testlet_no"), relationship = "many-to-many")
    supplied <- dplyr::distinct(duplicate_keys[setdiff(names(duplicate_keys), "variable_id")])
    absent <- dplyr::anti_join(expected, supplied, by = names(expected))
    if (nrow(absent)) {
      cli::cli_abort("The complete {.arg data} table is missing unit occurrences from {.arg positions}; classify before filtering to items.")
    }
  }
  assert_cols(result, "variable_order", "data")
  checkmate::assert_integerish(result$variable_order, any.missing = FALSE, lower = 1)
  order_keys <- keys[c(identifiers, "booklet_id")]
  if ("booklet_no" %in% names(data)) order_keys$booklet_no <- data$booklet_no
  order_keys$variable_order <- result$variable_order
  if (anyDuplicated(order_keys)) {
    cli::cli_abort("{.field variable_order} must uniquely identify positions within each person's booklet.")
  }
  static_order <- keys[design_keys]
  static_order$variable_order <- result$variable_order
  static_order <- dplyr::distinct(static_order)
  if (anyDuplicated(static_order[design_keys])) {
    cli::cli_abort("A variable occurrence must have the same static {.field variable_order} for all people assigned to its booklet.")
  }
  if (anyDuplicated(static_order[c("booklet_id", "variable_order")])) {
    cli::cli_abort("Each static booklet position must identify only one variable occurrence.")
  }

  metadata <- design_order_metadata(units)
  assert_cols(metadata, c("unit_key", "variable_id", "variable_source_type",
                          "variable_level", "basis_sources", "sources_known"), "units")
  if (anyDuplicated(metadata[c("unit_key", "variable_id")])) {
    cli::cli_abort("{.arg units} contains duplicate variable metadata.")
  }
  metadata$.metadata_present <- TRUE
  variable_metadata <- dplyr::left_join(
    keys[c("unit_key", "variable_id")], metadata,
    by = c("unit_key", "variable_id"), relationship = "many-to-one"
  )
  if (anyNA(variable_metadata$.metadata_present)) {
    cli::cli_abort("{.arg units} does not contain metadata for every variable in {.arg data}.")
  }
  basis <- (!is.na(variable_metadata$variable_source_type) &
              grepl("^(BASE|BASIS)", variable_metadata$variable_source_type)) |
    (is.na(variable_metadata$variable_source_type) &
       !is.na(variable_metadata$variable_level) & variable_metadata$variable_level == 0)
  occurrence_cols <- c(identifiers, "booklet_id", "testlet_no", "unit_booklet_no",
                        "unit_key", "unit_alias")
  occurrence_keys <- keys[occurrence_cols]
  if ("booklet_no" %in% names(data)) occurrence_keys$booklet_no <- data$booklet_no
  occurrence_ids <- dplyr::group_indices(dplyr::group_by(occurrence_keys,
                                         dplyr::across(dplyr::everything())))
  metadata_basis <- (!is.na(metadata$variable_source_type) &
                       grepl("^(BASE|BASIS)", metadata$variable_source_type)) |
    (is.na(metadata$variable_source_type) & !is.na(metadata$variable_level) &
       metadata$variable_level == 0)
  expected_bases <- split(metadata$variable_id[metadata_basis], metadata$unit_key[metadata_basis])
  occurrence_rows <- split(seq_len(nrow(data)), occurrence_ids)
  for (rows in occurrence_rows) {
    missing_bases <- setdiff(expected_bases[[data$unit_key[rows[1]]]], data$variable_id[rows])
    if (length(missing_bases)) {
      cli::cli_abort("The complete {.arg data} table is missing expected basis variables {.field {missing_bases}} in unit {.field {data$unit_key[rows[1]]}}; classify before filtering to items.")
    }
  }

  profile <- tibble::tribble(
    ~code_type, ~code_id, ~code_score,
    "MISSING_NOT_REACHED", -96, NA_real_,
    "MISSING_CODING_IMPOSSIBLE", -97, NA_real_,
    "MISSING_INVALID_RESPONSE", -98, 0,
    "MISSING_BY_OMISSION", -99, 0,
    "INTENDED_INCOMPLETE", -95, NA_real_,
    "NO_CODING", -93, NA_real_,
    "CODING_INCOMPLETE", -90, NA_real_,
    "DERIVE_PENDING", -90, NA_real_
  )
  default_profile <- profile
  if (!is.null(missings)) {
    assert_cols(missings, c("code_type", "code_id", "code_status", "code_score"), "missings")
    checkmate::assert_character(missings$code_type, any.missing = FALSE, unique = TRUE)
    checkmate::assert_numeric(missings$code_id)
    checkmate::assert_numeric(missings$code_score)
    profile <- dplyr::bind_rows(
      profile[!profile$code_type %in% missings$code_type, ],
      missings[c("code_type", "code_id", "code_score")]
    )
  }

  present <- rep(TRUE, nrow(data))
  if ("response_present" %in% names(data)) {
    checkmate::assert_logical(data$response_present, any.missing = FALSE)
    present <- data$response_present
  }
  has_value <- vapply(seq_len(nrow(data)), function(i) {
    value <- if (is.list(data$value)) data$value[[i]] else data$value[i]
    length(value) > 0L && any(!is.na(value))
  }, logical(1))
  original_type <- as.character(data$code_type)
  types <- original_type
  # A coded result can have a masked raw value or missing type metadata. Do not
  # erase that evidence merely because the technical status is also absent.
  valid_untyped_basis <- basis & present & is.na(types) &
    (is.na(data$code_status) | data$code_status %in% "CODING_COMPLETE") &
    ((!is.na(data$code_id) & data$code_id >= 0) |
       (is.na(data$code_id) & !is.na(data$code_score)))
  unresolved <- is.na(types) & (basis | present) & !valid_untyped_basis
  status_types <- c(
    DISPLAYED = "MISSING_BY_OMISSION", PARTLY_DISPLAYED = "MISSING_BY_OMISSION",
    INVALID = "MISSING_INVALID_RESPONSE", DERIVE_ERROR = "MISSING_INVALID_RESPONSE",
    CODING_ERROR = "MISSING_CODING_IMPOSSIBLE", NOT_REACHED = "MISSING_NOT_REACHED",
    NO_CODING = "NO_CODING", INTENDED_INCOMPLETE = "INTENDED_INCOMPLETE",
    CODING_INCOMPLETE = "CODING_INCOMPLETE", DERIVE_PENDING = "DERIVE_PENDING"
  )
  mapped_status <- unname(status_types[as.character(data$code_status)])
  types[unresolved & !is.na(mapped_status)] <- mapped_status[unresolved & !is.na(mapped_status)]
  # A missing score of zero is not evidence of work. When type and status are
  # absent, recognized negative missing IDs still identify basis missings.
  missing_ids <- dplyr::distinct(dplyr::bind_rows(default_profile, profile)[c("code_id", "code_type")])
  missing_ids <- missing_ids[!is.na(missing_ids$code_id) & missing_ids$code_id < 0, ]
  ambiguous_ids <- duplicated(missing_ids$code_id) | duplicated(missing_ids$code_id, fromLast = TRUE)
  missing_ids <- missing_ids[!ambiguous_ids, ]
  from_id <- basis & is.na(types) & !valid_untyped_basis &
    data$code_id %in% missing_ids$code_id
  types[from_id] <- missing_ids$code_type[match(data$code_id[from_id], missing_ids$code_id)]
  types[is.na(types) & basis & !has_value & !valid_untyped_basis & is.na(data$code_status)] <- "MISSING_NOT_REACHED"

  omission <- "MISSING_BY_OMISSION"
  not_reached <- "MISSING_NOT_REACHED"
  invalid <- "MISSING_INVALID_RESPONSE"
  nonwork_types <- c(omission, not_reached, "MISSING_CODING_IMPOSSIBLE", "NO_CODING",
                     "INTENDED_INCOMPLETE", "CODING_INCOMPLETE", "DERIVE_PENDING")
  valid_type <- !is.na(types) & !types %in% nonwork_types &
    !startsWith(ifelse(is.na(types), "", types), "MISSING_")
  reached <- basis & (valid_type | valid_untyped_basis | types %in% invalid |
    (has_value & !types %in% c(omission, not_reached)) |
    (!recode_omissions_to_not_reached & types %in% omission))
  candidate <- basis & (types %in% not_reached | (is.na(types) & !has_value & !valid_untyped_basis) |
    (recode_omissions_to_not_reached & types %in% omission))

  group_cols <- c(identifiers, "booklet_id", intersect("booklet_no", names(data)), "testlet_no")
  group_keys <- keys[c(identifiers, "booklet_id", "testlet_no")]
  if ("booklet_no" %in% names(data)) group_keys$booklet_no <- data$booklet_no
  group_ids <- dplyr::group_indices(dplyr::group_by(group_keys,
                                    dplyr::across(dplyr::all_of(group_cols))))
  last_reached <- rep(-Inf, nrow(data))
  for (rows in split(seq_len(nrow(data)), group_ids)) {
    anchors <- rows[reached[rows]]
    boundary <- if (length(anchors)) max(result$variable_order[anchors]) else -Inf
    last_reached[rows] <- boundary
    eligible <- rows[candidate[rows]]
    types[eligible] <- ifelse(result$variable_order[eligible] > boundary,
                              not_reached, omission)
  }

  invalid_override <- rep(FALSE, nrow(data))
  for (rows in occurrence_rows) {
    for (i in rows[!basis[rows]]) {
      if (!isTRUE(variable_metadata$sources_known[i])) next
      sources <- variable_metadata$basis_sources[[i]]
      if (!length(sources) || anyNA(sources)) next
      source_rows <- rows[match(sources, data$variable_id[rows])]
      if (anyNA(source_rows)) {
        cli::cli_abort("The complete {.arg data} table is missing basis source rows for {.field {data$variable_id[i]}} in unit {.field {data$unit_key[i]}}.")
      }
      if (!all(basis[source_rows])) {
        cli::cli_abort("The basis sources of {.field {data$variable_id[i]}} do not resolve to basis variables.")
      }
      in_tail <- max(result$variable_order[source_rows]) > last_reached[i]
      all_sources_missing <- all(types[source_rows] %in% c(omission, not_reached))
      if (types[i] %in% not_reached ||
          (recode_omissions_to_not_reached && types[i] %in% omission)) {
        types[i] <- if (in_tail) not_reached else omission
      } else if (recode_omissions_to_not_reached && types[i] %in% invalid &&
                 all_sources_missing && in_tail) {
        types[i] <- not_reached
        invalid_override[i] <- TRUE
      } else if (!present[i] && is.na(types[i]) && all_sources_missing) {
        types[i] <- if (in_tail) not_reached else omission
      }
    }
  }

  result$code_type <- types
  mapped <- match(types, profile$code_type)
  # Existing non-candidate derived outputs are coding results, not a missing
  # profile template. In particular, FALSE preserves invalid derived results.
  derived_changed <- !basis & (is.na(original_type) | original_type != types)
  derived_changed[is.na(derived_changed)] <- FALSE
  protected_derived <- !basis & types %in% c("MISSING_CODING_IMPOSSIBLE", "NO_CODING", invalid)
  known_sources <- !is.na(variable_metadata$sources_known) & variable_metadata$sources_known
  apply_profile <- !is.na(mapped) & !protected_derived & (basis | invalid_override |
    (known_sources & derived_changed) |
    (!basis & known_sources & original_type %in% c(omission, not_reached)))
  # Preserve the raw invalid ID on subsequent calls as well. After an allowed
  # override its analytical type is already O/NR, so the type alone no longer
  # identifies the exception. Technical status or the invalid profile ID does.
  invalid_ids <- unique(c(-98, profile$code_id[profile$code_type == invalid]))
  invalid_ids <- invalid_ids[!is.na(invalid_ids)]
  invalid_origin <- !basis & (data$code_status %in% c("INVALID", "DERIVE_ERROR") |
                                data$code_id %in% invalid_ids)
  preserve_invalid_id <- invalid_override |
    (invalid_origin & types %in% c(omission, not_reached))
  update_ids <- apply_profile & !preserve_invalid_id
  result$code_id[update_ids] <- profile$code_id[mapped[update_ids]]
  # Assign directly rather than coalescing: a custom NA score is intentional.
  result$code_score[apply_profile] <- profile$code_score[mapped[apply_profile]]
  result
}
